namespace CleverRake.XnaUtils

open Microsoft.Xna.Framework
open System.Threading

/// An update-able and drawable game component which performs light updates on the main thread,
/// then draws on a separate thread in parallel of more computation-heavy updates.
/// initialize_fun is called when assets are loaded.
/// dispose is called when the component is disposed, and should be used to unload assets.
/// update_fun takes a GameTime and a state, and should produce draw data and computation data.
/// draw_fun takes a game time and draw data and should return nothing.
/// compute_fun takes a game time and computation data and should return a new state.
type ParallelUpdateDrawGameComponent<'State, 'DrawData, 'ComputationData>
    (game,
     initial_state : 'State,
     initialize_fun : unit -> unit,
     update_fun : GameTime -> 'State -> 'DrawData * 'ComputationData,
     compute_fun : GameTime -> 'ComputationData -> 'State,
     draw_fun : GameTime -> 'DrawData -> unit,
     dispose : unit -> unit) =

    inherit DrawableGameComponent(game)

    let mutable state = initial_state
    let mutable draw_data = Unchecked.defaultof<'DrawData>
    let mutable compute_data = Unchecked.defaultof<'ComputationData>
    let mutable gt_shared = GameTime()

    let mutable enabled = true
    let mutable update_order = 0

    let signal_start = new AutoResetEvent(false)
    let mutable kill_requested = false
    let signal_done = new AutoResetEvent(false)

    let do_compute() =
        while not kill_requested do
            signal_start.WaitOne() |> ignore
            state <- compute_fun gt_shared compute_data
            signal_done.Set() |> ignore

    let compute_thread = new Thread(new ThreadStart(do_compute))
#if XBOX360
    // Set affinity
    do update_thread.SetProcessorAffinity([|3|]) // 0 and 2 are reserved, I assume the "main" thread is 1.
#endif

    // Must be called from the main thread.
    let post_compute_then_draw gt =
        if not kill_requested then
            let state = state
            gt_shared <- gt
            signal_start.Set() |> ignore
            draw_fun gt draw_data
            signal_done.WaitOne() |> ignore

    do compute_thread.Start()

    override this.Initialize() =
        base.Initialize()
        initialize_fun()

    override this.Update(gt) =
        if base.Enabled then
            base.Update(gt)
            let draw, compute = update_fun gt state
            draw_data <- draw
            compute_data <- compute

    override this.Draw(gt) =
        if base.Visible then
            base.Draw(gt)
            post_compute_then_draw gt
        else
            state <- compute_fun gt compute_data
    
    interface System.IDisposable with
        member this.Dispose() =
            base.Dispose()
            dispose()
            signal_start.Dispose()
            signal_done.Dispose()

    // member this.Dispose() = () // DrawableGameComponent.Dispose() is sealed. Thank you very much...

    member this.RequestKill() =
        kill_requested <- false
        signal_start.Set() |> ignore

    member this.WaitUntilDead() =
        compute_thread.Join()

    member this.IsDead = not(compute_thread.IsAlive)


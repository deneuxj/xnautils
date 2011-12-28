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

    let impl = new DrawableGameComponent(game)

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

    let update_thread = new Thread(new ThreadStart(do_compute))
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

    interface IGameComponent with
        member x.Initialize() =
            impl.Initialize()
            initialize_fun()

    interface IUpdateable with
        member x.Enabled = impl.Enabled
        member x.add_EnabledChanged(evh) = impl.EnabledChanged.AddHandler(evh)
        member x.remove_EnabledChanged(evh) = impl.EnabledChanged.RemoveHandler(evh)

        member x.UpdateOrder = impl.UpdateOrder
        member x.add_UpdateOrderChanged(evh) = impl.UpdateOrderChanged.AddHandler(evh)
        member x.remove_UpdateOrderChanged(evh) = impl.UpdateOrderChanged.RemoveHandler(evh)

        member x.Update(gt) =
            if impl.Enabled then
                impl.Update(gt)
                let draw, compute = update_fun gt state
                draw_data <- draw
                compute_data <- compute

    interface IDrawable with
        member x.Draw(gt) =
            if impl.Visible then
                impl.Draw(gt)
                post_compute_then_draw gt
            else
                state <- compute_fun gt compute_data

        member x.Visible = impl.Visible
        member x.add_VisibleChanged(evh) = impl.VisibleChanged.AddHandler(evh)
        member x.remove_VisibleChanged(evh) = impl.VisibleChanged.RemoveHandler(evh)

        member x.DrawOrder = impl.DrawOrder
        member x.add_DrawOrderChanged(evh) = impl.DrawOrderChanged.AddHandler(evh)
        member x.remove_DrawOrderChanged(evh) = impl.DrawOrderChanged.RemoveHandler(evh)

    member x.Dispose() =
        if update_thread.IsAlive then invalidOp "Cannot call Dispose until the draw function has been killed."
        
        dispose()
        impl.Dispose()
        signal_start.Dispose()
        signal_done.Dispose()

    interface System.IDisposable with
        member x.Dispose() = x.Dispose()

    // Must be called from the main thread.
    member x.RequestKill() =
        kill_requested <- false
        signal_start.Set() |> ignore

    member x.IsDead = not(update_thread.IsAlive)

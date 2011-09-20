namespace XNAUtils

open Microsoft.Xna.Framework
open System.Threading

/// An updateable and drawable game component which performs a light updates on the main thread,
/// then draw on a separate thread in parallel of more computation-heavy updates.
/// initialize_fun is called when assets are loaded.
/// update_fun and compute_fun take a game time and an immutable state and returns a state.
/// draw_fun takes a game time and an immutable state and returns nothing.
type ParallelUpdateDrawGameComponent<'State>(game, initial_state : 'State, initialize_fun, update_fun, compute_fun, draw_fun) =
    let impl = new DrawableGameComponent(game)

    let mutable state = initial_state
    let mutable gt_shared = GameTime()

    let mutable enabled = true
    let mutable update_order = 0

    let signal_start = new AutoResetEvent(false)
    let mutable kill_requested = false
    let signal_done = new AutoResetEvent(false)

    let do_compute() =
        while not kill_requested do
            signal_start.WaitOne() |> ignore
            state <- compute_fun gt_shared state
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
            draw_fun gt state
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
                state <- update_fun gt state

    interface IDrawable with
        member x.Draw(gt) =
            if impl.Visible then
                impl.Draw(gt)
                post_compute_then_draw gt
            else
                state <- compute_fun gt state

        member x.Visible = impl.Visible
        member x.add_VisibleChanged(evh) = impl.VisibleChanged.AddHandler(evh)
        member x.remove_VisibleChanged(evh) = impl.VisibleChanged.RemoveHandler(evh)

        member x.DrawOrder = impl.DrawOrder
        member x.add_DrawOrderChanged(evh) = impl.DrawOrderChanged.AddHandler(evh)
        member x.remove_DrawOrderChanged(evh) = impl.DrawOrderChanged.RemoveHandler(evh)

    member x.Dispose() =
        if update_thread.IsAlive then invalidOp "Cannot call Dispose until the draw function has been killed."

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

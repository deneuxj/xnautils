namespace CleverRake.XnaUtils.CoopMultiTasking.Animations

open CleverRake.XnaUtils.CoopMultiTasking.Core
open CleverRake.XnaUtils.CoopMultiTasking.Sys

open Microsoft.Xna.Framework

type FadeInOscillateFadeOut(sys : Environment, fade_in, period, fade_out) =
    let fade = ref 0.0f
    let osc = ref 0.0f

    let my_task stop = task {
        // Fade in
        let watch = sys.NewStopwatch()
        watch.Start()
        while not (!stop || watch.ElapsedSeconds >= fade_in) do
            fade := watch.ElapsedSeconds / fade_in
            do! sys.WaitNextFrame()
        fade := 1.0f

        // Oscillate
        let t = ref 0.0f
        watch.Reset()
        watch.Start()
        while not !stop do
            osc := sin (MathHelper.TwoPi * watch.ElapsedSeconds / period)
            do! sys.WaitNextFrame()
    
        // Fade out
        watch.Reset()
        watch.Start()
        while not (!stop || watch.ElapsedSeconds >= fade_out) do
            fade := 1.0f - watch.ElapsedSeconds / fade_out
            do! sys.WaitNextFrame()
        fade := 0.0f
    }

    member this.Task = my_task

    member this.Fade = !fade

    member this.Oscillation = !osc


// An animation which can be used for domino effects or "row of swimmers jumping into the wather one after another".
type MultipleFadeIn(sys : Environment, num_items, period, shift) =
    let values = Array.create num_items 0.0f

    // Wait some time, then fade from 0.0 to 1.0. The value is stored in values at the specified index.
    let delayed_fade_in stop i = task {
        let delay = shift * float32 i
        do! sys.WaitUnless(delay, fun() -> !stop)
        let watch = sys.NewStopwatch()
        watch.Start()
        while not !stop && watch.ElapsedSeconds <= period do
            values.[i] <- watch.ElapsedSeconds / period
            do! sys.WaitNextFrame()
        values.[i] <- 1.0f
    }

    // Execute a number of delayed fade ins concurrently. The delay is increaed by 'shift' for each new fade in.
    let my_task stop = task {
        let controllers =
            Array.init num_items (fun i ->
                sys.Spawn(fun stop -> delayed_fade_in stop  i)
            )

        do! sys.WaitUntil(fun() -> !stop)
        for c in controllers do
            c.Kill()

        do! sys.WaitUntil(fun() -> controllers |> Array.forall (fun c -> c.IsDead))
    }

    member this.Task = my_task

    member this.Values i = values.[i]

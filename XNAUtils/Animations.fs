module XNAUtils.Animations

open XNAUtils.CoopMultiTasking
open Microsoft.Xna.Framework

type FadeInOscillateFadeOut(sys : Environment, fade_in, period, fade_out, delta) =
    let fade = ref 0.0f
    let osc = ref 0.0f

    let my_task stop = task {
        // Fade in
        for t in 0.0f .. delta .. fade_in do
            fade := t / fade_in
            do! sys.Wait(delta)

        // Oscillate
        let t = ref 0.0f
        while not !stop do
            osc := sin (MathHelper.TwoPi * !t / period)
            do! sys.Wait(delta)
            t := !t + delta
    
        // Fade out
        for t in fade_out .. -delta .. 0.0f do
            fade := t / fade_in
            do! sys.Wait(delta)
    }

    member this.Task = my_task

    member this.Fade = !fade

    member this.Oscillation = !osc


// An animation which can be used for domino effects or "row of swimmers jumping into the wather one after another".
type MultipleFadeIn(sys : Environment, num_items, period, shift, delta) =
    let values = Array.create num_items 0.0f

    // Wait some time, then fade from 0.0 to 1.0. The value is stored in values at the specified index.
    let delayed_fade_in stop i = task {
        let delay = shift * float32 i
        do! sys.WaitUnless(delay, fun() -> !stop)
        let t = ref 0.0f
        while not !stop && !t <= period do
            values.[i] <- !t / period
            t := !t + delta
            do! sys.Wait(delta)
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

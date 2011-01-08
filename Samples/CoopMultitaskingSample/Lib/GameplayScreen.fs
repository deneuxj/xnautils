module CoopMultiTaskingSample.GameplayScreen

open Microsoft.Xna.Framework
open Microsoft.Xna.Framework.Input
open Microsoft.Xna.Framework.Graphics

open XNAUtils.CoopMultiTasking
open XNAUtils.ScreenManager
open XNAUtils.InputChanges


type private Resources =
                { font : SpriteFont
                  batch : SpriteBatch }


type GameplayScreen(sys : Environment, player) =
    inherit ScreenBase("ui")

    let rsc = ref None
    let blink = new XNAUtils.Animations.FadeInOscillateFadeOut(sys, 0.1f, 0.5f, 0.1f, 0.016f)

    let numbers = Array2D.create 3 3 0
    let target = ref 0
    let target_pos = ref None
    let has_missed = ref false
    let has_cheated = ref false
    let grace_time = ref 5.0

    let input = new InputChanges(player)

    let rnd = new System.Random()
    
    let rec nextTarget() =
        let n = rnd.Next(100)
        let exists =
            seq {
                for i in 0..2 do
                    for j in 0..2 do
                        yield numbers.[i, j] = n
            }
            |> Seq.exists id

        if exists then
            nextTarget()
        else
            n

    let rec nextNonTarget() =
        let n = rnd.Next(100)
        if n <> !target then
            n
        else
            nextNonTarget()

    let rec nextNonTargetPos() =
        let i, j = rnd.Next(3), rnd.Next(3)
        match !target_pos with
        | Some pos when pos = (i, j) -> nextNonTargetPos()
        | _ -> (i, j)

    let flipTarget() =
        rnd.Next(10) = 0

    let distract() =
        let i, j = nextNonTargetPos()
        let n = nextNonTarget()
        numbers.[i, j] <- n

    let play is_paused = task {
        let swap_period = 0.1f

        // Initialisation
        target := rnd.Next(100)
        for i in 0..2 do
            for j in 0..2 do
                numbers.[i,j] <- nextNonTarget()
        let watch = new System.Diagnostics.Stopwatch()

        while not !has_missed && not !has_cheated do
            match !target_pos, is_paused() with
            | Some _, true -> if watch.IsRunning then watch.Stop()
            | Some _, false -> if not watch.IsRunning then watch.Start()
            | None, _ -> ()

            if watch.Elapsed.TotalSeconds > !grace_time then
                has_missed := true
            else
                match !target_pos with
                | None ->
                    if input.IsButtonPress(Buttons.A) then
                        has_cheated := true

                    if flipTarget() then
                        let i, j = rnd.Next(3), rnd.Next(3)
                        target_pos := Some (i, j)
                        numbers.[i, j] <- !target
                        watch.Reset()
                        watch.Start()
                    else
                        distract()
                | Some _ ->
                    if input.IsButtonPress(Buttons.A) then
                        target_pos := None
                        target := nextTarget()
                        grace_time := 0.9 * !grace_time
                        watch.Stop()
                        watch.Reset()

                    distract()
            do! sys.Wait(swap_period)
            input.Update()

        let blinker = sys.Spawn(blink.Task)

        let input_updater =
            sys.SpawnRepeat(task {
                input.Update()
                do! sys.WaitNextFrame()
            })
        
        do! sys.WaitUntil(fun () -> input.IsButtonPress(Buttons.A))
        
        input_updater.Kill()
        blinker.Kill()
        do! sys.WaitUntil(fun () -> blinker.IsDead && input_updater.IsDead)

        return !grace_time
    }

    member this.Task =
        play (fun() -> not this.IsOnTop)
        
    override this.LoadContent() =
        match !rsc with
        | Some r -> r.batch.Dispose()
        | None -> ()

        let font : SpriteFont = base.Content.Load("font")
        rsc := Some
                { batch = new SpriteBatch(base.Game.GraphicsDevice)
                  font = font }
        
        base.LoadContent()

    override this.UnloadContent() =
        rsc := None
        base.UnloadContent()

    override this.Draw _ =
        match !rsc with
        | Some r ->
            try
                r.batch.Begin()
                
                if !has_cheated || !has_missed then
                    let k = blink.Fade * if blink.Oscillation >= 0.0f then 1.0f else 0.0f
                    let color = Color(k, k, k, k)
                    let txt =
                        if !has_cheated then
                            sprintf "You pressed too early. Your time: %5.2f" !grace_time
                        else
                            sprintf "You were too slow. Your time: %5.2f" !grace_time

                    r.batch.DrawString(r.font, txt, Vector2(100.0f, 100.0f), color)
                    
                else
                    r.batch.DrawString(r.font, sprintf "%d" !target, Vector2(100.0f, 100.0f), Color.White)

                    let left = 200.0f
                    let top = 200.0f
                    let spacing = 50.0f

                    for i in 0..2 do
                        for j in 0..2 do
                            let pos = Vector2(left + (float32 i) * spacing, top + (float32 j) * spacing)
                            r.batch.DrawString(r.font, sprintf "%d" numbers.[i,j], pos, Color.White)

            finally
                r.batch.End()

        | None -> ()
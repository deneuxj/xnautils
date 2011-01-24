module CoopMultiTaskingSample.GameplayScreen

open Microsoft.Xna.Framework
open Microsoft.Xna.Framework.Input
open Microsoft.Xna.Framework.Graphics

open XNAUtils.CoopMultiTasking
open XNAUtils.ScreenManager
open XNAUtils.InputChanges
open XNAUtils.MenuScreen

open CoopMultiTaskingSample.ResultScreen

type Resources =
    { font : SpriteFont
      batch : SpriteBatch }

type PauseMenuAction =
    | ShowInstructions
    | Abort
    | Resume

type GameplayScreen(ui_content_path, sys : Environment, player) =
    inherit ScreenBase<Resources>(ui_content_path)

    let rsc = ref None

    let numbers = Array2D.create 3 3 0
    let target = ref 0
    let target_pos = ref None
    let has_missed = ref false
    let has_cheated = ref false

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

    member private this.ShowPause : Eventually<bool> = task {
        let rec loop() = task {
            use menu =
                new MenuScreen<_>(
                    ui_content_path,
                    player,
                    sys,
                    [| (ShowInstructions, "How to play") ;
                       (Resume, "Resume") ;
                       (Abort, "Abort") |],
                    { period = 0.2f ; shift = 0.15f },
                    { left = 300.0f ; top = 100.0f ; spacing = 40.0f })
            
            this.ScreenManager.AddScreen(menu)
            let! action = menu.Task
            this.ScreenManager.RemoveScreen(menu)

            match action with
            | Some ShowInstructions ->
                use instructions = MiscScreens.mkInstructions player sys
                try
                    this.ScreenManager.AddScreen(instructions)
                    do! instructions.Task
                finally
                    this.ScreenManager.RemoveScreen(instructions)
                let! b = loop()
                return b
            | None | Some Resume ->
                return true
            | Some Abort ->
                return false
        }

        let! resume_chosen = loop()
        return resume_chosen
    }

    member this.Task = task {
        let swap_period = 0.1f

        // Initialisation
        target := rnd.Next(100)
        for i in 0..2 do
            for j in 0..2 do
                numbers.[i,j] <- nextNonTarget()
        let watch = sys.NewStopwatch()
        let score = ref 0.0
        let grace_time = ref 5.0
        let has_aborted = ref false

        // Draw the target and the matrix
        this.SetDrawer(this.DrawTarget >> this.DrawMatrix)

        // Loop until the player misses or presses too early.
        while not !has_missed && not !has_cheated && not !has_aborted do

            // Start/Stop the stopwatch, depending on whether the target number is visible and the game is paused.
            match !target_pos, this.IsActive with
            | Some _, false -> if watch.IsRunning then watch.Stop()
            | Some _, true -> if not watch.IsRunning then watch.Start()
            | None, _ -> ()

            // Wait until the game is no longer paused (or continue if it's not paused).
            do! sys.WaitUntil(fun () -> this.IsActive)

            if input.IsButtonPress(Buttons.Start) then
                // The user pressed start, bring up a pause menu
                // First stop the timer.
                let was_running = watch.IsRunning
                watch.Stop()

                let! resume_chosen = this.ShowPause
                if not resume_chosen then
                    has_aborted := true
                
                // Resume the timer if it was running when we stopped it.
                if was_running then watch.Start()

            else
                // Has the player missed?
                if watch.Elapsed.TotalSeconds > !grace_time then
                    has_missed := true
                else
                    // Check if the player presses too early and update the matrix.
                    match !target_pos with
                    | None ->
                        if input.IsButtonPress(Buttons.A) then
                            has_cheated := true

                        // Decide if we make the target number appear in the matrix at some random position...
                        if flipTarget() then
                            let i, j = rnd.Next(3), rnd.Next(3)
                            target_pos := Some (i, j)
                            numbers.[i, j] <- !target
                            watch.Reset()
                            watch.Start()
                        else
                            // ... or flip some position in the matrix to a number that is not the target.
                            distract()
                    | Some _ ->
                        // The target is visible and the player presses A:
                        // - Pick a new target
                        // - Update the score
                        // - Lower the grace time by 10%
                        if input.IsButtonPress(Buttons.A) then
                            target_pos := None
                            target := nextTarget()
                            score := !score + (!grace_time - watch.Elapsed.TotalSeconds) / !grace_time
                            grace_time := 0.9 * !grace_time
                            watch.Stop()
                            watch.Reset()

                        // Flip on some position that does not contain the target number to some random non-target number.
                        distract()

            do! sys.Wait(swap_period)
            input.Update()

        // Wait until A is no longer pressed, then wait until Start or A is pressed.
        if not !has_aborted then
            // Draw "Game over" instead of the target
            this.SetDrawer(this.DrawGameOver >> this.DrawMatrix)

            let input_updater =
                sys.SpawnRepeat(task {
                    input.Update()
                    do! sys.WaitNextFrame()
                })
        
            do! sys.WaitUntil(fun () -> not (input.IsButtonPress(Buttons.A)))
        
            do! sys.WaitUntil(fun () -> input.IsStartPressed())
        
            input_updater.Kill()
            do! sys.WaitUntil(fun () -> input_updater.IsDead)

        return
            if !has_aborted then Aborted
            elif !has_cheated then TooEarly(!grace_time, int (!score * 10000.0))
            else TooLate(!grace_time, int (!score * 10000.0))
    }
        
    override this.LoadContent() =
        match !rsc with
        | Some r -> r.batch.Dispose()
        | None -> ()

        let font : SpriteFont = base.Content.Load("font")
        rsc := Some
                { batch = new SpriteBatch(base.Game.GraphicsDevice)
                  font = font }
        
    override this.UnloadContent() =
        rsc := None

    override this.BeginDrawer() =
        match !rsc with
        | Some r -> r.batch.Begin()
        | None -> ()
        !rsc

    override this.EndDrawer(rsc) = rsc.batch.End()

    member private this.DrawTarget(rsc : Resources) =
        rsc.batch.DrawString(rsc.font, sprintf "%d" !target, Vector2(100.0f, 100.0f), Color.White)
        rsc

    member private this.DrawGameOver(rsc : Resources) =
        rsc.batch.DrawString(rsc.font, "Game over", Vector2(100.0f, 100.0f), Color.White)
        rsc

    member private this.DrawMatrix(rsc : Resources) =
        let left = 200.0f
        let top = 200.0f
        let spacing = 50.0f

        for i in 0..2 do
            for j in 0..2 do
                let pos = Vector2(left + (float32 i) * spacing, top + (float32 j) * spacing)
                rsc.batch.DrawString(rsc.font, sprintf "%d" numbers.[i,j], pos, Color.White)

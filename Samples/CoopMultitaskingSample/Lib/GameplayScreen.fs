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

type GameplayScreen(sys : Environment, player) =
    inherit ScreenBase<Resources>()

    let rsc = ref None

    let numbers = Array2D.create 3 3 0
    let target = ref 0
    let target_pos = ref None
    let score_mult = 10000.0

    let input = new InputChanges(player)

    let rnd = new System.Random()

    // Get a random number which is not currently visible in the matrix.    
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

    // Get a random number which is not the target.
    let rec nextNonTarget() =
        let n = rnd.Next(100)
        if n <> !target then
            n
        else
            nextNonTarget()

    // Get a position for the target number (must be different from the current position).
    let rec nextNonTargetPos() =
        let i, j = rnd.Next(3), rnd.Next(3)
        match !target_pos with
        | Some pos when pos = (i, j) -> nextNonTargetPos()
        | _ -> (i, j)

    // A function indicating if it is time to make the target visible.
    let flipTarget() =
        rnd.Next(10) = 0

    // Change the value of some number not in the target position to some value different from the target.
    let distract() =
        let i, j = nextNonTargetPos()
        let n = nextNonTarget()
        numbers.[i, j] <- n

    // Show a pause menu that allows to read this instruction or abort.
    member private this.ShowPause : Eventually<bool> = task {
        // Create the menu screen.
        use menu =
            new MenuScreen<_>(
                player,
                sys,
                [| (ShowInstructions, "How to play") ;
                    (Resume, "Resume") ;
                    (Abort, "Abort") |],
                { period = 0.2f ; shift = 0.15f },
                { left = 300.0f ; top = 100.0f ; spacing = 40.0f })

        let rec loop() = task {
            // Show the menu screen and wait for the user to select an entry or cancel.
            this.ScreenManager.AddScreen(menu)
            let! action = menu.Task
            // Hide the menu
            this.ScreenManager.RemoveScreen(menu)

            // Take the action corresponding to the selected entry, if any.
            // Cancelling the menu has the same effect as choosing "Resume".
            match action with
            | Some ShowInstructions ->
                // Create an instructions screen
                use instructions = MiscScreens.mkInstructions player sys
                // Show it
                this.ScreenManager.AddScreen(instructions)
                // Wait for the user to exit it.
                do! instructions.Task
                // Hide the screen.
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

    // A task that creates a "rolling digit" effect for the score.
    member private this.UpdateScore old_score new_score duration killed = task {
        let watch = sys.NewStopwatch()
        let score = ref old_score
        watch.Start()

        // Draw the score, the target and the matrix
        // Note that it's possible to embedd rendering code inside the update code,
        // which is used here for rendering the score.
        this.SetDrawer(
            fun (rsc) ->
                rsc.batch.DrawString(rsc.font, sprintf "%d" (int !score), Vector2(500.0f, 100.0f), Color.White)
                rsc
            >> this.DrawTarget
            >> this.DrawMatrix)

        while watch.Elapsed.TotalSeconds < duration && not !killed do
            if not(this.IsActive) && watch.IsRunning then
                watch.Stop()
            elif this.IsActive && not(watch.IsRunning) then
                watch.Start()
            score := score_mult * (old_score + (new_score - old_score) * watch.Elapsed.TotalSeconds / duration)
            do! sys.WaitNextFrame()

    }

    // The main task for this screen.
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
        let has_missed = ref false
        let has_cheated = ref false
        let has_aborted = ref false
        let score_updater =
            sys.Spawn(this.UpdateScore 0.0 0.0 0.0)
            |> Some
            |> ref

        // Draw the score, the target and the matrix
        // Note that it's possible to embedd rendering code inside the update code,
        // which is used here for rendering the score.
        this.SetDrawer(
            fun (rsc) ->
                rsc.batch.DrawString(rsc.font, sprintf "%d" (int !score), Vector2(500.0f, 100.0f), Color.White)
                rsc
            >> this.DrawTarget
            >> this.DrawMatrix)

        // Loop until the player loses or aborts.
        while not (!has_missed || !has_cheated || !has_aborted) do

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
                            let old_score = !score
                            score := !score + (!grace_time - watch.Elapsed.TotalSeconds) / !grace_time
                            grace_time := 0.9 * !grace_time
                            watch.Stop()
                            watch.Reset()

                            // Start rolling up the score.
                            match !score_updater with
                            | Some ctrl ->
                                // Already rolling, kill the on-going rolling effect.
                                ctrl.Kill()
                                // No need to wait until the on-going rolling effect is dead.
                                // The worst that might happen is that we can get two effects
                                // active during one frame, which is harmless.
                                // do! sys.WaitUntil(fun() -> ctrl.IsDead)
                            | None -> ()
                            // Spawn a new score rolling effect.
                            score_updater := sys.Spawn(this.UpdateScore old_score !score 1.0) |> Some

                        // Flip on some position that does not contain the target number to some random non-target number.
                        distract()

            do! sys.Wait(swap_period)
            input.Update()

        // Wait until A is no longer pressed, then wait until Start or A is pressed.
        if not !has_aborted then
            // Draw "Game over" instead of the target
            this.SetDrawer(
                fun (rsc) ->
                    rsc.batch.DrawString(rsc.font, sprintf "%d" (int (!score * score_mult)), Vector2(500.0f, 100.0f), Color.White)
                    rsc            
                >> this.DrawGameOver
                >> this.DrawMatrix)

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
            elif !has_cheated then TooEarly(!grace_time, int (!score * score_mult))
            else TooLate(!grace_time, int (!score * score_mult))
    }

    // Load a font and create a sprite batch. If a sprite batch was already created, dispose of it.        
    override this.LoadContent() =
        match !rsc with
        | Some r -> r.batch.Dispose()
        | None -> ()

        let font : SpriteFont = base.Content.Load("ui/font")
        rsc := Some
                { batch = new SpriteBatch(base.Game.GraphicsDevice)
                  font = font }

    // Remove the reference to the content, which is detected by the drawing code and prevents
    // ObjectDisposedException to be raised when content that's been unloaded is used.
    override this.UnloadContent() =
        rsc := None

    // Check if we have some content, in which case we prepare the sprite batch for rendering.
    override this.BeginDrawer() =
        match !rsc with
        | Some r -> r.batch.Begin()
        | None -> ()
        !rsc

    // Finish rendering with the sprite batch. Note that this is not called if we detected we
    // don't have any content in BeginDrawer.
    override this.EndDrawer(rsc) = rsc.batch.End()

    // Draw the target number in the upper left corner.
    member private this.DrawTarget(rsc : Resources) =
        rsc.batch.DrawString(rsc.font, sprintf "%d" !target, Vector2(100.0f, 100.0f), Color.White)
        rsc

    // Draw "Game over" in place of the target number.
    member private this.DrawGameOver(rsc : Resources) =
        rsc.batch.DrawString(rsc.font, "Game over", Vector2(100.0f, 100.0f), Color.White)
        rsc

    // Draw the matrix.
    member private this.DrawMatrix(rsc : Resources) =
        let left = 200.0f
        let top = 200.0f
        let spacing = 50.0f

        for i in 0..2 do
            for j in 0..2 do
                let pos = Vector2(left + (float32 i) * spacing, top + (float32 j) * spacing)
                rsc.batch.DrawString(rsc.font, sprintf "%d" numbers.[i,j], pos, Color.White)

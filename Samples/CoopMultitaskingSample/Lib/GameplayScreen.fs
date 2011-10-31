module CoopMultiTaskingSample.GameplayScreen

open Microsoft.Xna.Framework
open Microsoft.Xna.Framework.Input
open Microsoft.Xna.Framework.Graphics

open CleverRake.XnaUtils
open CleverRake.XnaUtils.Application
open CleverRake.XnaUtils.CoopMultiTasking
open CleverRake.XnaUtils.XnaExtensions

open CoopMultiTaskingSample.ResultScreen

type Resources =
    { font : SpriteFont
      batch : SpriteBatch }

type PauseMenuAction =
    | ShowInstructions
    | Abort
    | Resume

type GameplayScreen(sys : Environment, player) =
    let impl = new ScreenBase<Resources>()
    let impl_screen = impl :> Screen

    let rsc = ref None

    let numbers = Array2D.create 3 3 0
    let target = ref 0
    let target_pos = ref None
    let score_mult = 10000.0

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
            let! action = impl.ScreenManager.AddDoRemove(menu, menu.Task)

            // Take the action corresponding to the selected entry, if any.
            // Cancelling the menu has the same effect as choosing "Resume".
            match action with
            | Some ShowInstructions ->
                // Create an instructions screen
                use instructions = MiscScreens.mkInstructions(player, sys)
                // Show it
                do! impl.ScreenManager.AddDoRemove(instructions, instructions.Task)
                return! loop()
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
        impl.Drawer <-
            fun (rsc) ->
                rsc.batch.DrawString(rsc.font, sprintf "%d" (int !score), Vector2(500.0f, 100.0f), Color.White)
                rsc
            >> this.DrawTarget
            >> this.DrawMatrix

        while watch.Elapsed.TotalSeconds < duration && not !killed do
            if not(impl.IsActive) && watch.IsRunning then
                watch.Stop()
            elif impl.IsActive && not(watch.IsRunning) then
                watch.Start()
            score := score_mult * (old_score + (new_score - old_score) * watch.Elapsed.TotalSeconds / duration)
            do! sys.WaitNextFrame()

    }

    // The main task for this screen.
    member this.Task = task {

        // The matrix update period.
        let swap_period = 0.1f

        // The first target number.
        target := rnd.Next(100)

        // Fill in the matrix with non-target numbers.
        for i in 0..2 do
            for j in 0..2 do
                numbers.[i,j] <- nextNonTarget()

        // The score: measures how fast the player reacts to the target number appearing in the matrix.
        let score = ref 0.0

        // Maximum amount of time the player has to react to the target number appearing in the matrix.
        // Decreased after each successful button press.
        let grace_time = ref 5.0

        // True of the player failed to react within the grace time.
        let has_missed = ref false

        // True if the player pressed the button when the target number was not visible.
        let has_cheated = ref false

        // True if the user selects "Abort" in the pause menu.
        let has_aborted = ref false

        impl.PreDrawer <- this.PreDraw

        // Draw the score, the target and the matrix
        // Note that it's possible to embedd rendering code inside the update code,
        // which is used here for rendering the score.
        impl.Drawer <-
            fun (rsc) ->
                rsc.batch.DrawString(rsc.font, sprintf "%d" (int !score), Vector2(500.0f, 100.0f), Color.White)
                rsc
            >> this.DrawTarget
            >> this.DrawMatrix

        impl.PostDrawer <- this.PostDraw

        let gameIsOver() = (!has_missed || !has_cheated || !has_aborted)

        // A task that pauses and resumes a watch depending on whether the screen is active or not.
        // Meant to be spawned with SpawnRepeat.
        let controlWatch (watch : Stopwatch) = task {
            match impl.IsActive, watch.IsRunning with
            | false, true -> watch.Stop()
            | true, false -> watch.Start()
            | false, false | true, true -> ()
            return! sys.WaitNextFrame()
        }

        // The watch used to measure the player's reaction time, i.e.
        // the time between the target number becomes visible and the
        // user presses A.
        let reaction_watch = sys.NewStopwatch()

        // A task that updates the matrix at regular intervals.
        let updateMatrix killed = task {
            let watch = sys.NewStopwatch()
            let watch_controller = sys.SpawnRepeat(controlWatch watch)

            while not(gameIsOver() || !killed) do
                if watch.ElapsedSeconds >= swap_period then
                    watch.Reset()
                    // Decide if we make the target number appear in the matrix at some random position...
                    if (!target_pos).IsNone && flipTarget() then
                        let i, j = rnd.Next(3), rnd.Next(3)
                        target_pos := Some (i, j)
                        numbers.[i, j] <- !target
                        reaction_watch.Reset() // No need to start the watch, controlWatch takes care of that.
                    else
                        // ... or flip some position in the matrix to a number that is not the target.
                        distract()
                do! sys.WaitNextFrame()

            watch_controller.Kill()
            return! sys.WaitUntil(fun() -> watch_controller.IsDead)
        }

        // A task that checks if the user presses the button fast enough and not too early.
        let challenge killed = task {
            let watch_controller = sys.SpawnRepeat(controlWatch reaction_watch)
            let input = new InputChanges(player)
            let score_updater =
                sys.Spawn(this.UpdateScore 0.0 0.0 0.0)
                |> Some
                |> ref

            while not(gameIsOver() || !killed) do
                do! sys.WaitNextFrame()
                input.Update()
                // Has the player missed?
                if (!target_pos).IsSome && reaction_watch.Elapsed.TotalSeconds > !grace_time then
                    has_missed := true
                elif impl.IsActive then
                    // Check if the player presses too early.
                    match !target_pos with
                    | None ->
                        if input.IsButtonPress(Buttons.A) then
                            has_cheated := true

                    | Some _ ->
                        // The target is visible and the player presses A:
                        // - Pick a new target
                        // - Update the score
                        // - Lower the grace time by 10%
                        if input.IsButtonPress(Buttons.A) then
                            target_pos := None
                            target := nextTarget()
                            let old_score = !score
                            score := !score + (!grace_time - reaction_watch.Elapsed.TotalSeconds) / !grace_time
                            grace_time := 0.9 * !grace_time
                            reaction_watch.Reset()

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

            watch_controller.Kill()
            return! sys.WaitUntil(fun() -> watch_controller.IsDead)
        }

        // Start updating the matrix is watching the player's input
        let matrixUpdater = sys.Spawn(updateMatrix)
        let challenger = sys.Spawn(challenge)

        // Start a task that opens a pause menu.
        // When the pause menu is shown, a new screen is added, which causes
        // this screen to stop being active.
        // This is detected and handled in the matrix updater and the challenger.
        let pause_controller =
            sys.SpawnRepeat(
                let input = new InputChanges(player)
                task {
                    input.Update()
                    if impl.IsActive && input.IsButtonPress(Buttons.Start) then
                        let! resume_chosen = this.ShowPause
                        if not resume_chosen then
                            has_aborted := true
                    return! sys.WaitNextFrame()
                }
            )

        // Let all tasks we just spawned until the game is over.
        do! sys.WaitUntil(gameIsOver)

        // Kill all tasks. Not necessary, as they should exit of their own accord.
        matrixUpdater.Kill()
        challenger.Kill()
        pause_controller.Kill()

        // Wait for all tasks to die properly.
        do! sys.WaitUntil(fun() -> matrixUpdater.IsDead && challenger.IsDead && pause_controller.IsDead)

        // Unless the player aborted, wait until A is no longer pressed, then wait until Start or A is pressed.
        if not !has_aborted then
            // Draw "Game over" instead of the target
            impl.Drawer <-
                fun (rsc) ->
                    rsc.batch.DrawString(rsc.font, sprintf "%d" (int (!score * score_mult)), Vector2(500.0f, 100.0f), Color.White)
                    rsc            
                >> this.DrawGameOver
                >> this.DrawMatrix

            let input = new InputChanges(player)
            let input_updater =
                sys.SpawnRepeat(task {
                    input.Update()
                    do! sys.WaitNextFrame()
                })
        
            do! sys.WaitUntil(fun () -> impl.IsActive && not (input.IsButtonPress(Buttons.A)))
        
            do! sys.WaitUntil(fun () -> impl.IsActive && input.IsStartPressed())
        
            input_updater.Kill()
            do! sys.WaitUntil(fun () -> input_updater.IsDead)

        return
            if !has_aborted then Aborted
            elif !has_cheated then TooEarly(!grace_time, int (!score * score_mult))
            else TooLate(!grace_time, int (!score * score_mult))
    }

    interface Screen with
        member this.ClearScreenManager() = impl_screen.ClearScreenManager()
        member this.Draw() = impl_screen.Draw()
        member this.SetGame(ng) = impl_screen.SetGame(ng)
        member this.SetIsOnTop(b) = impl_screen.SetIsOnTop(b)
        member this.SetScreenManager(sm) = impl_screen.SetScreenManager(sm)

        // Load a font and create a sprite batch. If a sprite batch was already created, dispose of it.        
        member this.LoadContent() =
            impl_screen.LoadContent()
            match !rsc with
            | Some r -> r.batch.Dispose()
            | None -> ()

            let font : SpriteFont = impl.Content.Load("ui/font")
            rsc := Some
                    { batch = new SpriteBatch(impl.Game.GraphicsDevice)
                      font = font }

        // Remove the reference to the content, which is detected by the drawing code and prevents
        // ObjectDisposedException to be raised when content that's been unloaded is used.
        member this.UnloadContent() =
            rsc := None

    interface System.IDisposable with
        member this.Dispose() = (impl :> System.IDisposable).Dispose()

    // Check if we have some content, in which case we prepare the sprite batch for rendering.
    member private this.PreDraw() =
        match !rsc with
        | Some r -> r.batch.Begin()
        | None -> ()
        !rsc

    // Finish rendering with the sprite batch. Note that this is not called if we detected we
    // don't have any content in BeginDrawer.
    member private this.PostDraw(rsc) = rsc.batch.End()

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

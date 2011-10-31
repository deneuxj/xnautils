module CoopMultiTaskingSample.Main

open Microsoft.Xna.Framework
open Microsoft.Xna.Framework.GamerServices

open CleverRake.XnaUtils
open CleverRake.XnaUtils.Application
open CleverRake.XnaUtils.StorageTasks
open CleverRake.XnaUtils.CoopMultiTasking
open CleverRake.XnaUtils.CoopMultiTasking.Core
open CleverRake.XnaUtils.CoopMultiTasking.Sys
open CleverRake.XnaUtils.XnaExtensions

open GameplayScreen
open ResultScreen
open MiscScreens
open UserSettings.Module
open CoopMultiTaskingSample.UserSettings
open CoopMultiTaskingSample.ScoreScreen

// The state of the top level, used to react to sign-out.
type TopState =
    | Initializing
    | AtPressStartScreen
    | AnonPlayer
    | Player of SignedInGamer
    | KillingAllTasks
    | Exiting
with
    member this.Update(transition) =
        match this, transition with
        | Initializing, InitDone -> AtPressStartScreen
        | Initializing, _ -> invalidOp "Invalid transition from Initializing"

        | AtPressStartScreen, AnonPressedStart -> AnonPlayer
        | AtPressStartScreen, PlayerPressedStart(p) -> Player p
        | AtPressStartScreen, _ -> invalidOp "Invalid transition from AtPressStartScreen"

        | AnonPlayer, ExitGame | Player _, ExitGame -> Exiting

        | AnonPlayer, Back -> AtPressStartScreen
        | AnonPlayer, _ -> invalidOp "Invalid transition from AnonPlayer"

        | Player p, SignOut -> KillingAllTasks
        | Player p, Back -> AtPressStartScreen
        | Player p, _ -> invalidOp "Invalid transition from Player"

        | KillingAllTasks, AllTasksKilled -> Initializing
        | KillingAllTasks, _ -> invalidOp "Invalid transition from KillingAllTasks"

        | Exiting, _ -> invalidOp "Invalid transition from Exiting"


and TopStateTransition =
    | InitDone
    | AnonPressedStart
    | PlayerPressedStart of SignedInGamer
    | SignOut
    | AllTasksKilled
    | Back
    | ExitGame


type SignedInRequirement =
    | Free = 0
    | ShouldSignIn = 1
    | MustSignIn = 2

// Entries in the main menu.
type MainMenuEntries =
    | Play
    | Instructions
    | Options
    | Scores
    | Credits
    | BuyFullVersion
    | Exit


// The "main" class, which manages the top levels of the screen hierarchy.
// The type parameter 'G is the type of the game class created in the C# top-level.
// ISettingsNotifiable is an interface that components affected by user settings must implement.
type Main<'G  when 'G :> Game and 'G :> ISettingsNotifiable>(game : 'G, screen_manager : ScreenManager, sign_in_req) =
    inherit DrawableGameComponent(game)

    // Keep track of the top state.
    let top_state = ref Initializing
    
    // List of top state transition requests
    let transition_requests = ref []

    // The scheduler which executes slices of tasks every update cycle.
    let scheduler = new Scheduler()

    // The environment which tasks use to interact with the scheduler.
    let sys = new Environment(scheduler)

    // The storage component, makes loading/saving easier.
    let storage = new Storage(true)

    // The user settings, which are user-specific.
    let settings = ref(new UserSettings.Data())

    // The scores, which are shared across users.
    let scores = ref(new Scores())

    // When true, exit the game back to the board.
    let exit_game = ref false

    // The main menu loop:
    // - show the main menu
    // - wait for a selection
    // - do something based in the selection
    //
    // The main menu is showed after the press start screen.
    // This task is called from main_task, see below.
    let rec menu_loop controlling_player : Eventually<unit> = task {

        // A task to show a table of the 10 best scores, and wait for a button press.
        let showScores = task {
            use score_screen = new ScoreScreen(sys, controlling_player, !scores)
            return! screen_manager.AddDoRemove(score_screen, score_screen.Task)
        }

        // The menu screen.
        // We instantiate a new one every time we come back to the menu.
        // We could have reused the same one, but the extra instantiations don't cost much.
        use menu =
            new MenuScreen<_>(
                controlling_player,
                sys,
                [| Play, "Play now"
                   Instructions, "How to play"
                   Options, "Options"
                   Scores, "Scores"
                   Credits, "More information"
                   BuyFullVersion, "BuyFullVersion"
                   Exit, "Exit" |],
                menu_animation,
                menu_placement
            )

        // Hide and disable entries depending on whether we are running the full version.
        if Guide.IsTrialMode then
            menu.Disable(Credits)
        else
            menu.Hide(BuyFullVersion)

        // If running in trial mode, run a "watcher" that detects when the user unlocks the full version.
        // When that happens, hide BuyFullVersion and enable Credits in the menu.
        let maybe_buy_watcher =
            if Guide.IsTrialMode then
                let rec watcher killed = task {
                    if not !killed then
                        if not Guide.IsTrialMode then
                            menu.Hide(BuyFullVersion)
                            menu.Show(Credits)
                        else
                            do! sys.WaitNextFrame()
                            return! watcher killed
                }
                sys.Spawn(watcher)
                |> Some
            else
                None

        // Kill the watcher when the iteration of the menu loop ends.
        use _ =
            { new System.IDisposable with
                member this.Dispose() =
                    match maybe_buy_watcher with
                    | Some w -> w.Kill()
                    | None -> ()
            }

        // Show the menu
        let! action = screen_manager.AddDoRemove(menu, menu.Task)

        // Deal with the selection in the menu.
        match action with
        // Menu was cancelled (B, Back, sign-out), leave the menu loop and go back to the "press start" screen.
        | None -> ()
        
        // Back to the board.
        | Some Exit ->
            exit_game := true
        
        // Start playing.
        | Some Play ->
            // Create the screen showing the game.
            use gameplay = new GameplayScreen(sys, controlling_player)

            // Show the gameplay screen. Gameplay itself is in gameplay.Task
            let! gameplay_result = screen_manager.AddDoRemove(gameplay, gameplay.Task)

            // If the game wasn't aborted, and if a new high score was achieved,
            // add it to the score table and show the table.
            match gameplay_result with
            | Aborted _ -> ()
            | TooEarly (_, points) | TooLate (_, points) ->
                use results = new ResultScreen(sys, controlling_player, gameplay_result)
                do! screen_manager.AddDoRemove(results, results.Task)
                let player_name =
                    match SignedInGamer.SignedInGamers.ItemOpt(controlling_player) with
                    | None -> "Unknown"
                    | Some player -> player.Gamertag
                let is_hiscore = (!scores).AddScore(player_name, points)
                if is_hiscore then
                    // save the scores.
                    if storage.TitleStorage.IsSome then
                        do! storage.CheckTitleStorage
                        let! result = storage.DoTitleStorage(score_container, saveXml score_filename !scores)
                        match result with
                        | Some(Some()) -> ()
                        | _ -> do! doOnGuide <| fun() -> error "Failed to save scores"
                    // Show the scores screen.
                    do! showScores

            // Continue looping. Note the use of "return!" instead of "do!".
            // Using do! for recursive calls can lead to stack overflows.
            return! menu_loop controlling_player

        // Show a screen of text with instructions on how to play the game.
        | Some Instructions ->
            use instructions = mkInstructions(controlling_player, sys)
            do! screen_manager.AddDoRemove(instructions, instructions.Task)
            return! menu_loop controlling_player

        // Show and set user settings.
        | Some Options ->
            use settings_screen = mkUserSettingsScreen controlling_player sys menu_animation menu_placement storage
            // A recursive function that repeatedly shows the user settings menu.
            let rec work () = task {
                let! choice = screen_manager.AddDoRemove(settings_screen, settings_screen.Task)
                match choice with
                | Some _ ->
                    let! data' = handleUserSettingsMenu controlling_player sys menu_animation menu_placement screen_manager !settings choice
                    settings := data'
                    data'.Apply(game)
                    return! work()
                | None ->
                    // The player wants to go back to the main menu, now is a good time to save the settings.
                    if storage.PlayerStorage.IsSome then
                        // CheckPlayerStorage checks if the storage device is still connected.
                        // If not, it asks the user for a new one.
                        do! storage.CheckPlayerStorage
                        let! result = storage.DoPlayerStorage(user_container, saveXml user_settings_filename !settings)
                        match result with
                        | Some(Some()) -> () // Success, no error message to show.
                        | _ ->
                            do! doOnGuide <| fun() -> error "Failed to save user settings."
                    
                    // Go back to the main menu.
                    // Note that menu_loop and work are mutually recursive.
                    // As they both have their recursive calls at the "tail",
                    // no stack space is wasted.
                    return! menu_loop controlling_player
            }
            return! work()

        // Show the high-scores table.
        | Some Scores ->
            do! showScores
            return! menu_loop controlling_player

        // Show a screen of text with pointers to F# and XNA.
        | Some Credits ->
            use info = mkInfo(controlling_player, sys)
            do! screen_manager.AddDoRemove(info, info.Task)
            return! menu_loop controlling_player

        // The user wants to buy the full version
        | Some BuyFullVersion ->
            // check if the user has the rights
            match Gamer.SignedInGamers.ItemOpt(controlling_player) with
            | Some player ->
                if player.IsSignedInToLive && player.Privileges.AllowPurchaseContent then
                    do! doOnGuide <| fun() -> Guide.ShowMarketplace(controlling_player)
                else
                    do! doOnGuide <| fun() -> error "You do not have the permissions to buy content."
            | None ->
                do! doOnGuide <| fun() -> error "You must be signed in to buy content."
            return! menu_loop controlling_player

    }

    // The main task, which shows the "press start" screen and then shows the main menu.
    // The game is exited by selecting "Exit" in the main menu.
    // Pressing B or Back in the main menu brings back to the "press start" screen.
    let main_task = task {
        while not !exit_game do
            // Set the user settings to the default values.
            // We don't want the settings of the previous user to be used when they
            // leave the main menu for the "press start" screen.
            settings := new UserSettings.Data()
            (!settings).Apply(game)

            // Create the "press start" screen.
            use press_start = new PressStartScreen.PressStartScreen(sys, 0.5f, 0.1f, 0.5f)

            // Show it.
            let! controlling_player = screen_manager.AddDoRemove(press_start, press_start.Task)

            // Ask the player to sign in unless already signed in or there are no sign in requirement.
            if sign_in_req <> SignedInRequirement.Free && not(Gamer.IsSignedIn(controlling_player)) then
                do! doOnGuide <| fun () -> Guide.ShowSignIn(1, false)
                do! sys.WaitUntil(fun () -> Guide.IsVisible)
                do! sys.WaitUntil(fun () -> not (Guide.IsVisible))

            // Proceed only if the user actually signed in unless signing in is not strictly required.
            if sign_in_req = SignedInRequirement.MustSignIn && not(Gamer.IsSignedIn(controlling_player)) then
                do! doOnGuide <| fun () -> error "You must sign in to play this game."
            else
                // Ask the user to choose for a storage device used for the scores.
                do! storage.InitTitleStorage

                // Ask again for a storage used for the user settings.
                storage.Player <- Some controlling_player
                do! storage.InitPlayerStorage

                // Load user settings, if any. Otherwise, use default values.
                let! data = task {
                    if storage.PlayerStorage.IsSome then
                        do! storage.CheckPlayerStorage
                        let! maybe_data = storage.DoPlayerStorage(user_container, loadXml user_settings_filename)
                        match maybe_data with
                        | Some(Some d) -> return d
                        | _ -> return (new Data())
                    else
                        return (new Data())
                }
                settings := data

                // Apply the user settings, which affect the background color and the font size.
                (!settings).Apply(game)

                // Load scores.
                let! data = task {
                    if storage.TitleStorage.IsSome then
                        do! storage.CheckTitleStorage
                        let! maybe_score = storage.DoTitleStorage(score_container, loadXml score_filename)
                        match maybe_score with
                        | Some(Some d) -> return d
                        | _ -> return (new Scores())
                    else
                        return (new Scores())
                }
                scores := data

                transition_requests :=
                    if Gamer.IsSignedIn(controlling_player) then
                        PlayerPressedStart (Gamer.SignedInGamers.ItemOpt(controlling_player).Value)
                    else
                        AnonPressedStart
                    :: !transition_requests

                // Go into the main menu loop
                do! menu_loop controlling_player

                transition_requests :=
                    if !exit_game then
                        ExitGame :: !transition_requests
                    else
                        Back :: !transition_requests
    }

    // Every update cycle, instruct the scheduler to run slices of all tasks that
    // are registered.
    override this.Update(gt) =
        // Deal with transition requests for the top state
        for t in List.rev !transition_requests do
            top_state := (!top_state).Update(t)
        transition_requests := []

        // Initialization and killing of tasks.
        // Killing happens when a signed in player signs out.
        // Initialization happens during start-up and after killing.
        match !top_state with
        | Initializing ->
            scheduler.AddTask(main_task)
            top_state := (!top_state).Update(InitDone)
        | Player p when not(Gamer.IsSignedIn(p.PlayerIndex)) ->
            top_state := (!top_state).Update(SignOut)
            sys.StartKillAll(null)
        | KillingAllTasks when not(scheduler.HasLiveTasks) ->
            sys.StopKillAll()
            top_state := (!top_state).Update(AllTasksKilled)
            // Ideally, screen removal is done in finally handlers, and
            // killing should take care of removing all screens.
            // Nevertheless, we remove all screens here to be on the safe side.
            screen_manager.RemoveAllScreens()
        | _ -> ()

        // Exit the game if we are exiting and all tasks have completed.
        if not(scheduler.HasLiveTasks) && !top_state = Exiting then
            game.Exit()

        // Update screens when the game is active.
        if game.IsActive then
            scheduler.RunFor(float32 gt.ElapsedGameTime.TotalSeconds)

        // If exceptions other than TaskKilled were raised and left uncaught, re-raise them.
        // This will cause the game to die "properly", showing an error message box.
        // For a game that's been made avalaible on the market, this should never happen.
        // Nevertheless, silently ignoring exceptions is not a good idea.
        for e in
            scheduler.Exceptions
            |> List.filter(fun e -> not(e :? TaskKilled))
            do
            raise (new System.Exception("A task raised an exception", e))
        scheduler.ClearExceptions()

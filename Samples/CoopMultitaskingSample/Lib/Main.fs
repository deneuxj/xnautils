module CoopMultiTaskingSample.Main

open Microsoft.Xna.Framework

open XNAUtils.CoopMultiTasking
open XNAUtils.ScreenManager
open XNAUtils.PressStartScreen
open XNAUtils.MenuScreen
open XNAUtils.TextScreen
open XNAUtils.StorageTasks

open GameplayScreen
open ResultScreen
open MiscScreens
open UserSettings

type MainMenuEntries =
    | Play
    | Instructions
    | Options
    | Scores
    | Credits
    | Exit


type Main<'G  when 'G :> Game and 'G :> IBackgroundColorHolder>(game : 'G, screen_manager : ScreenManager) =
    inherit DrawableGameComponent(game)

    let scheduler = new Scheduler()
    let sys = new Environment(scheduler)
    let storage = new Storage(true)

    let menu_placement =
        { left = 300.0f;
          top = 100.0f;
          spacing = 40.0f }

    let menu_animation =
        { period = 0.2f;
          shift = 0.05f }

    let rec menu_loop exit_game controlling_player data = task {

        use menu =
            new MenuScreen<_>(
                content_path,
                controlling_player,
                sys,
                [| Play, "Play now"
                   Instructions, "How to play"
                   Options, "Options"
                   Scores, "Scores"
                   Credits, "Credits"
                   Exit, "Exit" |],
                menu_animation,
                menu_placement
            )
        screen_manager.AddScreen(menu)
        let! action = menu.Task
        screen_manager.RemoveScreen(menu)

        match action with
        | None -> () // Back to press start screen.
        | Some Exit ->
            exit_game := true
        | Some Play ->
            use gameplay = new GameplayScreen(content_path, sys, controlling_player)
            screen_manager.AddScreen(gameplay)
            let! gameplay_result = gameplay.Task
            screen_manager.RemoveScreen(gameplay)

            match gameplay_result with
            | Aborted _ -> ()
            | _ ->
                use results = new ResultScreen(content_path, sys, controlling_player, gameplay_result)
                screen_manager.AddScreen(results)
                do! results.Task
                screen_manager.RemoveScreen(results)

            do! menu_loop exit_game controlling_player data
        | Some Instructions ->
            use instructions = mkInstructions controlling_player sys
            screen_manager.AddScreen(instructions)
            do! instructions.Task
            screen_manager.RemoveScreen(instructions)
            do! menu_loop exit_game controlling_player data
        | Some Options ->
            use settings = mkUserSettingsScreen content_path controlling_player sys menu_animation menu_placement storage
            screen_manager.AddScreen(settings)
            let! choice = settings.Task
            screen_manager.RemoveScreen(settings)
            let! data' = handleUserSettingsMenu content_path controlling_player sys menu_animation menu_placement screen_manager storage data choice
            data'.Apply(game)
            do! menu_loop exit_game controlling_player data'
        | _ -> () // TODO. For now we send back to the press start screen
    }

    let main_task = task {
        let exit_game = ref false
        while not !exit_game do
            use press_start = new PressStartScreen(content_path, sys, 0.5f, 0.1f, 0.5f)
            screen_manager.AddScreen(press_start)
            let! controlling_player = press_start.Task
            screen_manager.RemoveScreen(press_start)

            if storage.TitleStorage.IsNone then
                do! storage.InitTitleStorage

            storage.Player <- Some controlling_player
            do! storage.InitPlayerStorage

            let! data = task {
                if storage.PlayerStorage.IsSome then
                    let! maybe_data = storage.DoPlayerStorage(user_container, loadXml user_settings_filename)
                    match maybe_data with
                    | Some(Some d) -> return d
                    | _ -> return (new Data())
                else
                    return (new Data())
            }
            data.Apply(game)
            do! menu_loop exit_game controlling_player data
    }

    override this.Initialize() =
        scheduler.AddTask(main_task)
        base.Initialize()

    override this.LoadContent() =
        base.LoadContent()

    override this.Update(gt) =
        if not(scheduler.HasLiveTasks) then
            game.Exit()

        scheduler.RunFor(float32 gt.ElapsedGameTime.TotalSeconds)

    override this.Draw(gt) =
        base.Draw(gt)
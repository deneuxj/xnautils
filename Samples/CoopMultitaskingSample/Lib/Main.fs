module CoopMultiTaskingSample.Main

open Microsoft.Xna.Framework
open Microsoft.Xna.Framework.GamerServices

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
open ScoreScreen

type MainMenuEntries =
    | Play
    | Instructions
    | Options
    | Scores
    | Credits
    | Exit


type Main<'G  when 'G :> Game and 'G :> ISettingsNotifiable>(game : 'G, screen_manager : ScreenManager) =
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

    let rec menu_loop exit_game controlling_player data (scores : Scores) : Eventually<Scores> = task {

        let showScores = task {
            use score_screen = new ScoreScreen(sys, controlling_player, scores)
            screen_manager.AddScreen(score_screen)
            do! score_screen.Task
            screen_manager.RemoveScreen(score_screen)
        }

        use menu =
            new MenuScreen<_>(
                controlling_player,
                sys,
                [| Play, "Play now"
                   Instructions, "How to play"
                   Options, "Options"
                   Scores, "Scores"
                   Credits, "More information"
                   Exit, "Exit" |],
                menu_animation,
                menu_placement
            )
        screen_manager.AddScreen(menu)
        let! action = menu.Task
        screen_manager.RemoveScreen(menu)

        match action with
        | None ->
            return scores
        | Some Exit ->
            exit_game := true
            return scores
        | Some Play ->
            use gameplay = new GameplayScreen(sys, controlling_player)
            screen_manager.AddScreen(gameplay)
            let! gameplay_result = gameplay.Task
            screen_manager.RemoveScreen(gameplay)

            match gameplay_result with
            | Aborted _ -> ()
            | TooEarly (_, points) | TooLate (_, points) ->
                use results = new ResultScreen(sys, controlling_player, gameplay_result)
                screen_manager.AddScreen(results)
                do! results.Task
                screen_manager.RemoveScreen(results)
                let player_name =
                    match SignedInGamer.SignedInGamers.[controlling_player] with
                    | null -> "Unknown"
                    | player -> player.Gamertag
                let is_hiscore = scores.AddScore(player_name, points)
                if is_hiscore then
                    do! showScores

            return! menu_loop exit_game controlling_player data scores
        | Some Instructions ->
            use instructions = mkInstructions(controlling_player, sys)
            screen_manager.AddScreen(instructions)
            do! instructions.Task
            screen_manager.RemoveScreen(instructions)
            return! menu_loop exit_game controlling_player data scores
        | Some Options ->
            use settings = mkUserSettingsScreen controlling_player sys menu_animation menu_placement storage
            let rec work data = task {
                screen_manager.AddScreen(settings)
                let! choice = settings.Task
                screen_manager.RemoveScreen(settings)
                match choice with
                | Some _ ->
                    let! data' = handleUserSettingsMenu controlling_player sys menu_animation menu_placement screen_manager data choice
                    data'.Apply(game)
                    return! work data'
                | None ->
                    if storage.PlayerStorage.IsSome then
                        do! storage.CheckPlayerStorage
                        let! result = storage.DoPlayerStorage(user_container, saveXml user_settings_filename data)
                        if result.IsNone then
                            do! doOnGuide <| fun() -> error "Failed to save user settings."
                    return! menu_loop exit_game controlling_player data scores
            }
            return! work data
        | Some Scores ->
            do! showScores
            return! menu_loop exit_game controlling_player data scores
        | Some Credits ->
            use info = mkInfo(controlling_player, sys)
            screen_manager.AddScreen(info)
            do! info.Task
            screen_manager.RemoveScreen(info)
            return! menu_loop exit_game controlling_player data scores
    }

    let main_task = task {
        let exit_game = ref false
        while not !exit_game do
            use press_start = new PressStartScreen(sys, 0.5f, 0.1f, 0.5f)
            screen_manager.AddScreen(press_start)
            let! controlling_player = press_start.Task
            screen_manager.RemoveScreen(press_start)

            if storage.TitleStorage.IsNone then
                do! storage.InitTitleStorage

            storage.Player <- Some controlling_player
            do! storage.InitPlayerStorage

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
            data.Apply(game)

            let! scores = task {
                if storage.TitleStorage.IsSome then
                    do! storage.CheckTitleStorage
                    let! maybe_score = storage.DoTitleStorage(score_container, loadXml score_filename)
                    match maybe_score with
                    | Some(Some d) -> return d
                    | _ -> return (new Scores())
                else
                    return (new Scores())
            }

            let! scores = menu_loop exit_game controlling_player data scores

            if storage.TitleStorage.IsSome then
                do! storage.CheckTitleStorage
                let! result = storage.DoTitleStorage(score_container, saveXml score_filename scores)
                match result with
                | Some(Some()) -> ()
                | _ -> do! doOnGuide <| fun() -> error "Failed to save scores"

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

        match scheduler.Exceptions with
        | e :: _ -> raise (new System.Exception("A task raised an exception", e))
        | _ -> ()

    override this.Draw(gt) =
        base.Draw(gt)
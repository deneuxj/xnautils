module CoopMultiTaskingSample.Main

open Microsoft.Xna.Framework

open XNAUtils.CoopMultiTasking
open XNAUtils.ScreenManager
open XNAUtils.PressStartScreen
open XNAUtils.MenuScreen
open XNAUtils.TextScreen

open CoopMultiTaskingSample.GameplayScreen

type MainMenuEntries =
    | Play
    | Instructions
    | Options
    | Scores
    | Credits
    | Exit

let content_path = "ui"

type Main(game : Game, screen_manager : ScreenManager) =
    inherit DrawableGameComponent(game)

    let scheduler = new Scheduler()
    let sys = new Environment(scheduler)

    let rec menu_loop exit_game controlling_player = task {
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
                { period = 0.15f
                  shift = 0.20f
                  delta = 0.016f },
                { left = 300.0f
                  top = 100.0f
                  spacing = 40.0f }
            )
        screen_manager.AddScreen(menu)
        let! action = menu.Task
        screen_manager.RemoveScreen(menu)

        match action with
        | None -> () // Back to press start screen.
        | Some Exit ->
            exit_game := true
        | Some Play ->
            use gameplay = new GameplayScreen(sys, controlling_player)
            screen_manager.AddScreen(gameplay)
            let! best_time = gameplay.Task
            screen_manager.RemoveScreen(gameplay)
            do! menu_loop exit_game controlling_player
        | Some Instructions ->
            use instructions =
                new TextScreen(content_path, controlling_player, sys,
                                [| "The game screen shows a number in the upper left corner"
                                   "and a 3x3 matrix in the middle of the screen."
                                   "Whenever you see the number in the matrix, press A."
                                   "You must press A within a delay, otherwise the game ends."
                                   "The delay decreases the further in the game you go."
                                   "Pressing A when the number is not in the matrix"
                                   "ends the game."
                                   ""
                                   "Press B to go back to the menu." |],
                                { left = 100.0f
                                  top = 100.0f
                                  spacing = 20.0f })
            screen_manager.AddScreen(instructions)
            do! instructions.Task
            screen_manager.RemoveScreen(instructions)
            do! menu_loop exit_game controlling_player

        | _ -> () // TODO. For now we send back to the press start screen
    }

    let main_task = task {
        let exit_game = ref false
        while not !exit_game do
            use press_start = new PressStartScreen(content_path, sys, 0.5f, 0.1f, 0.5f, 0.016f)
            screen_manager.AddScreen(press_start)
            let! controlling_player = press_start.Task
            screen_manager.RemoveScreen(press_start)

            do! menu_loop exit_game controlling_player

        do! sys.Wait(1.0f)
        game.Exit()
    }

    override this.Initialize() =
        scheduler.AddTask(main_task)
        base.Initialize()

    override this.LoadContent() =
        base.LoadContent()

    override this.Update(gt) =
        scheduler.RunFor(float32 gt.ElapsedGameTime.TotalSeconds)

    override this.Draw(gt) =
        base.Draw(gt)
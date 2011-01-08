module CoopMultiTaskingSample.Main

open Microsoft.Xna.Framework

open XNAUtils.CoopMultiTasking
open XNAUtils.ScreenManager
open XNAUtils.PressStartScreen
open XNAUtils.MenuScreen

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

    let main_task = task {
        let exit_game = ref false
        while not !exit_game do
            use press_start = new PressStartScreen(content_path, sys, 0.5f, 0.1f, 0.5f, 0.016f)
            screen_manager.AddScreen(press_start)
            let! controlling_player = press_start.Task
            screen_manager.RemoveScreen(press_start)

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
            | Exit ->
                exit_game := true
            | Play ->
                use gameplay = new GameplayScreen(sys, controlling_player)
                screen_manager.AddScreen(gameplay)
                let! best_time = gameplay.Task
                screen_manager.RemoveScreen(gameplay)
            | _ -> () // TODO. For now we send back to the press start screen

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
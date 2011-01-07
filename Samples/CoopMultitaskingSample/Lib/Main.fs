module CoopMultiTaskingSample.Main

open Microsoft.Xna.Framework

open XNAUtils.CoopMultiTasking
open XNAUtils.ScreenManager
open XNAUtils.PressStartScreen

type Main(game : Game, screen_manager : ScreenManager) =
    inherit DrawableGameComponent(game)

    let scheduler = new Scheduler()
    let sys = new Environment(scheduler)

    let main_task = task {
        use press_start = new PressStartScreen(sys, 5.0f, 5.0f, 0.25f, 0.016f)
        screen_manager.AddScreen(press_start)
        let! controlling_player = press_start.Task
        screen_manager.RemoveScreen(press_start)
        do! sys.Wait(10.0f)
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
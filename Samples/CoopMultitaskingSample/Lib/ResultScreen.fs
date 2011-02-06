module CoopMultiTaskingSample.ResultScreen

open Microsoft.Xna.Framework
open Microsoft.Xna.Framework.Input
open Microsoft.Xna.Framework.Graphics

open XNAUtils.CoopMultiTasking
open XNAUtils.ScreenManager
open XNAUtils.InputChanges
open XNAUtils.XNAExtensions

type Resources =
    { font : SpriteFont
      batch : SpriteBatch }

type GameplayResult =
    | TooEarly of float * int
    | TooLate of float * int
    | Aborted

type ResultScreen(sys : Environment, player, reason) =
    inherit ScreenBase<Resources>()

    let rsc = ref None
    let input = new InputChanges(player)
    let blink = new XNAUtils.Animations.FadeInOscillateFadeOut(sys, 0.1f, 0.5f, 0.1f)

    let rec wait() = task {
        input.Update()
        if not (input.IsStartPressed()) && GamerServices.Gamer.IsSignedIn(player) then
            do! sys.WaitNextFrame()
            return! wait()
    }

    member this.Task = task {
        this.SetDrawer(this.Drawer)

        let blinker = sys.Spawn(blink.Task)
        
        do! wait()

        blinker.Kill()
        do! sys.WaitUntil(fun () -> blinker.IsDead)
    }

    override this.LoadContent() =
        match !rsc with
        | Some r -> r.batch.Dispose()
        | None -> ()

        let font : SpriteFont = base.Content.Load("ui/font")
        rsc := Some
                { batch = new SpriteBatch(base.Game.GraphicsDevice)
                  font = font }

    override this.UnloadContent() =
        rsc := None

    override this.BeginDrawer() =
        match !rsc with
        | Some rsc -> rsc.batch.Begin()
        | None -> ()
        !rsc

    override this.EndDrawer(rsc) =
        rsc.batch.End()

    member private this.Drawer(rsc) =
        let k = blink.Fade * if blink.Oscillation >= 0.0f then 1.0f else 0.0f
        let color = Color(k, k, k, k)
        let reason_txt =
            match reason with
            | TooEarly _ -> "You pressed too early"
            | TooLate _ -> "You were too slow"
            | Aborted _ -> "You quit"

        rsc.batch.DrawString(rsc.font, reason_txt, Vector2(100.0f, 100.0f), Color.White)

        match reason with
        | TooLate (grace_time, score) | TooEarly (grace_time, score) ->
            let grace_txt = sprintf "Allowed delay: %5.2f" grace_time
            let score_txt = sprintf "Score: %d" score

            rsc.batch.DrawString(rsc.font, grace_txt, Vector2(100.0f, 140.0f), Color.White)
            rsc.batch.DrawString(rsc.font, score_txt, Vector2(100.0f, 160.0f), Color.White)
        | Aborted -> ()

        rsc.batch.DrawString(rsc.font, "Press A", Vector2(100.0f, 200.0f), color)

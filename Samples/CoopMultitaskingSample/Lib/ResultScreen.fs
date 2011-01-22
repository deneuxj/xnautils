﻿module CoopMultiTaskingSample.ResultScreen

open Microsoft.Xna.Framework
open Microsoft.Xna.Framework.Input
open Microsoft.Xna.Framework.Graphics

open XNAUtils.CoopMultiTasking
open XNAUtils.ScreenManager
open XNAUtils.InputChanges


type private Resources =
                { font : SpriteFont
                  batch : SpriteBatch }

type GameEndReason =
    | TooEarly
    | TooLate

type ResultScreen(content_path, sys : Environment, player, reason, grace_time, score) =
    inherit ScreenBase(content_path)

    let rsc = ref None
    let input = new InputChanges(player)
    let blink = new XNAUtils.Animations.FadeInOscillateFadeOut(sys, 0.1f, 0.5f, 0.1f, 0.016f)

    let rec wait() = task {
        input.Update()
        if not (input.IsButtonPress(Buttons.B) || input.IsButtonPress(Buttons.Back)) then
            do! sys.WaitNextFrame()
            do! wait()
    }

    member this.Task = task {
        let blinker = sys.Spawn(blink.Task)
        
        do! wait()

        blinker.Kill()
        do! sys.WaitUntil(fun () -> blinker.IsDead)
    }

    override this.LoadContent() =
        match !rsc with
        | Some r -> r.batch.Dispose()
        | None -> ()

        let font : SpriteFont = base.Content.Load("font")
        rsc := Some
                { batch = new SpriteBatch(base.Game.GraphicsDevice)
                  font = font }
        
        base.LoadContent()

    override this.UnloadContent() =
        rsc := None
        base.UnloadContent()

    override this.Draw _ =
        match !rsc with
        | Some r ->
            try
                r.batch.Begin()

                let k = blink.Fade * if blink.Oscillation >= 0.0f then 1.0f else 0.0f
                let color = Color(k, k, k, k)
                let reason_txt =
                    match reason with
                    | TooEarly -> "You pressed too early"
                    | TooLate -> "You were too slow"

                let grace_txt = sprintf "Allowed delay: %5.2f" grace_time
                let score_txt = sprintf "Score: %d" score

                r.batch.DrawString(r.font, reason_txt, Vector2(100.0f, 100.0f), Color.White)
                r.batch.DrawString(r.font, grace_txt, Vector2(100.0f, 140.0f), Color.White)
                r.batch.DrawString(r.font, score_txt, Vector2(100.0f, 160.0f), Color.White)

                r.batch.DrawString(r.font, "Press B", Vector2(100.0f, 200.0f), color)

            finally
                r.batch.End()

        | None -> ()
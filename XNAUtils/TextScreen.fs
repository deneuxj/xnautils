﻿module XNAUtils.TextScreen

open Microsoft.Xna.Framework
open Microsoft.Xna.Framework.Input
open Microsoft.Xna.Framework.Graphics

open XNAUtils.CoopMultiTasking
open XNAUtils.ScreenManager

type private Resources =
                { font : SpriteFont
                  batch : SpriteBatch }

type TextScreen(content_path, player : PlayerIndex, sys : Environment, lines : string[], placement : MenuScreen.PlacementParameters) =
    inherit ScreenBase(content_path)

    let rsc = ref None

    let animation = new Animations.FadeInOscillateFadeOut(sys, 0.1f, 0.1f, 0.1f, 0.016f)

    let input = new InputChanges.InputChanges(player)

    let my_task = task {
        let animator = sys.Spawn(animation.Task)

        while not (input.IsButtonPress(Buttons.B) || input.IsButtonPress(Buttons.A) || input.IsButtonPress(Buttons.Back)) do
            input.Update()
            do! sys.WaitNextFrame()

        animator.Kill()
        do! sys.WaitUntil(fun() -> animator.IsDead)

        return ()
    }

    member this.Task = my_task

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
                let color = new Color(1.0f, 1.0f, 1.0f, animation.Fade)
                r.batch.Begin()
                lines
                |> Array.iteri(fun i (txt) ->
                    let y = placement.top + (float32 i) * placement.spacing
                    let pos = new Vector2(placement.left, y)
                    r.batch.DrawString(r.font, txt, pos, color)
                    )
            finally
                r.batch.End()

        | None -> ()
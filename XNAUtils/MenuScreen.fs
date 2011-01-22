﻿module XNAUtils.MenuScreen

open Microsoft.Xna.Framework
open Microsoft.Xna.Framework.Input
open Microsoft.Xna.Framework.Graphics

open XNAUtils.CoopMultiTasking
open XNAUtils.ScreenManager

type private Resources =
                { font : SpriteFont
                  batch : SpriteBatch }

type AnimationParameters =
    {  period : float32
       shift : float32  }

type PlacementParameters =
    {  left : float32
       top : float32
       spacing : float32  }

type MenuScreen<'I>(content_path, player : PlayerIndex, sys : Environment, items : ('I * string)[], anim : AnimationParameters, placement : PlacementParameters) =
    inherit ScreenBase(content_path)

    let rsc = ref None

    let current = ref 0

    let animation = new Animations.MultipleFadeIn(sys, items.Length, anim.period, anim.shift)

    let input = new InputChanges.InputChanges(player)

    do if items.Length = 0 then invalidArg "items" "items may not be empty"

    let my_task = task {
        let animator = sys.Spawn(animation.Task)

        let num = items.Length
        let up() =
            current := (!current + num - 1) % num
        let down() =
            current := (!current + 1) % num

        let selected = ref false
        let backed = ref false
        while not (!selected || !backed) do
            input.Update()

            if input.IsButtonPress(Buttons.DPadDown) then down()
            elif input.IsButtonPress(Buttons.DPadUp) then up()
            elif input.IsButtonPress(Buttons.A) then selected := true
            elif input.IsButtonPress(Buttons.B) then backed := true

            do! sys.WaitNextFrame()

        animator.Kill()
        do! sys.WaitUntil(fun() -> animator.IsDead)

        return
            if !selected then items.[!current] |> fst |> Some
            else None
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
        
    override this.UnloadContent() =
        rsc := None

    override this.Draw _ =
        match !rsc with
        | Some r ->
            let default_color = Color.Yellow
            let selected_color = Color.Red
            let right = 10.0f + float32 base.Game.GraphicsDevice.Viewport.Width
            try
                r.batch.Begin()
                items
                |> Array.iteri(fun i (_, txt) ->
                    let y = placement.top + (float32 i) * placement.spacing
                    let dst = Vector2(placement.left, y)
                    let src = Vector2(right, y)
                    let k = animation.Values(i)
                    let pos = k * dst + (1.0f - k) * src
                    r.batch.DrawString(r.font, txt, pos, if i = !current then selected_color else default_color)
                    )
            finally
                r.batch.End()

        | None -> ()
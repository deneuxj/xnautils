module XNAUtils.TextScreen

open Microsoft.Xna.Framework
open Microsoft.Xna.Framework.Input
open Microsoft.Xna.Framework.Graphics

open XNAUtils.CoopMultiTasking
open XNAUtils.ScreenManager

type TextScreen(content_path, player : PlayerIndex, sys : Environment, lines : string[], placement : MenuScreen.PlacementParameters) =
    inherit ScreenBase<unit>(content_path)

    let animation = new Animations.FadeInOscillateFadeOut(sys, 0.5f, 0.1f, 0.5f)

    let input = new InputChanges.InputChanges(player)

    member this.Task = task {
        this.SetDrawer(this.Drawer)

        let animator = sys.Spawn(animation.Task)

        while not this.IsOnTop || not (input.IsButtonPress(Buttons.B) || input.IsButtonPress(Buttons.A) || input.IsButtonPress(Buttons.Back)) do
            input.Update()
            do! sys.WaitNextFrame()

        animator.Kill()
        do! sys.WaitUntil(fun() -> animator.IsDead)

        return ()
    }

    // The default implementation of BeginDrawer returns None, which prevents the drawer to be executed.
    // We return Some() so that this.Drawer below is called.
    override this.BeginDrawer() = Some()

    member private this.Drawer() =
        try
            let color =
                let k = animation.Fade
                new Color(k, k, k, k)

            this.SpriteBatch.Begin()

            lines
            |> Array.iteri(fun i txt ->
                let y = placement.top + (float32 i) * placement.spacing
                let pos = new Vector2(placement.left, y)
                this.SpriteBatch.DrawString(this.Font1, txt, pos, color)
                )
        finally
            this.SpriteBatch.End()

    override this.LoadContent() = ()
    override this.UnloadContent() = ()

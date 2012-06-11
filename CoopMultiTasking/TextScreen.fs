namespace CleverRake.XnaUtils.Application

open Microsoft.Xna.Framework
open Microsoft.Xna.Framework.Input
open Microsoft.Xna.Framework.Graphics

open CleverRake.XnaUtils
open CleverRake.XnaUtils.CoopMultiTasking
open CleverRake.XnaUtils.Application

type TextScreen(player : PlayerIndex, sys : Environment, lines : string[], placement : PlacementParameters) =
    let impl = new ScreenBase<unit>()
    let impl_screen = impl :> Screen

    let animation = new Animations.FadeInOscillateFadeOut(sys, 0.5f, 0.1f, 0.5f)

    let input = new InputChanges(player)

    let offset = ref 0.0f

    member this.Task = task {
        impl.PreDrawer <- fun () -> Some()
        impl.Drawer <- this.Drawer

        let animator = sys.Spawn(animation.Task)

        // Wait until the player presses a button.
        while
            not (impl.IsActive
                 && (input.IsBackPressed() || input.IsStartPressed())) do
            input.Update()
            if input.IsMenuDown() then
                offset := !offset - 10.0f
            elif input.IsMenuUp() then
                offset := !offset + 10.0f
            do! sys.WaitNextFrame()

        animator.Kill()
        do! sys.WaitUntil(fun() -> animator.IsDead)

        return ()
    }

    member private this.Drawer() =
        try
            let color =
                let k = animation.Fade
                new Color(k, k, k, k)

            impl.SpriteBatch.Begin()

            lines
            |> Array.iteri(fun i txt ->
                let y = !offset + placement.top + (float32 i) * placement.spacing
                let pos = new Vector2(placement.left, y)
                impl.SpriteBatch.DrawString(impl.Font1, txt, pos, color)
                )
        finally
            impl.SpriteBatch.End()

    interface Screen with
        member this.ClearScreenManager() = impl_screen.ClearScreenManager()
        member this.Draw() = impl_screen.Draw()
        member this.LoadContent() = impl_screen.LoadContent()
        member this.SetGame(ng) = impl_screen.SetGame(ng)
        member this.SetIsOnTop(b) = impl_screen.SetIsOnTop(b)
        member this.SetScreenManager(sm) = impl_screen.SetScreenManager(sm)
        member this.UnloadContent() = impl_screen.UnloadContent()

    interface System.IDisposable with
        member this.Dispose() = (impl :> System.IDisposable).Dispose()

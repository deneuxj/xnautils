module CleverRake.XnaUtils.PressStartScreen

open Microsoft.Xna.Framework
open Microsoft.Xna.Framework.Input
open Microsoft.Xna.Framework.Graphics

open CleverRake.XnaUtils.CoopMultiTasking

let all_players : PlayerIndex[] = [| for i in 0..3 do yield enum i |]

type PressStartScreen(sys : Environment, fade_in, fade_out, blink) =
    let impl = new ScreenManager.ScreenBase<_>()
    let impl_screen = impl :> ScreenManager.Screen

    let pos = ref Vector2.Zero
    let txt = "Press start"

    let anim = new Animations.FadeInOscillateFadeOut(sys, fade_in, blink, fade_out)

    // This task is chopped in blocks and each block is executed by the scheduler each frame (see Main.Update())
    member this.Task = task {
        impl.PreDrawer <- fun() -> Some()
        impl.Drawer <- this.Drawer

        // subtask that makes the "press start" text blink. Executes concurrently
        let blinker =
            sys.Spawn(anim.Task)

        // Task to check if a button is pressed. Sets player when that happens.
        let player : PlayerIndex option ref = ref None
        while (!player).IsNone do
            do! sys.WaitUntil(fun () -> impl.IsActive)
            for p in all_players do
                let state = GamePad.GetState(p)
                if state.IsConnected
                    && (state.Buttons.Start = ButtonState.Pressed
                        || state.Buttons.A = ButtonState.Pressed) then
                    player := Some p
            do! sys.WaitNextFrame()

        // wait until the buttons are depressed, otherwise the next screen may take action too quickly.
        do! sys.WaitUntil(fun() -> 
            let state = GamePad.GetState(player.Value.Value)
            state.Buttons.Start = ButtonState.Released
            && state.Buttons.A = ButtonState.Released)

        // Stop blinking
        blinker.Kill()
    
        // To be nice, wait until the blinker is done.
        // Depending on the blinking period, this could take long as we only check for the kill flag once per blink.
        do! sys.WaitUntil(fun () -> blinker.IsDead)

        // Return the index of the player that pressed start.
        return
            match !player with
            | Some p -> p
            | None -> failwith "Unreachable"
    }

    interface ScreenManager.Screen with
        member this.ClearScreenManager() = impl_screen.ClearScreenManager()
        member this.Draw() = impl_screen.Draw()
        member this.LoadContent() =
            impl_screen.LoadContent()

            // Now is a good time to compute values which depend on the graphics device.
            // Update the position where the text is drawn.
            let sz = impl.Font1.MeasureString(txt)
            let area = impl.Game.GraphicsDevice.Viewport.TitleSafeArea

            let x = area.Left + (area.Width - int sz.X) / 2
            let y = area.Top + (area.Height - int sz.Y) / 2

            pos := new Vector2(float32 x, float32 y)

        member this.SetGame(g) = impl_screen.SetGame(g)
        member this.SetIsOnTop(b) = impl_screen.SetIsOnTop(b)
        member this.SetScreenManager(sm) = impl_screen.SetScreenManager(sm)
        member this.UnloadContent() = impl_screen.UnloadContent()


    interface System.IDisposable with
        member this.Dispose() = (impl :> System.IDisposable).Dispose()


    // Draw "Press start" centered on the screen.
    member private this.Drawer() =
        let color =
            let blink = if anim.Oscillation >= 0.0f then 1.0f else 0.0f
            let k = anim.Fade * blink
            new Color(k, k, k, k)

        try
            impl.SpriteBatch.Begin()
            impl.SpriteBatch.DrawString(impl.Font1, "Press start", !pos, color)                
        finally
            impl.SpriteBatch.End()
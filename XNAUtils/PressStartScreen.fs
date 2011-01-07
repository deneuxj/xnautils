module XNAUtils.PressStartScreen

open Microsoft.Xna.Framework
open Microsoft.Xna.Framework.Input
open Microsoft.Xna.Framework.Graphics

open XNAUtils.CoopMultiTasking

let all_players : PlayerIndex[] = [| for i in 0..3 do yield enum i |]

// Graphics resources used by PressStartScreen
type private Resources =
                { font : SpriteFont
                  batch : SpriteBatch }


type PressStartScreen(sys : Environment, fade_in, fade_out, blink, dt) =
    inherit ScreenManager.ScreenBase()

    let rsc = ref None
    let pos = ref Vector2.Zero
    let txt = "Press start"

    // Set by the task, read by Draw()
    let light = ref 0.0f    // Used for the fade in/out effects
    let blink_k = ref 1.0f  // Used for blinking: 0.0f when not visible, 1.0f when visible.

    // This task is chopped in blocks and each block is executed by the scheduler each frame (see Main.Update())
    let press_start_task = task {
        // Fade in
        for t in 0.0f .. dt .. fade_in do
            light := t / fade_in
            do! sys.Wait(dt)

        // subtask that makes the "press start" text blink. Executes concurrently
        let blinker =
            sys.Spawn(fun stop ->
                task {
                    while not (!stop) do
                        blink_k := 0.0f
                        do! sys.Wait(blink)
                        //if !stop then return ()
                        blink_k := 1.0f
                        do! sys.Wait(blink)
                        //if !stop then return ()
                }
            )
        // Task to check if a button is pressed. Sets player when that happens.
        let player : PlayerIndex option ref = ref None
        while (!player).IsNone do
            for p in all_players do
                let state = GamePad.GetState(p)
                if state.IsConnected
                    && (state.Buttons.Start = ButtonState.Pressed
                        || state.Buttons.A = ButtonState.Pressed) then
                    player := Some p
            do! nextFrame()

        // Stop blinking
        blinker.Kill()
    
        // Fade out
        for t in fade_out .. -dt .. 0.0f do
            light := t / fade_in
            do! sys.Wait(dt)

        // To be nice, wait until the blinker is done.
        // Depending on the blinking period, this could take long as we only check for the kill flag once per blink.
        do! sys.WaitUntil(fun () -> blinker.IsDead)

        // Return the index of the player that pressed start.
        return
            match !player with
            | Some p -> p
            | None -> failwith "Unreachable"
    }

    // Exposes press_start_task. Also wraps it in try...with to display stack traces in uncaught exceptions.
    member x.Task =
        task {
            return!
                try
                    press_start_task
                with
                    e -> task {
                        // TODO: Show trace
                        // TODO: wait for button press
                        raise e // Raise e again to "crash properly"
                        return PlayerIndex.One // never executed.
                    }
        }

    // Load the font and create a sprite batch.
    // Note: Sprite batches must be recreated when their graphics device is reset.
    // As LoadContent is called whenever that happens, it's a good place to create a SpriteBatch.
    override this.LoadContent() =
        match !rsc with
        | Some r -> r.batch.Dispose()
        | None -> ()

        let font : SpriteFont = base.Content.Load("font")
        rsc := Some
                { batch = new SpriteBatch(base.Game.GraphicsDevice)
                  font = font }

        // Now is also a good time to compute values which depend on the graphics device.
        // Update the position where the text is drawn.
        let sz = font.MeasureString(txt)
        let area = base.Game.GraphicsDevice.Viewport.TitleSafeArea

        let x = area.Left + (area.Width - int sz.X) / 2
        let y = area.Top + (area.Height - int sz.Y) / 2

        pos := new Vector2(float32 x, float32 y)

    // Draw "Press start" centered on the screen.
    override this.Draw _ =
        match !rsc with
        | Some r ->
            let color =
                let k = !light * !blink_k in new Color(k, k, k, k)

            try
                r.batch.Begin()
                r.batch.DrawString(r.font, "Press start", !pos, color)                
            finally
                r.batch.End()

        | None -> ()
module CleverRake.XnaUtils.InputChanges

open Microsoft.Xna.Framework
open Microsoft.Xna.Framework.Input

type InputChanges(player : PlayerIndex) =
    let mutable oldState = GamePad.GetState(player)
    let mutable newState = GamePad.GetState(player)

    member x.Update() =
        oldState <- newState
        newState <- GamePad.GetState(player)

    member x.IsButtonPress(b : Buttons) =
        oldState.IsButtonDown(b) && newState.IsButtonUp(b)

    member x.IsButtonRelease(b : Buttons) =
        oldState.IsButtonUp(b) && newState.IsButtonDown(b)

    // Some typical button presses

    // Start or A: To start a sequence, confirm, go ahead.
    member x.IsStartPressed() =
        x.IsButtonPress(Buttons.A) || x.IsButtonPress(Buttons.Start)

    // B or Back: To cancel a sequence, go back to the main menu...
    member x.IsBackPressed() =
        x.IsButtonPress(Buttons.B) || x.IsButtonPress(Buttons.Back)

    member x.IsMenuLeft() =
        x.IsButtonPress(Buttons.DPadLeft) || x.IsButtonPress(Buttons.LeftThumbstickLeft)

    member x.IsMenuRight() =
        x.IsButtonPress(Buttons.DPadRight) || x.IsButtonPress(Buttons.LeftThumbstickRight)

    member x.IsMenuUp() =
        x.IsButtonPress(Buttons.DPadUp) || x.IsButtonPress(Buttons.LeftThumbstickUp)

    member x.IsMenuDown() =
        x.IsButtonPress(Buttons.DPadDown) || x.IsButtonPress(Buttons.LeftThumbstickDown)
module XNAUtils.InputChanges

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

    // Start or A: To start a sequence
    member x.IsStartPressed() =
        x.IsButtonPress(Buttons.A) || x.IsButtonPress(Buttons.Start)

    // B or Back: To cancel a sequence
    // Also used to pause before continuing.
    // Using A is dangerous as it's typically used in the sequence that just ended.
    member x.IsBackPressed() =
        x.IsButtonPress(Buttons.B) || x.IsButtonPress(Buttons.Back)
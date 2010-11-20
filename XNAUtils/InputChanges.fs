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
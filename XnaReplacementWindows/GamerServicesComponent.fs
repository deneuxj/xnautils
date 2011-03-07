namespace XnaReplacementWindows.Xna.Framework.GamerServices

open Microsoft.Xna.Framework
open Microsoft.Xna.Framework.Input
open Microsoft.Xna.Framework.GamerServices

// Replacement for Microsoft.Xna.Framework.GamerServices.GamerServicesComponent.
// Shows the sign in dialog when the big button is pressed.
type GamerServicesComponent(game) =
    inherit GameComponent(game)

    let input = ref(GamePad.GetState(PlayerIndex.One))

    let isBackStartPressed(input : GamePadState) =
        input.Buttons.Back = ButtonState.Pressed && input.Buttons.Start = ButtonState.Pressed

    override this.Update(gt) =
        let new_input = GamePad.GetState(PlayerIndex.One)

        if new_input.IsConnected && (!input).IsConnected &&
           isBackStartPressed(!input) && isBackStartPressed(new_input) then

            if not Guide.IsVisible then
                try
                    Guide.ShowSignIn(1, false)
                with
                | :? GuideAlreadyVisibleException -> ()

        input := new_input
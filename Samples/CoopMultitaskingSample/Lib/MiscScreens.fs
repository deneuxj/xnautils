module CoopMultiTaskingSample.MiscScreens

open Microsoft.Xna.Framework
open Microsoft.Xna.Framework.Input
open Microsoft.Xna.Framework.Graphics

open XNAUtils.CoopMultiTasking
open XNAUtils.ScreenManager
open XNAUtils.TextScreen

let user_container = "User data"
let user_settings_filename = "user_prefs.xml"

let mkInstructions controlling_player sys =
    new TextScreen(controlling_player, sys,
                    [| "The game screen shows a number in the upper left corner"
                       "and a 3x3 matrix in the middle of the screen."
                       "Whenever you see the number in the matrix, press A."
                       "You must press A within a delay, otherwise the game ends."
                       "The delay decreases the further in the game you go."
                       "Pressing A when the number is not in the matrix"
                       "ends the game."
                       ""
                       "Press B to go back to the menu." |],
                    { left = 100.0f
                      top = 100.0f
                      spacing = 20.0f })


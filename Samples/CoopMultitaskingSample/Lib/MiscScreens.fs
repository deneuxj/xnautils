module CoopMultiTaskingSample.MiscScreens

open Microsoft.Xna.Framework
open Microsoft.Xna.Framework.Input
open Microsoft.Xna.Framework.Graphics

open XNAUtils.CoopMultiTasking
open XNAUtils.ScreenManager
open XNAUtils.TextScreen
open XNAUtils.MenuScreen

let user_container = "User data"
let user_settings_filename = "user_prefs.xml"

let score_container = "Scores"
let score_filename = "scores.xml"

let text_placement =
    { left = 100.0f;
      top = 100.0f;
      spacing = 20.0f }

let menu_placement =
    { left = 300.0f;
      top = 100.0f;
      spacing = 40.0f }

let menu_animation =
    { period = 0.2f;
      shift = 0.05f }

let mkInstructions(controlling_player, sys) =
    let lines =
        [| "The game screen shows a number in the upper left corner";
            "and a 3x3 matrix in the middle of the screen.";
            "Whenever you see the number in the matrix, press A.";
            "You must press A within a delay, otherwise the game ends.";
            "The delay decreases the further in the game you go.";
            "Pressing A when the number is not in the matrix";
            "ends the game.";
            "";
            "Press B to go back to the menu."; |]
    new TextScreen(controlling_player, sys, lines, text_placement)

let mkInfo(controlling_player, sys) =
    let lines =
        [| "Learn more about XNA Game Studio:";
            " -> http://create.msdn.com";
            "";
            "Learn more about F#:";
            " -> http://www.fsharp.net"; |]
    new TextScreen(controlling_player, sys, lines, text_placement)

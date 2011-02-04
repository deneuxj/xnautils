module CoopMultiTaskingSample.UserSettings

open Microsoft.Xna.Framework

open XNAUtils.CoopMultiTasking
open XNAUtils.ScreenManager
open XNAUtils.MenuScreen
open XNAUtils.StorageTasks


type ISettingsNotifiable =
    interface
        abstract member SetBackgroundColor : Color -> unit
        abstract member SetFontSize : float32 -> unit
    end


type Data() =
    let background_color = ref Color.CornflowerBlue
    let font_size = ref 1.0f

    member this.BackgroundColor
        with get() = !background_color
        and set(c) = background_color := c

    member this.FontSize
        with get() = !font_size
        and set(s) = font_size := s

    member this.Apply(target : ISettingsNotifiable) =
        target.SetBackgroundColor(!background_color)
        target.SetFontSize(!font_size)


type MenuEntries =
    | SetBackgroundColor
    | SetFontSize

let menu_items =
    [| (SetBackgroundColor, "Background");
       (SetFontSize, "Font size") |]

let mkUserSettingsScreen player sys anim placement (storage : Storage) =
    new MenuScreen<MenuEntries>(player, sys, menu_items, anim, placement)

type Colors =
    | AliceBlue
    | CornflowerBlue
    | BlueViolet
    | CadetBlue
    | Reset

type FontSizes =
    | Small
    | Medium
    | Large
    | Reset

let handleUserSettingsMenu player sys anim placement (sm : ScreenManager) (data : Data) (choice : MenuEntries option) = task {
    match choice with
    | Some SetBackgroundColor ->
        let menu_items =
            [| (AliceBlue, "Alice Blue");
               (CornflowerBlue, "Cornflower Blue");
               (BlueViolet, "BlueViolet");
               (CadetBlue, "CadetBlue");
               (Colors.Reset, "Reset to default") |]
        use color_menu = new MenuScreen<Colors>(player, sys, menu_items, anim, placement)
        sm.AddScreen(color_menu)
        let! choice = color_menu.Task
        match choice with
        | Some AliceBlue -> data.BackgroundColor <- Color.AliceBlue
        | Some CornflowerBlue | Some Colors.Reset -> data.BackgroundColor <- Color.CornflowerBlue
        | Some BlueViolet -> data.BackgroundColor <- Color.BlueViolet
        | Some CadetBlue -> data.BackgroundColor <- Color.CadetBlue
        | None -> ()
        sm.RemoveScreen(color_menu)
        return data
    | Some SetFontSize ->
        let menu_items = 
            [| (Small, "Small");
               (Medium, "Medium");
               (Large, "Large");
               (FontSizes.Reset, "Reset") |]
        use font_menu = new MenuScreen<FontSizes>(player, sys, menu_items, anim, placement)
        sm.AddScreen(font_menu)
        let! choice = font_menu.Task
        match choice with
        | Some Medium | Some FontSizes.Reset -> data.FontSize <- 1.0f
        | Some Small -> data.FontSize <- 0.5f
        | Some Large -> data.FontSize <- 1.5f
        | None -> ()
        sm.RemoveScreen(font_menu)
        return data
    | None ->
        return data
}
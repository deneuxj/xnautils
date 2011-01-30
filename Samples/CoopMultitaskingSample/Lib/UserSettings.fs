module CoopMultiTaskingSample.UserSettings

open Microsoft.Xna.Framework

open XNAUtils.CoopMultiTasking
open XNAUtils.ScreenManager
open XNAUtils.MenuScreen
open XNAUtils.StorageTasks


type IBackgroundColorHolder =
    interface
        abstract member SetBackgroundColor : Color -> unit
    end


type Data() =
    let background_color = ref Color.CornflowerBlue

    member this.BackgroundColor
        with get() = !background_color
        and set(c) = background_color := c

    member this.Apply(bg_holder : IBackgroundColorHolder) =
        bg_holder.SetBackgroundColor(!background_color)


type MenuEntries =
    | SetBackgroundColor
    | Load
    | Save

let menu_items =
    [| (SetBackgroundColor, "Background");
       (Load, "Load preferences");
       (Save, "Save preferences") |]

let mkUserSettingsScreen content_path player sys anim placement (storage : Storage) =
    let menu_items =
        [|
            yield (SetBackgroundColor, "Background")
            if storage.PlayerStorage.IsSome then
                yield (Load, "Load preferences")
                yield (Save, "Save preferences")
        |]

    new MenuScreen<MenuEntries>(content_path, player, sys, menu_items, anim, placement)

type Colors =
    | AliceBlue
    | CornflowerBlue
    | BlueViolet
    | CadetBlue
    | Reset

let handleUserSettingsMenu content_path player sys anim placement (sm : ScreenManager) (storage : Storage) (data : Data) (choice : MenuEntries option)= task {
    match choice with
    | Some SetBackgroundColor ->
        let menu_items =
            [| (AliceBlue, "Alice Blue");
               (CornflowerBlue, "Cornflower Blue");
               (BlueViolet, "BlueViolet");
               (CadetBlue, "CadetBlue");
               (Reset, "Reset to default") |]
        use color_menu = new MenuScreen<Colors>(content_path, player, sys, menu_items, anim, placement)
        sm.AddScreen(color_menu)
        let! choice = color_menu.Task
        match choice with
        | Some AliceBlue -> data.BackgroundColor <- Color.AliceBlue
        | Some CornflowerBlue | Some Reset -> data.BackgroundColor <- Color.CornflowerBlue
        | Some BlueViolet -> data.BackgroundColor <- Color.BlueViolet
        | Some CadetBlue -> data.BackgroundColor <- Color.CadetBlue
        | None -> ()
        sm.RemoveScreen(color_menu)
        return data
    | Some Load ->
        do! storage.CheckPlayerStorage
        match storage.PlayerStorage with
        | Some _ ->
            let! maybe_data = storage.DoPlayerStorage(MiscScreens.user_container, loadXml MiscScreens.user_settings_filename)
            match maybe_data with
            | Some(Some data') -> return data'
            | None | Some None ->
                do! doOnGuide <| fun() -> error "Failed to load user settings."
                return data
        | None ->
            return data
    | Some Save ->
        do! storage.CheckPlayerStorage
        match storage.PlayerStorage with
        | Some _ ->
            let! maybe_data = storage.DoPlayerStorage(MiscScreens.user_container, saveXml MiscScreens.user_settings_filename data)
            match maybe_data with
            | Some(Some _) -> return data
            | None | Some None ->
                do! doOnGuide <| fun() -> error "Failed to save user settings."
                return data
        | None ->
            return data
    | None ->
        return data
}
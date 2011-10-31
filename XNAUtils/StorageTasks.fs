module CleverRake.XnaUtils.StorageTasks

open System
open System.Xml.Serialization

open Microsoft.Xna.Framework
open Microsoft.Xna.Framework.Storage
open Microsoft.Xna.Framework.GamerServices

open CoopMultiTasking
open XnaExtensions

let rec doOnGuide f = task {
    do! waitUntil(fun() -> not Guide.IsVisible)
    let! result = task {
        try
            return f()
        with
        | :? GuideAlreadyVisibleException ->
            do! wait(0.5f)
            let! eventually_some_bloody_result = doOnGuide f
            return eventually_some_bloody_result
    }
    return result
}

let getStorageDevice = task {
    let! async_result = doOnGuide(fun () -> StorageDevice.BeginShowSelector(null, null))
    do! waitUntil(fun() -> async_result.IsCompleted)
    let device = StorageDevice.EndShowSelector(async_result)
    return
        if device = null then
            None
        else
            Some device
}

let getUserStorageDevice player = task {
    let! async_result = doOnGuide(fun() -> StorageDevice.BeginShowSelector(player, null, null))
    do! waitUntil(fun() -> async_result.IsCompleted)
    let device = StorageDevice.EndShowSelector(async_result)
    return
        if device = null then
            None
        else
            Some device
}

let openContainer (device : StorageDevice) name = task {
    try
        let async_result = device.BeginOpenContainer(name, null, null)
        do! waitUntil(fun() -> async_result.IsCompleted)
        let container = device.EndOpenContainer(async_result)
        return
            if container = null then
                None
            else
                Some container
    with
    | :? StorageDeviceNotConnectedException
    | :? InvalidOperationException ->
        return None
}

let doInContainer device container_name f = task {
    let! maybe_container = openContainer device container_name

    return
        match maybe_container with
        | Some container ->
            try
                f container
                |> Some
            finally
                container.Dispose()
        | None ->
            None
}

let loadXml<'T> filename (container : StorageContainer) =
    try
        if container.FileExists(filename) then
            use file = container.OpenFile(filename, IO.FileMode.Open)
            let serializer = new XmlSerializer(typeof<'T>)
            try
                serializer.Deserialize(file) :?> 'T
                |> Some
            with
            | _ -> None
        else
            None
    with
    | :? StorageDeviceNotConnectedException ->
        None

let saveXml<'T> filename (data : 'T) (container : StorageContainer) =
    try
        use file = container.CreateFile(filename)
        let serializer = new XmlSerializer(typeof<'T>)
        serializer.Serialize(file, data)
        Some()
    with
    | :? StorageDeviceNotConnectedException ->
        None

let unpersonalAlert(title, text) =
    Guide.BeginShowMessageBox(title, text, ["Yes"; "No"], 0, MessageBoxIcon.Warning, null, null)

let personalAlert player (title, text) =
    Guide.BeginShowMessageBox(player, title, text, ["Yes"; "No"], 0, MessageBoxIcon.Warning, null, null)

let info text =
    Guide.BeginShowMessageBox("Information", text, ["OK"], 0, MessageBoxIcon.None, null, null)
    |> ignore

let error text =
    Guide.BeginShowMessageBox("Error", text, ["OK"], 0, MessageBoxIcon.Error, null, null)
    |> ignore

type Storage(use_player_storage) =
    let device = ref None
    let player_device = ref None
    let maybe_player = ref None
    let is_busy = ref false

    let titleAlert =
        match !maybe_player with
        | Some player -> personalAlert player
        | None -> unpersonalAlert

    let callAlert (beginAlert : string * string -> IAsyncResult, endAlert : IAsyncResult -> Nullable<int>) (title, text) = task {
        let! async_result =
            fun() -> beginAlert(title, text)
            |> doOnGuide
        do! waitUntil(fun () -> async_result.IsCompleted)
        let choice = endAlert async_result
        return
            if choice.HasValue && choice.Value <> 0 then
                false
            else
                true
    }

    let rec getSD getStorageDevice (alert : string * string -> IAsyncResult) storeResult = task {
        let! dev = getStorageDevice
        match dev with
        | Some dev -> storeResult(Some dev)
        | None ->
            let! yes_chosen =
                callAlert
                    (alert, Guide.EndShowMessageBox)
                    ("No device selected",
                        "You have not selected a storage device, which will disable some of the features of this game.\nDo you want to select a device now?")
            if yes_chosen then
                return! getSD getStorageDevice alert storeResult
            else
                storeResult None
    }

    member this.Player
        with get() =
            !maybe_player
        and set(player) =
            match !maybe_player, player with
            | Some old_player, Some new_player ->
                if old_player <> new_player then
                    player_device := None
                    maybe_player := player
            | Some _, None ->
                player_device := None
                maybe_player := None
            | None, Some _ ->
                maybe_player := player
            | None, None ->
                ()

    member this.TitleStorage = !device
    
    member this.PlayerStorage = !player_device
        
    member this.InitPlayerStorage = task {
        if use_player_storage then
            match !maybe_player with
            | Some p ->
                // If the player isn't signed in, let him/her sign in now.
                if Gamer.SignedInGamers.ItemOpt(p).IsNone then
                    do! doOnGuide <| fun () -> Guide.ShowSignIn(1, false)

                // If the player refused to sign in, reset player_device.
                if Gamer.SignedInGamers.ItemOpt(p).IsNone then
                    player_device := None
                else
                    return! getSD (getUserStorageDevice p) (personalAlert p) (fun v -> player_device := v)
            | None -> player_device := None
    }

    member this.InitTitleStorage = task {
        return! getSD getStorageDevice titleAlert (fun v -> device := v)
    }

    member this.CheckTitleStorage = task {
        match !device with
        | Some dev ->
            if not dev.IsConnected then
                let! yes_chosen =
                    callAlert
                        (titleAlert, Guide.EndShowMessageBox)
                        ("Device disconnected",
                         "A storage device was disconnected.\nWould you like to select a new device now?")
                if yes_chosen then
                    let! dev = getStorageDevice
                    match dev with
                    | Some dev -> device := Some dev
                    | None -> ()
                else
                    device := None
        | None -> () // No device was chosen, which means we haven't lost it!
    }

    member this.CheckPlayerStorage = task {
        if (!maybe_player).IsNone then invalidOp "No player assigned"
        let player = (!maybe_player).Value

        // If the player is no longer signed in, reset player_device.
        if Gamer.SignedInGamers.ItemOpt(player).IsNone then
            player_device := None
        else
            // Check the status of the device. If it's not connected, let the player select a new device.
            match !player_device with
            | Some dev ->
                if not dev.IsConnected then
                    let! yes_chosen =
                        callAlert
                            (personalAlert player, Guide.EndShowMessageBox)
                            ("Device disconnected",
                             "The storage device used for player data was disconnected.\nWould you like to select a new device now?")
                    if yes_chosen then
                        let! dev = getUserStorageDevice player
                        match dev with
                        | Some dev -> player_device := Some dev
                        | None -> ()
                    else
                        player_device := None
            | None -> () // No device was chosen, which means we haven't lost it!
    }

    member this.DoTitleStorage(container_name, f) = task {
        match !device with
        | None ->
            return None
        | Some device ->
            do! waitUntil(fun () -> not !is_busy)
            try
                is_busy := true

                let! result = doInContainer device container_name f
                return result
            finally
                is_busy := false
    }

    member this.DoPlayerStorage(container_name, f) = task {
        match !player_device, !maybe_player with
        | None, _ | _, None ->
            return None
        | Some device, Some player ->
            if Gamer.SignedInGamers.ItemOpt(player).IsSome then
                do! waitUntil(fun () -> not !is_busy)
                try
                    is_busy := true

                    let! result = doInContainer device container_name f
                    return result
                finally
                    is_busy := false
            else
                return None
    }
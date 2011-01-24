module XNAUtils.StorageTasks

open System

open Microsoft.Xna.Framework
open Microsoft.Xna.Framework.Storage
open Microsoft.Xna.Framework.GamerServices

open CoopMultiTasking

let dummyCallBack = new AsyncCallback(fun _ -> ())

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
    let! async_result = doOnGuide(fun () -> StorageDevice.BeginShowSelector(dummyCallBack, null))
    do! waitUntil(fun() -> async_result.IsCompleted)
    let device = StorageDevice.EndShowSelector(async_result)
    return
        if device = null then
            None
        else
            Some device
}

let getUserStorageDevice player = task {
    let! async_result = doOnGuide(fun() -> StorageDevice.BeginShowSelector(player, dummyCallBack, null))
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
        let async_result = device.BeginOpenContainer(name, dummyCallBack, null)
        do! waitUntil(fun() -> async_result.IsCompleted)
        let container = device.EndOpenContainer(async_result)
        return
            if container = null then
                None
            else
                Some container
    with
    | :? StorageDeviceNotConnectedException ->
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

type Storage(maybe_player : PlayerIndex option, use_player_storage) =
    let device = ref None
    let player_device = ref None

    let unpersonalAlert(title, text) =
        Guide.BeginShowMessageBox(title, text, ["Yes"; "No"], 0, MessageBoxIcon.Warning, dummyCallBack, null)

    let personalAlert player (title, text) =
        Guide.BeginShowMessageBox(player, title, text, ["Yes"; "No"], 0, MessageBoxIcon.Warning, dummyCallBack, null)

    let titleAlert =
        match maybe_player with
        | Some player -> personalAlert player
        | None -> unpersonalAlert
    do
        if use_player_storage && maybe_player.IsNone then invalidArg "maybe_player" "Must not be None when use_player_storage is true"

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
                    
    member this.Init = task {
        let rec getSD (alert : string * string -> IAsyncResult) storeResult = task {
            let! dev = getStorageDevice
            match dev with
            | Some dev -> storeResult(Some dev)
            | None ->
                let! yes_chosen =
                    callAlert
                        (alert, Guide.EndShowMessageBox)
                        ("No device selected",
                         "You have not selected a storage device,\nwhich will disable some of the features of this game.\nDo you want to select a device now?")
                if yes_chosen then
                    do! getSD alert storeResult
                else
                    storeResult None
        }
        do! getSD titleAlert (fun v -> device := v)
        if use_player_storage then
            match maybe_player with
            | Some p -> do! getSD (personalAlert p) (fun v -> player_device := v)
            | None -> failwith "Unreachable"
    }

    member this.CheckStorage = task {
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
                    | None -> device := None
        | None -> () // No device was chosen, which means we haven't lost it!
    }

    member this.CheckUserStorage = task {
        if maybe_player.IsNone then invalidOp "No player assigned"
        let player = maybe_player.Value
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
                    | None -> player_device := None
        | None -> () // No device was chosen, which means we haven't lost it!
    }
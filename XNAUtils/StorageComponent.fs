module XNAUtils.StorageComponent

(*
Copyright [2010] [Johann Deneux]

   Licensed under the Apache License, Version 2.0 (the "License");
   you may not use this file except in compliance with the License.
   You may obtain a copy of the License at

     http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS,
   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
   See the License for the specific language governing permissions and
   limitations under the License.
*)

open System
open System.IO
open System.Threading
open Microsoft.Xna.Framework
open Microsoft.Xna.Framework.GamerServices
open Microsoft.Xna.Framework.Storage
open Microsoft.FSharp.Control

type State =
    | StartReqTitleStorage of (StorageDevice -> unit) * (unit -> unit)
    | RequestingTitleStorage
    | StartSignIn of PlayerIndex * (StorageDevice -> unit) * (unit -> unit)
    | SigningIn of PlayerIndex  * (StorageDevice -> unit) * (unit -> unit)
    | NotSignedInInfo
    | StartReqUserStorage of PlayerIndex * (StorageDevice -> unit) * (unit -> unit)
    | RequestingUserStorage
    | DoingIO
    | FailedIO of (unit -> unit)
    | SuccessfulIO of (unit -> unit)
    | Ready

type Data =
    { state : State
      titleStorage : StorageDevice option
      userStorage : StorageDevice option }

let requestTitleStorage completion exc data = 
    match data.state with
    | Ready -> { data with state = StartReqTitleStorage(completion, exc) }
    | _     -> raise (new InvalidOperationException())

let requestUserStorage completion exc (p : PlayerIndex) data =
    match data.state with
    | Ready when Gamer.SignedInGamers.[p] <> null -> { data with state = StartReqUserStorage(p, completion, exc) }
    | Ready                                       -> { data with state = StartSignIn(p, completion, exc) }
    | _     -> raise (new InvalidOperationException())

let resetTitleStorage data =
    match data.state with
    | Ready -> { data with titleStorage = None }
    | _     -> raise (new InvalidOperationException())

let resetUserStorage data =
    match data.state with
    | Ready -> { data with userStorage = None }
    | _     -> raise (new InvalidOperationException())

let doStorageIO
    (getStorageDevice : Data -> StorageDevice option) setStorageDevice
    returnData
    containerName filename
    (mode : FileMode)
    operation completion (excHandler : Exception -> unit) data =
    
    match data.state, getStorageDevice data with
    | Ready, Some storage_dev ->
        try
            let callback (result : IAsyncResult) =
                use container = storage_dev.EndOpenContainer(result)
                if container <> null
                then
                    if mode = FileMode.Open && not(container.FileExists(filename))
                    then
                        { data with state = FailedIO(fun () ->
                                FileNotFoundException(filename)
                                |> excHandler) }
                        |> returnData
                    else
                        use stream = container.OpenFile(filename, mode)
                        operation(stream)
                        { data with state = SuccessfulIO(completion) }
                        |> returnData

            storage_dev.BeginOpenContainer(containerName, new AsyncCallback(callback), null) |> ignore

        with
            | exc ->
                match exc with
                | :? GamerPrivilegeException | :? InvalidOperationException | :? StorageDeviceNotConnectedException ->
                    { data with state = FailedIO(fun () -> excHandler(exc)) }
                    |> setStorageDevice None
                    |> returnData
                | _ -> failwith "Unexpected exception"
        { data with state = DoingIO }
    
    | _, _ -> raise (new InvalidOperationException())

let doTitleStorageIO returnData filename mode operation completion excHandler data =
    doStorageIO
        (fun (data : Data) -> data.titleStorage)
        (fun dev (data : Data) -> { data with titleStorage = dev })
        returnData
        "TitleStorage"
        filename
        mode
        operation
        completion
        excHandler
        data

let doUserStorageIO returnData containerName filename mode operation completion excHandler data =
    doStorageIO
        (fun (data : Data) -> data.userStorage)
        (fun dev (data : Data) -> { data with userStorage = dev })
        returnData
        containerName
        filename
        mode
        operation
        completion
        excHandler
        data

let resetDisconnectedStorage (data : Data) =
    let filter (storage : StorageDevice) =
        if storage.IsConnected
        then
            Some storage
        else
            None
            
    { data with
        titleStorage =
            data.titleStorage |> Option.bind filter
        userStorage =
            data.userStorage |> Option.bind filter }

let update returnData (dt : GameTime) (data : Data) =
    match data.state with
    | SuccessfulIO f | FailedIO f  ->
        f()
        { data with state = Ready }
        
    | StartReqTitleStorage(completion, failed) when not(Guide.IsVisible) ->
        StorageDevice.BeginShowSelector(
            (fun (result : IAsyncResult) ->
                let storage_dev = StorageDevice.EndShowSelector(result)
                if storage_dev <> null
                then
                    completion storage_dev
                    { data with state = Ready ; titleStorage = Some storage_dev }
                    |> returnData
                else
                    failed()
                    { data with state = Ready ; titleStorage = None }
                    |> returnData
            ), null)
        |> ignore
        { data with state = RequestingTitleStorage }
        
    | StartReqTitleStorage(_, _) ->
        data
        
    | StartReqUserStorage(p, completion, failed) when not(Guide.IsVisible) ->
        StorageDevice.BeginShowSelector(
            p,
            (fun (result : IAsyncResult) ->
                let storage_dev = StorageDevice.EndShowSelector(result)
                if storage_dev <> null
                then
                    completion storage_dev
                    { data with state = Ready ; userStorage = Some storage_dev }
                    |> returnData
                else
                    failed()
                    { data with state = Ready ; userStorage = None }
                    |> returnData
            ), null)
        |> ignore
        { data with state = RequestingUserStorage }
        
    | StartReqUserStorage _ ->
        data
        
    | StartSignIn(p, req_storage_completion, req_storage_failed) when not(Guide.IsVisible) ->
        Guide.ShowSignIn(1, false)
        { data with state = SigningIn(p, req_storage_completion, req_storage_failed) }
        
    | StartSignIn _ ->
        data
        
    | SigningIn(p, req_storage_completion, req_storage_failed) when not(Guide.IsVisible) ->
        if Gamer.SignedInGamers.[p] = null
        then
            Guide.BeginShowMessageBox(
                p,
                "Not signed in",
                "You must be signed in in order to load and save user settings",
                [|"Sign in now"; "Don't sign in"|],
                0,
                MessageBoxIcon.Alert,
                (fun result ->
                    let choice = Guide.EndShowMessageBox(result)
                    if choice.HasValue && choice.Value = 0
                    then
                        { data with state = StartSignIn(p, req_storage_completion, req_storage_failed) }
                        |> returnData
                    else
                        { data with state = Ready }
                        |> returnData
                ),
                null)
            |> ignore
            { data with state = NotSignedInInfo }
        else
            { data with state = StartReqUserStorage(p, req_storage_completion, req_storage_failed) }
            
    | SigningIn _ ->
        data
        
    | DoingIO | RequestingTitleStorage | RequestingUserStorage | NotSignedInInfo ->
        data
    
    | Ready ->
        data
        |> resetDisconnectedStorage        

            
type IOAction = delegate of Stream -> unit

type StorageEventArgs(storage : StorageDevice) =
    inherit EventArgs()
    
    member x.StorageDevice = storage
    
type StorageEventHandler = EventHandler<StorageEventArgs>

type StorageComponent(game) =
    inherit GameComponent(game)
    
    let title_storage_acquired_event = new Control.Event<StorageEventArgs>()
    let title_storage_not_acquired_event = new Control.Event<EventArgs>()
    let title_storage_lost_event = new Control.Event<StorageEventArgs>()
    let user_storage_acquired_event = new Control.Event<StorageEventArgs>()
    let user_storage_not_acquired_event = new Control.Event<EventArgs>()
    let user_storage_lost_event = new Control.Event<StorageEventArgs>()
    
    let data = ref { state = Ready ; titleStorage = None ; userStorage = None }

    let setData data' =
        lock data (fun () -> data := data')
    
    let getData() =
        lock data (fun () -> !data)

    let getDoSet f =
        let d0 = getData()
        let d' = f d0
        lock data (fun () ->
            // Check if a side effect in f changed data before setting data to d'
            // This is needed when Guide.Begin* methods execute immediately (e.g. on the PC)
            if Object.ReferenceEquals(!data, d0)
            then
                data := d'
            else
                ()
            )

    member x.IsReady
        with get () =
            let d = getData()
            match d.state with
            | Ready -> true
            | _     -> false

    member x.IsTitleStorageEnabled
        with get () =
            let d = getData()
            d.titleStorage.IsSome
    
    member x.IsUserStorageEnabled
        with get () =
            let d = getData()
            d.userStorage.IsSome
            
    member x.RequestTitleStorage() =
        getDoSet (requestTitleStorage
            (fun storage -> title_storage_acquired_event.Trigger(new StorageEventArgs(storage)))
            (fun () -> title_storage_not_acquired_event.Trigger(new EventArgs())))
    
    member x.RequestUserStorage(p : PlayerIndex) =
        getDoSet (requestUserStorage
            (fun storage -> user_storage_acquired_event.Trigger(new StorageEventArgs(storage)))
            (fun () -> user_storage_not_acquired_event.Trigger(new EventArgs()))
            p)
        
    member x.ResetTitleStorage() =
        getDoSet resetTitleStorage
    
    member x.ResetUserStorage() =
        getDoSet resetUserStorage
    
    member x.DoTitleStorageIO(filename : string, mode : FileMode, operation : IOAction, completionHandler : Action, excHandler : Action<Exception>) =
        getDoSet (doTitleStorageIO setData filename mode operation.Invoke completionHandler.Invoke excHandler.Invoke)
    
    member x.DoUserStorageIO(containerName : string, filename : string, mode : FileMode, operation : IOAction, completionHandler : Action, excHandler : Action<Exception>) =
        getDoSet (doUserStorageIO setData containerName filename mode operation.Invoke completionHandler.Invoke excHandler.Invoke)
        
    override x.Update(dt : GameTime) =
        let bak_user_storage = (!data).userStorage
        let bak_title_storage = (!data).titleStorage
        
        getDoSet (update setData dt)
    
        match bak_user_storage, (!data).userStorage with
        | Some storage, None when not (storage.IsConnected) -> user_storage_lost_event.Trigger (new StorageEventArgs(storage))
        | _ -> ()
        
        match bak_title_storage, (!data).userStorage with
        | Some storage, None when not (storage.IsConnected) -> title_storage_lost_event.Trigger (new StorageEventArgs(storage))
        | _ -> ()
         
    [<CLIEvent>]
    member x.TitleStorageAcquired = title_storage_acquired_event.Publish
    
    [<CLIEvent>]
    member x.TitleStorageNotAcquired = title_storage_not_acquired_event.Publish
    
    [<CLIEvent>]
    member x.TitleStorageLost = title_storage_lost_event.Publish
    
    [<CLIEvent>]
    member x.UserStorageAcquired = user_storage_acquired_event.Publish
    
    [<CLIEvent>]
    member x.UserStorageNotAcquired = user_storage_not_acquired_event.Publish
    
    [<CLIEvent>]
    member x.UserStorageLost = user_storage_lost_event.Publish
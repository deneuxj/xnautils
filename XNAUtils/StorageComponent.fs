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

open XNAUtils.CircularQueue

type RunningThreadState<'R>() =
    let mutable is_done = false
    let mutable result = Unchecked.defaultof<'R>
    
    member x.Complete(r) =
        lock x (fun () ->
            result <- r
            is_done <- true
        )            
    
    member x.IsCompleted =
        lock x (fun () ->
            is_done
        )
    
    member x.Result =
        lock x (fun () ->
            result
        )
        
type State =
    | StartReqTitleStorage of (StorageDevice -> unit) * (unit -> unit)
    | RequestingTitleStorage of IAsyncResult * (StorageDevice -> unit) * (unit -> unit)
    | FailedReqTitleStorage of (unit -> unit)
    | DoneReqTitleStorage of (unit -> unit)
    | StartSignIn of PlayerIndex * (StorageDevice -> unit) * (unit -> unit)
    | SigningIn of PlayerIndex  * (StorageDevice -> unit) * (unit -> unit)
    | NotSignedInInfo of IAsyncResult * PlayerIndex  * (StorageDevice -> unit) * (unit -> unit)
    | StartReqUserStorage of PlayerIndex * (StorageDevice -> unit) * (unit -> unit)
    | RequestingUserStorage of IAsyncResult * (StorageDevice -> unit) * (unit -> unit)
    | FailedReqUserStorage of (unit -> unit)
    | DoneReqUserStorage of (unit -> unit)
    | DoingIO of RunningThreadState<State>
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
    containerName filename
    (mode : FileMode)
    operation completion (excHandler : Exception -> unit) data =
    
    match getStorageDevice data with
    | Some storage_dev ->
        let thread_state = new RunningThreadState<State>()
        
        // FIXME: Exception handling. I don't think XNA 4.0 behaves the same way XNA 3.1 did when it comes to what methods throw which exceptions.
        let callback =
            fun (result : IAsyncResult) ->
                try
                    use container = storage_dev.EndOpenContainer(result)
                    if container <> null
                    then
                        if mode = FileMode.Open && not(container.FileExists(filename))
                        then
                            FailedIO(fun () ->
                                FileNotFoundException(filename)
                                |> excHandler)
                            |> thread_state.Complete
                        else
                            use stream = container.OpenFile(filename, mode)
                            operation(stream)
                            SuccessfulIO(completion)
                            |> thread_state.Complete
                with
                    | exc ->
                        match exc with
                        | :? GamerPrivilegeException | :? InvalidOperationException | :? StorageDeviceNotConnectedException ->
                            FailedIO (fun () -> excHandler(exc))
                            |> thread_state.Complete
                        | _ -> failwith "Unexpected exception"
            |> fun f -> new AsyncCallback(f)

        storage_dev.BeginOpenContainer(
            containerName,
            callback,
            null)
        |> ignore

        { data with state = DoingIO thread_state }
    
    | _ -> raise (new InvalidOperationException())

let doTitleStorageIO filename mode operation completion excHandler data =
    match data.state with
    | Ready ->
        doStorageIO
            (fun (data : Data) -> data.titleStorage)
            (fun dev (data : Data) -> { data with titleStorage = dev })
            "TitleStorage"
            filename
            mode
            operation
            completion
            excHandler
            data
    | _ -> raise (new InvalidOperationException())

let doUserStorageIO containerName filename mode operation completion excHandler data =
    match data.state with
    | Ready ->
        doStorageIO
            (fun (data : Data) -> data.userStorage)
            (fun dev (data : Data) -> { data with userStorage = dev })
            containerName
            filename
            mode
            operation
            completion
            excHandler
            data
    | _ -> raise (new InvalidOperationException())

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

let update (dt : GameTime) (data : Data) =
    match data.state with        
    | StartReqTitleStorage(completion, failed) when not(Guide.IsVisible) ->
        try
            let async_res =
                StorageDevice.BeginShowSelector(
                    (fun _ -> ()), null)
            { data with state = RequestingTitleStorage (async_res, completion, failed) }
        with
        | :? GuideAlreadyVisibleException -> data
        
    | StartReqTitleStorage(_, _) ->
        data
        
    | StartReqUserStorage(p, completion, failed) when not(Guide.IsVisible) ->
        try
            let async_res =
                StorageDevice.BeginShowSelector(
                    p,
                    (fun _ -> ()), null)
            { data with state = RequestingUserStorage (async_res, completion, failed) }
        with
        | :? GuideAlreadyVisibleException -> data
        
    | StartReqUserStorage _ ->
        data
        
    | StartSignIn(p, req_storage_completion, req_storage_failed) when not(Guide.IsVisible) ->
        try 
            Guide.ShowSignIn(1, false)
            { data with state = SigningIn(p, req_storage_completion, req_storage_failed) }
        with
        | :? GuideAlreadyVisibleException -> data
                
    | StartSignIn _ ->
        data
        
    | SigningIn(p, req_storage_completion, req_storage_failed) when not(Guide.IsVisible) ->
        if Gamer.SignedInGamers.[p] = null
        then
            try
                let async_res =
                    Guide.BeginShowMessageBox(
                        p,
                        "Not signed in",
                        "You must be signed in in order to load and save user settings",
                        [|"Sign in now"; "Don't sign in"|],
                        0,
                        MessageBoxIcon.Alert,
                        (fun _ -> ()), 
                        null)
                { data with state = NotSignedInInfo (async_res, p, req_storage_completion, req_storage_failed) }
            with
            | :? GuideAlreadyVisibleException -> data
        else
            { data with state = StartReqUserStorage(p, req_storage_completion, req_storage_failed) }
            
    | SigningIn _ ->
        data
    
    | RequestingTitleStorage (async_res, completion, failed) ->
        if async_res.IsCompleted
        then
            let storage_dev = StorageDevice.EndShowSelector(async_res)
            if storage_dev <> null
            then
                { data with state = DoneReqTitleStorage (fun () -> completion storage_dev); titleStorage = Some storage_dev }
            else
                { data with state = FailedReqTitleStorage failed ; titleStorage = None }
        else
            data
    
    | RequestingUserStorage (async_res, completion, failed) ->
        if async_res.IsCompleted
        then
            let storage_dev = StorageDevice.EndShowSelector(async_res)
            if storage_dev <> null
            then
                { data with state = DoneReqUserStorage (fun () -> completion storage_dev) ; userStorage = Some storage_dev }
            else
                { data with state = FailedReqUserStorage failed ; userStorage = None }
        else
            data
    
    | SuccessfulIO f ->
        f()
        { data with state = Ready }
        
    | DoneReqTitleStorage f | DoneReqUserStorage f | FailedReqTitleStorage f | FailedReqUserStorage f | SuccessfulIO f | FailedIO f  ->
        f()
        { data with state = Ready }
    
    | NotSignedInInfo (async_res, p, req_storage_completion, req_storage_failed) ->
        if async_res.IsCompleted
        then
            let choice = Guide.EndShowMessageBox(async_res)
            if choice.HasValue && choice.Value = 0
            then
                { data with state = StartSignIn(p, req_storage_completion, req_storage_failed) }
            else
                { data with state = Ready }
        else
            data
        
    | DoingIO (thread_state) ->
        if thread_state.IsCompleted
        then
            { data with state = thread_state.Result } 
        else
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
    let queued_ops : (Data -> Data) CircularQueue = newQueue 4
    let title_storage_acquired_event = new Control.Event<StorageEventArgs>()
    let title_storage_not_acquired_event = new Control.Event<EventArgs>()
    let title_storage_lost_event = new Control.Event<StorageEventArgs>()
    let user_storage_acquired_event = new Control.Event<StorageEventArgs>()
    let user_storage_not_acquired_event = new Control.Event<EventArgs>()
    let user_storage_lost_event = new Control.Event<StorageEventArgs>()
    
    let data = ref { state = Ready ; titleStorage = None ; userStorage = None }

    let setData data' =
        data := data'
    
    let getData() =
        !data

    let getDoSet f =
        let d0 = getData()
        let d' = f d0
        setData d'

    let isReady state =
        match state with
        | Ready -> true
        | _ -> false
        
    let execQueued() =
        if isReady (getData().state) && not (CircularQueue.isEmpty queued_ops) then
            let op = CircularQueue.pick queued_ops
            getDoSet op

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
        CircularQueue.add queued_ops (requestTitleStorage
            (fun storage -> title_storage_acquired_event.Trigger(new StorageEventArgs(storage)))
            (fun () -> title_storage_not_acquired_event.Trigger(new EventArgs())))
    
    member x.RequestUserStorage(p : PlayerIndex) =
        CircularQueue.add queued_ops (requestUserStorage
            (fun storage -> user_storage_acquired_event.Trigger(new StorageEventArgs(storage)))
            (fun () -> user_storage_not_acquired_event.Trigger(new EventArgs()))
            p)
        
    member x.ResetTitleStorage() =
        CircularQueue.add queued_ops resetTitleStorage
    
    member x.ResetUserStorage() =
        CircularQueue.add queued_ops resetUserStorage
    
    member x.DoTitleStorageIO(filename : string, mode : FileMode, operation : IOAction, completionHandler : Action, excHandler : Action<Exception>) =
        CircularQueue.add queued_ops (doTitleStorageIO filename mode operation.Invoke completionHandler.Invoke excHandler.Invoke)
    
    member x.DoUserStorageIO(containerName : string, filename : string, mode : FileMode, operation : IOAction, completionHandler : Action, excHandler : Action<Exception>) =
        CircularQueue.add queued_ops (doUserStorageIO containerName filename mode operation.Invoke completionHandler.Invoke excHandler.Invoke)
        
    override x.Update(dt : GameTime) =
        execQueued()
        
        let bak_user_storage = (!data).userStorage
        let bak_title_storage = (!data).titleStorage
        
        getDoSet (update dt)
    
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
// Learn more about F# at http://fsharp.net

namespace Microsoft.Xna.Framework.Storage

open System
open System.IO
open System.Windows.Forms
open System.Threading.Tasks
open Microsoft.Xna.Framework

open Dialogs

type StorageDeviceNotConnectedException() =
    inherit Exception()


[<AllowNullLiteral>]
type StorageDevice =
    class
        val drive : DriveInfo
        val path : string
        new (d, p) = { drive = d; path = p }
    end


[<AllowNullLiteral>]
type StorageContainer =
    class
        val name : string
        val device : StorageDevice
        val mutable is_disposed : bool
    
        new(name, dev) = { name = name; device = dev; is_disposed = false }
    end
with interface IDisposable with
        member this.Dispose() = this.is_disposed <- true


module StorageInternals =
    let checkConnected(dev : StorageDevice) =
        if not dev.drive.IsReady then raise(StorageDeviceNotConnectedException())


    let selectDevice _ =
        let dialog = new FolderBrowser("Select device directory", None)
        async {
            try
                do! Async.SwitchToContext(gui_context)
                dialog.Show()
                let! _ = Async.AwaitEvent(dialog.Closed)
                return
                    match dialog.Selected with
                    | Some path ->
                        if Directory.Exists(path) then
                            let candidates =
                                DriveInfo.GetDrives()
                                |> Array.filter (fun di -> di.RootDirectory.Name = Path.GetPathRoot(path))
                            if candidates.Length > 0 then
                                new StorageDevice(candidates.[0], path)
                            else
                                null
                        else
                            null
                    | None ->
                        null
            finally
                dialog.Dispose()
        }
        |> Async.RunSynchronously

    let openContainer(dev : StorageDevice, name) _ =
        let dialog = new FolderBrowser(sprintf "Select container directory [%s]" name, Some dev.path)
        async {
            try
                do! Async.SwitchToContext(gui_context)
                dialog.Show()
                let! _ = Async.AwaitEvent(dialog.Closed)
                return
                    match dialog.Selected with
                    | Some path ->
                        if not(Directory.Exists(path)) then
                            Directory.CreateDirectory(path) |> ignore
                        new StorageContainer(name, dev)
                    | None ->
                        null
            finally
                dialog.Dispose()
        }
        |> Async.RunSynchronously


type StorageDevice with
    member this.FreeSpace : int64 =
        if this.drive.IsReady then
            try
                this.drive.AvailableFreeSpace
            with
            | :? UnauthorizedAccessException
            | :? IOException -> raise (StorageDeviceNotConnectedException())
        else
            raise(StorageDeviceNotConnectedException())

    member this.IsConnected : bool =
        this.drive.IsReady

    member this.TotalSpace : int64 =
        if this.drive.IsReady then
            try
                this.drive.TotalSize
            with
            | :? UnauthorizedAccessException
            | :? IOException -> raise (StorageDeviceNotConnectedException())
        else
            raise(StorageDeviceNotConnectedException())

    static member BeginShowSelector(cb : AsyncCallback, state : Object) =
        let f = new Task<_>(StorageInternals.selectDevice, state)

        Dialogs.doThenMaybeCallback(f, cb)

        f :> IAsyncResult

    static member BeginShowSelector(player : PlayerIndex, cb, state) =
        StorageDevice.BeginShowSelector(cb, state)

    static member EndShowSelector(result : IAsyncResult) =
        let task = result :?> Task<StorageDevice>
        task.Wait()
        task.Result

    member this.BeginOpenContainer(displayName : string, cb : AsyncCallback, state : Object) =
        let f = new Task<_>(StorageInternals.openContainer(this, displayName), state)

        Dialogs.doThenMaybeCallback(f, cb)

        f :> IAsyncResult

    member this.EndOpenContainer(result : IAsyncResult) =
        let task = result :?> Task<StorageContainer>
        task.Wait()
        task.Result


type StorageContainer with
    member this.DisplayName = this.name
    member this.IsDisposed = this.is_disposed
    member this.StorageDevice = this.device

    member this.CreateDirectory(dir) =
        StorageInternals.checkConnected(this.device)
        let path = Path.Combine(this.device.path, this.name, dir)
        Directory.CreateDirectory(path) |> ignore

    member this.DeleteDirectory(dir) =
        StorageInternals.checkConnected(this.device)
        let path = Path.Combine(this.device.path, this.name, dir)
        Directory.Delete(path) |> ignore

    member this.DirectoryExists(dir) =
        StorageInternals.checkConnected(this.device)
        let path = Path.Combine(this.device.path, this.name, dir)
        Directory.Exists(path)

    member this.GetDirectoryNames() =
        StorageInternals.checkConnected(this.device)
        let path = Path.Combine(this.device.path, this.name)
        Directory.EnumerateDirectories(path)
        |> Array.ofSeq

    member this.GetDirectoryNames(pattern) =
        StorageInternals.checkConnected(this.device)
        let path = Path.Combine(this.device.path, this.name)
        Directory.EnumerateDirectories(path, pattern)
        |> Array.ofSeq

    member this.CreateFile(file) =
        StorageInternals.checkConnected(this.device)
        let path = Path.Combine(this.device.path, this.name, file)
        File.Create(path) :> Stream

    member this.DeleteFile(file) =
        StorageInternals.checkConnected(this.device)
        let path = Path.Combine(this.device.path, this.name, file)
        File.Delete(path)

    member this.FileExists(file) =
        StorageInternals.checkConnected(this.device)
        let path = Path.Combine(this.device.path, this.name, file)
        File.Exists(path)

    member this.GetFileNames() =
        StorageInternals.checkConnected(this.device)
        let path = Path.Combine(this.device.path, this.name)
        Directory.EnumerateFiles(path)
        |> Array.ofSeq

    member this.GetFileNames(pattern) =
        StorageInternals.checkConnected(this.device)
        let path = Path.Combine(this.device.path, this.name)
        Directory.EnumerateFiles(path, pattern)
        |> Array.ofSeq

    member this.OpenFile(name, mode) =
        StorageInternals.checkConnected(this.device)
        let path = Path.Combine(this.device.path, this.name, name)
        File.Open(path, mode) :> Stream

    member this.OpenFile(name, mode, access) =
        StorageInternals.checkConnected(this.device)
        let path = Path.Combine(this.device.path, this.name, name)
        File.Open(path, mode, access) :> Stream

    member this.OpenFile(name, mode, access, share) =
        StorageInternals.checkConnected(this.device)
        let path = Path.Combine(this.device.path, this.name, name)
        File.Open(path, mode, access, share) :> Stream

    member this.Dispose() = (this :> IDisposable).Dispose()
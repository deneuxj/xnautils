// This file is a script that can be executed with the F# Interactive.  
// It can be used to explore and test the library project.
// Note that script files will not be part of the project build.

#r "Microsoft.Xna.Framework.dll"

#load "Dialogs.fs"
#load "Storage.fs"

open Microsoft.Xna.Framework.Storage

async {
    let res = StorageDevice.BeginShowSelector(null, null)
    let! _ = Async.AwaitIAsyncResult(res)
    let dev = StorageDevice.EndShowSelector(res)

    if dev <> null then
        let res = dev.BeginOpenContainer("Test", null, null)
        let! _ = Async.AwaitIAsyncResult(res)
        let container = dev.EndOpenContainer(res)
        ()
}
|> Async.StartImmediate
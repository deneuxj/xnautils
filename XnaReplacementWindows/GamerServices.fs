namespace Microsoft.Xna.Framework.GamerServices

open System
open System.Windows.Forms
open System.Threading
open System.Threading.Tasks

open Microsoft.Xna.Framework

                
module Internals =
    let is_visible = ref false

    let signed_in_gamer : string option ref = ref None

    let showMessageBox (title : string, text : string, buttons : string seq, focusButton : int) (_ : obj) =
        let msgBox = new Dialogs.MessageBox(title, text, buttons, focusButton)
        async {
            do! Async.SwitchToContext(Dialogs.gui_context)
            msgBox.Show()
            let! _ = Async.AwaitEvent(msgBox.Closed)
            return
                match msgBox.Selected with
                | Some i -> Nullable(i)
                | None -> Nullable()
        }
        |> Async.RunSynchronously

    let showSignIn () =
        let diag = new Dialogs.SingleSignInDialog()
        async {
            do! Async.SwitchToContext(Dialogs.gui_context)
            let! _ = Async.AwaitEvent(diag.Closed)
            lock signed_in_gamer (fun () ->
                signed_in_gamer := diag.Gamertag)                
        }
        |> Async.StartImmediate

type MessageBoxIcon =
    | Alert = 0
    | Error = 1
    | None = 2
    | Warning  = 3


[<AllowNullLiteral>]
type GamerPrivileges() =
    member this.AllowCommunication = false
    member this.AllowOnlineSessions = false
    member this.AllowPremiumContent = false
    member this.AllowProfileViewing = false
    member this.AllowPurchaseContent = false
    member this.AllowTradeContent = false
    member this.AllowUserCreatedContent = false


[<AllowNullLiteral>]
type Gamer(gamertag) =
    member this.Gamertag = gamertag
    static member SignedInGamers =
        new SignedInGamerCollection(
            match !Internals.signed_in_gamer with
            | None -> Array.create 4 null
            | Some s -> [| new SignedInGamer(s, PlayerIndex.One); null; null; null |]
        )

and
    [<AllowNullLiteral>]
    SignedInGamerCollection(gamers : SignedInGamer[]) =
    member this.Count = gamers.Length
    member this.Item(i : int) = gamers.[i]
    member this.Item(p : PlayerIndex) = gamers.[int p]

and
    [<AllowNullLiteral>]
    SignedInGamer(gamertag, pi : PlayerIndex) =
    inherit Gamer(gamertag)

    member this.PlayerIndex = pi
    member this.IsSignedInToLive = false
    member this.Privileges = new GamerPrivileges()


type GuideAlreadyVisibleException() =
    inherit Exception()


[<CompilationRepresentation(CompilationRepresentationFlags.Static)>]
type Guide =
    class        
        static member IsVisible = !Internals.is_visible

        static member IsTrialMode = false

        static member BeginShowMessageBox (player : PlayerIndex, title : string, text : string, buttons : string seq, focusButton : int, icon : MessageBoxIcon, cb, state : Object) =
            let f = new Task<_>(Internals.showMessageBox(title, text, buttons, focusButton), state)

            Dialogs.doThenMaybeCallback(f, cb)

            f :> IAsyncResult

        static member BeginShowMessageBox (title : string, text : string, buttons : string seq, focusButton : int, icon : MessageBoxIcon, cb, state : Object) =
            let f = new Task<_>(Internals.showMessageBox(title, text, buttons, focusButton), state)

            Dialogs.doThenMaybeCallback(f, cb)

            f :> IAsyncResult

        static member EndShowMessageBox(result : IAsyncResult) =
            let task = result :?> Task<Nullable<int>>
            task.Wait()
            task.Result

        static member ShowSignIn(paneCount: int, onlineOnly : bool) =
            if paneCount <> 1 then
                raise (NotImplementedException("paneCount must be 1"))
            Internals.showSignIn()

        static member ShowMarketplace(playerIndex : PlayerIndex) =
            raise (new NotImplementedException())
    end

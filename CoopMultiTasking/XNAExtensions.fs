module CleverRake.XnaUtils.XnaExtensions

open Microsoft.Xna.Framework

type GamerServices.SignedInGamerCollection with
    member this.ItemOpt(i : int) =
        match this.[i] with
        | null -> None
        | s -> Some s

    member this.ItemOpt(player : PlayerIndex) =
        match this.[player] with
        | null -> None
        | s -> Some s

type GamerServices.Gamer with
    static member IsSignedIn(player : PlayerIndex) =
        GamerServices.Gamer.SignedInGamers.ItemOpt(player).IsSome

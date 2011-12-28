namespace CleverRake.XnaUtils

(* Maybe
   Computations which are implicitly aborted when failures occur.
   Failures are denoted by None values.
 *)
module Maybe =
    type Maybe<'a> = option<'a>
 
    let succeed x = Some(x)
    let fail = None
    let bind p rest =
        match p with
            | None -> fail
            | Some r -> rest r
    let delay f = f()
 
    type MaybeBuilder() =
        member b.Return(x)  = succeed x
        member b.Bind(p, rest) = bind p rest
        member b.Delay(f)   = delay f
        member b.Let(p,rest) : Maybe<'a> = rest p
   
    let maybe = MaybeBuilder()


module RepeatUntil =
    /// Repeat f until it returns Some(x), and return x.
    let repeatUntilSome f =
        Seq.initInfinite (fun _ -> f())
        |> Seq.find Option.isSome
        |> Option.get

    /// Repeat f until it returns true. Returns nothing.
    let repeatUntilTrue f =
        Seq.initInfinite (fun _ -> f())
        |> Seq.find id
        |> ignore


module Option =
    /// Returns v if the second parameter is None, otherwise x if its value is Some(x)
    let optIfNone v =
        function
        | None -> v
        | Some x -> x
                
(* Parallel map *)

module Parallel =
#if DEBUG

    let pmap = Array.map
    let pmapi = Array.mapi

#else

    let pmap f arr =
        Array.Parallel.map f arr

    let pmapi f arr =
        Array.Parallel.mapi f arr

#endif

module EvilNull =
    let (|NonNull|Null|) x =
        if x = null then Null
        else NonNull x

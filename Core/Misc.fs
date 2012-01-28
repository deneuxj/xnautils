namespace CleverRake.XnaUtils

(* Maybe
   Computations which are implicitly aborted when failures occur.
   Failures are denoted by None values.
 *)
module Maybe =
    type Maybe<'a> = option<'a>
 
    let succeed x = Some(x)

    let fail = None

    let bind rest p =
        match p with
            | None -> fail
            | Some r -> rest r

    let rec whileLoop cond body =
        if cond() then
            match body() with
            | Some() ->
                whileLoop cond body
            | None ->
                fail
        else
            succeed()

    let combine e1 e2 =
        e1 |> bind (fun () -> e2)

    let forLoop (xs : 'T seq) f =
        using (xs.GetEnumerator()) (fun it ->
                whileLoop
                    (fun () -> it.MoveNext())
                    (fun () -> it.Current |> f)
            )

    type MaybeBuilder() =
        member b.Return(x)  = succeed x
        member x.ReturnFrom(r) = r
        member b.Bind(p, rest) = p |> bind rest
        member b.Let(p,rest) : Maybe<'a> = rest p
        member x.Combine(e1, e2) = combine e1 e2
        member x.For(xs, body) = forLoop xs body
        member x.Zero() = succeed()

    let maybe = MaybeBuilder()

module MaybeTests =
    open Maybe

    let testCombine() =
        maybe {
            printfn "Hello"
            return Some()
        }

    let testZero() =
        maybe {
            printfn "Hello"
        }

    let testFor() =
        maybe {
            for x in [Some "Should be printed"; None; Some "Should NOT be printed"] do
                let! x = x
                printfn "%s" x
        }

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


module Dispose =
    let dispose (d : #System.IDisposable) =
        d.Dispose()
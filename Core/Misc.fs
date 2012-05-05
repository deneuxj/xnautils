namespace CleverRake.XnaUtils

(* Maybe
   Computations which are implicitly aborted when failures occur.
   Failures are denoted by None values.
 *)
module Maybe =
    type Maybe<'a> = option<'a>
 
    let succeed x = Some(x)

    let fail = None

    let delay (f : unit -> Maybe<'a>) = f
    
    let run (f : unit -> Maybe<'a>) = f()

    let bind rest p =
        match p with
            | None -> fail
            | Some r -> rest r

    let rec whileLoop cond body =
        if cond() then
            body() |> bind (fun () -> whileLoop cond body)
        else
            succeed()
    
    let combine e1 (f2 : unit -> Maybe<'a>) =
        e1 |> bind f2

    let tryWith (f : unit -> Maybe<'a>) handler =
        let res =
            try
                Choice1Of2 (f())
            with
                e -> Choice2Of2 e

        match res with
        | Choice1Of2 res -> res
        | Choice2Of2 exc -> handler exc

    let tryFinally (f : unit -> Maybe<'a>) compensation =
        try
            f()
        finally
            compensation()
                            
    let forLoop (xs : 'T seq) f =
        using (xs.GetEnumerator()) (fun it ->
                whileLoop
                    (fun () -> it.MoveNext())
                    (fun () -> it.Current |> f)
            )

    type MaybeBuilder() =
        member this.Delay(e) = delay e
        member this.Run(f) = run f
        member this.Return(x)  = succeed x
        member this.ReturnFrom(r) = r
        member this.Bind(p, rest) = p |> bind rest
        member this.Let(p,rest) : Maybe<'a> = rest p
        member this.Combine(e1, e2) = combine e1 e2
        member this.Zero() = succeed()
        member this.For(xs, body) = forLoop xs body
        member this.While(cond, body) = whileLoop cond body
        member this.TryWith(body, handler) = tryWith body handler
        member this.TryFinally(body, compensation) = tryFinally body compensation

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

    let testWhile() =
        maybe {
            let x = ref 3
            while x.Value >= 0 do
                x := x.Value - 1
                let! n = Some x.Value
                printfn "%d" n
            while x.Value <= 3 do
                x := x.Value + 1
                let! n = if x.Value <= 2 then Some x.Value else None
                printfn "%d" n
        }

    let testTryWith() =
        maybe {
            try
                printfn "Printed 1"
                failwith "Stop"
                printfn "Not printed"
            with
                e -> printfn "Printed 2"
        }

    let testTryFinally() =
        maybe {
            try
                printfn "Printed 1"
            finally
                printfn "Printed 2"

            try
                try
                    printfn "Printed 3"
                    failwith "Stop"
                    printfn "Not printed"
                finally
                    printfn "Printed 4"
            with
                _ -> ()

            try
                printfn "Printed 5"
                let! n = None
                printfn "Not printed"
            finally
                printfn "Printed 6"
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
                

module EvilNull =
    let (|NonNull|Null|) x =
        if x = null then Null
        else NonNull x


module Dispose =
    let dispose (d : #System.IDisposable) =
        d.Dispose()
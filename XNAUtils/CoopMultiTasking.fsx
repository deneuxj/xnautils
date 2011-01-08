#r @"bin\Debug\XNAUtils.dll"

open XNAUtils.CoopMultiTasking

type M =
    | Delay of (unit -> M)
    | Bind of M * (unit -> M)
    | Return of obj
    | ReturnFrom of M
    | Combine of M * M
    | Zero
    | TryWith of M * (System.Exception -> M)
    | TryFinally of M * (unit -> unit)
    | While of (unit -> bool) * M
    | For of (obj seq) * (obj -> M)

type DummyBuilder() =                
    member x.Bind(e, f) =
        Bind(e, f)

    member x.Return(r) =
        Return r

    member x.ReturnFrom(r) =
        ReturnFrom r

    member x.Combine(e1, e2) =
        Combine(e1, e2)

    member x.Delay(f) =
        Delay f

    member x.Zero() =
        Zero
    
    member x.TryWith(e, h) =
        TryWith(e, h)

    member x.TryFinally(e, c) =
        TryFinally(e, c)

    member x.While(cond, body) =
        While(cond, body)

    member x.For(objs, body) =
        For(objs, body)

let debug = DummyBuilder()

let rec trace indent m : string list =
    let wrap (kw : string) (sub : string list) : string list =
        (sprintf "%sBEGIN %s" indent kw) :: sub @ [sprintf "%sEND %s" indent kw]

    match m with
    | Delay f ->
        let f' = f() |> trace (indent + " ")
        wrap "Delay" f'

    | Bind(e, f) ->
        let f' = f() |> trace (indent + " ")
        let e' = e |> trace (indent + " ")
        wrap "Bind" (("e:" :: e') @ ("fun() ->" :: f'))

    | Combine(e1, e2) ->
        let e1' = e1 |> trace (indent + " ")
        let e2' = e2 |> trace (indent + " ")
        wrap "Combine" (e1' @ e2')

    | Return(v) ->
        wrap "Return" [sprintf "%s %A" indent v]

    | ReturnFrom(e) ->
        let e' = e |> trace (indent + " ")
        wrap "ReturnFrom" e'

    | For(objs, f) ->
        let objs' = sprintf "%A" objs
        let f' = f() |> trace (indent + " ")
        wrap "For" (objs' :: f')

    | TryFinally(e, f) ->
        e
        |> trace (indent + " ")
        |> wrap "TryFinally"

    | TryWith(e, h) ->
        e
        |> trace (indent + " ")
        |> wrap "TryWith"

    | While(cond, body) ->
        body
        |> trace (indent + " ")
        |> wrap "While"

    | Zero -> wrap "Zero" []


let f() =
    let d = debug {
        while true do
            do! debug {
                return 123
            }
    }

    let s = trace "" d

    for d in s do printfn "%s" d;;

(* tests *)

let runAllFixed (scheduler : Scheduler) dt evs =
    evs
    |> Array.iter (fun t -> scheduler.AddTask(t))

    while scheduler.HasLiveTasks do
        scheduler.RunFor(dt)

let test1() =
    let t1 = task {
        for i in 0..3 do
            do! wait 3.0f
        printfn "Hello"
    }
    runAllFixed (new Scheduler()) 1.0f [|t1|]

let test2() =
    let c = BlockingChannel<string>()

    let sender = task {
        do! c.Send("Hello receiver")
    }

    let receiver = task {
        let! msg = c.Receive()
        printfn "Received: %s" msg
    }

    let sys = [| sender ; receiver |]
    runAllFixed (new Scheduler()) 0.01f sys

exception IntEarly of int

let test3() =
    let bs = Array.init 10 id

    let find n = task {
        let i =
            try
                for i in 0..bs.Length-1 do
                    if bs.[i] = n then raise (IntEarly i)
                -1
            with
                IntEarly i -> i
        printfn "Result: %d" i
    }

    runAllFixed (new Scheduler()) 1.0f [| find 5 |]
    runAllFixed (new Scheduler()) 1.0f [| find 100 |]

let test4() =
    let t1 = task {
        let f () =
            use d = { new System.IDisposable with member this.Dispose() = printfn "1: Disposed" }
            printfn "0: Left"
        f()
        printfn "2: Done"
    }

    runAllFixed (new Scheduler()) 1.0f [| t1 |]

exception StrException of string

let test5() =
    let t1 = task {
        try
            try
                printfn "0: Started"
                raise (StrException "2: Caught")
                printfn "X: Mau says does not happen"
            finally
                printfn "1: finally"
        with
            StrException s -> printfn "%s" s
    }

    runAllFixed (new Scheduler()) 1.0f [| t1 |]

let test6() =
    let sch = new Scheduler()
    let sys = new Environment(sch)

    let t1 = task {
        let ticker =
            sys.Spawn(fun stop ->
                task {
                    let t = ref 0.0f
                    while not !stop do
                        printfn "Tick: %f" !t
                        t := !t + 0.5f
                        do! sys.Wait(0.5f)
                }
            )

        let controllers = Array.create 3 None
        for i in 0..2 do
            controllers.[i] <- Some
                (sys.SpawnRepeat(
                    task {
                        printfn "%i: Hop!" i
                        do! sys.Wait(1.0f)
                    })
                )
            do! sys.Wait(2.0f)
        do! wait(2.0f)
        
        controllers
        |> Array.iter (function None -> failwith "Unreachable" | Some c -> c.Kill())

        for i in 0..2 do
            match controllers.[i] with
            | None -> failwith "Unreachable"
            | Some c -> do! sys.WaitUntil(fun () -> c.IsDead)

        ticker.Kill()
        do! sys.WaitUntil(fun() -> ticker.IsDead)
    }

    runAllFixed sch 0.1f [| t1 |]

    
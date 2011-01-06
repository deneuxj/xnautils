#load "Heap.fs"
#load "CoopMultiTasking.fs"

open XNAUtils.CoopMultiTasking

(* tests *)

let runAllFixed dt evs =
    let scheduler = new Scheduler()
    
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
    runAllFixed 1.0f [|t1|]

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
    runAllFixed 0.01f sys

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

    runAllFixed 1.0f [| find 5 |]
    runAllFixed 1.0f [| find 100 |]

let test4() =
    let t1 = task {
        let f () =
            use d = { new System.IDisposable with member this.Dispose() = printfn "1: Disposed" }
            printfn "0: Left"
        f()
        printfn "2: Done"
    }

    runAllFixed 1.0f [| t1 |]

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

    runAllFixed 1.0f [| t1 |]
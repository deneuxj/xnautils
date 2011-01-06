﻿module XNAUtils.CoopMultiTasking

open XNAUtils.Heap

type Eventually<'R> =
    | Completed of 'R
    | Blocked of float32 * (unit -> Eventually<'R>)
    | BlockedNextFrame of (unit -> Eventually<'R>)
    | Running of (unit -> Eventually<'R>)
    | Yield of (unit -> Eventually<'R>)

let rec bind k e =
    match e with
    | Completed(r) -> Running(fun () -> k r)
    | Running(work) -> Running(fun () -> bind k (work()))
    | Blocked(dt, work) -> Blocked(dt, fun () -> bind k (work()))
    | BlockedNextFrame(work) -> BlockedNextFrame(fun () -> bind k (work()))
    | Yield(work) -> Yield(fun () -> work() |> bind k)

let result r = Completed(r)

let delay f = Running f

type OkOrException<'T> =
    | Ok of 'T
    | Exception of System.Exception

let rec catch e =
    let newWork work =
        let res = try Ok(work()) with e -> Exception e
        match res with
        | Ok cont -> catch cont
        | Exception e -> result(Exception e)
    match e with
    | Completed r -> result(Ok r)
    | Blocked(dt, work) -> Blocked(dt, fun () -> newWork work)
    | BlockedNextFrame(work) -> BlockedNextFrame(fun () -> newWork work)
    | Running work -> Running(fun() -> newWork work)
    | Yield work -> Yield(fun() -> newWork work)

(* Boiler-plate *)
let tryFinally e compensation =
    catch e
    |> bind (fun res ->
        compensation()
        match res with
        | Ok x -> result x
        | Exception e -> raise e)

let tryWith e handler =
    catch e
    |> bind (function
        | Ok x -> result x
        | Exception e -> handler e)

let rec whileLoop cond body =
    if cond() then
        body
        |> bind (fun () -> whileLoop cond body)
    else
        result()

let combine e1 e2 =
    e1 |> bind (fun () -> e2)

let using (r : #System.IDisposable) f =
    tryFinally (f r) (fun () -> r.Dispose())

let forLoop (xs : 'T seq) f =
    using (xs.GetEnumerator()) (fun it ->
            whileLoop (fun () -> it.MoveNext())
                (delay (fun () -> it.Current |> f))
        )

(* The builder type *)
type TaskBuilder() =
    member x.Bind(e, f) = bind f e
    member x.Return(r) = result r
    member x.ReturnFrom(r) = r
    member x.Combine(e1, e2) = combine e1 e2
    member x.Delay(f) = delay f
    member x.Zero() = result ()
    member x.TryWith(e, handler) = tryWith e handler
    member x.TryFinally(e, compensation) = tryFinally e compensation
    member x.While(e, f) = whileLoop e f
    member x.For(e, f) = forLoop e f
    member x.Using(e, f) = using e f

(* Task API *)
let task = TaskBuilder()

// Wait a fixed amount of time.
let wait dt =
    Blocked(dt, fun () -> Completed())

// Wait until next frame, i.e. next call to Scheduler.RunFor
let nextFrame () =
    BlockedNextFrame(fun() -> Completed())

// Stop executing, let some other task execute, if any is ready. Otherwise, we get control back.
let nextTask () =
    Yield(fun() -> Completed())

// Wait until a specified condition is true.
// nextTask is always called, then the condition is checked.
// If it's false, we wait until next frame, check the condition,
// call nextTask if it's not true, and so on...
let waitUntil f = task {
    let stop = ref false
    while not !stop do
        do! nextTask()
        if f() then
            stop := true
        else
            do! nextFrame()
            stop := f()
}

// Lock, can be used for mutual exclusion.
// Locks should not be shared across instances of Scheduler.
type Lock() =
    let mutable locked = false;

    member this.Grab() =
        task {
            do! waitUntil (fun() -> not locked)
            locked <- true
        }

    member this.Release() =
        task {
            locked <- false
            do! nextTask()
        }

// A blocking channel which can be used for cross-task communication.
// Note that sending blocks until the message is received.
type BlockingChannel<'M>() =
    let mutable content = None
    let mutable flag_send = false
    let mutable flag_receive = false

    member this.Send(m : 'M) =
        task {
            do! waitUntil (fun () -> not flag_send)
            content <- Some m
            flag_send <- true
            do! waitUntil (fun () -> flag_receive)
            flag_send <- false
            content <- None
        }

    member this.Receive() =
        task {
            do! waitUntil (fun () -> flag_send)
            let ret = content.Value
            flag_receive <- true
            do! waitUntil (fun () -> not flag_send)
            flag_receive <- false
            return ret
        }

(* Execution *)

type Scheduler() =
    // Ordered queue of tasks.
    let tasks : Heap<Eventually<unit> * int> = newHeap 4

    // Next id to use for tie-breaks.
    // It's important that tasks with identical priorities are added and picked in a FIFO fashion.
    let mutable id = System.Int32.MinValue

    // Ordering of tasks in the queue. RunFor repeatedly picks the minimum task.
    let cmp ((ev1, n1), (ev2, n2)) =
        match ev1, ev2 with
        
        // Tie-breaks        
        | Running _, Running _
        | Yield _, Yield _
        | Completed _, Completed _ -> n1 <= n2

        // Running is minimal
        | Running _, _ -> true

        // Then comes Yield
        | Yield _, Running _ -> false
        | Yield _, _ -> true
        
        // BlockedNextFrame before Blocked. RunFor turns these to Blocked(time to next frame, f).
        | BlockedNextFrame _, Running _ | BlockedNextFrame _, Yield _ -> false
        | BlockedNextFrame _, _ -> true

        // Blocked, ordered by time to wake-up.
        | Blocked _, Running _ | Blocked _, Yield _ | Blocked _, BlockedNextFrame _ -> false
        | Blocked (t1, _), Blocked (t2, _) -> t1 < t2 || t1 = t2 && n1 <= n2
        | Blocked _, Completed _ -> true

        // Completed is maximal (never executed)
        | Completed _, _ -> false

    let peekTask() =
        let t, _ = tasks.arr.[0]
        t

    let insertTask(t) =
        insert cmp tasks (t, id)
        id <- id + 1

    let takeTask() =
        take cmp tasks |> ignore

    member x.AddTask(t) =
        insertTask  t
    
    member x.HasLiveTasks =
        tasks.count > 0
        &&
        tasks.arr.[0..tasks.count-1]
        |> Array.exists (function Completed _, _ -> false | _ -> true)

    // Run tasks for the current frame. dt is the frame time (typically 0.016666... on Xbox for 60 fps)
    member x.RunFor(dt) =
        // Let time pass when there are no task ready to be executed.
        let decrBlockingTimes delta =
            for i in 0..tasks.count-1 do
                tasks.arr.[i] <-
                    match tasks.arr.[i] with
                    | Yield _, _ | Running _, _ | BlockedNextFrame _, _ -> failwith "All tasks should be blocked."
                    | Blocked (t, f), n -> Blocked (t-delta, f), n
                    | Completed (), _ as v -> v

        // Repeatedly pick and execute a task, until there are only blocked tasks left (with a waiting time larger than the time to the next frame).
        let rec work dt =
            if dt > 0.0f && tasks.count > 0 then
                let t = peekTask()
                match t with
                | Yield f | Running f ->
                    takeTask()
                    f()
                    |> insertTask
                    work dt
                | BlockedNextFrame f ->
                    takeTask()
                    Blocked(dt, f)
                    |> insertTask
                | Completed () ->
                    takeTask()
                    work dt
                | Blocked (t, f) when t < dt ->
                    takeTask()
                    decrBlockingTimes t
                    f()
                    |> insertTask
                    work (dt - t)
                | Blocked _ ->
                    decrBlockingTimes dt
                    ()
                    
        work dt


let toEventuallyObj ev = task {
    let! res = ev
    return box res
}
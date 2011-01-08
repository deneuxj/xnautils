module XNAUtils.CoopMultiTasking

// An extension of the Eventually computation expression as described in the F# spec.
type Eventually<'R> =
    | Completed of 'R
    | Blocked of float32 * (unit -> Eventually<'R>)
    | BlockedNextFrame of (unit -> Eventually<'R>)
    | Running of (unit -> Eventually<'R>)
    | Yield of (unit -> Eventually<'R>)


let rec bind k e =
    match e with
    | Completed r ->
        Running(fun () -> k r)
    | Running f ->
        Running(fun () -> f() |> bind k)
    | Blocked(dt, f) ->
        Blocked(dt, fun () -> f() |> bind k)
    | BlockedNextFrame f ->
        BlockedNextFrame(fun () -> f() |> bind k)
    | Yield f ->
        Yield(fun () -> f() |> bind k)


let result r = Completed(r)

let delay f = Running f

type OkOrException<'T> =
    | Ok of 'T
    | Exception of System.Exception

let rec catch e handleExc last =
    let newWork work =
        let res = try Ok(work()) with e -> Exception e
        match res with
        | Ok cont -> catch cont handleExc last
        | Exception e -> handleExc e

    match e with
    | Completed _ -> last(); e
    | Blocked(dt, f) -> Blocked(dt, fun () -> newWork f)
    | BlockedNextFrame f -> BlockedNextFrame(fun () -> newWork f)
    | Running f -> Running(fun() -> newWork f)
    | Yield f -> Yield(fun() -> newWork f)

(* Boiler-plate *)
let tryFinally e compensation =
    catch e (fun exc -> compensation(); raise exc) compensation

let tryWith e handler =
    catch e handler (fun() -> ())

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
    member x.Combine(e1, e2) = combine e1 e2
    member x.Delay(f) = delay f
    member x.Zero() = result ()
    member x.TryWith(e, handler) = tryWith e handler
    member x.TryFinally(e, compensation) = tryFinally e compensation
    member x.While(cond, body) = whileLoop cond body
    member x.For(xs, body) = forLoop xs body
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
            ()
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
    let tasks : Heap.Heap<Eventually<unit> * int> = Heap.newHeap 4

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
        | BlockedNextFrame _, Running _ |  BlockedNextFrame _, Yield _ -> false
        | BlockedNextFrame _, _ -> true

        // Blocked, ordered by time to wake-up.
        | Blocked _, Running _ | Blocked _, Yield _ | Blocked _, BlockedNextFrame _ -> false
        | Blocked (t1, _), Blocked (t2, _) -> t1 < t2 || t1 = t2 && n1 <= n2
        | Blocked _, Completed _ -> true

        // Completed is maximal
        | Completed _, _ -> false

    let peekTask() =
        let t, _ = tasks.arr.[0]
        t

    let insertTask(t) =
        Heap.insert cmp tasks (t, id)
        id <- id + 1

    let takeTask() =
        Heap.take cmp tasks |> ignore

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

// Allows the parent of a task to request the task to terminate.
// Also allows the parent to be notified when the task terminates.
type TaskController internal (kill, killed) =
    member x.Kill() = kill := true
    member x.IsDead = !killed

// An "operating system API" to be used from tasks.
type Environment(scheduler : Scheduler) =

    // Spawn a new concurrent subtask.
    // The subtask must take a reference to a bool which indicates (when true) when the task should terminate itself.
    member this.Spawn t =
        let kill = ref false
        let killed = ref false

        let once_t = task {
            do! t kill
            killed := true
            do! nextTask()
        }

        scheduler.AddTask(once_t)

        TaskController(kill, killed)

    // Spawn a new concurrent subtask that will be executed in a loop.
    member this.SpawnRepeat t =
        let kill = ref false
        let killed = ref false

        let repeat_t = task {
            while not !kill do
                do! t
            killed := true
            do! nextTask()
        }

        scheduler.AddTask(repeat_t)

        TaskController(kill, killed)

    member this.Wait dt = wait dt

    member this.Yield() = nextTask()

    member this.WaitNextFrame() = nextFrame()

    member this.WaitUntil cond = waitUntil cond

    member this.NewLock() = new Lock()

    member this.NewChannel<'M>() = new BlockingChannel<'M>()
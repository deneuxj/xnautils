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
    member x.ReturnFrom(r) = r
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
    // Ordered queue of tasks that are waiting (Blocked). The integer indicates the time in ticks when the task will wake up.
    let waiting : Heap.Heap<(unit -> Eventually<unit>) * int64> = Heap.newHeap 4

    // Circular queue of tasks that are ready to execute (Running, or Yield).
    let ready : CircularQueue.CircularQueue<unit -> Eventually<unit>> = CircularQueue.newQueue 4

    // Tasks that are waiting until next frame (BlockedNextFrame).
    let blocked_next_frame = ResizeArray<unit -> Eventually<unit>>(4)

    let ticks = ref 0L

    // Ordering of tasks in the waiting queue.
    let cmp ((ev1, n1), (ev2, n2)) = n1 <= n2

    let peekWaitingTask() =
        waiting.arr.[0]

    let insertWaitingTask(delay, f) =
        let release = !ticks + int64 (delay * 10000000.0f)
        Heap.insert cmp waiting (f, release)

    let takeWaitingTask() =
        Heap.take cmp waiting |> ignore

    let deltaToTicks dt =
        int64 (dt * 10000000.0f)

    member x.AddTask(t) =
        match t with
        | Running f | Yield f -> CircularQueue.add ready f
        | Blocked(delay, f) -> insertWaitingTask(delay, f)
        | BlockedNextFrame f -> blocked_next_frame.Add(f)
        | Completed _ -> ()
    
    member x.HasLiveTasks =
        waiting.count > 0
        || ready.len > 0
        || blocked_next_frame.Count > 0

    member x.GetTicks() = !ticks

    // Run tasks for the current frame. dt is the frame time (typically 0.016666... on Xbox for 60 fps)
    member x.RunFor(dt) =
        let end_frame = !ticks + deltaToTicks dt

        // Execute all tasks that are ready
        let executeReady() =
            while not (CircularQueue.isEmpty ready) do
                let f = CircularQueue.pick ready
                let t = f()
                x.AddTask(t)

        // Move waiting tasks that wake up within this frame to the ready queue and execute them
        let rec executeWaiting() =
            if !ticks < end_frame && waiting.count > 0 then
                let (f, release) = peekWaitingTask()
                if release < end_frame then
                    ticks := release
                    takeWaitingTask()
                    CircularQueue.add ready f
                    executeReady()
                    executeWaiting()
        
        executeReady()
        executeWaiting()

        for f in blocked_next_frame do
            CircularQueue.add ready f
        blocked_next_frame.Clear()

        ticks := end_frame

let toEventuallyObj ev = task {
    let! res = ev
    return box res
}

// Allows the parent of a task to request the task to terminate.
// Also allows the parent to be notified when the task terminates.
type TaskController internal (kill, killed) =
    member x.Kill() = kill := true
    member x.IsDead = !killed

// A class offering a subset of the functionalities of System.Diagnostics.Stopwatch which uses the scheduler's time.
type Stopwatch internal (sch : Scheduler) =
    let mutable acc = 0L
    let mutable start = 0L
    let mutable is_running = false

    member this.Reset() =
        acc <- 0L
        is_running <- false

    member this.Start() =
        if not is_running then
            is_running <- true
            start <- sch.GetTicks()

    member this.Stop() =
        if is_running then
            is_running <- false
            acc <- acc + sch.GetTicks() - start

    member this.Elapsed
        with get() =
            new System.TimeSpan(
                acc
                +
                if is_running then sch.GetTicks() - start else 0L)

    member this.ElapsedSeconds
        with get() =
            float32 (acc + if is_running then sch.GetTicks() - start else 0L)
            / 10000000.0f

    member this.IsRunning
        with get() = is_running


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

    member this.WaitUnless(dt, cond) = task {
        let watch = new Stopwatch(scheduler)
        watch.Start()
        do! nextTask()
        while not (watch.ElapsedSeconds >= dt || cond()) do
            do! nextFrame()
    }

    member this.NewLock() = new Lock()

    member this.NewChannel<'M>() = new BlockingChannel<'M>()

    member this.NewStopwatch() = new Stopwatch(scheduler)
namespace CleverRake.XnaUtils.CoopMultiTasking

[<AutoOpen>]
module Core =
    // An extension of the Eventually computation expression as described in the F# spec.
    type Eventually<'R> =
        | Completed of 'R
        | BlockedCond of (unit -> bool) * (unit -> Eventually<'R>)
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
        | BlockedCond(cond, f) ->
            BlockedCond(cond, fun () -> f() |> bind k)
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
        | BlockedCond(cond, f) -> BlockedCond(cond, fun () -> newWork f)
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
    let internal wait dt =
        Blocked(dt, fun () -> Completed())

    // Wait until next frame, i.e. next call to Scheduler.RunFor
    let internal nextFrame () =
        BlockedNextFrame(fun() -> Completed())

    // Stop executing, let some other task execute, if any is ready. Otherwise, we get control back.
    let internal nextTask () =
        Yield(fun() -> Completed())

    // Wait until a specified condition is true.
    // If the condition is initially true, no waiting occurs.
    let internal waitUntil f = task {
        if not(f()) then
            return! BlockedCond(f, fun() -> Completed())
    }


[<AutoOpen>]
module Sys =
    open Core
    open CleverRake.XnaUtils

    /// A lock which can be used for mutual exclusion between tasks on the same scheduler.
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

    /// A blocking channel which can be used for cross-task communication and synchronization.
    /// Note that sending blocks until the message is received.
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

    /// Value-type used by the scheduler.
    type internal SleepingTaskTuple =
        struct
            val f : unit -> Eventually<unit>
            val release_at : int64

            new (f, r) = { f = f; release_at = r }

            static member CompareLTE(A : SleepingTaskTuple, B : SleepingTaskTuple) = A.release_at <= B.release_at
        end

    type Scheduler() =
        // Ordered queue of tasks that are waiting (Blocked). The integer indicates the time in ticks when the task will wake up.
        let waiting : Heap.Heap<SleepingTaskTuple> = Heap.newHeap 4

        // Circular queue of tasks that are ready to execute (Running, or Yield).
        let ready : CircularQueue.CircularQueue<unit -> Eventually<unit>> = CircularQueue.newQueue 4

        // Tasks that are waiting until next frame (BlockedNextFrame).
        let blocked_next_frame = ResizeArray<unit -> Eventually<unit>>(4)

        // Tasks that are waiting watching a condition
        let watching = ResizeArray<TupleStruct2<unit -> bool, unit -> Eventually<unit>>>(4)

        let ticks = ref 0L

        let exceptions = ref []

        let peekWaitingTask() =
            waiting.arr.[0]

        let insertWaitingTask(delay, f) =
            let release = !ticks + int64 (delay * 10000000.0f)
            Heap.insert SleepingTaskTuple.CompareLTE waiting (new SleepingTaskTuple(f, release))

        let takeWaitingTask() =
            Heap.take SleepingTaskTuple.CompareLTE waiting |> ignore

        let deltaToTicks dt =
            int64 (dt * 10000000.0f)

        /// Add a new task.
        member x.AddTask(t) =
            match t with
            | Running f | Yield f -> CircularQueue.add ready f
            | Blocked(delay, f) -> insertWaitingTask(delay, f)
            | BlockedNextFrame f -> blocked_next_frame.Add(f)
            | BlockedCond(cond, f) -> watching.Add(new TupleStruct2<_,_>(cond, f))
            | Completed _ -> ()
    
        /// Returns true if the scheduler has tasks that haven't run to completion.
        member x.HasLiveTasks =
            waiting.count > 0
            || ready.len > 0
            || blocked_next_frame.Count > 0
            || watching.Count > 0

        /// Get the amount of time this scheduler has been active, in ticks.
        member x.GetTicks() = !ticks

        /// Move all tasks in the waiting and watching queues to the ready queue.
        member x.WakeAll() =
            while not (Heap.isEmpty waiting) do
                let f = peekWaitingTask().f
                takeWaitingTask()
                CircularQueue.add ready f
        
            for cond_f in watching do
                let f = cond_f.Item2
                CircularQueue.add ready f
            watching.Clear()
            

        /// Run tasks for the current frame. dt is the frame time (typically 0.016666... for 60 fps)
        /// A task that throws an uncaught exception will be marked as completed.
        /// The exception is recorded and can be accessed in this.Exceptions.
        member x.RunFor(dt) =
            let end_frame = !ticks + deltaToTicks dt

            // Execute all tasks that are ready
            let executeReady() =
                while not (CircularQueue.isEmpty ready) do
                    let f = CircularQueue.pick ready
                    let mutable t =
                        try
                            f()
                        with
                        | e ->
                            exceptions := e :: !exceptions
                            Completed()
                    while
                            match t with
                            | Running f -> true
                            | _ -> false
                        do
                        match t with
                        | Running f ->
                            try
                                t <- f()
                            with
                            | e ->
                                t <- Completed()
                                exceptions := e :: !exceptions
                        | _ -> failwith "Unreachable"

                    x.AddTask(t)

            // Move waiting tasks that wake up within this frame to the ready queue and execute them
            let rec executeWaiting() =
                if !ticks < end_frame && waiting.count > 0 then
                    let tuple = peekWaitingTask()
                    let f = tuple.f
                    let release = tuple.release_at
                    if release < end_frame then
                        ticks := release
                        takeWaitingTask()
                        CircularQueue.add ready f
                        executeReady()
                        executeWaiting()
        
            // Move all tasks watching a condition that evaluates to true.
            let executeWatching() =
                let tmp = ResizeArray<_>(watching)
                watching.Clear()
                for cond_f in tmp do
                    if cond_f.Item1() then
                        CircularQueue.add ready cond_f.Item2
                    else
                        watching.Add(cond_f)
                executeReady()

            executeReady()
            executeWaiting()
            executeWatching()

            for f in blocked_next_frame do
                CircularQueue.add ready f
            blocked_next_frame.Clear()

            ticks := end_frame

        /// Get the list of uncaught exceptions thrown by tasks.
        member x.Exceptions = !exceptions

        /// Return true if the list of uncaught exceptions is non-empty.
        member x.HasExceptions = not (List.isEmpty !exceptions)

        /// Clear the list of uncaught exceptions.
        member x.ClearExceptions() = exceptions := []


    let toEventuallyObj ev = task {
        let! res = ev
        return box res
    }

    /// Allows the parent of a task to request the task to terminate.
    /// Also allows the parent to be notified when the task terminates.
    type TaskController internal (kill, killed) =
        member x.Kill() = kill := true
        member x.IsDead = !killed

    /// A class offering a subset of the functionalities of System.Diagnostics.Stopwatch which uses the scheduler's time.
    type Stopwatch internal (sch : Scheduler) =
        let mutable acc = 0L
        let mutable start = 0L
        let mutable is_running = false

        /// Reset the elapsed time and stop the watch.
        member this.Reset() =
            acc <- 0L
            is_running <- false

        /// Start the watch.
        member this.Start() =
            if not is_running then
                is_running <- true
                start <- sch.GetTicks()

        /// Pause the watch. The elapsed time is not reset.
        member this.Stop() =
            if is_running then
                is_running <- false
                acc <- acc + sch.GetTicks() - start

        /// Return the amount of time the watch has been running since it was last reset.
        member this.Elapsed
            with get() =
                new System.TimeSpan(
                    acc
                    +
                    if is_running then sch.GetTicks() - start else 0L)

        /// Return the elapsed time in seconds.
        member this.ElapsedSeconds
            with get() =
                float32 (acc + if is_running then sch.GetTicks() - start else 0L)
                / 10000000.0f

        /// Return true if this watch is running.
        member this.IsRunning
            with get() = is_running

    /// Type of exception thrown by Environment to notify tasks that they are being killed.
    /// Tasks can catch this exception to shut down properly, but should not continue executing.
    exception TaskKilled of obj

    // An "operating system API" to be used from tasks.
    type Environment(scheduler : Scheduler) =
        // When killing all tasks, the data to embed in TaskKilled exceptions.
        let killing_data : obj option ref = ref None

        let checkAndKill = task {
            match !killing_data with
            | Some o -> raise(TaskKilled(o))
            | None -> ()
        }

        /// Request to kill all task, passing user-specific data.
        member this.StartKillAll(data) =
            killing_data := Some data
            scheduler.WakeAll()

        /// Stop killing tasks. Use this after an invokation of StartKill after all tasks are dead and before adding new tasks.
        member this.StopKillAll() =
            killing_data := None

        /// Spawn a new concurrent subtask.
        /// The subtask must take a reference to a bool which indicates (when true) when the task should terminate itself.
        member this.Spawn t =
            let kill = ref false
            let killed = ref false

            let once_t = task {
                do! checkAndKill
                do! t kill
                do! checkAndKill
                killed := true
            }

            scheduler.AddTask(once_t)

            TaskController(kill, killed)

        /// Spawn a new concurrent subtask that will be executed in a loop.
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

        /// Wait until for an amount of time.
        member this.Wait dt = task {
            do! checkAndKill
            do! wait dt
            do! checkAndKill
        }

        /// Yield control to the next ready task.
        member this.Yield() = task {
            do! checkAndKill
            do! nextTask()
            do! checkAndKill
        }

        /// Wait until next frame.
        member this.WaitNextFrame() = task {
            do! checkAndKill
            do! nextFrame()
            do! checkAndKill
        }

        /// Wait until a confition is fulfilled.
        member this.WaitUntil cond = task {
            do! checkAndKill
            do! waitUntil cond
            do! checkAndKill
        }

        /// Wait until an amount of time as passed or a condition is fulfilled.
        member this.WaitUnless(dt, cond) = task {
            do! checkAndKill
            let watch = new Stopwatch(scheduler)
            watch.Start()
            do! waitUntil (fun() -> watch.ElapsedSeconds >= dt || cond())
            do! checkAndKill
        }

        /// Wait until an event is raised, then return the event argument.
        member this.AwaitEvent (ev : System.IObservable<'T>) =
            let result = ref None
            let subscription : System.IDisposable option ref = ref None
            let handler arg =
                result := Some arg
                match subscription.Value with
                | Some disposable -> disposable.Dispose()
                | None -> ()

            task {
                subscription := Some <| ev.Subscribe handler
                do! this.WaitUntil (fun () -> result.Value.IsSome)            
                return result.Value.Value
            }

        /// Return a new lock.
        member this.NewLock() = new Lock()

        /// Return a new blocking channel.
        member this.NewChannel<'M>() = new BlockingChannel<'M>()

        /// Return a new stop watch.
        member this.NewStopwatch() = new Stopwatch(scheduler)

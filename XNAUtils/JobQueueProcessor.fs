module XNAUtils.JobQueueProcessor
open System.Threading
open XNAUtils.CircularQueue

type Processor(setAffinity : unit -> unit) =
    let jobs = newQueue 4
    let quit = ref false
    let wait = new AutoResetEvent(false)
    let mutable disposed = false
    
    let thread = Thread(fun () ->
        setAffinity()
        let rec work() =
            wait.WaitOne() |> ignore
            let rec doAllJobs() =
                if not !quit && not (isEmpty jobs) then
                    let job = lock jobs (fun () -> pick jobs)
                    job()
                    doAllJobs()
            doAllJobs()
            if not !quit then work()
        work()
        )
    
    let killAndWait() =
        if not disposed then
            disposed <- true
            quit := true
            wait.Set() |> ignore
            wait.Close()
            thread.Join()
        
    do thread.Start()
    
    member x.AddJob(job) =
        lock jobs (fun () -> add jobs job)
        wait.Set() |> ignore
    
    member x.KillAndWait() =
        killAndWait()
        
    interface System.IDisposable with
        member x.Dispose() = killAndWait()
    
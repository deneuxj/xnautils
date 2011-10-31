module CleverRake.XnaUtils.JobQueueProcessor
open System.Threading
open CleverRake.XnaUtils.CircularQueue

(*
Copyright [2010] [Johann Deneux]

   Licensed under the Apache License, Version 2.0 (the "License");
   you may not use this file except in compliance with the License.
   You may obtain a copy of the License at

     http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS,
   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
   See the License for the specific language governing permissions and
   limitations under the License.
*)

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
    
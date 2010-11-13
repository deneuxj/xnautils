module XNAUtils.CircularQueue

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

type CircularQueue<'T> =
    {  mutable first : int
       mutable len : int
       mutable capacity : int
       mutable content : 'T[]  }

let newQueue capacity : CircularQueue<'T> =
    {  first = 0
       len = 0
       capacity = capacity
       content = Array.zeroCreate capacity  }
       
let grow (q : CircularQueue<'T>) =
    let content : 'T[] = Array.zeroCreate (q.capacity * 2)
    let l0 =
        if q.first + q.len > q.capacity
        then
            q.capacity - q.first
        else
            q.len
    let l1 = q.len - l0
            
    System.Array.Copy(q.content, q.first, content, 0, l0)
    System.Array.Copy(q.content, 0, content, l0, l1)
    
    q.first <- 0
    q.capacity <- content.Length
    q.content <- content

let add (q : CircularQueue<'T>) (item : 'T) =
    if q.len >= q.capacity then grow q
    let pos = (q.first + q.len) % q.capacity
    q.content.[pos] <- item
    q.len <- q.len + 1

let pick (q : CircularQueue<'T>) : 'T =
    if q.len <= 0 then failwith "Cannot pick from an empty queue"
    q.len <- q.len - 1
    let old_first = q.first
    q.first <- (q.first + 1) % q.capacity
    q.content.[old_first]

let isEmpty (q : CircularQueue<'T>) : bool =
    q.len = 0
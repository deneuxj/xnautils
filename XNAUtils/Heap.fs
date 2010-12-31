module XNAUtils.Heap

(*
Copyright (C) 2010 Johann Deneux

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


type Heap<'a> =
    {  mutable arr : 'a[]
       mutable count : int  }

       
let swap (heap : Heap<'a>) idx1 idx2 =
    let tmp = heap.arr.[idx1]
    heap.arr.[idx1] <- heap.arr.[idx2]
    heap.arr.[idx2] <- tmp


let newHeap<'a> size : Heap<'a> =
    {  arr = Array.zeroCreate size
       count = 0  }


let isEmpty (heap : Heap<'a>) =
    heap.count = 0
    

let first (heap : Heap<'a>) =
    heap.arr.[0]
    
    
let growHeap (heap : Heap<'a>) =
    if heap.count >= heap.arr.Length then
        let arr = Array.zeroCreate (heap.arr.Length * 2)
        System.Array.Copy(heap.arr, arr, heap.arr.Length)
        heap.arr <- arr

        
let (*inline*) insert (cmp : 'a * 'a -> bool) (heap : Heap<'a>) (item : 'a) =
    growHeap heap
    let count' = heap.count + 1
    heap.arr.[heap.count] <- item
    heap.count <- count'
    let rec sift_up idx =
        if idx > 0 then
            let root = (idx - 1) / 2
            if not (cmp (heap.arr.[root], heap.arr.[idx])) then
                swap heap root idx
                sift_up root
    sift_up (heap.count - 1)


let inline take (cmp : 'a * 'a -> bool) (heap : Heap<'a>) =
    let ret = heap.arr.[0]
    let last = heap.count - 1
    heap.arr.[0] <- heap.arr.[last]
    heap.count <- last
    let count = last // Same as heap.Count at this point
    
    let rec sift_down idx =
        let left_child = idx * 2 + 1
        let right_child = left_child + 1
        let min =
            if left_child >= count then idx
            elif right_child >= count then
                if cmp (heap.arr.[idx], heap.arr.[left_child])
                then idx
                else left_child
            else
                if cmp (heap.arr.[idx], heap.arr.[left_child]) then
                    if cmp (heap.arr.[idx], heap.arr.[right_child])
                    then idx
                    else right_child
                else
                    if cmp (heap.arr.[left_child], heap.arr.[right_child])
                    then left_child
                    else right_child
        if min <> idx then
            swap heap idx min
            sift_down min
            
    sift_down 0
    ret
    

let rec isHeap cmp (heap : Heap<'a>) idx =
    let count = heap.count
    if idx >= count then true
    else
        let left = 2 * idx + 1
        let right = 2 * idx + 2
        (left >= count || cmp (heap.arr.[idx], heap.arr.[left]) && isHeap cmp heap left) &&
        (right >= count || cmp (heap.arr.[idx], heap.arr.[right]) && isHeap cmp heap right)



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

module CleverRake.XnaUtils.ArrayInlined

// Early exit: No.
// Array bounds check removed: Yes
let inline exists1 (pred : 'T -> bool) (xs : 'T[]) : bool =
    let mutable found = false
    for i in 0..xs.Length-1 do
        found <- found || pred xs.[i]
    found

// Early exit: Yes.
// Array bounds check removed: ?
let inline exists (pred : 'T -> bool) (xs : 'T[]) : bool =
    let mutable i = 0
    while i < xs.Length && not (pred xs.[i]) do
        i <- i + 1
    i < xs.Length

// Early exit: No.
// Array bounds check removed: Yes
let inline tryMapFirst (pred : 'T -> 'R option) (xs : 'T[]) : 'R option =
    let mutable res = None
    for i in 0..xs.Length-1 do
        if Option.isNone res then
            match pred xs.[i] with
            | Some _ as v -> res <- v
            | None -> ()
    res

let inline filteri (pred : int -> 'T -> bool) (xs : 'T[]) : 'T[] =
    let temp = new System.Collections.Generic.List<'T>(xs.Length)
    for i in 0 .. xs.Length - 1 do
        let v = xs.[i]
        if pred i v then temp.Add(v)
    temp.ToArray()

/// Filter an array ts using a predicate on a matching array rs. ts and rs must have equal length.
let inline filterRef (pred : 'R -> bool) (rs : 'R[]) (ts : 'T[]) : 'T[] =
    let temp = new System.Collections.Generic.List<'T>(ts.Length)
    for i in 0 .. rs.Length - 1 do
        let r = rs.[i]
        if pred r then temp.Add(ts.[i])
    temp.ToArray()

let inline mapi3 f (xs : 'X[]) (ys : 'Y[]) (zs : 'Z[]) =
    let len = Array.length xs
    let ret = Array.zeroCreate len
    for i in 0 .. len - 1 do
        ret.[i] <- f i xs.[i] ys.[i] zs.[i]
    ret

let inline map3 f (xs : 'X[]) (ys : 'Y[]) (zs : 'Z[]) =
    let len = Array.length xs
    let ret = Array.zeroCreate len
    for i in 0 .. len - 1 do
        ret.[i] <- f xs.[i] ys.[i] zs.[i]
    ret
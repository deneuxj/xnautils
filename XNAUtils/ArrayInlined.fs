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

module XNAUtils.ArrayInlined

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
module CleverRake.XnaUtils.StorageComponent

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

open System
open System.IO
open Microsoft.Xna.Framework
            
type IOAction = delegate of Stream -> unit

type IStorageComponent =
    abstract member IsReady : bool
    abstract member IsTitleStorageEnabled : bool
    abstract member IsUserStorageEnabled : bool
    abstract member RequestTitleStorage : unit -> unit
    abstract member RequestUserStorage : PlayerIndex -> unit
    abstract member ResetTitleStorage : unit -> unit
    abstract member ResetUserStorage : unit -> unit
    abstract member DoTitleStorageIO : string * FileMode * IOAction * Action * Action<Exception> -> unit
    abstract member DoUserStorageIO : string * string * FileMode * IOAction * Action * Action<Exception> -> unit
    abstract member Update : unit -> unit
    [<CLIEvent>]
    abstract member TitleStorageAcquired : IEvent<EventArgs>
    [<CLIEvent>]
    abstract member TitleStorageLost : IEvent<EventArgs>
    [<CLIEvent>]
    abstract member UserStorageAcquired : IEvent<EventArgs>
    [<CLIEvent>]
    abstract member UserStorageLost : IEvent<EventArgs>

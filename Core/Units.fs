﻿module CleverRake.XnaUtils.Units

open Microsoft.FSharp.Core

/// Position, meters
[<Measure>] type m

/// Time, seconds
[<Measure>] type s

let int2float32(v : int<'u>) : float32<'u> = LanguagePrimitives.Float32WithMeasure (float32 v)

/// An array whose index has a unit of measure
type MarkedArray<[<Measure>] 'K, 'T> = MarkedArray of 'T[]
with
    member this.Item
        with get (i : int<'K>) =
            let (MarkedArray arr) = this
            arr.[int i]
        and set (i : int<'K>) (v : 'T) =
            let (MarkedArray arr) = this
            arr.[int i] <- v



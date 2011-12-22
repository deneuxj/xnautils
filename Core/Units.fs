module CleverRake.XnaUtils.Units

open Microsoft.FSharp.Core

/// Position, meters
[<Measure>] type m

/// Time, seconds
[<Measure>] type s

let int2float32(v : int<'u>) : float32<'u> = LanguagePrimitives.Float32WithMeasure (float32 v)

/// An array whose index has a unit of measure
type MarkedArray<[<Measure>] 'K, 'T> = MarkedArray of 'T[]
with
    member this.Content =
        let (MarkedArray arr) = this
        arr

    member this.Item
        with get (i : int<'K>) =
            let (MarkedArray arr) = this
            arr.[int i]
        and set (i : int<'K>) (v : 'T) =
            let (MarkedArray arr) = this
            arr.[int i] <- v

[<RequireQualifiedAccess>]
module MarkedArray =
    let inline set (arr : MarkedArray<'K, 'T>) idx v =
        arr.[idx] <- v

    let inline get (arr : MarkedArray<'K, 'T>) idx =
        arr.[idx]

    /// arr.[idx] <- f (arr.[idx])
    let inline mutate f (arr, idx) =
        let v = get arr idx
        set arr idx (f v)
module CleverRake.XnaUtils.SeqUtil

let groupPairs (kvs : ('K * 'V) seq) : ('K * 'V seq) seq when 'K : equality =
    kvs
    |> Seq.groupBy fst
    |> Seq.map (fun (k, v) -> (k, v |> Seq.map snd))

/// Generate an infinite sequence by repeatedly applying a function f to the previous value. The first value in the sequence is f(v0).
let rec seqInfinite f v0 =
    seq {
        let v1 = f v0
        yield v1
        yield! seqInfinite f v1
    }

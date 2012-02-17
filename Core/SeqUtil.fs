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

let listZip4 l1 l2 l3 l4 =
    let rec work l1 l2 l3 l4 =
        match l1, l2, l3, l4 with
        | i1 :: r1, i2 :: r2, i3 :: r3, i4 :: r4 ->
            (i1, i2, i3, i4) :: work r1 r2 r3 r4
        | [], [], [], [] -> []
        | _ -> failwith "Lengths don't match"
    work l1 l2 l3 l4
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

/// Return the indices of the two smallest items. Uses index -1 for "no such index" (if xs contains less than two items)
let minBy2 f xs =
    let far = System.Single.PositiveInfinity
    let _, ((m0, _), (m1, _)) =
        xs
        |> Array.fold (fun (i, ((i0, d0), (i1, d1) as x)) v ->
            let d = f v
            let matches =
                if d < d0 then
                    ((i, d), (i0, d0))
                elif d < d1 then
                    ((i0, d0), (i, d))
                else
                    x
            (i + 1, matches)) (0, ((-1, far), (-1, far)))

    (m0, m1)

module CleverRake.XnaUtils.SeqUtil

let groupPairs (kvs : ('K * 'V) seq) : ('K * 'V seq) seq when 'K : equality =
    kvs
    |> Seq.groupBy fst
    |> Seq.map (fun (k, v) -> (k, v |> Seq.map snd))

namespace CleverRake.XnaUtils

(* Value-type tuples *)
type StructTuple2<'T1, 'T2>(x : 'T1, y : 'T2) =
    struct
        member this.Item1 = x
        member this.Item2 = y
    end
namespace XNAUtils

type TupleStruct2<'T1, 'T2> =
    struct
        val Item1 : 'T1
        val Item2 : 'T2

        new(t1, t2) = { Item1 = t1; Item2 = t2 }
    end


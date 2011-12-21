namespace CleverRake.XnaUtils

open Microsoft.Xna.Framework

open CleverRake.XnaUtils.Units

type TypedVector3<[<Measure>] 'M> =
    struct
        val v : Vector3
        new(x, y, z) = { v = Vector3(x, y, z) }
        new(V) = { v = V }

        static member public (+) (U : TypedVector3<'M>, V : TypedVector3<'M>) = new TypedVector3<'M>(U.v + V.v)
        static member public (-) (U : TypedVector3<'M>, V : TypedVector3<'M>) = new TypedVector3<'M>(U.v - V.v)
        static member public (*) (k : float32, U : TypedVector3<'M>) = new TypedVector3<'M>(k * U.v)
    end

module Speed3 =
    let (*) (k : float32<s>, V : TypedVector3<m/s>) : TypedVector3<m> =
        new TypedVector3<m>((float32 k) * V.v)

module Accel3 =
    let (*) (k : float32<s>, V : TypedVector3<m/s^2>) : TypedVector3<m/s> =
        new TypedVector3<m/s>((float32 k) * V.v)
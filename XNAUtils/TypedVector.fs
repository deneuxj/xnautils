namespace CleverRake.XnaUtils

open Microsoft.Xna.Framework

/// A two-dimensional vector with a unit of measure. Built on top of Xna's Vector2.
type TypedVector2<[<Measure>] 'M> =
    struct
        val v : Vector2
        new(x : float32<'M>, y : float32<'M>) =
            { v = Vector2(float32 x, float32 y) }
        new(V) = { v = V }

        member this.X : float32<'M> = LanguagePrimitives.Float32WithMeasure this.v.X
        member this.Y : float32<'M> = LanguagePrimitives.Float32WithMeasure this.v.Y
    end

/// A three-dimensional vector with a unit of measure. Built on top of Xna's Vector3.
type TypedVector3<[<Measure>] 'M> =
    struct
        val v : Vector3
        new(x : float32<'M>, y : float32<'M>, z : float32<'M>) =
            { v = Vector3(float32 x, float32 y, float32 z) }
        new(V) = { v = V }

        member this.X : float32<'M> = LanguagePrimitives.Float32WithMeasure this.v.X
        member this.Y : float32<'M> = LanguagePrimitives.Float32WithMeasure this.v.Y
        member this.Z : float32<'M> = LanguagePrimitives.Float32WithMeasure this.v.Z
    end

[<RequireQualifiedAccessAttribute>]
module TypedVector =
    let add3 (U : TypedVector3<'M>, V : TypedVector3<'M>) =
        new TypedVector3<'M>(U.v + V.v)

    let sub3 (U : TypedVector3<'M>, V : TypedVector3<'M>) =
        new TypedVector3<'M>(U.v - V.v)

    let dot3 (U : TypedVector3<'M>, V : TypedVector3<'N>) =
        Vector3.Dot(U.v, V.v)
        |> LanguagePrimitives.Float32WithMeasure<'M 'N>

    let cross3 (U : TypedVector3<'M>, V : TypedVector3<'N>) =
        let conv = LanguagePrimitives.Float32WithMeasure<'M 'N>
        let temp = Vector3.Cross(U.v, V.v)
        new TypedVector3<_>(conv temp.X, conv temp.Y, conv temp.Z)

    let len3 (U : TypedVector3<'M>) =
        LanguagePrimitives.Float32WithMeasure<'M> (U.v.Length())

    let scale3 (k : float32<'K>, U : TypedVector3<'M>) : TypedVector3<'K 'M> =
        let conv = LanguagePrimitives.Float32WithMeasure<'K 'M>
        let v = Vector3.Multiply(U.v, float32 k)
        new TypedVector3<_>(conv v.X, conv v.Y, conv v.Z)

    let normalize3 (U : TypedVector3<'M>) =
        let len = len3 U
        scale3 ((1.0f / len), U)

    let tryNormalize3 (U : TypedVector3<'M>) =
        let len = len3 U
        if len > LanguagePrimitives.Float32WithMeasure<'M>(1e-3f) then
            Some <| scale3 ((1.0f/ len), U)
        else
            None

    let add2 (U : TypedVector2<'M>, V : TypedVector2<'M>) =
        new TypedVector2<'M>(U.v + V.v)

    let sub2 (U : TypedVector2<'M>, V : TypedVector2<'M>) =
        new TypedVector2<'M>(U.v - V.v)

    let dot2 (U : TypedVector2<'M>, V : TypedVector2<'N>) =
        Vector2.Dot(U.v, V.v)
        |> LanguagePrimitives.Float32WithMeasure<'M 'N>

    let len2 (U : TypedVector2<'M>) =
        LanguagePrimitives.Float32WithMeasure<'M> (U.v.Length())

    let scale2 (k : float32<'K>, U : TypedVector2<'M>) : TypedVector2<'K 'M> =
        let conv = LanguagePrimitives.Float32WithMeasure<'K 'M>
        let v = Vector2.Multiply(U.v, float32 k)
        new TypedVector2<_>(conv v.X, conv v.Y)

    let normalize2 (U : TypedVector2<'M>) =
        let len = len2 U
        scale2 ((1.0f / len), U)

    let tryNormalize2 (U : TypedVector2<'M>) =
        let len = len2 U
        if len > LanguagePrimitives.Float32WithMeasure<'M>(1e-3f) then
            Some <| scale2 ((1.0f/ len), U)
        else
            None

type TypedVector3<[<Measure>] 'M>
with
    static member public (*) (k, U) = TypedVector.scale3 (k, U)
    static member public (+) (U, V) = TypedVector.add3 (U, V)
    static member public (-) (U, V) = TypedVector.sub3 (U, V)
    member public this.Length = this |> TypedVector.len3
    static member public Zero = TypedVector3<'M>()

type TypedVector2<[<Measure>] 'M>
with
    static member public (*) (k, U) = TypedVector.scale2 (k, U)
    static member public (+) (U, V) = TypedVector.add2 (U, V)
    static member public (-) (U, V) = TypedVector.sub2 (U, V)
    member public this.Length = this |> TypedVector.len2
    static member public Zero = TypedVector2<'M>()
module CleverRake.XnaUtils.QuaternionExtensions

open CleverRake.XnaUtils.Units

type Quaternion = Microsoft.Xna.Framework.Quaternion

type Microsoft.Xna.Framework.Quaternion with
    static member public CreateFromAxisAngle(v: TypedVector3<1>, angle : float32<rad>) =
        Quaternion.CreateFromAxisAngle(v.v, float32 angle)
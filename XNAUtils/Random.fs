namespace CleverRake.XnaUtils

open Microsoft.Xna.Framework

module Random =
    type System.Random with
        member this.NextSingle() = this.NextDouble() |> float32

        member this.NextQuaternion() =
            let twopi = MathHelper.TwoPi
            Quaternion.CreateFromYawPitchRoll(twopi * this.NextSingle(), twopi * this.NextSingle(), twopi * this.NextSingle())
        
        member this.NextVector3(mag : float32) =
            let q = this.NextQuaternion()
            Vector3.Transform(Vector3.UnitX, q) * mag

module XNAUtils.Rumble

open Microsoft.Xna.Framework
open Microsoft.Xna.Framework.Input


type IUpdatableEffect =
    abstract Update : float32 -> unit
    abstract BigRumble : float32
    abstract SmallRumble : float32
    abstract HasExpired : bool

    
type CollisionEffect(strength : float32) =
    let duration = 0.5f
    let w = 3.0f
    let mutable time = 0.0f
    
    interface IUpdatableEffect with
        member x.Update dt =
            time <- time + dt
        
        member x.BigRumble =
            MathHelper.SmoothStep(strength, 0.0f, time / duration)
        
        member x.SmallRumble =
            sin(w * MathHelper.TwoPi * time / duration) * MathHelper.Lerp(1.0f, 0.0f, time / duration)

        member x.HasExpired =
            time >= duration

            
type GamePadRumble(idx : PlayerIndex) =
    let mutable effects : IUpdatableEffect list = []
    
    member x.Update(dt : float32) =
        for fx in effects do fx.Update(dt)
        effects <- effects |> List.filter (fun fx -> not fx.HasExpired)
    
    member x.Pause() =
        GamePad.SetVibration(idx, 0.0f, 0.0f)
        |> ignore
    
    member x.Render() =
        let clamp x = MathHelper.Clamp(x, 0.0f, 1.0f)
        let small = effects |> Seq.map (fun fx -> fx.SmallRumble) |> Seq.sum |> clamp
        let big = effects |> Seq.map (fun fx -> fx.BigRumble) |> Seq.sum |> clamp
        GamePad.SetVibration(idx, big, small)

    member x.Collide(strength) =
        effects <- (CollisionEffect(strength) :> IUpdatableEffect) :: effects
    
    member x.Reset() =
        effects <- []
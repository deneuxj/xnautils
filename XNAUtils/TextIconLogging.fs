module CleverRake.XnaUtils.TextIconLogging

open Microsoft.Xna.Framework

open TextIcons


type Log(icons, ratios, font, topSpace, bottomSpace, spacing, capacity, lifeTime) =
    let renderer = new Renderer(icons, ratios, font, topSpace, bottomSpace)
    let content : Logging.LogContent<TextWithIcons> =
        { capacity = capacity
          messageLifeTime = lifeTime
          messages = CircularQueue.newQueue capacity
          timeLeft = CircularQueue.newQueue capacity }

    member this.Update(dt) =
        Logging.update dt content

    member this.AddMessage(msg) =
        Logging.addMessageTo content msg

    member this.Render(bat, color, scale, x, y) =
        let ys = Seq.initInfinite (fun i -> y + (float32 i) * spacing)
        for y, line in Seq.zip ys (Logging.messagesIn content) do
            renderer.Render(bat, scale, color, Vector2(x, y), line) |> ignore


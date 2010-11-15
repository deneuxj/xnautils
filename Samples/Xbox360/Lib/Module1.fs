module XNAUtils.Sample.Module1

open System

open Microsoft.Xna.Framework
open Microsoft.Xna.Framework.Content
open Microsoft.Xna.Framework.Graphics
open Microsoft.Xna.Framework.Input

open XNAUtils.TextIcons
open XNAUtils.StorageComponent

let idxButtonA = 0
let idxButtonB = 1
let idxButtonBack = 2

type TextOrIcon =
    | Text of string
    | Icon of int

let rec toTextWithIcons = function
    | [] -> TextWithIcons.Nil
    | Text txt :: rest -> TextWithIcons.String(txt, toTextWithIcons rest)
    | Icon idx :: rest -> TextWithIcons.Icon(idx, toTextWithIcons rest)

let txt_leave =
    [  Icon idxButtonBack
       Text " Quit"  ]
    |> toTextWithIcons

let txt_new_num =
    [  Icon idxButtonA
       Text " New number"  ]
    |> toTextWithIcons

let txt_save_num =
    [  Icon idxButtonB
       Text " Save"  ]
    |> toTextWithIcons

type Component(game, player : PlayerIndex) =
    inherit DrawableGameComponent(game)

    let mutable renderer : Renderer option = None
    let mutable batch : SpriteBatch option = None

    let random = new System.Random()
    let nextRandom() =
        random.Next(100)
    let mutable num : int = nextRandom()
    let mutable num_str = num.ToString()

    let storage = new StorageComponent(game)
    let container_name = "Data"
    let filename = "data.dat"

    override x.LoadContent() =
        let content = game.Content
        let font : SpriteFont = content.Load("font")
        let img_A : Texture2D = content.Load("btnA")
        let img_B : Texture2D = content.Load("btnB")
        let img_Back : Texture2D = content.Load("btnBack")
        batch <- Some (new SpriteBatch(game.GraphicsDevice))
        
        renderer <-
            let images = [| img_A ; img_B ; img_Back |]
            let ratios = [| 0.5f ; 0.5f ; 0.5f |]
            Some (new Renderer(images, ratios, font, 0, 0))

    override x.Initialize() =
        storage.RequestUserStorage(player)
        storage.DoUserStorageIO(container_name, filename, System.IO.FileMode.Open, IOAction(x.Load), Action(x.LoadDone), Action<Exception>(x.LoadFailed))

    override x.Update(game_time) =
        let gamepad = GamePad.GetState(player)

        if gamepad.Buttons.A = ButtonState.Pressed then
            num <- nextRandom()
        
        if gamepad.Buttons.B = ButtonState.Pressed then
            storage.DoUserStorageIO(container_name, filename, System.IO.FileMode.Create, IOAction(x.Save), Action(x.SaveDone), Action<Exception>(x.SaveFailed))

        if gamepad.Buttons.Back = ButtonState.Pressed then
            game.Exit()

    override x.Draw(game_time) =
        match renderer, batch with
        | Some renderer, Some batch ->
            let mutable pos = Vector2(100.0f, 100.0f)
            let incr = Vector2(0.0f, 100.0f)
            for txt in [ TextWithIcons.String(num_str, TextWithIcons.Nil) ; txt_new_num ; txt_save_num ; txt_leave] do
                pos <- renderer.Render(batch, 1.0f, Color.White, pos, txt) + incr
        | _, _ -> ()

    member x.Load(stream) =
        num <- stream.ReadByte()
        num_str <- num.ToString()

    member x.LoadDone() = ()

    member x.LoadFailed(exc) = ()

    member x.Save(stream) =
        stream.WriteByte(byte(num))

    member x.SaveDone() = ()

    member x.SaveFailed(exc) = ()

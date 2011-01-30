module XNAUtils.ScreenManager

open Microsoft.Xna.Framework
open Microsoft.Xna.Framework.Content
open Microsoft.Xna.Framework.Graphics

open System.Collections.Generic

open XNAUtils.CoopMultiTasking

// Interface of screens as exposed to the ScreenManager
type Screen =
    interface
        abstract member Draw : unit -> unit
        abstract member LoadContent : unit -> unit
        abstract member UnloadContent : unit -> unit
        abstract member SetIsOnTop : bool -> unit
        abstract member SetGame : Game -> unit
        abstract member SetScreenManager : ScreenManager -> unit
        abstract member ClearScreenManager : unit -> unit
    end

// Most of the screens in XNAUtils need a couple of fonts and a sprite batch to draw themselves.
and IUiContentProvider =
    interface
        abstract member Font1 : SpriteFont with get
        abstract member Font2 : SpriteFont with get
        abstract member Blank : Texture2D with get
        abstract member SpriteBatch : SpriteBatch with get
    end

// The screen manager keeps track of screens which must be drawn.
// It is responsible for telling screens when to load and unload content.
and ScreenManager(game, ui_content_provider : IUiContentProvider) =
    inherit DrawableGameComponent(game)

    let screens = new List<Screen>()
    let screens_tmp = new List<Screen>()

    let get_top_screen() =
        if screens.Count > 0 then
            let s = screens.[screens.Count - 1]
            Some s
        else
            None

    override this.LoadContent() =
        for s in screens do s.LoadContent()
        base.LoadContent()

    override this.Update(gt) =
        base.Update(gt)

    override this.Draw(gt) =
        // Copy screens, so that s.Draw can modify it while we are iterating over the copy.
        screens_tmp.AddRange(screens)
        for s in screens_tmp do s.Draw()
        screens_tmp.Clear()

        base.Draw(gt)

    member this.AddScreen(s : Screen) =
        match get_top_screen() with
        | Some top -> top.SetIsOnTop(false)
        | None -> ()

        screens.Add(s)

        s.SetGame(game)
        s.SetScreenManager(this)
        s.SetIsOnTop(true)
        
        s.LoadContent()

        System.Diagnostics.Debug.WriteLine(sprintf "Screen %s added." (s.GetType().Name))

    member this.RemoveScreen(s : Screen) =
        match get_top_screen() with
        | None -> ()
        | Some top ->
            if System.Object.ReferenceEquals(s, top) then
                s.SetIsOnTop(false)
            screens.Remove(s)
            |> function false -> failwith "Failed to remove screen" | true -> ()

        match get_top_screen() with
        | None -> ()
        | Some top ->
            top.SetIsOnTop(true)

        s.UnloadContent()
        s.ClearScreenManager()

        System.Diagnostics.Debug.WriteLine(sprintf "Screen %s removed." (s.GetType().Name))

    member this.UiContent = ui_content_provider

// A screen implementation that provides its own content manager and access to the game object.
[<AbstractClass>]
type ScreenBase<'T>(relative_content_path) =

    let is_on_top = ref false
    let game : Game option ref = ref None
    let content : ContentManager option ref = ref None
    let screen_manager : ScreenManager option ref = ref None
    let drawer : ('T -> unit) ref = ref (fun _ -> ())

    abstract member SetGame : Game -> unit
    default this.SetGame(new_game) =
        let g, c =
            let mkContent() =
                new ContentManager(
                        new_game.Content.ServiceProvider,
                        System.IO.Path.Combine(new_game.Content.RootDirectory, relative_content_path))

            match !game, !content with
            | Some game, Some content when not (System.Object.ReferenceEquals(game, new_game)) ->
                content.Unload()
                new_game, mkContent()
            | None, None ->
                new_game, mkContent()
            | _ -> failwith "Unreachable"

        game := Some g
        content := Some c

    member this.SetDrawer(d) = drawer := d

    abstract member BeginDrawer : unit -> 'T option
    default this.BeginDrawer() = None

    abstract member EndDrawer : 'T -> unit
    default this.EndDrawer(_) = ()

    abstract member LoadContent : unit -> unit
    
    abstract member UnloadContent : unit -> unit    
    
    interface Screen with
        member this.SetGame(ng) = this.SetGame(ng)

        member this.Draw() =
            match this.BeginDrawer() with
            | Some rsc ->
                try
                    (!drawer)(rsc)
                finally
                    this.EndDrawer(rsc)
            | None -> ()

            if not this.IsOnTop then
                match !game with
                | Some g ->
                    let viewport = g.GraphicsDevice.Viewport
                    try
                        this.SpriteBatch.Begin()
                        this.SpriteBatch.Draw(
                            this.ScreenManager.UiContent.Blank,
                            new Rectangle(0, 0,  viewport.Width, viewport.Height),
                            Color.Black * 0.5f)
                    finally
                        this.SpriteBatch.End()
                | _ -> ()


        member this.LoadContent() =
            this.LoadContent()

        member this.UnloadContent() =
            this.UnloadContent()
            match !content with
            | Some content -> content.Unload()
            | None -> ()
        
        member this.SetIsOnTop(b) = is_on_top := b

        member this.SetScreenManager(s) =
            match !screen_manager with
            | None -> screen_manager := Some s
            | Some _ -> invalidOp "A screen cannot be added to more than one screen manager at a time."
        
        member this.ClearScreenManager() =
            match !screen_manager with
            | Some _ -> screen_manager := None
            | None -> invalidOp "Screen is not attached to a screen manager."

    interface System.IDisposable with
        member this.Dispose() =
            match !content with
            | Some content ->
                content.Unload()
                content.Dispose()
            | None -> ()

    member this.IsOnTop = !is_on_top

    member this.IsActive =
        !is_on_top
#if XBOX360
        && not Microsoft.Xna.Framework.GamerServices.Guide.IsVisible
#endif

    member this.HasGame = (!game).IsSome

    member this.Game =
        match !game with
        | Some g -> g
        | None -> invalidOp "Game not set"

    member this.HasContent = (!content).IsSome

    member this.Content =
        match !content with
        | Some c -> c
        | None -> invalidOp "Content not set"

    member this.HasScreenManager = (!screen_manager).IsSome

    member this.ScreenManager =
        match !screen_manager with
        | Some s -> s
        | None -> invalidOp "Screen must be added to a screen manager first"

    member this.Font1 =
        this.ScreenManager.UiContent.Font1

    member this.Font2 =
        this.ScreenManager.UiContent.Font2

    member this.SpriteBatch =
        this.ScreenManager.UiContent.SpriteBatch
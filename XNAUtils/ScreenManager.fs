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

    member private this.AddScreen(s : Screen) =
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

    // A task that adds a screen, executes a given task then removes the screen.
    // Screen removal is guaranteed using finally.
    member this.AddDoRemove(s : Screen, t : Eventually<'T>) = task {
        try
            this.AddScreen(s)
            return! t
        finally
            this.RemoveScreen(s)
    }

    member this.RemoveAllScreens() =
        let tmp = screens.ToArray()
        for s in tmp do
            this.RemoveScreen(s)

    member this.UiContent = ui_content_provider


// A screen implementation that provides its own content manager and access to the game object.
type ScreenBase<'T> (relative_content_path) =
    let is_on_top = ref false
    let game : Game option ref = ref None
    let content : ContentManager option ref = ref None
    let screen_manager : ScreenManager option ref = ref None

    // Called before the drawer. Expected to return some data or resource passed to the drawer.
    // If it returns None, the drawer is not called.
    let pre_drawer = ref (fun() -> None)
    // The drawer, expected to issue draw commands.
    let drawer : ('T -> unit) ref = ref (fun _ -> ())
    // Called after the drawer, to clean up resources allocated by pre_drawer, if any.
    let post_drawer : ('T -> unit) ref = ref (fun _ -> ())

    new() = new ScreenBase<'T>("")

    member this.PreDrawer
        with get() = !pre_drawer
        and set(d) = pre_drawer := d

    member this.Drawer
        with get() = !drawer
        and set(d) = drawer := d

    member this.PostDrawer
        with get() = !post_drawer
        and set(d) = post_drawer := d
    
    interface Screen with
        member this.SetGame(new_game) =
            let g, c =
                let mkContent() =
                    new ContentManager(
                            new_game.Content.ServiceProvider,
                            System.IO.Path.Combine(new_game.Content.RootDirectory, relative_content_path))

                match !game, !content with
                | Some game, Some content ->
                    if System.Object.ReferenceEquals(game, new_game) then
                        game, content
                    else
                        content.Unload()
                        new_game, mkContent()
                | None, None ->
                    new_game, mkContent()
                | _ -> failwith "Unreachable"

            game := Some g
            content := Some c

        // Draw this screen.
        // Check if the pre-drawer returns some value.
        // If so, call the drawer, then finally the post-drawer.
        // If this screen is not on top, draw a half-transparent black veil over it.
        member this.Draw() =
            match (!pre_drawer)() with
            | Some rsc ->
                try
                    (!drawer)(rsc)
                finally
                    (!post_drawer)(rsc)
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


        member this.LoadContent() = ()

        member this.UnloadContent() =
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
module XNAUtils.ScreenManager

open Microsoft.Xna.Framework
open Microsoft.Xna.Framework.Content
open System.Collections.Generic

open XNAUtils.CoopMultiTasking

// Interface of screens as exposed to the ScreenManager
type Screen =
    interface
        abstract member Draw : GameTime -> unit
        abstract member LoadContent : unit -> unit
        abstract member UnloadContent : unit -> unit
        abstract member SetIsOnTop : bool -> unit
        abstract member SetGame : Game -> unit
    end

// A screen implementation that provides its own content manager and access to the game object.
[<AbstractClass>]
type ScreenBase(relative_content_path) =
    let is_on_top = ref false
    let game : Game option ref = ref None
    let content : ContentManager option ref = ref None
    
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

    abstract member Draw : GameTime -> unit

    abstract member LoadContent : unit -> unit
    
    abstract member UnloadContent : unit -> unit    
    
    interface Screen with
        member this.SetGame(ng) = this.SetGame(ng)
        member this.Draw(gt) = this.Draw(gt)
        member this.LoadContent() = this.LoadContent()
        member this.UnloadContent() =
            this.UnloadContent()
            match !content with
            | Some content -> content.Unload()
            | None -> ()

        member this.SetIsOnTop(b) = is_on_top := b

    interface System.IDisposable with
        member this.Dispose() =
            match !content with
            | Some content ->
                content.Unload()
                content.Dispose()
            | None -> ()

    member this.IsOnTop = !is_on_top

    member this.Game =
        match !game with
        | Some g -> g
        | None -> invalidOp "Game not set"

    member this.Content =
        match !content with
        | Some c -> c
        | None -> invalidOp "Content not set"


// The screen manager keeps track of screens which must be drawn.
// It is responsible for telling screens when to load and unload content.
type ScreenManager(game) =
    inherit DrawableGameComponent(game)

    let screens = new List<Screen>()

    let get_top_screen() =
        if screens.Count > 0 then
            let s = screens.[screens.Count - 1]
            Some s
        else
            None

    let add_screen (s : Screen) =
        match get_top_screen() with
        | Some top -> top.SetIsOnTop(false)
        | None -> ()

        screens.Add(s)

        s.SetGame(game)
        s.SetIsOnTop(true)
        
        s.LoadContent()

        System.Diagnostics.Debug.WriteLine(sprintf "Screen %s added." (s.GetType().Name))

    let remove_screen (s : Screen) =
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

        System.Diagnostics.Debug.WriteLine(sprintf "Screen %s removed." (s.GetType().Name))

    override this.LoadContent() =
        for s in screens do s.LoadContent()
        base.LoadContent()

    override this.Update(gt) =
        base.Update(gt)

    override this.Draw(gt) =
        // Copy screens, so that s.Draw can modify it while we are iterating over the copy.
        let screens = screens.ToArray()
        for s in screens do s.Draw(gt)
        base.Draw(gt)

    member this.AddScreen(s : Screen) =
        add_screen s

    member this.RemoveScreen(s : Screen) =
        remove_screen s

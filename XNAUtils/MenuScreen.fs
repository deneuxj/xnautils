namespace CleverRake.XnaUtils.Application

open Microsoft.Xna.Framework
open Microsoft.Xna.Framework.Input
open Microsoft.Xna.Framework.Graphics

open CleverRake.XnaUtils
open CleverRake.XnaUtils.XnaExtensions

open CleverRake.XnaUtils.CoopMultiTasking.Core
open CleverRake.XnaUtils.CoopMultiTasking.Sys
open CleverRake.XnaUtils.CoopMultiTasking

open CleverRake.XnaUtils.Application

type AnimationParameters =
    {  period : float32
       shift : float32  }

type PlacementParameters =
    {  left : float32
       top : float32
       spacing : float32  }

type EntryVisibility =
    | Visible
    | Disabled
    | Hidden

type MenuScreen<'I when 'I : equality>(player : PlayerIndex, sys : Environment, items : ('I * string)[], anim : AnimationParameters, placement : PlacementParameters) =
    let impl = new ScreenBase<unit>()
    let impl_screen = impl :> Screen

    let current = ref 0

    let animation = new Animations.MultipleFadeIn(sys, items.Length, anim.period, anim.shift)

    let input = new InputChanges.InputChanges(player)

    let visibility = items |> Array.map (fun _ -> Visible)

    do if items.Length = 0 then invalidArg "items" "items may not be empty"

    let next succ pos =
        let rec work bound c =
            if bound <= 0 then
                None
            else
                let c = succ c
                match visibility.[c] with
                | Visible -> Some c
                | _ -> work (bound - 1) c
        work items.Length pos

    let moveDown() =
        match next (fun c -> (c + 1) % items.Length) !current with
        | Some c -> current := c
        | None -> failwith "Can't move down"

    let moveUp() =
        match next (fun c -> (c + items.Length - 1) % items.Length) !current with
        | Some c -> current := c
        | None -> failwith "Can't move up"

    member this.Hide(entry : 'I) =
        let mutable must_move = false
        for i in 0..items.Length-1 do
            if fst items.[i] = entry then
                visibility.[i] <- Hidden
                if i = !current then
                    must_move <- true
        if must_move then
            moveDown()

    member this.Show(entry : 'I) =
        for i in 0..items.Length-1 do
            if fst items.[i] = entry then
                visibility.[i] <- Visible        

    member this.Disable(entry : 'I) =
        let mutable must_move = false
        for i in 0..items.Length-1 do
            if fst items.[i] = entry then
                visibility.[i] <- Disabled
                must_move <- true
        if must_move then
            moveDown()

    member this.Task = task {
        impl.PreDrawer <- fun() -> Some()
        impl.Drawer <- this.Drawer

        let animator = sys.Spawn(animation.Task)

        // A trick to move to the first enabled entry (from the top).
        moveUp()
        moveDown()

        let selected = ref false
        let backed = ref false
        while not (!selected || !backed) do
            // If this screen is not active, i.e. it is not on top or the guide is visible, wait.
            // We don't want to react to input that's not for us.
            do! sys.WaitUntil(fun () -> impl.IsActive)

            input.Update()

            if input.IsMenuDown() then moveDown()
            elif input.IsMenuUp() then moveUp()
            elif input.IsStartPressed() then selected := true
            elif input.IsBackPressed() then backed := true

            do! sys.WaitNextFrame()

        animator.Kill()
        do! sys.WaitUntil(fun() -> animator.IsDead)

        return
            if !selected then items.[!current] |> fst |> Some
            else None
    }

    interface Screen with
        member this.ClearScreenManager() = impl_screen.ClearScreenManager()
        member this.Draw() = impl_screen.Draw()
        member this.LoadContent() = impl_screen.LoadContent()
        member this.SetGame(ng) = impl_screen.SetGame(ng)
        member this.SetIsOnTop(b) = impl_screen.SetIsOnTop(b)
        member this.SetScreenManager(sm) = impl_screen.SetScreenManager(sm)
        member this.UnloadContent() = impl_screen.UnloadContent()

    interface System.IDisposable with
        member this.Dispose() = (impl :> System.IDisposable).Dispose()

    member private this.Drawer() =
        let visible_color = Color.Yellow
        let disabled_color = Color.Gray
        let selected_color = Color.Red
        
        let right = 10.0f + float32 impl.Game.GraphicsDevice.Viewport.Width
        try
            impl.SpriteBatch.Begin()
            let rec work(i, y) =
                if i < items.Length then
                    let dst = Vector2(placement.left, y)
                    let src = Vector2(right, y)
                    let k = animation.Values(i)
                    let pos = k * dst + (1.0f - k) * src
                    let color, next_y =
                        match visibility.[i] with
                        | Hidden -> Color.Black, y
                        | Disabled ->
                            if i = !current then
                                Color.Pink, y + placement.spacing
                            else
                                Color.Gray, y + placement.spacing
                        | Visible ->
                            if i = !current then
                                Color.Red
                            else
                                Color.Yellow
                            ,
                            y + placement.spacing
                    
                    match visibility.[i] with
                    | Hidden -> ()
                    | _ -> impl.SpriteBatch.DrawString(impl.Font1, snd items.[i], pos, color)

                    work(i + 1, next_y)
                else
                    ()

            work(0, placement.top)

        finally
            impl.SpriteBatch.End()

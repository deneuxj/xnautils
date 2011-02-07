module CoopMultiTaskingSample.ScoreScreen

open Microsoft.Xna.Framework
open Microsoft.Xna.Framework.Input
open Microsoft.Xna.Framework.Graphics

open XNAUtils.CoopMultiTasking
open XNAUtils.ScreenManager
open XNAUtils.InputChanges
open XNAUtils.XNAExtensions

type Resources =
    { font : SpriteFont
      batch : SpriteBatch }

type ScoreTuple =
    struct
        new(n, p) = { name = n ; points = p }
        val mutable name : string
        val mutable points : int
    end

type Scores() =
    let num_entries = 10
    let mutable scores : ScoreTuple[] =
        [|
            for i in 1..num_entries do
                yield ScoreTuple("Someone", i * 7000)
        |]

    member this.AddScore(name, points) =
        let rec work2 (prev, i) =
            if i >= 0 then
                let bak = scores.[i]
                scores.[i] <- prev
                work2 (bak, i-1)
            else
                true

        let rec work1 i =
            if i >= 0 then
                if scores.[i].points < points then
                    let bak = scores.[i]
                    scores.[i] <- ScoreTuple(name, points)
                    work2 (bak, i-1)
                else
                    work1 (i-1)
            else false

        work1 (num_entries - 1)

    member this.NumEntries = num_entries

    member this.Item(i) = ((match scores.[i].name with null -> "" | s -> s), scores.[i].points)

    member this.SerializableData
        with get() =
            scores
        and set(s) =
            scores <- s


type ScoreScreen(sys : Environment, player : PlayerIndex, scores : Scores) =
    inherit ScreenBase<Resources>()

    let rsc = ref None
    let input = new InputChanges(player)

    member this.Task = task {
        let input_updater = sys.SpawnRepeat(task { input.Update(); return! sys.WaitNextFrame() })

        this.SetDrawer(this.DrawScores)

        do! sys.WaitUntil(fun() -> input.IsStartPressed() || input.IsBackPressed() || not(GamerServices.Gamer.IsSignedIn(player)))

        input_updater.Kill()
        return! sys.WaitUntil(fun() -> input_updater.IsDead)
    }

    override this.LoadContent() =
        match !rsc with
        | Some r -> r.batch.Dispose()
        | None -> ()

        let font : SpriteFont = base.Content.Load("ui/font")
        rsc := Some
                { batch = new SpriteBatch(base.Game.GraphicsDevice)
                  font = font }

    override this.UnloadContent() =
        rsc := None

    override this.BeginDrawer() =
        match !rsc with
        | Some rsc -> rsc.batch.Begin()
        | None -> ()
        !rsc

    override this.EndDrawer(rsc) =
        rsc.batch.End()

    member private this.DrawScores(rsc : Resources) =
        let startY = 400.0f
        let offY = 30.0f
        let col0 = 100.0f
        let col1 = 500.0f

        let rec renderPrefix (s : string, pos : Vector2, num) =
            let sz = rsc.font.MeasureString(s.Substring(0, num))
            if num = 1 || sz.X < 0.95f * (col1 - col0) then
                rsc.batch.DrawString(rsc.font, s.Substring(0, num), pos, Color.White)
            else
                renderPrefix(s, pos, num-1)

        for i in 0..scores.NumEntries-1 do
            let name, points = scores.[i]
            let y = startY - (float32 i) * offY
            renderPrefix(name, Vector2(col0, y), name.Length)
            rsc.batch.DrawString(rsc.font, sprintf "%6d" points, Vector2(col1, y), Color.White)
             
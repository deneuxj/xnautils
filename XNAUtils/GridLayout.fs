module CleverRake.XnaUtils.GridLayout

open System.Collections.Generic

open CleverRake.XnaUtils.Error

type IGridContent =
    abstract member GetWidth : unit -> float32 option
    abstract member GetHeight : unit -> float32 option
    abstract member RenderAt : float32 * float32 * float32 * float32 -> unit


let noContent =
    { new IGridContent with
        member this.GetWidth() = None
        member this.GetHeight() = None
        member this.RenderAt (_, _, _, _) = () }


type Constraint =
    | FixedSize
    | Proportional of float32


type Grid =
    { columns : Constraint[]
      rows : Constraint[]
      content : IGridContent[,] }


let computeSizes width height grid =
    let column_sizes =
        [| for j in 0 .. grid.columns.Length - 1 do
            yield
                match grid.columns.[j] with
                | FixedSize ->
                    seq { for i in 0 .. Array2D.length1 grid.content - 1 do
                            yield defaultArg (grid.content.[i, j].GetWidth()) 0.0f
                    }
                    |> Seq.max
                    |> Some
                | Proportional _ ->
                    None
        |]

    let row_sizes =
        [| for i in 0 .. grid.rows.Length - 1 do
            yield
                match grid.rows.[i] with
                | FixedSize ->
                    seq { for j in 0 .. Array2D.length2 grid.content - 1 do
                            yield defaultArg (grid.content.[i, j].GetHeight()) 0.0f
                    }
                    |> Seq.max
                    |> Some
                | Proportional _ ->
                    None
        |]

    let excess_width =
        max 0.0f
            (width
             -
             (column_sizes
              |> Seq.map (function None -> 0.0f | Some x -> x)
              |> Seq.sum))

    let excess_height =
        max 0.0f
            (height
             -
             (row_sizes
              |> Seq.map (function None -> 0.0f | Some x -> x)
              |> Seq.sum))

    let compute spec sizes excess =
        let total =
            spec
            |> Seq.map (function Proportional w -> w | FixedSize -> 0.0f)
            |> Seq.sum

        Array.zip spec sizes
        |> Array.map
            (function
             | (FixedSize, Some x) -> x
             | (Proportional w, None) -> excess * (w / total)
             | _ -> failInternalWith "Unexpected combination")

    let column_sizes = compute grid.columns column_sizes excess_width
    let row_sizes = compute grid.rows row_sizes excess_height

    column_sizes, row_sizes


let render x y grid (widths : float32[], heights : float32[]) =
    let xs : (float32 * float32)[] =
        widths
        |> Seq.scan (+) x
        |> Seq.pairwise
        |> Array.ofSeq

    let ys : (float32 * float32)[] =
        heights
        |> Seq.scan (+) y
        |> Seq.pairwise
        |> Array.ofSeq

    let len2 = Array2D.length2 grid.content
    let slice i =
        grid.content.[i..i, 0..]
        |> fun (arr2 : _[,]) -> Array.init len2 (fun i -> arr2.[0, i])
        
    for i in 0 .. Array2D.length1 grid.content - 1 do
        let (y0, y1) = ys.[i]
        let row : IGridContent[] = slice i            
        for (x0, x1), cell in Seq.zip xs row do
            cell.RenderAt(x0, y0, x1, y1)


module Construction =
    let private haveSameLength A B =
        eprintfn "%d %d" (Array.length A) (Array.length B)
        Array.length A = Array.length B

    let private rowMatchesColumns columns (_, row) =
        haveSameLength row columns

    let private validateRows columns rows =
        let are_valid =
            rows
            |> Seq.forall (rowMatchesColumns columns)

        if not are_valid then
            failUserErrorWith "Content size does not match column specification"
        
    type InitialStage = Initial
    type PartialStage = Populating of List<Constraint * IGridContent[]> * Constraint[]

    let newGrid() = Initial
    
    let setColumns (columns : Constraint[]) Initial =
        Populating (new List<Constraint * IGridContent[]>(), columns)

    let addVerticalSpace weight (Populating(rows, columns)) =
        Populating (rows.Add((Proportional weight, Array.create columns.Length noContent)); rows, columns)

    let addRow (row : Constraint * IGridContent[]) (Populating(rows, columns)) =
        validateRows columns [| row |]
        Populating (rows.Add(row); rows, columns)

    let addRows more_rows (Populating(rows, columns)) =
        validateRows columns rows
        Populating (rows.AddRange(more_rows); rows, columns)

    let finalize (Populating(rows, columns)) =
        let rows_spec = Seq.map fst rows |> Array.ofSeq
        let rows_content : IGridContent[][] = Seq.map snd rows |> Array.ofSeq
        
        validateRows columns rows

        { columns = columns
          rows = rows_spec
          content =
            Array2D.init rows_spec.Length columns.Length (fun i j -> rows_content.[i].[j]) }
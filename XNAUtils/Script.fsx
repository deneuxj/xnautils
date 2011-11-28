// This file is a script that can be executed with the F# Interactive.  
// It can be used to explore and test the library project.
// Note that script files will not be part of the project build.

#r "bin/debug/CleverRake.XnaUtils.Core.dll"

#load "GridLayout.fs"

open CleverRake.XnaUtils.GridLayout

let rect label w h =
    { new IGridContent with
        member this.GetWidth() = Some w
        member this.GetHeight() = Some h
        member this.RenderAt (x0, y0, x1, y1) = printfn "%s %f %f %f %f" label x0 y0 x1 y1 }

let horiz label w =
    { new IGridContent with
        member this.GetWidth() = Some w
        member this.GetHeight() = None
        member this.RenderAt (x0, y0, x1, y1) = printfn "%s %f %f %f %f" label x0 y0 x1 y1 }

let vert label h =
    { new IGridContent with
        member this.GetWidth() = None
        member this.GetHeight() = Some h
        member this.RenderAt (x0, y0, x1, y1) = printfn "%s %f %f %f %f" label x0 y0 x1 y1 }

let grid =
    Construction.newGrid()
    |> Construction.setColumns [| Proportional 1.0f; FixedSize; FixedSize; Proportional 0.1f; FixedSize; Proportional 1.0f |]
    |> Construction.addRow (FixedSize, [| noContent ; rect "A" 10.0f 1.0f ; rect "B" 20.0f 1.0f ; noContent ; rect "C" 10.0f 1.0f ; noContent |])
    |> Construction.addVerticalSpace 0.1f
    |> Construction.addRow (FixedSize, [| noContent ; rect "X" 15.0f 1.0f ; rect "Y" 20.0f 2.0f ; noContent ; rect "Z" 5.0f 1.0f ; noContent |])
    |> Construction.finalize

let sizes = computeSizes 1024.0f 768.0f grid
printfn "%A" sizes
render 0.0f 0.0f grid sizes
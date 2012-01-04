module CleverRake.XnaUtils.Octree

(*
Copyright (C) 2010 Johann Deneux

   Licensed under the Apache License, Version 2.0 (the "License");
   you may not use this file except in compliance with the License.
   You may obtain a copy of the License at

     http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS,
   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
   See the License for the specific language governing permissions and
   limitations under the License.
*)


open Microsoft.Xna.Framework
open System

type Node<'TS> =
    { bbox : BoundingBox
      data : Data<'TS> }
and Data<'TS> =
    | Leaf of int * 'TS          // sequence length, sequence of items
    | Inner of Node<'TS> array   // Array of 8 items exactly


let newEmpty<'T> bbox : Node<'T list>=
    { bbox = bbox ; data = Leaf(0, []) }


[<Struct>]
type FloatPair (a : float32, b : float32) =
    member x.A = a
    member x.B = b

let mkBBox (p1 : Vector3) (p2 : Vector3) =
    let prX =
        if p1.X < p2.X then FloatPair(p1.X, p2.X) else FloatPair(p2.X, p1.X)
    let prY =
        if p1.Y < p2.Y then FloatPair(p1.Y, p2.Y) else FloatPair(p2.Y, p1.Y)
    let prZ =
        if p1.Z < p2.Z then FloatPair(p1.Z, p2.Z) else FloatPair(p2.Z, p1.Z)
    BoundingBox (Vector3 (prX.A, prY.A, prZ.A), Vector3 (prX.B, prY.B, prZ.B))

let split intersect (node : Node<'T list>) =
    match node.data with
    | Leaf(count, items) ->
        let sub_bboxes =
            let center = 0.5f * (node.bbox.Min + node.bbox.Max)
            node.bbox.GetCorners()
            |> Array.map (mkBBox center)

        let children =
            sub_bboxes
            |> Array.map (fun sub_bbox ->
                let in_bbox =
                    items
                    |> List.filter (intersect sub_bbox)
                    
                { bbox = sub_bbox ; data = Leaf (List.length in_bbox, in_bbox) }
                )
                                  
        { node with data = Inner(children) }
        
    | Inner _ -> raise (new ArgumentException("Cannot split an inner node", "node"))

    
let insert max_count max_depth intersect (node : Node<'T list>) item depth =
    let rec work node item depth =
        assert (
            intersect node.bbox item
        )
    
        match node.data with
        | Leaf(count, items) when (depth >= max_depth || count < max_count) ->
            { node with data = Leaf(count + 1, item :: items) }
        | Leaf(count, items) ->
            let node' = split intersect node
            work node' item depth
        | Inner(children) ->
            { node with
                data = Inner(
                        children
                        |> Array.map (fun child_node ->                            
                            if intersect child_node.bbox item
                            then work child_node item (depth + 1)
                            else child_node
                            )
                        )
            }

    work node item depth

/// Turn an octree using lists to store items to an octree that uses arrays
let rec freeze octree =
    match octree with
    | { bbox = bbox; data = Leaf(len, items) } -> { bbox = bbox; data = Leaf(len, Array.ofList items) }
    | { bbox = bbox; data = Inner(children) } ->
        let children =
            children
            |> Array.map freeze
        { bbox = bbox; data = Inner children }


let checkIntersection intersectBbox intersectSomeItem node =
    let rec work node =
        if intersectBbox node.bbox then
            match node.data with
            | Leaf(_, items) ->
                intersectSomeItem items
            | Inner(children) ->
                children |> ArrayInlined.tryMapFirst work
        else
            None
    work node

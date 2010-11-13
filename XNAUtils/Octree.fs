module XNAUtils.Octree

(*
Copyright [2010] [Johann Deneux]

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

type Node<'T> =
    | Leaf of BoundingBox * int * 'T list  // bounding, list length, list of items
    | Inner of BoundingBox * Node<'T> array   // Array of 8 items exactly


let newEmpty<'T> bbox : Node<'T>=
    Leaf(bbox, 0, [])


[<Struct>]
type private FloatPair (a : float32, b : float32) =
    member x.A = a
    member x.B = b


let split intersect node =
    match node with
    | Leaf(bbox, count, items) ->
        let inline mkBBox (p1 : Vector3) (p2 : Vector3) =
            let prX =
                if p1.X < p2.X then FloatPair(p1.X, p2.X) else FloatPair(p2.X, p1.X)
            let prY =
                if p1.Y < p2.Y then FloatPair(p1.Y, p2.Y) else FloatPair(p2.Y, p1.Y)
            let prZ =
                if p1.Z < p2.Z then FloatPair(p1.Z, p2.Z) else FloatPair(p2.Z, p1.Z)
            BoundingBox (Vector3 (prX.A, prY.A, prZ.A), Vector3 (prX.B, prY.B, prZ.B))

        let sub_bboxes =
            let center = 0.5f * (bbox.Min + bbox.Max)
            bbox.GetCorners()
            |> Array.map (mkBBox center)

        let children =
            sub_bboxes
            |> Array.map (fun sub_bbox ->
                let in_bbox =
                    items
                    |> List.filter (intersect sub_bbox)
                    
                Leaf (sub_bbox, List.length in_bbox, in_bbox)
                )
                                  
        Inner(bbox, children)
        
    | Inner _ -> raise (new InvalidOperationException("Cannot split an inner node"))

    
let rec insert max_count max_depth intersect node item depth =
    assert (
        match node with
        | Leaf(bbox, _, _) | Inner(bbox, _) -> intersect bbox item
    )
    
    match node with
    | Leaf(bbox, count, items) when (depth >= max_depth || count < max_count) ->
        Leaf(bbox, count + 1, item :: items)
    | Leaf(bbox, count, items) ->
        split intersect node
    | Inner(bbox, children) ->
        Inner(bbox,
            children
            |> Array.map (fun child_node ->
                match child_node with
                | Leaf(bbox, _, _) | Inner(bbox, _) ->
                    if intersect bbox item
                    then insert max_count max_depth intersect child_node item (depth + 1)
                    else child_node
                )
            )


let rec checkIntersection intersectBbox intersectItem node =
    match node with
    | Leaf(bbox, _, items) ->
        intersectBbox bbox &&
        items |> List.exists intersectItem
    | Inner(bbox, children) ->
        intersectBbox bbox &&
        children |> Array.exists (checkIntersection intersectBbox intersectItem)
        
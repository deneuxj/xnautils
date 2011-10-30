module XNAUtils.Hermite

(*
Copyright [2010, 2011] [Johann Deneux]

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

type Vec2 = Microsoft.Xna.Framework.Vector2

/// <summary>Compute the two-dimensional Hermite curve between two points</summary>
/// The function accepts the end points, first derivative at the end points and values of the parameters at the end points.
/// <param name="p0">Start point.</param>
/// <param name="v0">Derivative at the starting point ("speed").</param>
/// <param name="t0">Value of the parameter at the starting point ("starting time").</param>
/// <param name="p1">End point.</param>
/// <param name="v1">Derivative at the end point ("speed").</param>
/// <param name="t1">Value of the parameter at the end point ("arrival time").</param>
/// <returns>
/// - None if the hermite cannot be generated, because t1 <= t0.
/// - Some(h, h', h''), where h is the Hermite curve, h' is the first derivative and h'' the second derivative.
/// </returns>
let hermite (p0 : Vec2) (v0 : Vec2) (t0 : float32) (p1 : Vec2) (v1 : Vec2) (t1 : float32) =

    match t1 - t0 with
    | diff_t when diff_t <= 0.0f -> None
    | diff_t ->
        let sq x = x * x
        let h t =
            let h00 t = (1.0f + 2.0f * t) * sq (1.0f - t)
            let h10 t = t * sq (1.0f - t)
            let h01 t = sq t * (3.0f - 2.0f * t)
            let h11 t = sq t * (t - 1.0f)

            let t = (t - t0) / diff_t
            
            (h00 t * p0) +
            (h10 t * diff_t * v0) +
            (h01 t * p1) +
            (h11 t * diff_t * v1)   

        let h' t =
            let h00' t = (6.0f * t) * (t - 1.0f)
            let h10' t = (t - 1.0f) * (3.0f * t - 1.0f)
            let h01' t = (6.0f * t) * (1.0f - t)
            let h11' t = t * (3.0f * t - 2.0f)

            let t = (t - t0) / diff_t

            (1.0f / diff_t) *
            ((h00' t * p0) +
             (h10' t * diff_t * v0) +
             (h01' t * p1) +
             (h11' t * diff_t * v1))
        
        let h'' t =
            let h00'' t = 12.0f * t - 6.0f
            let h10'' t = 6.0f * t - 4.0f
            let h01'' t = -12.0f * t + 6.0f
            let h11'' t = 6.0f * t - 2.0f

            let t = (t - t0) / diff_t

            (sq (1.0f / diff_t)) *
            ((h00'' t * p0) +
             (h10'' t * diff_t * v0) +
             (h01'' t * p1) +
             (h11'' t * diff_t * v1))

        Some (h, h', h'')


// Naive numerical computation of derivatives. Used to test hermite.
let deriv (op_sub, op_scal) f t =
    let eps = 1e-6
    op_scal (1.0 / eps, op_sub (f(t + eps), f(t)))

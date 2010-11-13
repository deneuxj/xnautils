module XNAUtils.TextIcons

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
open Microsoft.Xna.Framework.Graphics


type TextWithIcons =
    | String of string * TextWithIcons
    | Icon of int * TextWithIcons
    | Nil

type FontInfo =
    {  font : SpriteFont ;
       top_space : float32 ;
       bottom_space : float32  }
       
let render
    (render_txt_fun : SpriteFont * string * Vector2 * Color * float32 -> unit)
    (render_img_fun : Texture2D * Rectangle * Color -> unit)
    (getTexture : int -> Texture2D) (getRatio : int -> float32) (font_info : FontInfo) (scale : float32) (color : Color) =
    let sizeT = scale * (font_info.font.MeasureString("T").Y - font_info.bottom_space - font_info.top_space)
    let rec work (txt : TextWithIcons) (pos : Vector2) =
        match txt with
        | String(s, rest) ->
            let pos' = pos + scale * Vector2(font_info.font.MeasureString(s).X, 0.0f)
            let s = s.Replace(System.Environment.NewLine, System.String.Empty)
            render_txt_fun(font_info.font, s, pos, color, scale)
            work rest pos'
        | Icon(idx, rest) ->
            let texture = getTexture idx
            let ratio = getRatio idx
            let icon_width = sizeT * ratio
            let pos' = pos + Vector2(icon_width, 0.0f)
            render_img_fun(texture, new Rectangle(int pos.X, int pos.Y + int (font_info.top_space * scale), int icon_width, int sizeT), color)
            work rest pos'
        | Nil ->
            pos
    work

/// Build a renderer.
/// topSpace: amount of space (in points) between the top of the target rectangle and the top of the icon.
/// bottomStapce: amount of space (in points) between the bottom of the target rectangle and the bottom of the icon.
type Renderer(icons : Texture2D[], ratios : float32[], font : SpriteFont, topSpace : int, bottomSpace : int) =
    do if icons.Length <> ratios.Length then raise (System.ArgumentException("icons and ratios must have identical lengths"))
    let getTexture idx = icons.[idx]
    let getRatio idx = ratios.[idx]
    let font_info = { font = font ; bottom_space = float32 bottomSpace ; top_space = float32 topSpace }
    
    let real_render_txt (bat : SpriteBatch) (font : SpriteFont, txt : string, pos : Vector2, color : Color, scale : float32) =
        bat.DrawString(font, txt, pos, color, 0.0f, Vector2.Zero, scale, SpriteEffects.None, 0.0f)
    
    let real_render_img (bat : SpriteBatch) (texture : Texture2D, rect : Rectangle, color : Color) =
        bat.Draw(texture, rect, color)
    
    let nop_render_txt _ _ = ()
    let nop_render_img _ _ = ()
    
    member x.Render(bat, scale, color, pos, txt : string) =
        render (real_render_txt bat) (nop_render_img ()) getTexture getRatio font_info scale color (String(txt, Nil)) pos
    
    member x.Render(bat, scale, color, pos, icon : int) =
        render (nop_render_txt ()) (real_render_img bat) getTexture getRatio font_info scale color (Icon(icon, Nil)) pos
    
    member x.Render(bat, scale, color, pos, twi : TextWithIcons) =
        render (real_render_txt bat) (real_render_img bat) getTexture getRatio font_info scale color twi pos
        
    member x.Measure(scale, twi : TextWithIcons) =
        render (nop_render_txt ()) (nop_render_img ()) getTexture getRatio font_info scale (Color.White) twi (Vector2.Zero)

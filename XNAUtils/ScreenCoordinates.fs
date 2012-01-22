module CleverRake.XnaUtils.ScreenCoordinates

open Microsoft.Xna.Framework
open Microsoft.Xna.Framework.Graphics

open CleverRake.XnaUtils.Units

[<Measure>]
type Screen

[<Measure>]
type Std

type Rectangle with
    /// Construct a rectangle in screen coordinates.
    static member NewForScreen(x : float32<Screen>, y : float32<Screen>, w : float32<Screen>, h : float32<Screen>) =
        new Rectangle(int x, int y, int w, int h)

type SpriteBatch with
    /// Render a texture with rotation using screen coordinates.
    member this.Draw(texture : Texture2D, destination : Rectangle, angle : float32<rad>, color : Color) =
        this.Draw(texture, destination, System.Nullable<Rectangle>(), color, float32 angle, Vector2(0.5f * float32 texture.Width, 0.5f * float32 texture.Height), SpriteEffects.None, 0.0f)

/// Transform centered unit coordinates to screen coordinates
let toScreen (ww : float32<Screen>) (hh : float32<Screen>) (x : float32<Std>) (y : float32<Std>) =
    0.5f * ww * (1.0f + x / 1.0f<Std>),
    0.5f * hh * (1.0f - y / 1.0f<Std>)

/// Get a rectangle from standard coordinates.
/// w and h are the with and height of the rectangle.
/// ww and hh are the with and height of the screen.
/// x, y, w and h are in centered unit coordinates.
/// ww and hh are in screen coordinates.
let getScreenRectangle (ww : float32<Screen>) (hh : float32<Screen>) (w : float32<Std>) (h : float32<Std>) (x : float32<Std>) (y : float32<Std>) =
    let x, y = toScreen ww hh x y
    let w = w / 1.0f<Std> * ww
    let h = h / 1.0f<Std> * hh
    Rectangle.NewForScreen(
        x,
        y,
        w,
        h
    )

type Rectangle with
    static member NewFromStd(ww, hh, w, h, x, y) =
        getScreenRectangle ww hh w h x y
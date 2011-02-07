using System;
using System.Collections.Generic;
using System.Linq;
using Microsoft.Xna.Framework;
using Microsoft.Xna.Framework.Audio;
using Microsoft.Xna.Framework.Content;
#if XBOX360
using Microsoft.Xna.Framework.GamerServices;
#endif
using Microsoft.Xna.Framework.Graphics;
using Microsoft.Xna.Framework.Input;
using Microsoft.Xna.Framework.Media;

using XNAUtils;
using System.Diagnostics;

namespace CoopMultiTaskingSample
{
  /// <summary>
  /// This is the main type for your game
  /// </summary>
  public class Game1 : Microsoft.Xna.Framework.Game, ScreenManager.IUiContentProvider, UserSettings.ISettingsNotifiable
  {
    GraphicsDeviceManager graphics;
    SpriteBatch spriteBatch;
    SpriteFont font;
    SpriteFont mediumFont;
    SpriteFont smallFont;
    SpriteFont bigFont;
    Texture2D blankTexture;

    ScreenManager.ScreenManager screenManager;
    Main.Main<Game1> mainComponent;
#if XBOX360
    GamerServicesComponent gamerServices;
#endif
    
    Stopwatch fpsWatch = Stopwatch.StartNew();
    int FPS_SMOOTH = 10;
    int fpsSmooth = 1;
    int fps = 0;

    Color clearColor = Color.CornflowerBlue;

    public Game1()
    {
      graphics = new GraphicsDeviceManager(this);
      Content.RootDirectory = "Content";
    }

    /// <summary>
    /// Allows the game to perform any initialization it needs to before starting to run.
    /// This is where it can query for any required services and load any non-graphic
    /// related content.  Calling base.Initialize will enumerate through any components
    /// and initialize them as well.
    /// </summary>
    protected override void Initialize()
    {
      // TODO: Add your initialization logic here
      screenManager = new ScreenManager.ScreenManager(this, (ScreenManager.IUiContentProvider)this);
      mainComponent = new Main.Main<Game1>(this, screenManager);
      base.Components.Add(screenManager);
      base.Components.Add(mainComponent);

#if XBOX360
      gamerServices = new GamerServicesComponent(this);
      base.Components.Add(gamerServices);
#endif

      base.Initialize();
    }

    /// <summary>
    /// LoadContent will be called once per game and is the place to load
    /// all of your content.
    /// </summary>
    protected override void LoadContent()
    {
      // Create a new SpriteBatch, which can be used to draw textures.
      spriteBatch = new SpriteBatch(GraphicsDevice);

      // TODO: use this.Content to load your game content here
      mediumFont = Content.Load<SpriteFont>(@"ui\font");
      font = mediumFont;
      smallFont = Content.Load<SpriteFont>(@"ui\small_font");
      bigFont = Content.Load<SpriteFont>(@"ui\big_font");
      blankTexture = Content.Load<Texture2D>(@"ui\blank");
    }

    /// <summary>
    /// UnloadContent will be called once per game and is the place to unload
    /// all content.
    /// </summary>
    protected override void UnloadContent()
    {
      // TODO: Unload any non ContentManager content here
    }

    /// <summary>
    /// Allows the game to run logic such as updating the world,
    /// checking for collisions, gathering input, and playing audio.
    /// </summary>
    /// <param name="gameTime">Provides a snapshot of timing values.</param>
    protected override void Update(GameTime gameTime)
    {
      base.Update(gameTime);
    }

    /// <summary>
    /// This is called when the game should draw itself.
    /// </summary>
    /// <param name="gameTime">Provides a snapshot of timing values.</param>
    protected override void Draw(GameTime gameTime)
    {
      if (fpsSmooth == FPS_SMOOTH)
      {
        fps = (int)(FPS_SMOOTH / fpsWatch.Elapsed.TotalSeconds);
        fpsWatch.Reset();
        fpsWatch.Start();
        fpsSmooth = 1;
      }
      else
        fpsSmooth++;

      GraphicsDevice.Clear(clearColor);

      // TODO: Add your drawing code here
      try
      {
        spriteBatch.Begin();
        spriteBatch.DrawString(font, fps.ToString(), new Vector2(550.0f, 50.0f), gameTime.IsRunningSlowly ? Color.Red : Color.Green);
      } 
      finally
      {
        spriteBatch.End();
      }
      base.Draw(gameTime);
    }

    SpriteFont ScreenManager.IUiContentProvider.Font1
    {
      get { return font; }
    }

    SpriteFont ScreenManager.IUiContentProvider.Font2
    {
      get { return font; }
    }

    Texture2D ScreenManager.IUiContentProvider.Blank
    {
      get { return blankTexture; }
    }

    SpriteBatch ScreenManager.IUiContentProvider.SpriteBatch
    {
      get { return spriteBatch; }
    }

    void UserSettings.ISettingsNotifiable.SetBackgroundColor(Color value)
    {
      clearColor = value;
    }

    void UserSettings.ISettingsNotifiable.SetFontSize(UserSettings.FontSize sz)
    {
      switch (sz)
      {
        case UserSettings.FontSize.Large:
          font = bigFont;
          break;
        case UserSettings.FontSize.Medium:
          font = mediumFont;
          break;
        case UserSettings.FontSize.Small:
          font = smallFont;
          break;
      }
    }
  }
}

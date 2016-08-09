{-# LANGUAGE LambdaCase #-}
import Control.Concurrent     (threadDelay)
import Control.Monad          (forM_, when, unless, forever)
import Control.Lens
import qualified Data.Vector.Unboxed as V
import qualified Data.Vector as B
import Data.ByteString        (ByteString)
import Data.ByteString.Lazy   (fromStrict)
import System.Directory       (getCurrentDirectory)
import System.FilePath        ((</>))
import System.Exit            (exitSuccess, exitFailure)
import SDL
import Gelatin.SDL2
import Gelatin.Fruity
--import Paths_gelatin_example
import Halive.Utils

myPicture :: GLuint -> Picture GLuint
myPicture tex = picture $ do
  draw $ do
    usingStroke [StrokeWidth 3, StrokeFeather 1]
    usingColoredGeometry colorGeom
  draw $ do
    usingStroke [StrokeWidth 3, StrokeFeather 1]
    usingTexture tex
    drawGeometry .= textured [texGeom]
  draw $ do
    usingStroke [StrokeWidth 2, StrokeFeather 0.5]
    drawGeometry .= colored [move (V2 0 200) $ recolor white $ geometry colorGeom]
  draw $ usingColoredGeometry $
    add $ beziers $ vertices $ do
      bez (V2 400 0, white) (V2 600 0, blue) (V2 600 200, green)
      bez (V2 800 200, white) (V2 800 0, blue) (V2 600 0, green)

  where tris = vertices $ do
                 tri (0, red) (V2 100 0, green) (100, blue)
                 tri (0, magenta) (V2 0 100, canary) (100, cyan)
        bcurve :: RawGeometry (V2 Float, V4 Float)
        bcurve = color white $ line $ vertices $ curve (V2 0 100) (V2 50 (-50)) 100
        circle = color white $ line $ vertices $ arc 50 50 0 (2*pi)
        colorGeom = do add $ triangles tris
                       add $ move (V2 100 0) $ beziers tris
                       add $ move (V2 200 0) $ line tris
                       add $ move (V2 300 0) bcurve
                       add $ move (V2 300 100) $ scale (V2 1 (-1)) bcurve
                       add $ move (V2 350 50) circle
        toUVs (V2 x y, _) = (V2 x y, V2 (x/200) (y/200))
        texGeom = move (V2 0 100) $ mapVertices toUVs $ geometry colorGeom

coloredTextPicture :: Font -> Picture t
coloredTextPicture font =
  coloredString font 128 128 "Colored Strings" $ const white

texturedTextPicture :: t -> Font -> Picture t
texturedTextPicture t font =
  texturedString font 128 128 "Textured Strings" t $ \(V2 x y) -> V2 (x/200) ((y+128)/200)

outlinedTextPicture :: Font -> Picture t
outlinedTextPicture font = picture $ draw $ do
  let outline = B.map (color white ) $
                  stringOutline font 128 128 "Outlined Strings"
  drawGeometry .= ColorGeometry outline
  drawStroke .= [StrokeWidth 3, StrokeFeather 1]

isQuit :: Event -> Bool
isQuit (Event _ payload) = isKeyQ payload || payload == QuitEvent
  where
    isKeyQ (KeyboardEvent (KeyboardEventData _ _ _ (Keysym _ KeycodeQ _))) = True
    isKeyQ _ = False

main :: IO ()
main = do
    --ttfName <- getDataFileName $ "assets" </> "Neuton-Regular.ttf"
    --font <- loadFontFile ttfName >>= \case
    --          Left err -> putStrLn err >> exitFailure
    --          Right f  -> return $ fontyData f
    --let (bs,ts) = fontStringGeom font 72 64 "FontData"
    let imgName = "assets" </> "tex.jpg"
        ttfName = "assets" </> "Neuton-Regular.ttf"
    font <- loadFontFile ttfName >>= \case
              Left err -> putStrLn err >> exitFailure
              Right f  -> return f
    (rez, window)   <- reacquire 0 $ startupSDL2Backend 800 600 "gelatin-example" True
    Just (_, tex)   <- reacquire 1 $ loadImage imgName
    forever $ do
      threadDelay 1
      events <- pollEvents
      when (any isQuit events) exitSuccess
      colorPicRender  <- compilePicture rez $ myPicture tex
      colorTextRender <- compilePicture rez $ coloredTextPicture font
      texTextRender   <- compilePicture rez $ texturedTextPicture tex font
      outlineRender   <- compilePicture rez $ outlinedTextPicture font
      renderWithSDL2 window rez $ do
        snd colorPicRender mempty
        snd colorTextRender $ move (V2 0 400) mempty
        snd texTextRender $ move (V2 0 500) mempty
        snd outlineRender $ move (V2 0 600) mempty
      fst colorPicRender
      fst colorTextRender
      fst texTextRender
      fst outlineRender

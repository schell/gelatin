{-# LANGUAGE LambdaCase #-}
import Control.Concurrent     (threadDelay)
import Control.Monad          (forM_, when, unless, forever)
import Control.Arrow
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
import Paths_gelatin_example
import Halive.Utils

pictures :: GLuint -> (ColorPicture (), TexturePicture ())
pictures tex = (colorPic, texPic)
  where colorPic = do
          embed $ do
            setStroke [StrokeWidth 3, StrokeFeather 1]
            setGeometry $ geometry geom

          embed $ do
            setPosition (V2 0 200)
            setStroke [StrokeWidth 2, StrokeFeather 0.5]
            setGeometry $ geometry geom

          embed $ setGeometry $ geometry $ add $ beziers $ vertices $ do
              bez (V2 400 0, white) (V2 600 0, blue) (V2 600 200, green)
              bez (V2 800 200, white) (V2 800 0, blue) (V2 600 0, green)

        texPic = do
            move (V2 0 100)
            setStroke [StrokeWidth 3, StrokeFeather 1]
            setTextures [tex]
            setGeometry texGeom

        tris = vertices $ do
                 tri (0, red) (V2 100 0, green) (100, blue)
                 tri (0, magenta) (V2 0 100, canary) (100, cyan)
        bcurve :: RawGeometry V2V4
        bcurve = line $ mapVertices (\v -> (v,white)) $ vertices $ curve (V2 0 100) (V2 50 (-50)) 100
        circle :: RawGeometry V2V4
        circle = line $ mapVertices (\v -> (v,white)) $ vertices $ arc 50 50 0 (2*pi)
        geom = do add $ triangles tris
                  add $ beziers $ mapVertices (first (+ V2 100 0)) tris
                  add $ line $ mapVertices (first (+V2 200 0)) tris
                  add $ mapRawGeometry (first (+V2 300 0)) bcurve
                  add $ mapRawGeometry (first ((+V2 300 100) . (*V2 1 (-1)))) bcurve
                  add $ mapRawGeometry (first (+V2 350 50)) circle
        toUVs (V2 x y, _) = (V2 x y, V2 (x/200) (y/200))
        texGeom = mapGeometry toUVs $ geometry geom

coloredTextPicture :: Font -> ColorPicture ()
coloredTextPicture font =
  coloredString font 128 128 "Colored Strings" $ const white

texturedTextPicture :: GLuint -> Font -> TexturePicture ()
texturedTextPicture t font =
  texturedString font 128 128 "Textured Strings" t $ \(V2 x y) -> V2 (x/200) ((y+128)/200)

outlinedTextPicture :: Font -> ColorPicture ()
outlinedTextPicture font = do
  let outline = B.map (mapRawGeometry (\v -> (v, white))) $
                  stringOutline font 128 128 "Outlined Strings"
  setRawGeometry outline
  setStroke [StrokeWidth 3, StrokeFeather 1]

isQuit :: Event -> Bool
isQuit (Event _ payload) = isKeyQ payload || payload == QuitEvent
  where
    isKeyQ (KeyboardEvent (KeyboardEventData _ _ _ (Keysym _ KeycodeQ _))) = True
    isKeyQ _ = False

main :: IO ()
main = do
    ttfName <- getDataFileName $ "assets" </> "Neuton-Regular.ttf"
    imgName <- getDataFileName $ "assets" </> "tex.jpg"
    font <- loadFontFile ttfName >>= \case
              Left err -> putStrLn err >> exitFailure
              Right f  -> return f
    (rez, window)   <- reacquire 0 $ startupSDL2Backend 800 600 "gelatin-example" True
    Just (_, tex)   <- reacquire 1 $ loadImage imgName
    forever $ do
      threadDelay 1
      events <- pollEvents
      when (any isQuit events) exitSuccess
      -- We will alloc, compile, display and reslease our picture resources
      -- every frame so we can profile performance
      let (colorPic,texPic) = pictures tex
      colorPicRender  <- compileColorPicture rez colorPic
      texPicRender    <- compileTexturePicture rez texPic
      colorTextRender <- compileColorPicture rez $ coloredTextPicture font
      texTextRender   <- compileTexturePicture rez $ texturedTextPicture tex font
      outlineRender   <- compileColorPicture rez $ outlinedTextPicture font
      renderWithSDL2 window rez $ do
        snd colorPicRender mempty
        snd texPicRender mempty
        snd colorTextRender $ PictureTransform (V2 0 400) 1 (0, V3 0 0 1) 1 1
        snd texTextRender   $ PictureTransform (V2 0 500) 1 (0, V3 0 0 1) 1 1
        snd outlineRender   $ PictureTransform (V2 0 600) 1 (0, V3 0 0 1) 1 1
      fst colorPicRender
      fst texPicRender
      fst colorTextRender
      fst texTextRender
      fst outlineRender

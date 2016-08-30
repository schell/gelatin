{-# LANGUAGE LambdaCase #-}
import Control.Concurrent     (threadDelay)
import Control.Monad          (forM_, when, unless, forever, void)
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
import Gelatin.FreeType2
import Paths_gelatin_example
import Halive.Utils

--------------------------------------------------------------------------------
-- Regular pure pictures
--------------------------------------------------------------------------------
pictures :: GLuint -> (ColorPicture (), TexturePicture ())
pictures tex = (colorPic, texPic)
  where colorPic = do
          embed $ do
            setStroke [StrokeWidth 3, StrokeFeather 1]
            setGeometry geom

          embed $ do
            move (V2 0 200)
            setStroke [StrokeWidth 2, StrokeFeather 0.5]
            setGeometry geom

          embed $ setGeometry $ beziers $ do
              bez (V2 400 0, white) (V2 600 0, blue) (V2 600 200, green)
              bez (V2 800 200, white) (V2 800 0, blue) (V2 600 0, green)

        texPic = do
            move (V2 0 100)
            setStroke [StrokeWidth 3, StrokeFeather 1]
            setTextures [tex]
            setGeometry texGeom

        tris = do tri (0, red) (V2 100 0, green) (100, blue)
                  tri (0, magenta) (V2 0 100, canary) (100, cyan)
        bcurve = mapVertices (\v -> (v,white)) $ curve (V2 0 100) (V2 50 (-50)) 100
        circle = mapVertices (\v -> (v,white)) $ arc 50 50 0 (2*pi)
        geom = do triangles tris
                  beziers $ mapVertices (first (+ V2 100 0)) tris
                  line $ mapVertices (first (+V2 200 0)) tris
                  line $ mapVertices (first (+V2 300 0)) bcurve
                  line $ mapVertices (first ((+V2 300 100) . (*V2 1 (-1)))) bcurve
                  line $ mapVertices (first (+V2 350 50)) circle
        toUVs (V2 x y, _) = (V2 x y, V2 (x/200) (y/200))
        texGeom = mapGeometry toUVs geom

coloredTextPicture :: Font -> ColorPicture ()
coloredTextPicture font =
  coloredString font 100 128 "Colored Strings" $ const white

texturedTextPicture :: GLuint -> Font -> TexturePicture ()
texturedTextPicture t font =
  texturedString font 100 128 "Textured Strings" t $ \(V2 x y) -> V2 (x/200) ((y+128)/200)

outlinedTextPicture :: Font -> ColorPicture ()
outlinedTextPicture font = do
  let outline = B.map (mapRawGeometry (\v -> (v, white))) $
                  stringOutline font 100 128 "Outlined Strings"
  setRawGeometry outline
  setStroke [StrokeWidth 3, StrokeFeather 1]
--------------------------------------------------------------------------------
-- FreeType2 pictures must be run in a MonadIO transformer
--------------------------------------------------------------------------------
freetype2Picture :: FilePath -> TexturePictureT IO ()
freetype2Picture font = do
  let chars = "FreetypePictureswithkerning2,! "
  void $ withAtlas font (PixelSize 100 100) chars $ \atlas -> do
    embed $ freetypePicture atlas orange "Freetype2 Pictures,"
    embed $ do
      move (V2 0 50)
      freetypePicture atlas orange "with kerning!"

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
      -- We will alloc, compile, display and release our picture resources
      -- every frame so we can profile performance
      let (colorPic,texPic) = pictures tex
      colorPicRender  <- compileColorPicture rez colorPic
      texPicRender    <- compileTexturePicture rez texPic
      colorTextRender <- compileColorPicture rez $ coloredTextPicture font
      texTextRender   <- compileTexturePicture rez $ texturedTextPicture tex font
      outlineRender   <- compileColorPicture rez $ outlinedTextPicture font
      freetypeRender  <- compileTexturePictureT rez $ freetype2Picture ttfName
      renderWithSDL2 window rez $ do
        snd colorPicRender mempty
        snd texPicRender mempty
        let mv400 = affineToModelview $ Translate $ V2 0 400
            mv500 = affineToModelview $ Translate $ V2 0 500
            mv600 = affineToModelview $ Translate $ V2 0 600
            ftmv  = affineToModelview $ Translate $ V2 100 250
        snd colorTextRender $ PictureTransform mv400 1 1 Nothing
        snd texTextRender   $ PictureTransform mv500 1 1 Nothing
        snd outlineRender   $ PictureTransform mv600 1 1 Nothing
        snd freetypeRender $ PictureTransform ftmv 1 1 Nothing
      fst colorPicRender
      fst texPicRender
      fst colorTextRender
      fst texTextRender
      fst outlineRender

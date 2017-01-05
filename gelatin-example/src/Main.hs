{-# LANGUAGE LambdaCase #-}
import           Control.Arrow
import           Control.Concurrent    (threadDelay)
import           Control.Monad         (forever, when)
import qualified Data.Vector           as B
import           Gelatin.FreeType2
import           Gelatin.Fruity
import           Gelatin.SDL2
import           Paths_gelatin_example
import           SDL
import           System.Exit           (exitFailure, exitSuccess)
import           System.FilePath       ((</>))

--------------------------------------------------------------------------------
-- Regular pure pictures
--------------------------------------------------------------------------------
colorGeometry :: Geometry (V2 Float, V4 Float)
colorGeometry = do
  triangles tris
  beziers $ mapVertices (first (+ V2 100 0)) tris
  line $ mapVertices (first (+V2 200 0)) tris
  line $ mapVertices (first (+V2 300 0)) bcurve
  line $ mapVertices (first ((+V2 300 100) . (*V2 1 (-1)))) bcurve
  line $ mapVertices (first (+V2 350 50)) circle
  where tris = do tri (0, red) (V2 100 0, green) (100, blue)
                  tri (0, magenta) (V2 0 100, canary) (100, cyan)
        bcurve = mapVertices (\v -> (v,white)) $
                   curve (V2 0 100) (V2 50 (-50)) 100
        circle = mapVertices (\v -> (v,white)) $ arc 50 50 0 (2*pi)

colorPicture :: ColorPicture ()
colorPicture = do
  setStroke [StrokeWidth 3, StrokeFeather 1]
  setGeometry colorGeometry

bezierPicture :: ColorPicture ()
bezierPicture = setGeometry $ beziers $ do
  bez (V2 0   0,   white) (V2 200 0, blue) (V2 200 200, green)
  bez (V2 400 200, white) (V2 400 0, blue) (V2 200 0,   green)

texturePicture :: GLuint -> V2 Int -> TexturePicture ()
texturePicture tex (V2 w h) = do
  setStroke [StrokeWidth 3, StrokeFeather 1]
  setTextures [tex]
  setGeometry $ mapGeometry toUV colorGeometry
    where toUV (V2 x y, _) = (V2 x y, V2 (x/fromIntegral w) (y/fromIntegral h))

outlinedTextPicture :: Font -> ColorPicture ()
outlinedTextPicture font = do
  let outline = B.map (mapRawGeometry (\v -> (v, white))) $
                  stringOutline font 100 128 "Outlined Strings"
  setRawGeometry outline
  setStroke [StrokeWidth 3, StrokeFeather 1]

isQuit :: Event -> Bool
isQuit (Event _ payload) = isKeyQ payload || payload == QuitEvent
  where
    isKeyQ (KeyboardEvent (KeyboardEventData _ _ _ (Keysym _ KeycodeQ _))) = True
    isKeyQ _ = False
--------------------------------------------------------------------------------
-- Main stuff, including actual font outline text and freetype2 text.
--------------------------------------------------------------------------------
main :: IO ()
main = do
  ttfName <- getDataFileName $ "assets" </> "Neuton-Regular.ttf"
  imgName <- getDataFileName $ "assets" </> "tex.jpg"
  font <- loadFontFile ttfName >>= \case
            Left err -> putStrLn err >> exitFailure
            Right f  -> return f

  SDL2Backends glv2v4 glv2v2 <-
    startupSDL2Backends 1000 600 "gelatin-example" True

  Just (sz, tex)   <- loadImage imgName

  (_, colorPicRender)  <- compilePicture glv2v4 colorPicture
  (_, bezPicRender)    <- compilePicture glv2v4 bezierPicture
  (_, texPicRender)    <- compilePicture glv2v2 $ texturePicture tex sz
  -- Font outlines filled with white
  colorTextRender <- coloredString glv2v4 font 100 128 "Colored Strings" $
                       const white
  -- Font outlines filled with texture
  texTextRender   <- texturedString glv2v2 font 100 128 "Textured Strings" tex $
                       \(V2 x y) -> V2 (x/200) ((y+128)/200)
  -- Font outline, outlined in white
  (_, outlineRender)   <- compilePicture glv2v4 $ outlinedTextPicture font
  -- Colored freetype2 text with kerning and newline support
  Just atlas <- allocAtlas ttfName (PixelSize 32 32) asciiChars
  (ft2r,_,_)  <- freetypeRenderer2 glv2v2 atlas white
                   "Hello freetype,\nThanks for everything.\n    - gelatin"

  forever $ do
    threadDelay 1
    events <- getEvents glv2v4
    when (any isQuit events) exitSuccess
    clearWindow glv2v4
    snd colorPicRender  []
    snd texPicRender    [move 0 100]
    snd bezPicRender    [move 400 100]
    snd colorTextRender [move 0 400]
    snd texTextRender   [move 0 500]
    snd outlineRender   [move 0 600]
    snd ft2r            [move 500 50]
    updateWindow glv2v4

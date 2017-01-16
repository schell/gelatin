{-# LANGUAGE LambdaCase #-}
import           Control.Arrow
import           Control.Concurrent         (threadDelay)
import           Control.Monad              (forM_, forever, when)
import           Control.Monad.Trans.Either (runEitherT)
import           Gelatin.SDL2
import           Paths_gelatin_sdl2
import           SDL
import           System.Exit                (exitFailure, exitSuccess)
import           System.FilePath            ((</>))

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

isQuit :: Event -> Bool
isQuit (Event _ payload) = isKeyQ payload || payload == QuitEvent
  where
    isKeyQ (KeyboardEvent (KeyboardEventData _ _ _ (Keysym _ KeycodeQ _))) = True
    isKeyQ _ = False

-- Start up our backend(s) and go!
main :: IO ()
main =
  runEitherT (startupSDL2Backends 920 420 "gelatin-sdl2-example" True) >>= \case
    Left err -> putStrLn err >> exitFailure
    Right (SDL2Backends glv2v4 glv2v2) -> do
      -- Load up a texture. This can be done with either backend, as they both
      -- share the same OpenGL context.
      imgName <- getDataFileName $ "img" </> "lava.png"
      Just (tex, sz) <- allocTexture glv2v2 imgName
      -- Compiler our picture descriptions, sending their geometry to the GPU and
      -- returning a renderable resource and a cleanup action. The result of the
      -- picture computation is discarded.
      (_, colorRender)     <- compilePicture glv2v4 colorPicture
      (_, bezierRenderer)  <- compilePicture glv2v4 bezierPicture
      (_, texRender)       <- compilePicture glv2v2 $ texturePicture tex sz
      -- Forever run the main loop, which polls for SDL events, clear the window,
      -- render our resources at different places with different transforms, and
      -- update the window with the new frame.
      forever $ do
        threadDelay 1
        events <- getEvents glv2v4
        when (any isQuit events) exitSuccess
        clearWindow glv2v4
        let indices = [0..10]
        forM_ indices $ \i -> do
          let txy  = move (100 - 10 * i) (100 - 10 * i)
              a    = alpha $ i/10
              rs   = [txy, a]
          snd colorRender rs
          snd bezierRenderer $ move 400 0 : rs
          snd texRender $ move 0 200 : rs
        updateWindow glv2v4

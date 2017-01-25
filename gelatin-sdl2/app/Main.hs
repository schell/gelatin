{-# LANGUAGE LambdaCase #-}
import           Control.Arrow
import           Control.Concurrent         (threadDelay)
import           Control.Monad              (forM_, forever, when)
import           Control.Monad.Trans.Either (runEitherT)
import           Gelatin.SDL2
import           Paths_gelatin_sdl2
import           SDL                        hiding (rotate)
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

textureCube :: GLuint -> TexturePicture3 ()
textureCube tex = do
  setTextures [tex]
  setGeometry $ triangles $ do
    let lx = -0.5
        rx =  0.5
        by = -0.5
        fy =  0.5
        tz =  0.5
        bz =  0.5
        --  a----b      z
        --  |\   |\     ^
        --  | i----j    |
        --  c-|--d |    \-->x
        --   \|   \|     \
        --    k----l      y
        a = V3 lx by tz
        b = V3 rx by tz
        c = V3 lx by bz
        d = V3 rx by bz
        i = V3 lx fy tz
        j = V3 rx fy tz
        k = V3 lx fy bz
        l = V3 rx fy bz
    -- back
    tri (b, V2 0 0) (a, V2 1 0) (d, V2 0 1)
    tri (a, V2 1 0) (d, V2 0 1) (c, V2 1 1)
    -- bottom
    tri (k, V2 0 0) (l, V2 1 0) (d, V2 1 1)
    tri (k, V2 0 0) (d, V2 1 1) (c, V2 0 1)
    -- front
    tri (i, V2 0 0) (j, V2 1 0) (k, V2 0 1)
    tri (j, V2 1 0) (k, V2 0 1) (l, V2 1 1)
    -- top
    tri (a, V2 0 0) (b, V2 1 0) (i, V2 0 1)
    tri (b, V2 1 0) (i, V2 0 1) (j, V2 1 1)
    -- left
    tri (a, V2 0 0) (i, V2 1 0) (c, V2 0 1)
    tri (i, V2 1 0) (c, V2 0 1) (k, V2 1 1)
    -- right
    tri (b, V2 0 0) (j, V2 1 0) (l, V2 1 1)
    tri (b, V2 0 0) (d, V2 0 1) (l, V2 1 1)


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
    Right (SDL2Backends glv2v4 glv2v2 glv3v4 glv3v2) -> do
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
      (_, cubeRender)      <- compilePicture glv3v2 $ textureCube tex

      --glEnable GL_DEPTH_TEST
      -- Forever run the main loop, which polls for SDL events, clear the window,
      -- render our resources at different places with different transforms, and
      -- update the window with the new frame.
      forever $ do
        threadDelay 1
        events <- getEvents glv2v4
        when (any isQuit events) exitSuccess
        clearWindow glv2v4
        -- draw our 2d scene
        let indices = [0..10]
        forM_ indices $ \i -> do
          let txy  = move2 (100 - 10 * i) (100 - 10 * i)
              a    = alpha $ i/10
              rs   = [txy, a]
          snd colorRender rs
          snd bezierRenderer $ move2 400 0 : rs
          snd texRender $ move2 0 200 : rs
        -- draw our 3d scene
        V2 w h <- (fromIntegral <$>) <$> getFrameBufferSize glv3v2
        let pj = perspective (45 * pi / 180) (w / h) 0.1 10
                   !*! lookAt (V3 0 4 2) (V3 0 0 (-4)) (V3 0 1 0)
        updateWindowProjection glv3v2 pj
        clearErrors "win proj"
        snd cubeRender [move3 0 0 (-4), rotate $ axisAngle (V3 0 0 1) (pi/4)]
        updateWindow glv2v4

{-# LANGUAGE LambdaCase #-}
module Main where

import           Control.Arrow                  (first)
import           Control.Concurrent             (threadDelay)
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Either
import           GHCJS.DOM.JSFFI.Generated.Node
import           GHCJS.DOM.Types

import           Gelatin.WebGL

import           ImageData

frag :: String
frag = "https://raw.githubusercontent.com/schell/gelatin/master/gelatin-shaders/shaders/simple2dwebgl.frag"

vert :: String
vert = "https://raw.githubusercontent.com/schell/gelatin/master/gelatin-shaders/shaders/simple2dwebgl.vert"

colorGeometry :: MonadIO m => GeometryT (V2 Float, V4 Float) m ()
colorGeometry = do
  triangles $ tri (0, red) (V2 100 0, red) (100, red)
  return ()
  --triangles tris
  --beziers $ mapVertices (first (+ V2 100 0)) tris
  --line $ mapVertices (first (+V2 200 0)) tris
  --line $ mapVertices (first (+V2 300 0)) bcurve
  --line $ mapVertices (first ((+V2 300 100) . (*V2 1 (-1)))) bcurve
  --line $ mapVertices (first (+V2 350 50)) circle
  --where tris = do tri (0, red) (V2 100 0, green) (100, blue)
  --                tri (0, magenta) (V2 0 100, canary) (100, cyan)
  --      bcurve = mapVertices (\v -> (v,white)) $
  --                 curve (V2 0 100) (V2 50 (-50)) 100
  --      circle = mapVertices (\v -> (v,white)) $ arc 50 50 0 (2*pi)

colorPicture :: MonadIO m => ColorPictureT m ()
colorPicture = do
  setStroke [StrokeWidth 3, StrokeFeather 1]
  setGeometry colorGeometry

bezierPicture :: MonadIO m => ColorPictureT m ()
bezierPicture = setGeometry $ beziers $ do
  bez (V2 0   0,   white) (V2 200 0, blue) (V2 200 200, green)
  bez (V2 400 200, white) (V2 400 0, blue) (V2 200 0,   green)

texturePicture :: MonadIO m => WebGLTexture -> V2 Int -> TexturePictureT m ()
texturePicture tex (V2 w h) = do
  setTextures [tex]
  setGeometry $ triangles $ tri (toUV tl) (toUV tr) (toUV br)
    where toUV :: V2 Float -> (V2 Float, V2 Float)
          toUV (V2 x y) = (V2 x y, V2 (x/fromIntegral w) (y/fromIntegral h))
          tl = 0
          tr = fromIntegral <$> V2 w 0
          br = fromIntegral <$> V2 w h

app :: MonadIO m => EitherT String m ()
app = do
  body <- webBody
  be   <- startupWebGLBackends 600 400 vert frag
  void $ liftIO $ appendChild body $ Just $ backendCanvas be
  liftIO $ backendOpGetWindowSize (backendOps $ backendV2V4 be) >>= print

  let v2v2 = backendV2V2 be
      v2v4 = backendV2V4 be

  --loadImage imageData >>= void . liftIO . appendChild body . Just
  (_, colorPicRender)  <- compilePictureT v2v4 colorPicture
  (_, bezPicRender)    <- compilePictureT v2v4 bezierPicture

  liftIO (allocTexture (backendV2V2 be) imageData) >>= liftIO . \case
    Nothing -> putStrLn "Could not alloc texture."
    Just (tex, sz@(V2 w h)) -> do
      putStrLn $ "Texture is " ++ show (w, h) ++ "px"
      (_, texPicRender) <- compilePictureT v2v2 $ texturePicture tex sz
      clearWindow v2v4
      snd bezPicRender   [move 0 150]
      snd texPicRender   []
      snd colorPicRender []

main :: IO ()
main = runEitherT app >>= \case
  Left str -> putStrLn str
  Right _ -> putStrLn "Done."

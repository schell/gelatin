module Examples.PolylineTest where

import GHC.Float
import Linear
import System.Exit
import Gelatin.Core.Render
import Gelatin.Core.Color
import Graphics.UI.GLFW
import Graphics.GL.Core33
import Control.Concurrent (threadDelay)
import Data.IORef
import Data.Bits
import Data.Monoid

polylineTest :: Window -> GeomRenderSource -> BezRenderSource -> IO ()
polylineTest win grs _ = do
    ref  <- newIORef ((0,0), False)
    pnts <- newIORef []
    rRef <- newIORef (mempty :: Renderer)

    setMouseButtonCallback win $ Just $ \_ _ mbs _ ->
        if mbs == MouseButtonState'Released
        then modifyIORef ref $ \(p, _) -> (p, True)
        else return ()

    setCursorPosCallback win $ Just $ \_ x y ->
        modifyIORef ref $ \(_, b) -> ((x,y), b)

    box  <- colorRenderer win grs GL_TRIANGLES [V2 0 0, V2 100 0, V2 100 50] $ replicate 3 red
    let t = Triangle (V2 0 0) (V2 100 0) (V2 100 50)
        -- A canary to white gradient fill
        gradFill  = FillColor $ \(V2 _ y) -> V4 1 1 (y/200) 1
        -- A magenta fill
        magFill = FillColor $ const magenta
        -- A cyan fill
        cynFill  = FillColor $ const cyan
    tris <- filledTriangleRenderer win grs [ t
                                           , (V2 0 50 +)  <$> t
                                           , (V2 0 100 +) <$> t
                                           , (V2 0 150 +) <$> t
                                           , (V2 0 200 +) <$> t
                                           ] gradFill

    glEnable GL_BLEND
    glBlendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA

    let loop = do -- Update input events.
                  --es <- lift $ readIORef ref
                  --lift $ writeIORef ref []
                  (fbw,fbh) <- getFramebufferSize win
                  glViewport 0 0 (fromIntegral fbw) (fromIntegral fbh)
                  glClear $ GL_COLOR_BUFFER_BIT .|. GL_DEPTH_BUFFER_BIT

                  poly <- do ((x,y), b) <- readIORef ref
                             oldR <- readIORef rRef
                             if b
                             then do
                               modifyIORef pnts (++ map (fmap double2Float) [V2 x y])
                               modifyIORef ref (const ((x,y), False))
                               ps <- readIORef pnts
                               print ps
                               let ln = polyline ps 5
                                   ol = polyOutline ps 5
                                   ln' = polyline ol 0.5
                               r' <- filledTriangleRenderer win grs ln magFill
                               r'' <- filledTriangleRenderer win grs ln' cynFill
                               rCleanup oldR
                               let r = r' <> r''
                               modifyIORef rRef $ const r
                               return r
                             else return oldR

                  mapM_ (uncurry rRender) [ (box, translate 25 25 mempty)
                                          , (tris, mempty)
                                          , (poly, mempty)]

                  pollEvents
                  swapBuffers win
                  shouldClose <- windowShouldClose win
                  if shouldClose
                  then exitSuccess
                  else threadDelay 100
                  loop
    loop

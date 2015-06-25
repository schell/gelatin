module Examples.PolylineWinding where

import GHC.Float
import Linear
import System.Exit
import Gelatin.Core.Render
import Gelatin.Core.Color
import Gelatin.Core.Triangulation.Common
import Graphics.UI.GLFW
import Graphics.GL.Core33
import Control.Concurrent (threadDelay)
import Control.Monad
import Data.IORef
import Data.Bits
import Data.Monoid

polylineWinding :: Window -> GeomRenderSource -> BezRenderSource -> IO ()
polylineWinding win grs _ = do
    ref  <- newIORef (0,0)
    rRef <- newIORef (mempty :: Renderer)

    setCursorPosCallback win $ Just $ \_ x y ->
        modifyIORef ref $ const (x,y)

    glEnable GL_BLEND
    glBlendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA

    let loop = do (fbw,fbh) <- getFramebufferSize win
                  glViewport 0 0 (fromIntegral fbw) (fromIntegral fbh)
                  glClear $ GL_COLOR_BUFFER_BIT .|. GL_DEPTH_BUFFER_BIT

                  poly <- do (x,y) <- readIORef ref
                             oldR  <- readIORef rRef
                             rCleanup oldR
                             let a = V2 300 300
                                 b = V2 400 300
                                 c = fmap double2Float $ V2 x y
                                 n = angleBetween (signorm $ b - a) (signorm $ c - b)
                                 t = triangleArea a b c
                                 ln = polyline EndCapRound LineJoinBevel 1 [a, b, c]
                                 ftr = filledTriangleRenderer win grs
                             r  <- ftr ln $ solid white
                             modifyIORef rRef $ const r
                             print (n, t)
                             return r

                  mapM_ (uncurry rRender) [ (poly, mempty)
                                          ]

                  pollEvents
                  swapBuffers win
                  shouldClose <- windowShouldClose win
                  if shouldClose
                  then exitSuccess
                  else threadDelay 100
                  loop
    loop

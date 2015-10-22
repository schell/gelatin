{-# LANGUAGE OverloadedStrings #-}
module Examples.ExpandedPolylines where

import GHC.Float
import Linear
import System.Exit
import Gelatin.Core.Rendering
import Gelatin.Core.Color
import Gelatin.Core.Triangulation.Common
import Graphics.UI.GLFW
import Graphics.GL.Core33
import Graphics.Text.TrueType
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async
import Control.Monad
import Control.Lens
import Data.IORef
import Data.Bits

expandedPolylines :: Window -> SumShader -> IO ()
expandedPolylines win shaders = do
    ref <- newIORef (0,0)
    rRef <- newIORef []

    setCursorPosCallback win $ Just $ \_ x y ->
        modifyIORef ref $ const (x,y)

    glEnable GL_BLEND
    glBlendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA

    let sh = shaders^.shPolyline
        vs = [V2 100 100, V2 150 100, V2 150 150, V2 100 150, V2 50 50, V2 80 60]
        ss = [V2 150 300, V2 250 300, V2 200 400, V2 100 400, V2 150 300]
        cs = cycle [V4 1 1 1 1, V4 1 1 0 1, V4 0 1 1 1, V4 1 0 1 1, V4 1 1 0 1]
        loop = do (fbw,fbh) <- getFramebufferSize win
                  glViewport 0 0 (fromIntegral fbw) (fromIntegral fbh)
                  glClear $ GL_COLOR_BUFFER_BIT .|. GL_DEPTH_BUFFER_BIT

                  rs <- do (x,y) <- readIORef ref
                           -- Clean up last frame's rendering
                           rs <- readIORef rRef
                           mapM_ (\(Rendering _ c) -> c) rs
                           rs' <- mapM (\i -> expandedPolylineRendering win sh i vs cs)
                                       [0.5, 1,3,5,9]

                           r <- expandedPolylineRendering win sh 50 ss cs
                           writeIORef rRef $ r:rs'
                           return $ r:rs'

                  mapM_ (\(Rendering f _, i) -> f $ Transform (20*^V2 0 i) 1 0) $
                      zip rs [0..]
                  let Rendering f _ = head rs
                  f $ Transform (V2 (-100) (-300)) (V2 2 2) 0

                  pollEvents
                  swapBuffers win
                  shouldClose <- windowShouldClose win
                  if shouldClose
                  then exitSuccess
                  else threadDelay 100
                  loop
    loop

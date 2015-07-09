{-# LANGUAGE OverloadedStrings #-}
module Examples.Masking where

import Linear
import System.Exit
import Gelatin.Core.Render
import Gelatin.Core.Color
import Graphics.UI.GLFW
import Graphics.GL.Core33
import Control.Concurrent (threadDelay)
import Data.Bits

masking :: Window -> GeomRenderSource -> BezRenderSource -> IO ()
masking win grs _ = do
    mrs <- loadMaskRenderSource

    let vs = [V2 0 0, V2 100 0, V2 100 100, V2 0 100]
        cs = replicate 4 red
        vs' = map (100 +) vs
        cs' = replicate 4 blue
        ms = [V2 0 0, V2 200 0, V2 200 200, V2 0 200]
        mcs = [white `alpha` 0, white `alpha` 0, white, white]
    rbox <- colorRenderer win grs GL_TRIANGLE_FAN vs cs
    bbox <- colorRenderer win grs GL_TRIANGLE_FAN vs' cs'
    wbox <- colorRenderer win grs GL_TRIANGLE_FAN ms mcs
    let r1 = mapM_ (\(Renderer f _) -> f mempty) [rbox, bbox]
        r2 = (\(Renderer f _) -> f mempty) wbox
    mask <- alphaMask win mrs r1 r2

    let loop = do (fbw,fbh) <- getFramebufferSize win

                  glEnable GL_BLEND
                  glBlendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA

                  glViewport 0 0 (fromIntegral fbw) (fromIntegral fbh)
                  glClear $ GL_COLOR_BUFFER_BIT .|. GL_DEPTH_BUFFER_BIT

                  let f (Renderer r _) = r
                  mapM_ (`f` mempty) [rbox, bbox, wbox]
                  f mask $ translate 200 0 mempty

                  pollEvents
                  swapBuffers win
                  shouldClose <- windowShouldClose win
                  if shouldClose
                  then exitSuccess
                  else threadDelay 100
                  loop
    loop

{-# LANGUAGE OverloadedStrings #-}
module Examples.ClipTexture where

import Linear
import System.Exit
import Gelatin.Core.Rendering
import Gelatin.Core.Color
import Graphics.UI.GLFW
import Graphics.GL.Core33
import Control.Concurrent (threadDelay)
import Data.Bits

clippingTexture :: Window -> SumShader -> IO ()
clippingTexture win ss = do
    let grs = _shGeometry ss
        brs = _shBezier ss
    glEnable GL_BLEND
    glBlendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA

    let vs = [ Bezier GT (V2 0 0) (V2 100 0) (V2 100 100)
             , Bezier GT (V2 0 0) (V2 0 100) (V2 100 100)
             ]
        cs = [Triangle red blue green, Triangle red blue green]
        bxvs = [V2 0 0, V2 800 0, V2 800 600, V2 0 600]
        bxcs = [V4 1 1 0 1, V4 1 0 0 1, V4 0 0 0 1, V4 0 0 1 1]
    boxr <- colorRendering win grs GL_TRIANGLE_FAN bxvs bxcs
    bezr <- colorBezRendering win brs vs cs
    tex  <- toTexture win $ do runRendering mempty boxr
                               runRendering mempty bezr
    clip1 <- clipTexture tex (V2 0 600, V2 50 550)
    clip2 <- clipTexture tex (V2 50 550, V2 100 500)

    let tvs = [V2 0 0, V2 50 0, V2 50 50, V2 0 50]
    clpr <- textureRendering win grs GL_TRIANGLE_FAN tvs $ map (/50) tvs

    let loop = do (fbw,fbh) <- getFramebufferSize win
                  glViewport 0 0 (fromIntegral fbw) (fromIntegral fbh)
                  glClear $ GL_COLOR_BUFFER_BIT .|. GL_DEPTH_BUFFER_BIT

                  runRendering mempty bezr

                  glActiveTexture GL_TEXTURE0
                  glBindTexture GL_TEXTURE_2D clip1
                  runRendering (translate (100 :: Float) 0 mempty) clpr
                  glActiveTexture GL_TEXTURE0
                  glBindTexture GL_TEXTURE_2D clip2
                  runRendering (translate (150 :: Float) 50 mempty) clpr

                  pollEvents
                  swapBuffers win
                  shouldClose <- windowShouldClose win
                  if shouldClose
                  then exitSuccess
                  else threadDelay 100
                  loop
    loop

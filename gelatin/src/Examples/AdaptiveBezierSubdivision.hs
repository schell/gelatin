module Examples.AdaptiveBezierSubdivision where

import GHC.Float
import Linear
import System.Exit
import Gelatin.Core.Rendering
import Gelatin.Core.Rendering.Bezier
import Gelatin.Core.Color
import Gelatin.Core.Triangulation.Common
import Graphics.UI.GLFW hiding (init)
import Graphics.GL.Core33
import Graphics.Text.TrueType
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async
import Control.Monad
import Control.Lens
import Data.IORef
import Data.Bits
import Data.Monoid

adaptiveBezierSubdivision :: Window -> SumShader -> IO ()
adaptiveBezierSubdivision win shaders = do
    let polysh = shaders^.shPolyline
        projsh = shaders^.shProjectedPolyline
        bezsh = shaders^.shBezier
        bz = bez 0 (V2 200 25) (V2 0 50)
        testPoints = [0, V2 100 100, V2 150 100, 0]
        vs = subdivideAdaptive 0.1 0 bz
        vs1 = subdivideAdaptive 1 0 bz
        vs10 = subdivideAdaptive 10 0 bz
        vs100 = subdivideAdaptive 100 0 bz
        cmy = cycle [V4 0 1 1 1, V4 1 0 1 1, V4 1 1 0 1]
        cs = repeat $ V4 1 1 1 1
        ts = repeat $ Triangle 1 1 1
        lw = 5

    glEnable GL_BLEND
    glBlendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA

    x <- projectedPolylineRendering win projsh 5 1 (LineCapTriIn,LineCapTriOut) testPoints cmy
    y <- projectedPolylineRendering win projsh 10 1 (LineCapTriIn,LineCapTriOut) (init testPoints) cmy
    z <- projectedPolylineRendering win projsh 10 10 (LineCapTriIn,LineCapTriOut) (init testPoints) cmy
    x' <- expandedPolylineRendering win polysh 5 testPoints cmy
    y' <- expandedPolylineRendering win polysh 10 (init testPoints) cmy

    b <- colorBezRendering win bezsh [bz] ts

    p <- expandedPolylineRendering win polysh lw vs cs
    p1 <- expandedPolylineRendering win polysh lw vs1 cs
    p10 <- expandedPolylineRendering win polysh lw vs10 cs
    p100 <- expandedPolylineRendering win polysh lw vs100 cs

    p' <- projectedPolylineRendering win projsh lw 1 (LineCapRound,LineCapRound) vs cs
    p1' <- projectedPolylineRendering win projsh lw 1 (LineCapRound,LineCapRound) vs1 cs
    p10' <- projectedPolylineRendering win projsh lw 1 (LineCapRound,LineCapRound) vs10 cs
    p100' <- projectedPolylineRendering win projsh lw 1 (LineCapRound,LineCapRound) vs100 cs

    let render (Rendering f _) = f
        loop = do
          (fbw,fbh) <- getFramebufferSize win
          glViewport 0 0 (fromIntegral fbw) (fromIntegral fbh)
          glClear $ GL_COLOR_BUFFER_BIT .|. GL_DEPTH_BUFFER_BIT

          render x $ Transform 50 1 0
          render y $ Transform (V2 50 100) 1 0
          render z $ Transform (V2 70 90) 1 0
          render x' $ Transform (V2 180 50) 1 0
          render y' $ Transform (V2 180 100) 1 0

          let bt = Transform (V2 50 200) 1 0
              ty = Transform (V2 0 70) 1 0
              tx = Transform (V2 130 0) 1 0

          render b bt

          render p' $ bt <> ty
          render p1' $ bt <> ty <> ty
          render p10' $ bt <> ty <> ty <> ty
          render p100' $ bt <> ty <> ty <> ty <> ty

          render p $ bt <> tx <> ty
          render p1 $ bt <> tx <> ty  <> ty
          render p10 $ bt <> tx <> ty  <> ty <> ty
          render p100 $ bt <> tx <> ty  <> ty <> ty <> ty

          pollEvents
          swapBuffers win
          shouldClose <- windowShouldClose win
          if shouldClose
          then exitSuccess
          else threadDelay 100
          loop

    loop

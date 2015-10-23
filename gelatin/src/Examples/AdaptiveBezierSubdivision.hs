module Examples.AdaptiveBezierSubdivision where

import GHC.Float
import Linear
import System.Exit
import Gelatin.Core.Rendering
import Gelatin.Core.Rendering.Bezier
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

adaptiveBezierSubdivision :: Window -> SumShader -> IO ()
adaptiveBezierSubdivision win shaders = do
    let polysh = shaders^.shPolyline
        bezsh = shaders^.shBezier
        bz = bez 0 (V2 200 25) (V2 0 50)
        vs = subdivideAdaptive 0.1 0 bz
        vs1 = subdivideAdaptive 1 0 bz
        vs10 = subdivideAdaptive 10 0 bz
        vs100 = subdivideAdaptive 100 0 bz
        cs = cycle [V4 1 1 1 1]
        ts = cycle [Triangle 1 1 1]
        lw = 5

    glEnable GL_BLEND
    glBlendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA

    p <- expandedPolylineRendering win polysh lw vs cs
    p1 <- expandedPolylineRendering win polysh lw vs1 cs
    p10 <- expandedPolylineRendering win polysh lw vs10 cs
    p100 <- expandedPolylineRendering win polysh lw vs100 cs
    b <- colorBezRendering win bezsh [bz] ts

    let [rp,rp1,rp10,rp100] = map (\(Rendering f _) -> f) [p,p1,p10,p100]
        Rendering rb _ = b
        loop = do (fbw,fbh) <- getFramebufferSize win
                  glViewport 0 0 (fromIntegral fbw) (fromIntegral fbh)
                  glClear $ GL_COLOR_BUFFER_BIT .|. GL_DEPTH_BUFFER_BIT

                  rb $ Transform 10 1 0
                  rp $ Transform (V2 110 10) 1 0
                  rp1 $ Transform (V2 110 80) 1 0
                  rp10 $ Transform (V2 110 150) 1 0
                  rp100 $ Transform (V2 110 220) 1 0

                  pollEvents
                  swapBuffers win
                  shouldClose <- windowShouldClose win
                  if shouldClose
                  then exitSuccess
                  else threadDelay 100
                  loop

    loop

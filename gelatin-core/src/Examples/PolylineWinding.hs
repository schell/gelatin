{-# LANGUAGE OverloadedStrings #-}
module Examples.PolylineWinding where

import GHC.Float
import Linear
import System.Exit
import Gelatin.Core.Render
import Gelatin.Core.Color
import Gelatin.Core.Triangulation.Common
import Graphics.UI.GLFW
import Graphics.GL.Core33
import Graphics.Text.TrueType
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async
import Control.Monad
import Data.IORef
import Data.Bits

polylineWinding :: Window -> GeomRenderSource -> BezRenderSource -> IO ()
polylineWinding win grs brs = do
    afc  <- compileFontCache
    fc   <- wait afc
    Just arial <- withFont fc (FontDescriptor "Arial" $ FontStyle False False)
                              return

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
                             Renderer oldf oldc <- readIORef rRef
                             oldc
                             let a = V2 100 100
                                 b = V2 200 200
                                 c = fmap double2Float $ V2 x y
                                 n = angleBetween (signorm $ b - a) (signorm $ c - b)
                                 t = triangleArea a b c
                                 ps = [a, b, c]
                                 ln = polyline EndCapButt LineJoinMiter 5 ps
                                 ln' = polyline EndCapButt LineJoinBevel 5 $ map (+ V2 0 50) ps
                                 ftr = filledTriangleRenderer win grs
                             r  <- ftr ln $ solid white
                             r' <- ftr ln' $ solid grey
                             modifyIORef rRef $ const r
                             let ss = ["Angle: " ++ show n
                                      ,"Area: " ++ show t
                                      ]
                             txs <- forM (zip ss [1..]) $ \(s, i) -> do
                                 let fstr = FontString arial 16 (0,i * 16) s
                                 colorFontRenderer win grs brs fstr $ const white
                             return $ mconcat $ r:r':txs

                  mapM_ (\(Renderer f _, t) -> f t) [ (poly, mempty) ]

                  pollEvents
                  swapBuffers win
                  shouldClose <- windowShouldClose win
                  if shouldClose
                  then exitSuccess
                  else threadDelay 100
                  loop
    loop

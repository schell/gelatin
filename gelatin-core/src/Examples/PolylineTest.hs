module Examples.PolylineTest where

import GHC.Float
import Linear
import System.Exit
import Gelatin.Core.Rendering
import Gelatin.Core.Color
import Graphics.UI.GLFW
import Graphics.GL.Core33
import Control.Concurrent (threadDelay)
import Control.Monad
import Data.IORef
import Data.Bits
import Data.Monoid

polylineTest :: Window -> GeomRenderSource -> BezRenderSource -> IO ()
polylineTest win grs _ = do
    ref  <- newIORef ((0,0), False)
    pnts <- newIORef []
    rRef <- newIORef (mempty :: Rendering)

    setMouseButtonCallback win $ Just $ \_ _ mbs _ ->
        if mbs == MouseButtonState'Released
        then modifyIORef ref $ \(p, _) -> (p, True)
        else return ()

    setCursorPosCallback win $ Just $ \_ x y ->
        modifyIORef ref $ \(_, b) -> ((x,y), b)


    box   <- colorRendering win grs GL_TRIANGLES [V2 0 0, V2 100 0, V2 100 50] $
                                                replicate 3 red
    let spklns = [ [ V2 0 0
                   , V2 300 50
                   , V2 0 100
                   , V2 300 150
                   , V2 0 200
                   , V2 300 250
                   , V2 0 300
                   , V2 300 350
                   , V2 0 400
                   , V2 20 410
                   ]
                 ]
    lns <- forM spklns $ \ln -> do
        spike <- filledTriangleRendering win grs (polyline EndCapRound LineJoinBevel 10 ln) $ FillColor $ const $ alpha red 0.5
        inner <- filledTriangleRendering win grs (polyline EndCapButt LineJoinMiter 0.5 ln) $ FillColor $ const white
        return $ spike <> inner
    let lns' = foldl (<>) mempty lns

    let t = Triangle (V2 0 0) (V2 100 0) (V2 100 50)
        -- A canary to white gradient fill
        gradFill  = FillColor $ \(V2 _ y) -> V4 1 1 (y/200) 1
        -- A magenta fill
        magFill = FillColor $ const $ alpha magenta 0.5
        -- A cyan fill
    tris <- filledTriangleRendering win grs [ t
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
                             Rendering oldf oldc <- readIORef rRef
                             if b
                             then do
                               modifyIORef pnts (++ map (fmap double2Float) [V2 x y])
                               modifyIORef ref (const ((x,y), False))
                               ps <- readIORef pnts

                               let ln = polyline EndCapButt LineJoinMiter 10 ps
                                   --ol = polyline EndCapButt LineJoinMiter 1 $
                                   --         polygonExpand 5 $
                                   --         outlinePolyline EndCapButt LineJoinMiter 10 ps
                                   --ln' = polyline EndCapButt LineJoinBevel 1 ps
                                   --js  = joints EndCapRound LineJoinBevel 10 ps
                                   --(ns,xs) = unzip $ map (\j -> (entryLine j, exitLine j)) js
                                   --f (a,b) = polyline EndCapButt LineJoinMiter 1 [a,b]
                                   --ns' = concatMap f ns
                                   --xs' = concatMap f xs
                                   ftr = filledTriangleRendering win grs

                               r'  <- ftr ln magFill
                               --r'' <- ftr ln' cynFill
                               --o   <- ftr ol $ solid $ alpha green 0.5
                               --n   <- ftr ns' $ solid white
                               --x   <- ftr xs' $ solid yellow
                               oldc
                               let r = r' {-<> r'' <> n <> x <> o-}
                               modifyIORef rRef $ const r
                               return r
                             else return $ Rendering oldf oldc

                  let f (Rendering r _) = r
                  mapM_ (uncurry f) [ (box, translate 25 25 mempty)
                                    , (tris, mempty)
                                    , (poly, mempty)
                                    , (lns', translate 100 100 mempty)]

                  pollEvents
                  swapBuffers win
                  shouldClose <- windowShouldClose win
                  if shouldClose
                  then exitSuccess
                  else threadDelay 100
                  loop
    loop

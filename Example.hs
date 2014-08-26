module Main where

import Gelatin
import Gelatin.Shaders.Core
import Linear
import Control.Monad
import Control.Concurrent
import Data.IORef
import Data.Monoid
import Graphics.Rendering.OpenGL hiding (position, color)
import Graphics.GLUtil
import Graphics.GLUtil.Camera3D
import Graphics.VinylGL
import Data.Vinyl
import System.Exit
import qualified Data.Map as M

scene :: Drawing Double ()
scene = do
    -- Draw a textured rectangle for the background.
    withTexture (Relative "img/quantum-foam.jpg") $
        textris (rectangle (V2 0 0) (V2 640 480)) (rectangle (V2 0 0) (V2 1 1))

    let r = rectangle (V2 0 0) (V2 100 100)
    -- Draw a yellow square with width and height = 100px
    fill r yellow
    -- Draw a gradient box.
    withPosition (V2 100 0) $
        gradient r (take 6 $ cycle [red, green, blue])
    -- Transalte by (10,10) then scale to 10% and rotate pi/4, then draw
    -- some more squares.
    withTransform (tfrm (V2 10 10) (V2 0.1 0.1) $ rotateZ $ pi/4) $ do
        fill r red
        withPosition (V2 100 0) $
            fill r pink
    renderWith $ \_ _ rndr -> do
        let pj = projectionMatrix (pi/4) 1 0.1 10
            mv = transform2Mat $ Transform (V3 0 0 (-5)) (V3 1 1 1) $ rotateX (pi/8)
            vs = [ V3 (-0.5) ( 0.5) ( 0.5)
                 , V3 ( 0.5) ( 0.5) ( 0.5)
                 , V3 (-0.5) (-0.5) ( 0.5)
                 , V3 ( 0.5) (-0.5) ( 0.5)

                 , V3 (-0.5) ( 0.5) (-0.5)
                 , V3 ( 0.5) ( 0.5) (-0.5)
                 , V3 (-0.5) (-0.5) (-0.5)
                 , V3 ( 0.5) (-0.5) (-0.5)
                 ]

            cs = map ((color =:) . up . fmap (+0.5)) vs
            es = [ 0, 2, 3 -- front
                 , 0, 1, 3
                 , 4, 0, 1 -- top
                 , 4, 5, 1
                 , 4, 6, 7 -- bock
                 , 4, 5, 7
                 , 6, 2, 3 -- bottom
                 , 6, 7, 3
                 , 0, 2, 6 -- left
                 , 0, 4, 6
                 , 1, 3, 7 -- right
                 , 1, 5, 7
                 ]
            up (V3 x y z) = V4 x y z 1
            s  = colorShader rndr
        vbo <- bufferVertices $ zipWith (<+>) (map (position =:) vs) cs
        ebo <- bufferIndices es
        let draw = do depthFunc $= Just Less
                      currentProgram $= (Just $ program s)
                      setUniforms s (projection =: pj <+>
                                     modelview =: mv)
                      bindVertices vbo
                      enableVertices' s vbo
                      bindBuffer ElementArrayBuffer $= Just ebo
                      drawIndexedTris $ floor $ (fromIntegral $ length es) / 3
                      bindBuffer ElementArrayBuffer $= Nothing
                      depthFunc $= Nothing
            clean = do deleteVertices vbo
                       deleteObjectName ebo
        return (draw, clean)

    --withPosition (V2 0 100) $


main :: IO ()
main = do
    putStrLn "Welcome to Gelatin!"
    putStrLn "With Gelatin you can easily render drawings to strings:\n"
    putStrLn $ showDrawing scene

    putStrLn "...or you can render to an OpenGL window..."

    wref <- initWindow (V2 0 0) (V2 600 600) "Gelatin"
    scs <- simpleColorShader
    sts <- simpleTextureShader
    (_, win)    <- readIORef wref
    (fbw, fbh)  <- getFramebufferSize win
    let proj = orthoMatrix 0 600 0 600 0 1
        rndr = Renderer scs sts M.empty
        fbsize = Size (fromIntegral fbw) (fromIntegral fbh)
    (draw, cleanup) <- renderDrawing fbsize proj eye4 rndr scene

    forever $ do
        pollEvents
        (_, window) <- readIORef wref
        writeIORef wref ([], window)
        makeContextCurrent $ Just window

        draw

        swapBuffers window
        shouldClose <- windowShouldClose window
        when shouldClose $ cleanup >> exitSuccess

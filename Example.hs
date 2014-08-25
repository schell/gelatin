module Main where

import Gelatin
import Linear
import Control.Monad
import Control.Concurrent
import Graphics.Rendering.OpenGL
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
    --withPosition (V2 0 100) $


main :: IO ()
main = do
    putStrLn "Welcome to Gelatin!"
    putStrLn "With Gelatin you can easily render drawings to strings:\n"
    putStrLn $ showDrawing scene

    putStrLn "...or you can render to an OpenGL window..."

    wvar <- initWindow (V2 0 0) (V2 600 600) "Gelatin"
    scs <- simpleColorShader
    sts <- simpleTextureShader
    (_, win) <- readMVar wvar
    (fbw, fbh)  <- getFramebufferSize win
    let proj = orthoMatrix 0 600 0 600 0 1
        rndr = Renderer scs sts M.empty
        fbsize = Size (fromIntegral fbw) (fromIntegral fbh)
    (draw, cleanup) <- renderDrawing fbsize proj eye4 rndr scene

    forever $ do
        pollEvents
        (_, window) <- takeMVar wvar
        putMVar wvar ([], window)
        makeContextCurrent $ Just window

        draw

        swapBuffers window
        shouldClose <- windowShouldClose window
        when shouldClose $ cleanup >> exitSuccess

module Main where

import Linear
import Gelatin
import Graphics.Rendering.OpenGL hiding (position, color, drawElements)
import Control.Monad
import Data.IORef
import Graphics.GLUtil hiding (setUniform)
import Graphics.GLUtil.Camera3D
import System.Exit

cube :: [V3 GLfloat]
cube = [ V3 (-0.5) ( 0.5) ( 0.5)
       , V3 ( 0.5) ( 0.5) ( 0.5)
       , V3 (-0.5) (-0.5) ( 0.5)
       , V3 ( 0.5) (-0.5) ( 0.5)

       , V3 (-0.5) ( 0.5) (-0.5)
       , V3 ( 0.5) ( 0.5) (-0.5)
       , V3 (-0.5) (-0.5) (-0.5)
       , V3 ( 0.5) (-0.5) (-0.5)
       ]

cubeIndices :: [Word32]
cubeIndices = [ 0, 2, 3 -- front
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

--background :: Drawing () ()
--background = do
--    -- Draw a textured rectangle for the background.
--    withTexture (Relative "img/quantum-foam.jpg") $
--        textris (rectangle (V2 0 0) (V2 640 480)) (rectangle (V2 0 0) (V2 1 1))
--
--    let r = rectangle (V2 0 0) (V2 100 100)
--    -- Draw a yellow square with width and height = 100px
--    fill r yellow
--    -- Draw a gradient box.
--    withPosition (V2 100 0) $
--        gradient r (take 6 $ cycle [red, green, blue])
--    -- Transalte by (10,10) then scale to 10% and rotate pi/4, then draw
--    -- some more squares.
--    withTransform (tfrm (V2 10 10) (V2 0.1 0.1) $ rotateZ $ pi/4) $ do
--        fill r red
--        withPosition (V2 100 0) $
--            fill r pink

--renderCube :: ShaderProgram -> IO ()
--renderCube shader = do
--    let vs = cube
--        es = cubeIndices
--        cs = map ((color =:) . up . fmap (+0.5)) vs
--        up (V3 x y z) = V4 x y z 1
--        s  = colorShader rndr
--    vbo <- bufferVertices $ zipWith (<+>) (map (position =:) vs) cs
--    ebo <- bufferIndices es
--
--    depthFunc $= Just Less
--    currentProgram $= (Just $ program s)
--    bindVertices vbo
--    enableVertices' s vbo
--    bindBuffer ElementArrayBuffer $= Just ebo
--    drawIndexedTris $ floor $ (fromIntegral $ length es) / 3
--    bindBuffer ElementArrayBuffer $= Nothing
--    depthFunc $= Nothing

colorCube :: ShaderProgram -> Rendering ()
colorCube shader = do
    usingDepthFunc Less $ usingShader shader $ do
        setUniform projection $ projectionMatrix (pi/4) 1 0.1 10
        setUniform modelview t
        setVertices $ do
            addVertexComponent position cube
            addVertexComponent color $ map (up . fmap (+0.5)) cube
        withIndices cubeIndices $ drawElements (12*3) Triangles

    where up (V3 x y z) = V4 x y z 1
          t = transform (V3 0 0 (-5)) (V3 1 1 1) $ rotateX (pi/8)

--scene :: Drawing () ()
--scene = background >> colorCube

main :: IO ()
main = do
    --putStrLn "Welcome to Gelatin!"
    --putStrLn "With Gelatin you can easily render drawings to strings:\n"
    --putStrLn $ showDrawing scene

    --putStrLn "...or you can render to an OpenGL window..."

    wref          <- initWindow (V2 0 0) (V2 600 600) "Gelatin"
    scs           <- simpleColorShader
    --sts           <- simpleTextureShader

    forever $ do
        pollEvents

        (_, window) <- readIORef wref
        writeIORef wref ([], window)

        makeContextCurrent $ Just window
        performRendering $ colorCube scs

        swapBuffers window
        shouldClose <- windowShouldClose window
        when shouldClose exitSuccess

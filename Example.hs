module Main where

import Linear hiding (rotate)
import Gelatin
import Gelatin.Rendering.Two
import Gelatin.Transform
import Graphics.Rendering.OpenGL hiding (position, color, drawElements,
                                         translate, rotate, perspective,
                                         ortho, clearDepth, drawArrays)
import Control.Monad
import Data.IORef
import Graphics.GLUtil hiding (setUniform)
import System.Exit

-- | TODO: Add a main renderer type that will hold shaders, projections and
-- possibly

cubePoints :: [V3 GLfloat]
cubePoints =
    [ V3 (-0.5) ( 0.5) ( 0.5)
    , V3 ( 0.5) ( 0.5) ( 0.5)
    , V3 (-0.5) (-0.5) ( 0.5)
    , V3 ( 0.5) (-0.5) ( 0.5)

    , V3 (-0.5) ( 0.5) (-0.5)
    , V3 ( 0.5) ( 0.5) (-0.5)
    , V3 (-0.5) (-0.5) (-0.5)
    , V3 ( 0.5) (-0.5) (-0.5)
    ]

cubeColors :: [V4 GLfloat]
cubeColors = map (up . fmap (+0.5)) cubePoints
    where up (V3 x y z) = V4 x y z 1

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

colorCube :: ShaderProgram -> Rendering ()
colorCube shader = do
    usingDepthFunc Less $ usingShader shader $ do
        setProjection pj
        setModelview mv
        withVertices (comp position cubePoints .+ comp color cubeColors) $
            drawIndexedTriangles cubeIndices 12
    where pj = fmap (fmap realToFrac) $ perspective (pi/4) 1 0.1 10
          mv = fmap (fmap realToFrac) $ mkM44 $ do translate $ V3 0 0 (-5)
                                                   rotate (pi/8) $ V3 1 0 0

boxes :: Rendering2d ()
boxes = do
    gradient box grad
    withPosition (V2 100 200) $ fillTex (Relative "img/quantum-foam.jpg") box tbx
    withPosition (V2 100 100) $ do
        fill box yellow
        withPosition (V2 100 (-50)) $ withScale (V2 0.5 0.5) $ do
            fill box red
            withScale (V2 0.5 0.5) $ withRotation (pi/4) $ do
                fill box blue
    where box  = rectangle (V2 0 0) (V2 100 100)
          tbx  = rectangle (V2 0 0) (V2 1 1)
          grad = take 6 $ cycle [red, green, blue]

sceneRendering :: ShaderProgram -> ShaderProgram -> Rendering ()
sceneRendering colorShader textureShader = do
    -- Clear the stage.
    clearDepth
    clearColorWith black
    -- Draw a background using a texture.
    usingTexture Texture2D (Relative "img/quantum-foam.jpg") params $
        usingShader textureShader $ do
            setProjection pj
            setModelview eye4
            setSampler 0
            withVertices (comp position tr .+ comp texcoord t) $
                drawArrays Triangles $ length tr
    clearDepth
    colorCube colorShader
    where pj = ortho 0 600 0 600 0 1
          t  = rectangle (V2 0 0) (V2 1 1)
          tr = map embedGL $ (rectangle (V2 0 0) (V2 640 480) :: [V2 GLfloat])
          params = do setFilter (Nearest, Nothing) Nearest
                      setWrapMode S Repeated Clamp
                      setWrapMode T Repeated Clamp

main :: IO ()
main = do
    wref  <- initWindow (V2 0 0) (V2 600 600) "Gelatin"
    scs   <- simpleColorShader
    sts   <- simpleTextureShader
    scene <- compileRendering $ sceneRendering scs sts
    box   <- compileTwo (V2 600 600) boxes

    forever $ do
        pollEvents
        (_, window) <- readIORef wref
        writeIORef wref ([], window)
        makeContextCurrent $ Just window
        render scene
        render box
        printError
        swapBuffers window
        shouldClose <- windowShouldClose window
        when shouldClose $ do cleanup scene
                              cleanup box
                              exitSuccess

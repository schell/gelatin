module Main where

import Gelatin
import Control.Monad
import Data.IORef
import System.Exit
import Data.Monoid

-- | TODO: Add a main renderer type that will hold textures loaded.

cubePoints :: [V3 Double]
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

cubeTexs :: [V2 Double]
cubeTexs = face ++ face
    where face = [ V2 0 0
                 , V2 1 0
                 , V2 0 1
                 , V2 1 1
                 ]

cubeColors :: [V4 Double]
cubeColors = map (up . fmap (+0.5)) cubePoints
    where up (V3 x y z) = V4 x y z 1

cubeIndices :: [Int]
cubeIndices = [ 0, 1, 3
              , 0, 2, 3 -- front
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

cube :: ShaderProgram -> M44 Double -> VertexBufferCommand () -> Rendering ()
cube shader mv addColor = do
    usingDepthFunc Less $ usingShader shader $ do
        setProjection pj
        setModelview mv
        withVertices cubeVertices $ drawIndexedTriangles cubeIndices 12
    where pj = perspective (pi/4) 1 0.1 10
          cubeVertices = do addComponent $ position cubePoints
                            addColor

boxes :: Rendering2d ()
boxes = do
    gradient box grad
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
            let vb = do addComponent $ position tr
                        addComponent $ texcoord t
            withVertices vb $ drawArrays Triangles $ length tr
    clearDepth
    cube colorShader leftmv $ addComponent $ color cubeColors
    usingTexture Texture2D (Relative "img/quantum-foam.jpg") params $
        cube textureShader ritemv $ addComponent $ texcoord cubeTexs
    where pj = ortho 0 600 0 600 0 1
          t  = rectangle (V2 0 0) (V2 1 1)
          tr = map embed (rectangle (V2 0 0) (V2 640 480) :: [V2 Double])
          leftmv = mkM44 $ do translate $ V3 (-1) 0 (-5)
                              rotate (pi/8) $ V3 1 0 0
          ritemv = mkM44 $ do translate $ V3 1 0 (-5)
                              rotate (pi/8) $ V3 1 0 0
          params = do setFilter (Nearest, Nothing) Nearest
                      setWrapMode S Repeated Clamp
                      setWrapMode T Repeated Clamp

main :: IO ()
main = do
    wref  <- initWindow (V2 0 0) (V2 600 600) "Gelatin"
    scs   <- simpleColorShader
    sts   <- simpleTextureShader
    r1    <- compileRendering $ sceneRendering scs sts
    r2    <- compileRendering2d 600 600 boxes

    let scene = r1 `mappend` r2

    forever $ do
        pollEvents
        (_, window) <- readIORef wref
        writeIORef wref ([], window)
        makeContextCurrent $ Just window

        render scene

        swapBuffers window
        shouldClose <- windowShouldClose window
        when shouldClose exitSuccess

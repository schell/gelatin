module Main where

import Gelatin
import Control.Monad
import Data.IORef
import System.Exit
import Debug.Trace

cubePoints :: [V3 Double]
cubePoints = [ V3 (-0.5) ( 0.5) ( 0.5)
             , V3 ( 0.5) ( 0.5) ( 0.5)
             , V3 (-0.5) (-0.5) ( 0.5)
             , V3 ( 0.5) (-0.5) ( 0.5)
             , V3 (-0.5) ( 0.5) (-0.5)
             , V3 ( 0.5) ( 0.5) (-0.5)
             , V3 (-0.5) (-0.5) (-0.5)
             , V3 ( 0.5) (-0.5) (-0.5)
             ]

cubeIndices :: [Int]
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


cube :: ShaderProgram -> M44 Double -> VertexBufferCommand () -> Rendering ()
cube shader mv addColor = do
    usingDepthFunc Less $ usingShader shader $ do
        setProjection pj
        setModelview mv
        withVertices cubeVertices $ drawIndexedTriangles cubeIndices 12
    where pj = perspective (pi/4) 1 0.1 10
          cubeVertices = do addComponent $ position cubePoints
                            addColor

--boxes :: Rendering2d ()
--boxes = withSize 600 600 $ do
--    -- Clear the stage.
--    clear
--    -- Draw a rectangle with a texture as a background.
--    fillTex (Relative "img/quantum-foam.jpg")
--            -- 2d geometry
--            (rectangle (V2 0 0) (V2 600 480))
--            -- uv mapping
--            (rectangle (V2 0 0) (V2 1 1))
--    -- Draw a gradient box using red, green and blue.
--    -- The color definitions come from Gelatin.Color, which includes all
--    -- the familiar named colors from CSS :)
--    let box  = rectangle (V2 0 0) (V2 100 100)
--        grad = take 6 $ cycle [red, green, blue]
--    gradient box grad
--    -- Translate (100,100) and draw a yellow box
--    withPosition (V2 100 100) $ do
--        fill box yellow
--        -- Translate again, scale down and draw some more.
--        withPosition (V2 100 (-50)) $ withScale (V2 0.5 0.5) $ do
--            fill box red
--            -- Scale down again and rotate and draw again.
--            withScale (V2 0.5 0.5) $ withRotation (pi/4) $ do
--                fill box blue


cubes :: ShaderProgram -> ShaderProgram -> Rendering ()
cubes colorShader textureShader = do
    -- Clear the stage.
    clearDepth
    clearColorWith black
    let leftmv = mkM44 $ do translate $ V3 (-1) 0 (-5)
                            rotate (pi/8) $ V3 1 0 0
        ritemv = mkM44 $ do translate $ V3 1 0 (-5)
                            rotate (pi/8) $ V3 1 0 0
        params = do setFilter (Nearest, Nothing) Nearest
                    setWrapMode S Repeated Clamp
                    setWrapMode T Repeated Clamp
    -- Draw the cube with colors, translated left and slightly rotated.
    cube colorShader leftmv $ addComponent $ color cubeColors
    -- Draw the cube with a texture, translated right and slightly rotated.
    usingTexture Texture2D (Relative "img/quantum-foam.jpg") params $
        cube textureShader ritemv $ addComponent $ texcoord cubeTexs

main :: IO ()
main = do
    wref  <- initWindow (V2 0 0) (V2 600 600) "Gelatin"
    rdr   <- mkRenderer2d
    r1    <- compileRendering $ cubes (twoColorShader rdr) (twoTextureShader rdr)
    r2    <- compileRendering $ mk2dRendering rdr $ withSize 600 600 $ withPosition (V2 100 100) $ do
                 let ps   = [ V2 0 0, V2 400 10, V2 250 300, V2 200 100, V2 25 45]
                     poly = PrimPoly ps
                     tris = map PrimTri $ clipEars ps
                 clear
                 fillPrimitives (hex 0x333333FF) [poly]
                 withPosition (V2 10 10) $ do
                     outlinePrimitives white tris
                     withPosition (V2 10 10) $ outlinePrimitives skyBlue [poly]

--    r2    <- compileRendering2d boxes

    loop r1 r2 wref emptyInputEnv

beziers = [ [V3 0 0 0, V3 100 0 0, V3 100 100 0]
          , [V3 100 100 0, V3 0 100 0, V3 0 0 0]
          ]
paths = map (\bz -> [ deCasteljau t bz | t <- [0,0.5,1] ]) beziers
path = concat paths

loop :: CompiledRendering -> CompiledRendering -> WindowRef -> InputEnv -> IO ()
loop r1 r2 wref env = do
    pollEvents
    (events, window) <- readIORef wref
    let env' = foldl foldInput env events
    writeIORef wref ([], window)
    makeContextCurrent $ Just window

    if isLeftMouseDown env'
    then render r1
    else render r2

    swapBuffers window
    shouldClose <- windowShouldClose window
    when shouldClose exitSuccess
    loop r1 r2 wref $ clearEvents env'

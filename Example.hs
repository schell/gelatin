module Main where

import Gelatin
import Control.Monad
import Control.Applicative
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
    where pj = perspective (pi/4) 1 0.1 10 :: M44 Float
          cubeVertices = do addComponent $ position3 cubePoints
                            addColor

grad v =
    let v' = (/100) <$> v
        r = (max 0) $ 1 - V2 0 0 `distance` v'
        g = (max 0) $ 1 - V2 1 0 `distance` v'
        b = (max 0) $ 1 - V2 1 (0.5) `distance` v'
        fs = [(r*), (g*), (b*)] :: [Float -> Float]
        cs = [red, green, blue] :: [V4 Float]
    in flip alpha 1 $ sum $ zipWith (<$>) fs cs


boxes :: Rendering2d ()
boxes = withSize 600 600 $ do
    -- Clear the stage.
    clear
    -- Draw a rectangle with a texture as a background.
    let texbox = texture (Relative "img/quantum-foam.jpg") $ \(V2 x y) -> V2 (x/600) (y/480)
    fill texbox [rectangle (V2 0 0) 600 480]
    -- Draw a gradient box using red, green and blue.
    -- The color definitions come from Gelatin.Color, which includes all
    -- the familiar named colors from CSS :)
    let box  = [rectangle (V2 0 0) 100 100]
    fill (gradient grad) box
    -- Translate (100,100) and draw a yellow box
    withPosition (V2 100 100) $ do
        fill (Color yellow) box
        -- Translate again, scale down and draw some more.
        withPosition (V2 100 (-50)) $ withScale (V2 0.5 0.5) $ do
            fill (Color red) box
            -- Scale down again and rotate and draw again.
            withScale (V2 0.5 0.5) $ withRotation (pi/4) $ do
                fill (Color blue) box

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

circles :: Rendering2d ()
circles = withSize 300 300 $ do
    clear
    fill (solid white) $ [rectangle (V2 0 0) 300 300]
    let canary = hex 0xFFFF00FF
        c1 = circle (V2 50 50) 10
        c2 = circle (V2 150 150) 75
    fill (solid canary) [c1, c2]
    withPosition (V2 5 5) $
        outline (solid $ hex 0xFF00FFFF) $ concatMap triangulate [c1, c2]

main :: IO ()
main = do
    wref  <- initWindow (V2 0 0) (V2 300 300) "Gelatin"
    r1    <- runRendering2d boxes
    r2    <- runRendering2d circles

    --r2    <- runRendering2d boxes
    loop r2 r1 wref emptyInputEnv

loop :: CompiledRendering -> CompiledRendering -> WindowRef -> InputEnv -> IO ()
loop r1 r2 wref env = do
    pollEvents
    (events, window) <- readIORef wref
    let env' = foldl foldInput env events
    writeIORef wref ([], window)
    makeContextCurrent $ Just window

    if isLeftMouseDown env'
    then render r2
    else render r1

    swapBuffers window
    shouldClose <- windowShouldClose window
    when shouldClose exitSuccess
    loop r1 r2 wref $ clearEvents env'

module Main where

import Gelatin
import Control.Monad
import Control.Applicative
import Control.Concurrent
import Data.IORef
import System.Exit

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

cubesolids :: [V4 Double]
cubesolids = map (up . fmap (+0.5)) cubePoints
    where up (V3 x y z) = V4 x y z 1


cube :: ShaderProgram -> M44 Double -> VertexBufferCommand () -> Rendering ()
cube shader mv addsolid = do
    usingDepthFunc Less $ usingShader shader $ do
        setProjection pj
        setModelview mv
        withVertices cubeVertices $ drawIndexedTriangles cubeIndices 12
    where pj = perspective (pi/4) 1 0.1 10 :: M44 Float
          cubeVertices = do addComponent $ position3 cubePoints
                            addsolid

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
    -- The color definitions come from Gelatin.solid, which includes all
    -- the familiar named colors from CSS :)
    let box  = [rectangle (V2 0 0) 100 100]
    fill (gradient grad) box
    -- Translate (100,100) and draw a yellow box
    withPosition (V2 100 100) $ do
        fill (solid yellow) box
        -- Translate again, scale down and draw some more.
        withPosition (V2 100 (-50)) $ withScale (V2 0.5 0.5) $ do
            fill (solid red) box
            -- Scale down again and rotate and draw again.
            withScale (V2 0.5 0.5) $ withRotation (pi/4) $ do
                fill (solid blue) box

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
    cube colorShader leftmv $ addComponent $ color cubesolids
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
    outline (solid black) $ arrows $ polygon $ circlePath (V2 50 200) 20 20
    withPosition (V2 5 5) $
        outline (solid $ hex 0xFF00FFFF) $ concatMap triangulate [c1, c2]

strokes :: Rendering2d ()
strokes = withSize 300 300 $ do
    clear
    fill (solid white) [ rectangle (V2 0 0) 300 300 ]
    stroke 5 (solid orange) [ rectangle (V2 50 50) 200 200
                            , curve [ V2 100 100, V2 150 100, V2 150 200, V2 200 200]
                            ]

strokeTest :: Rendering2d ()
strokeTest = withSize 300 300 $ do
    clear
    fill (solid white) [ rectangle (V2 0 0) 300 300 ]
    let ps = bezierToList 10 [ V2 0 0, V2 10 0, V2 10 20, V2 20 20 ]
        stk = minkowskiConvolution c ps
        c  = circlePath (V2 0 0) 1 8 :: [V2 Float]
        ptop = rectangle (head stk - V2 0.25 0.25) 0.5 0.5
        tris = triangulate $ polygon stk
        s  = 250/20
        t  = do translate $ V3 15 15 0
                scale $ V3 s s 1
    withTransform t $ do outline (solid orange) [ Path ps ]
                         outline (solid blue) $ ptop : (arrows $ path stk)
                         forM_ (zip tris $ map hex $ cycle [0xFFFF00FF,0x00FFFFFF,0xFF00FFFF]) $ \(p, a) -> do
                             fill (solid $ alpha a 0.2) [p]
                             outline (solid $ alpha gray 0.3) [p]
                         withPosition (V2 10 10) $ outline (solid $ hex 0x00ccccff) [ Path c ]

arrows :: (Floating a, Ord a) => Primitive V2 a -> [Primitive V2 a]
arrows (Line a b) = [ path [a, b]
                    , path [b - u*l + n * w, b]
                    , path [b - u*l + n * (-w), b]
                    ]
    where n = signorm $ perp $ b - a
          u = signorm $ b - a
          l = 0.3 -- head length
          w = 0.3  -- head width
arrows p = concatMap arrows $ primitiveToLines p

main :: IO ()
main = do
    wref  <- initWindow (V2 0 0) (V2 300 300) "Gelatin"
    r1    <- runRendering2d boxes
    r2    <- runRendering2d circles
    r3    <- runRendering2d strokeTest

    --r2    <- runRendering2d boxes
    loop [r3, r2, r1] wref emptyInputEnv

loop :: [CompiledRendering] -> WindowRef -> InputEnv -> IO ()
loop [] _ _ = return ()
loop (r:rs) wref env = do
    pollEvents
    (events, window) <- readIORef wref
    let env' = foldl foldInput env events
    writeIORef wref ([], window)
    makeContextCurrent $ Just window

    rs' <- if isLeftMouseUp env'
             then render r >> (return $ rs ++ [r])
             else render r >> return (r:rs)

    swapBuffers window
    shouldClose <- windowShouldClose window
    when shouldClose exitSuccess
    threadDelay 100
    loop rs' wref $ clearEvents env'

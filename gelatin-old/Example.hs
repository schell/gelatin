module Main where

import Gelatin
import Control.Monad
import Control.Applicative
import Control.Concurrent
import Data.IORef
import Data.Maybe
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
    outline (solid black) $ arrows 5 $ polygon $ circlePath (V2 50 200) 20 20
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
    let ps = concatMap (bezierToList 50) [ [V2 20 280, V2 150 280, V2 150 20] , [V2 150 20, V2 150 280, V2 280 280] ]
        stk' = strokeWith squareCap bevelJoin 5 ps
        stk = minkowskiConvolution c $ reverse ps
        c  = circlePath (V2 0 0) 5 8
    --forM_ c $ \p -> outline (solid orange) [Path $ map (p+) ps]
    --outline (solid gray) $ concatMap (arrows 2) [Path stk]
    --outline (solid blue) $ concatMap (arrows 2) [Path ps]
    forM_ (toElbows ps) (drawElbow 5)
    fill (solid $ alpha pink 0.3) stk'

drawElbow :: (RealFloat a, Enum a) => a -> Elbow a -> Rendering2d ()
drawElbow w e = do
    let asect = fromJust $ acuteIntersection w e
        osect = fromJust $ obtuseIntersection w e
        ((aa,ab),(ac,ad)) = acuteLines w e
        ((oa,ob),(oc,od)) = obtuseLines w e
        ps    = unElbow e
        clr (CCWElbow _ _ _) = solid orange
        clr (CWElbow _ _ _) = solid gray
    outline (clr e) $ concatMap (arrows 1) [Path ps]
    outline (solid blue) $ circle asect 1 : (arrows 1 $ Path [aa, ab, ac, ad])
    outline (solid green) $ circle osect 1 : (arrows 1 $  Path [oa, ob, oc, od])

elbowTest :: Rendering2d ()
elbowTest = withSize 300 300 $ do
    clear
    let es = [ elbow (V2 0 0) (V2 25 0) (V2 25 25)
             , elbow (V2 25 25) (V2 25 0) (V2 0 0)

             , elbow (V2 12.5 25) (V2 0 12.5) (V2 12.5 0)
             , elbow (V2 12.5 0) (V2 0 12.5) (V2 12.5 25)

             , elbow (V2 0 25) (V2 25 25) (V2 25 0)
             , elbow (V2 25 0) (V2 12.5 12.5) (V2 0 0)
             ]
        ps = [V2 (-50) 50, V2 (-50) (-50), V2 (-45) 50, V2 50 (-50), V2 50 50, V2 (-50) 75] :: [V2 Float]
        st = strokeWith squareCap bevelJoin 5 ps
        zipWithM_' xs ys f = zipWithM_ f xs ys
    withPosition (V2 20 20) $ zipWithM_' es [0 ..] $ \e i ->
        withPosition (V2 (i*40) (i*40)) $ drawElbow 5 e
    withPosition (V2 20 150) $ drawElbow 5 $ elbow (V2 0 0) (V2 0 100) (V2 100 100)
    withPosition (V2 220 70) $ do
        forM_ (toElbows ps) (drawElbow 5)
        fill (solid $ alpha orange 0.5) st


allAngles :: (Metric f, R1 f, R2 f, Eq (f a), Num a, Ord a, RealFloat a)
          => [f a] -> [a]
allAngles ps = map signedAt ndxs
    where ndxs = [0 .. len - 1]
          len = length ps
          signedAt i = triangleArea (ps !! (mod (i - 1) len))
                                    (ps !! (mod (i    ) len))
                                    (ps !! (mod (i + 1) len))

lineTest :: Rendering2d ()
lineTest = withSize 300 300 $ do
    clear
    let ps = concatMap (bezierToList 1) [ [V2 20 280, V2 150 280, V2 150 20] , [V2 150 20, V2 150 280, V2 280 280] ]
    lift2d $ lineWidth $= 10
    outline (solid $ alpha orange 0.5) [path ps]
    lift2d $ lineWidth $= 1

arrows :: (Floating a, Ord a) => a -> Primitive V2 a -> [Primitive V2 a]
arrows w (Line a b) = [ path [a, b]
                      , path [b - u ^* w + n ^* w, b]
                      , path [b - u ^* w + n ^* (-w), b]
                      ]
    where n = signorm $ perp $ b - a
          u = signorm $ b - a
arrows w p = concatMap (arrows w) $ primitiveToLines p

main :: IO ()
main = do
    wref  <- initWindow (V2 100 300) (V2 800 800) "Gelatin"
    renderings <- mapM runRendering2d [boxes, circles, strokeTest, elbowTest, lineTest]
    loop renderings wref emptyInputEnv

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

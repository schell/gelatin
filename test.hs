{-# LANGUAGE TypeOperators #-}
module Test where

import Prelude hiding (and)
import Gelatin.Shaders.Core
import Linear
import Graphics.VinylGL
import Data.Vinyl

data a + b = And a b

instance (Show a, Show b) => Show (a + b) where
    show (And a b) = unwords [show a, "+", show b]

(.+.) :: a -> b -> a + b
(.+.) a b = And a b

main :: IO ()
main = putStrLn $ show (two .+. aoeu .+. threef)

two :: Int
two = 2

aoeu :: String
aoeu = "aoeu"

threef :: Float
threef = 3.0

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

cubeColors :: [V4 Double]
cubeColors = map (up . fmap (+0.5)) cubePoints
    where up (V3 x y z) = V4 x y z 1

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


colorCube s = do
    setUniforms s (projection =: eye4)
    setUniforms s (modelview =: eye4)


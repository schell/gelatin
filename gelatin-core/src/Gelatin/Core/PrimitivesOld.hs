{-# LANGUAGE DeriveGeneric #-}
module Gelatin.Core.Primitives where

import Gelatin.Core.Bezier
import Gelatin.Core.Triangle
import Gelatin.Core.Path
import Gelatin.Core.Font
import Data.Hashable
import Linear
import GHC.Generics

--------------------------------------------------------------------------------
-- Primitives
--------------------------------------------------------------------------------
data Primitives = PathPrims [Path (V2 Float)]
                | BezierPrims [Bezier (V2 Float)]
                | TrianglePrims [Triangle (V2 Float)]
                | TextPrims FontData Int Float String
                deriving (Show, Generic)
instance Hashable Primitives

path2ConcavePoly :: Path a -> [Triangle a]
path2ConcavePoly (Path vs)
    | length vs >= 3
    , x:xs <- vs = zipWith (Triangle x) xs (drop 1 xs)
    | otherwise = []

primToPaths :: Primitives -> [Path (V2 Float)]
primToPaths (PathPrims ps) = ps
primToPaths (BezierPrims bs) = map bezToPath bs
primToPaths (TrianglePrims ts) = map triToPath ts
primToPaths (TextPrims fd dpi px str) = stringCurvesToPaths fd dpi px str

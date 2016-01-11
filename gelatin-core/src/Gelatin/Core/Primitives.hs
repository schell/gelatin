{-# LANGUAGE DeriveGeneric #-}
module Gelatin.Core.Primitives where

import Gelatin.Core.Bezier
import Gelatin.Core.Triangle
import Gelatin.Core.Path
import Gelatin.Core.Color
import Gelatin.Core.Stroke
import Data.Hashable
import Linear
import GHC.Generics

--------------------------------------------------------------------------------
-- Paths
--------------------------------------------------------------------------------
data PathPrimitives f = Paths [Path (V2 Float)]
                      | PathText f Float String
                      deriving (Show)

instance (Hashable f) => Hashable (PathPrimitives f) where
    hashWithSalt s (Paths ps) = s `hashWithSalt` ps
    hashWithSalt s (PathText f px str) =
        s `hashWithSalt` f `hashWithSalt` px `hashWithSalt` str

--------------------------------------------------------------------------------
-- Fills
--------------------------------------------------------------------------------
data FillPrimitives f = FillBeziers Fill [Bezier (V2 Float)]
                      | FillTriangles Fill [Triangle (V2 Float)]
                      | FillPaths Fill [Path (V2 Float)]
                      | FillText Fill f Float String

fillPrimsString :: FillPrimitives f -> String
fillPrimsString (FillBeziers _ _) = "FillBeziers"
fillPrimsString (FillTriangles _ _) = "FillTriangles"
fillPrimsString (FillPaths _ _) = "FillPaths"
fillPrimsString (FillText _ _ _ _) = "FillText"

fillPrimsFill :: FillPrimitives f -> Fill
fillPrimsFill (FillBeziers f _) = f
fillPrimsFill (FillTriangles f _) = f
fillPrimsFill (FillPaths f _) = f
fillPrimsFill (FillText f _ _ _) = f

fillPrimsPoints :: FillPrimitives f -> [[V2 Float]]
fillPrimsPoints (FillBeziers _ bs) = [trisToComp $ map bezToTri bs]
fillPrimsPoints (FillTriangles _ ts) = [trisToComp ts]
fillPrimsPoints (FillPaths _ ps) = map unPath ps
fillPrimsPoints _ = []

instance Hashable f => Hashable (FillPrimitives f) where
    hashWithSalt s (FillText f fd px str) =
            s `hashWithSalt` show f `hashWithSalt` fd
                `hashWithSalt` px `hashWithSalt` str
    hashWithSalt s fp
        | FillColor f <- fillPrimsFill fp =
            s `hashWithSalt` fillPrimsString fp
                `hashWithSalt` "FillColor"
                    `hashWithSalt` map (map f) (fillPrimsPoints fp)
        | FillTexture p f <- fillPrimsFill fp =
            s `hashWithSalt` fillPrimsString fp
                `hashWithSalt` p
                    `hashWithSalt` p
                        `hashWithSalt` map (map f) (fillPrimsPoints fp)
        | otherwise = s

path2ConcavePoly :: Path a -> [Triangle a]
path2ConcavePoly (Path vs)
    | length vs >= 3
    , x:xs <- vs = zipWith (Triangle x) xs (drop 1 xs)
    | otherwise = []
--------------------------------------------------------------------------------
-- The sum type of paths and fills
--------------------------------------------------------------------------------
data R2Primitives f = R2PathPrimitives (Stroked (PathPrimitives f))
                    | R2FillPrimitives (FillPrimitives f)
                    deriving (Generic)
instance Hashable f => Hashable (R2Primitives f)

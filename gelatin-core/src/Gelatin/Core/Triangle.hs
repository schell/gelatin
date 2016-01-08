{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
module Gelatin.Core.Triangle where

import Gelatin.Core.Bounds
import Gelatin.Core.Path
import Gelatin.Core.Transform
import Gelatin.Core.Bezier
import Linear
import Data.Hashable

data Triangle a = Triangle a a a deriving (Show, Eq)

trisToComp :: [Triangle (V2 a)] -> [V2 a]
trisToComp = concatMap triPoints

triPoints :: Triangle (V2 a) -> [V2 a]
triPoints (Triangle a b c) = [a, b, c]

bezToTri :: Bezier a -> Triangle a
bezToTri (Bezier _ a b c) = Triangle a b c

instance Functor Triangle where
    fmap f (Triangle a b c) = Triangle (f a ) (f b) (f c)

instance Transformable Transform a => Transformable Transform (Triangle a) where
    transform = fmap . transform

instance BoundedByBox (Triangle (V2 Float)) where
    type BoundingBoxR (Triangle (V2 Float)) = ()
    type BoundingBox (Triangle (V2 Float)) = BBox
    boundingBox _ (Triangle a b c) = boundingBox () [a,b,c]
--------------------------------------------------------------------------------
-- Decomposing things into triangles
--------------------------------------------------------------------------------
sizeToTris :: Size -> [Triangle (V2 Float)]
sizeToTris (Size (V2 w h)) = [Triangle a b c, Triangle a c d]
    where [a,b,c,d] = [V2 (-hw) (-hh), V2 hw (-hh), V2 hw hh, V2 (-hw) hh]
          (hw,hh) = (w/2,h/2)

--class ToTriangles a where
--    toTriangles :: a -> [Triangle (V2 Float)]
--
--instance ToTriangles a => ToTriangles [a] where
--    toTriangles = concatMap toTriangles
--
--instance ToTriangles Size where
--    toTriangles = sizeToTris
--
--instance ToTriangles (Bezier (V2 Float)) where
--    toTriangles (Bezier _ a b c) = [Triangle a b c]

instance Hashable a => Hashable (Triangle a) where
    hashWithSalt s (Triangle a b c) =
        s `hashWithSalt`  a `hashWithSalt` b `hashWithSalt` c

{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveGeneric #-}
module Gelatin.Core.Path where

import Gelatin.Core.Bounds
import Gelatin.Core.Transform
import Linear
import Data.Hashable
import Data.Hashable.Vector
import GHC.Generics
import qualified Data.Vector.Unboxed as V
import Data.Vector.Unboxed (Vector, Unbox)

-- | A path is a consecutive list of points.
newtype Path a = Path { unPath :: Vector a } deriving (Show, Generic)

instance Transformable Transform (Path (V2 Float)) where
    transform t (Path vs) = Path $ transform t vs

-- | Determine the smallest bounding box that contains the path.
pathBounds :: Path (V2 Float) -> BBox
pathBounds = polyBounds . unPath

-- | A simple size.
newtype Size = Size { unSize :: V2 Float }

-- | Convert a size to a path.
sizeToPath :: Size -> Path (V2 Float)
sizeToPath = Path . box . unSize

-- | Convert a size to a list of paths.
sizeToPaths :: Size -> [Path (V2 Float)]
sizeToPaths = (:[]) . sizeToPath

-- | Return whether or not a point lies within the given path.
pointInside :: (Fractional a, Ord a, Unbox a) => V2 a -> Path (V2 a) -> Bool
pointInside p path = pathHasPoint (unPath path) p

-- | Determine if a point lies within a polygon path using the even/odd
-- rule.
-- A point is inside a path if it has an odd number of intersections with
-- the boundary (Jordan Curve theorem)
pathHasPoint :: (Ord a, Fractional a, Unbox a) => Vector (V2 a) -> V2 a -> Bool
pathHasPoint vs v = V.foldr' (\s a -> if s then not a else a) False $
  V.zipWith3 f vs (V.drop 1 vs) (V.drop 2 vs)
  where f a b c = if t1 a b c && t2 a b c then True else False
        t1 p p1 p2 = (y p2 > y p) /= (y p1 > y p)
        t2 p p1 p2 = x p < (x p1 - x p2) * (y p - y p2) / (y p1 - y p2) + x p2
        x (V2 a _) = a
        y (V2 _ b) = b
--pathHasPoint [] _ = False
--pathHasPoint poly@(p1':_) p' = pointInPath' False p' (poly ++ [p1'])
--    where pointInPath' c _ []  = c
--          pointInPath' c _ [_] = c
--          pointInPath' c p (p1:p2:ps) = pointInPath' (test p p1 p2 c) p (p2:ps)
--          test p p1 p2 = if t1 p p1 p2 && t2 p p1 p2 then not else id
--          t1 p p1 p2 = (y p2 > y p) /= (y p1 > y p)
--          t2 p p1 p2 = x p < (x p1 - x p2) * (y p - y p2) / (y p1 - y p2) + x p2
--          x (V2 a _) = a
--          y (V2 _ b) = b

-- | Create a closed box path.
box :: (Fractional a, Unbox a) => V2 a -> Vector (V2 a)
box (V2 w h) = poly
    where poly = V.fromList [V2 x1 y1, V2 x2 y1, V2 x2 y2, V2 x1 y2, V2 x1 y1]
          (hw,hh) = (w/2,h/2)
          x1 = -hw
          x2 = hw
          y1 = -hh
          y2 = hh

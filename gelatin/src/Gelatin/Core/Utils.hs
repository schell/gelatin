{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
module Gelatin.Core.Utils where

import           Data.Vector.Unboxed (Unbox, Vector)
import qualified Data.Vector.Unboxed as V
import           Linear

-- | Determine if a point lies within a polygon path using the even/odd
-- rule.
-- A point is inside a path if it has an odd number of intersections with
-- the boundary (Jordan Curve theorem)
pathHasPoint :: (Ord a, Fractional a, Unbox a) => Vector (V2 a) -> V2 a -> Bool
pathHasPoint vs v = V.foldr' (\s a -> if s then not a else a) False $
  V.zipWith3 f vv vs (V.drop 1 vs)
  where vv = V.replicate (V.length vs) v
        f a b c = t1 a b c && t2 a b c
        t1 p p1 p2 = (y p2 > y p) /= (y p1 > y p)
        t2 p p1 p2 = x p < (x p1 - x p2) * (y p - y p2) / (y p1 - y p2) + x p2
        x (V2 a _) = a
        y (V2 _ b) = b

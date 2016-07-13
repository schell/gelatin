{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Gelatin.Core.Bounds where

import Linear
import Gelatin.Core.Transform
import qualified Data.Vector.Unboxed as V
import           Data.Vector.Unboxed (Vector)

type BBox = (V2 Float, V2 Float)

polyBounds :: Vector (V2 Float) -> BBox
polyBounds = V.foldl' f (br,tl)
  where f :: (V2 Float, V2 Float) -> (V2 Float) -> (V2 Float, V2 Float)
        f (V2 nx ny, V2 xx xy) (V2 x y) = ( V2 (min nx x) (min ny y)
                                          , V2 (max xx x) (max xy y)
                                          )
        inf = 1/0
        ninf = (-1)/0
        tl = V2 ninf ninf
        br = V2 inf inf

pointsBounds :: [V2 Float] -> BBox
pointsBounds = polyBounds . V.fromList

boundsBounds :: Vector BBox -> BBox
boundsBounds bs = polyBounds $
  V.foldl' f V.empty bs
    where f :: Vector (V2 Float) -> (V2 Float, V2 Float) -> Vector (V2 Float)
          f vs (v1,v2) = vs `V.snoc` v1 `V.snoc` v2

instance Transformable Transform (V2 Float, V2 Float) where
  transform t (a,b) = (transform t a, transform t b)

pointInBounds :: V2 Float -> BBox -> Bool
pointInBounds (V2 px py) (V2 minx miny, V2 maxx maxy) =
  (px >= minx && px <= maxx) && (py >= miny && py <= maxy)

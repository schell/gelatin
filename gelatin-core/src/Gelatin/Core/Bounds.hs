{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
module Gelatin.Core.Bounds where

import Linear

type BBox = (V2 Float, V2 Float)

polyBounds :: [V2 Float] -> (V2 Float, V2 Float)
polyBounds [] = (0,0)
polyBounds ps = (V2 minx miny, V2 maxx maxy)
    where xs = map fx ps
          ys = map fy ps
          minx = minimum xs
          miny = minimum ys
          maxx = maximum xs
          maxy = maximum ys
          fx (V2 x _) = x
          fy (V2 _ y) = y

boundsBounds :: [BBox] -> BBox
boundsBounds [] = (0,0)
boundsBounds bs = polyBounds $ concatMap (\(v1,v2) -> [v1,v2]) bs

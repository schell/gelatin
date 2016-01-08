{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
module Gelatin.Core.Bounds where

import Linear

class BoundedByBox a where
    type BoundingBoxR a :: *
    type BoundingBox a :: *
    boundingBox :: BoundingBoxR a -> a -> BoundingBox a

type BBox = (V2 Float, V2 Float)

instance BoundedByBox [V2 Float] where
    type BoundingBoxR [V2 Float] = ()
    type BoundingBox [V2 Float] = (V2 Float, V2 Float)
    boundingBox _ [] = (0,0)
    boundingBox _ ps = (V2 minx miny, V2 maxx maxy)
        where xs = map fx ps
              ys = map fy ps
              minx = minimum xs
              miny = minimum ys
              maxx = maximum xs
              maxy = maximum ys
              fx (V2 x _) = x
              fy (V2 _ y) = y

instance BoundedByBox [BBox] where
    type BoundingBoxR [BBox] = ()
    type BoundingBox [BBox] = BBox
    boundingBox _ [] = (0,0)
    boundingBox _ bs = boundingBox () ps
        where ps = concatMap (\(v1,v2) -> [v1,v2]) bs


module Gelatin.Geometry where

import Linear
--------------------------------------------------------------------------------
-- Classes
--------------------------------------------------------------------------------
class Embedable a where
    embed  :: Fractional b => a -> V3 b
--------------------------------------------------------------------------------
-- Instances
--------------------------------------------------------------------------------
instance (Real a, Fractional a) => Embedable (V1 a) where
    embed (V1 x) = V3 (realToFrac x) 0 0

instance Real a => Embedable (V2 a) where
    embed (V2 x y) = V3 (realToFrac x) (realToFrac y) 0

instance Real a => Embedable (V3 a) where
    embed = fmap realToFrac
--------------------------------------------------------------------------------
-- Building geometry
--------------------------------------------------------------------------------
-- | Given two points, creates a pair of triangles representing
-- a rectangle.
rectangle :: Num a
          => V2 a -- ^ The top left of the rectangle
          -> V2 a -- ^ The width and height of the rectangle
          -> [V2 a]
rectangle (V2 x y) (V2 w h) = [ tl, bl, br, tl, tr, br ]
    where tl = V2 x y
          tr = V2 (x+w) y
          bl = V2 x (y+h)
          br = V2 (x+w) (y+h)

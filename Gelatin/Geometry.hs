module Gelatin.Geometry where

import Linear
import Graphics.Rendering.OpenGL (GLfloat)
--------------------------------------------------------------------------------
-- Classes
--------------------------------------------------------------------------------
class Embedable a where
    embedWith :: Double -> a -> V3 Double
--------------------------------------------------------------------------------
-- Instances
--------------------------------------------------------------------------------
instance Real a => Embedable (V1 a) where
    embedWith i (V1 x) = V3 (realToFrac x) i i

instance Real a => Embedable (V2 a) where
    embedWith i (V2 x y) = V3 (realToFrac x) (realToFrac y) i

instance Real a => Embedable (V3 a) where
    embedWith = const (fmap realToFrac)
--------------------------------------------------------------------------------
-- API
--------------------------------------------------------------------------------
embed :: Embedable a => a -> V3 Double
embed = embedWith 0

embedGL :: Embedable a => a -> V3 GLfloat
embedGL = fmap realToFrac . embed
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

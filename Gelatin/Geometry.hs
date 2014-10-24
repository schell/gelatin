{-# LANGUAGE GADTs #-}
module Gelatin.Geometry (
    Primitive(..),
    Embedable,
    toLines,
    toTriangles,
    triangleToLines,
    primitiveToLines,
    embed,
    embedWith,
    rectangle,
    deCasteljau,
    module G
) where

import Linear
import Gelatin.Geometry.Triangulation as G
import Gelatin.Geometry.Types as G

--------------------------------------------------------------------------------
-- Classes
--------------------------------------------------------------------------------
class Embedable a where
    embedWith :: Fractional b => b -> a -> V3 b
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
-- Types
--------------------------------------------------------------------------------
data Primitive a = PrimTri (Triangle a)
                 | PrimLine (Line a)
                 deriving (Show)
--------------------------------------------------------------------------------
-- API
--------------------------------------------------------------------------------
embed :: (Embedable a, Fractional b) => a -> V3 b
embed = embedWith 0
--------------------------------------------------------------------------------
-- Building geometry
--------------------------------------------------------------------------------
-- | Given two points, creates a polygon rectangle.
rectangle :: Num a
          => V2 a -- ^ The top left of the rectangle
          -> a    -- ^ The width of the rectangle
          -> a    -- ^ The height of the rectangle
          -> Polygon a
rectangle (V2 x y) w h = [ tl, tr, br, bl ]
    where tl = V2 x y
          tr = V2 (x+w) y
          bl = V2 x (y+h)
          br = V2 (x+w) (y+h)

-- | Turns a polygon into a list of line primitives.
toLines :: Polygon a -> [Primitive a]
toLines [] = []
toLines [_] = []
toLines (p1:p2:ps) = PrimLine (p1,p2) : toLines (p2:ps)

-- | Turns a polygon into a list of triangle primitives.
-- Uses a dirty ear-clipping triangulation.
toTriangles :: (Ord a, Fractional a) => Polygon a -> [Primitive a]
toTriangles = map PrimTri . clipEars

-- | Turns a list of triangles into a list of lines.
triangleToLines :: Triangle a -> [Line a]
triangleToLines (a,b,c) = [(a,b), (b,c), (c,a)]

-- | Converts any primitive to a list of lines.
primitiveToLines :: Primitive a -> [Line a]
primitiveToLines (PrimLine l) = [l]
primitiveToLines (PrimTri t) = triangleToLines t
--------------------------------------------------------------------------------
-- Helpers
--------------------------------------------------------------------------------
-- | Compute the point at `t` along an N-bezier curve.
deCasteljau :: (Fractional a, Num a) => a -> [V3 a] -> V3 a
deCasteljau _ [b] = b
deCasteljau t coefs = deCasteljau t reduced
  where
    reduced = zipWith (flip (lerp t)) coefs (tail coefs)
    --lerpP t' (V3 x0 y0 z0) (V3 x1 y1 z1) = V3 (lerp t' x0 x1) (lerp t' y0 y1) (lerp t' z0 z1)
    --lerp t' a b = t' * b + (1 - t') * a

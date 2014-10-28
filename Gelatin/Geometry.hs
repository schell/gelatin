{-# LANGUAGE GADTs #-}
module Gelatin.Geometry (
    Primitive(..),
    triangleToLines,
    primitiveToLines,
    primitiveToList,
    rectangle,

    polygon,
    triangles,
    lines',

    deCasteljau,
    clipEars,
    pathHasPoint
) where

import Linear
import Control.Lens

--------------------------------------------------------------------------------
-- Building geometry
--------------------------------------------------------------------------------
-- | Given two points, creates a polygon rectangle.
rectangle :: Num a
          => V2 a -- ^ The top left of the rectangle
          -> a    -- ^ The width of the rectangle
          -> a    -- ^ The height of the rectangle
          -> Primitive (V2 a)
rectangle (V2 x y) w h = Polygon [ tl, tr, br, bl ]
    where tl = V2 x y
          tr = V2 (x+w) y
          bl = V2 x (y+h)
          br = V2 (x+w) (y+h)

-- | Turns a path into a polygon primitive.
polygon :: Eq a => [a] -> Primitive a
polygon = Polygon

-- | Turns a list of points representing a polygon into a list of
-- triangle primitives by performing ear clipping.
triangles :: (Ord a, Fractional a) => [V2 a] -> [Primitive (V2 a)]
triangles = clipEars . Polygon

-- | Turns a list of points representing a polygon int a list of
-- line primitives.
lines' :: Eq a => [a] -> [Primitive a]
lines' = pathToLines

-- | Turns a path into a list of line primitives.
pathToLines :: Eq a => [a] -> [Primitive a]
pathToLines [] = []
pathToLines ps'@(p':_) = toLines' ps'
    where toLines' [] = []
          toLines' [p] = Line p p':[]
          toLines' (p1:p2:ps) = Line p1 p2  : toLines' (p2:ps)

-- | Turns a list of triangles into a list of lines.
triangleToLines :: Primitive a -> [Primitive a]
triangleToLines (Triangle a b c) = [ Line a b
                                   , Line b c
                                   , Line c a
                                   ]
triangleToLines _ = []

-- | Converts any primitive to a list of lines.
primitiveToLines :: Eq a => Primitive a -> [Primitive a]
primitiveToLines (Polygon p) = pathToLines p
primitiveToLines l@(Line _ _) = [l]
primitiveToLines t@(Triangle _ _ _) = triangleToLines t

primitiveToList :: Primitive a -> [a]
primitiveToList (Polygon p) = p
primitiveToList (Triangle a b c) = [a, b, c]
primitiveToList (Line a b) = [a,b]
--------------------------------------------------------------------------------
-- Helpers
--------------------------------------------------------------------------------
-- | Compute the point at `t` along an N-bezier curve.
deCasteljau :: (Fractional a, Num a) => a -> [V2 a] -> V2 a
deCasteljau _ [b] = b
deCasteljau t coefs = deCasteljau t reduced
  where
    reduced = zipWith (flip (lerp t)) coefs (tail coefs)
    --lerpP t' (V3 x0 y0 z0) (V3 x1 y1 z1) = V3 (lerp t' x0 x1) (lerp t' y0 y1) (lerp t' z0 z1)
    --lerp t' a b = t' * b + (1 - t') * a

-- | The dirtiest O(n^3) ear clipping I could write.
clipEars :: (Ord a, Fractional a) => Primitive (V2 a) -> [Primitive (V2 a)]
clipEars (Polygon ps) = clipEars' [] ps
    where clipEars' ts ps'
              | (p1:p2:p3:[]) <- ps' = Triangle p1 p2 p3 :ts
              | (p1:p2:p3:rest) <- ps' =
                  if any (pathHasPoint [p1,p2,p3]) rest
                    -- Cycle through and check the next triangle
                    then clipEars' ts $ p2:p3:rest ++ [p1]
                    else clipEars' (Triangle p1 p2 p3 :ts) $ p1:p3:rest
              | otherwise = ts
clipEars t@(Triangle _ _ _) = [t]
clipEars _ = []

-- | Determine if a point lies within a polygon path using the even/odd
-- rule.
pathHasPoint :: (Ord a, Fractional a) => [V2 a] -> V2 a -> Bool
pathHasPoint [] _ = False
pathHasPoint poly@(p1':_) p' = pointInPath' False p' (poly ++ [p1'])
    where pointInPath' c _ []  = c
          pointInPath' c _ [_] = c
          pointInPath' c p (p1:p2:ps) = pointInPath' (test p p1 p2 $ c) p (p2:ps)
          test p p1 p2 = if t1 p p1 p2 && t2 p p1 p2 then not else id
          t1 p p1 p2 = (y p2 > y p) /= (y p1 > y p)
          t2 p p1 p2 = x p < (x p1 - x p2) * (y p - y p2) / (y p1 - y p2) + x p2
          x v = v ^. _x
          y v = v ^. _y
--------------------------------------------------------------------------------
-- Instances
--------------------------------------------------------------------------------
instance Real a => Embedable (V1 a) where
    embedWith i (V1 x) = V3 (realToFrac x) i i

instance Real a => Embedable (V2 a) where
    embedWith i (V2 x y) = V3 (realToFrac x) (realToFrac y) i

instance Real a => Embedable (V3 a) where
    embedWith = const (fmap realToFrac)

instance Functor Primitive where
    fmap f (Line a b) = Line (f a) (f b)
    fmap f (Triangle a b c) = Triangle (f a) (f b) (f c)
    fmap f (Polygon ps) = Polygon $ fmap f ps
--------------------------------------------------------------------------------
-- Classes
--------------------------------------------------------------------------------
class Embedable a where
    embedWith :: Fractional b => b -> a -> V3 b
--------------------------------------------------------------------------------
-- Types
--------------------------------------------------------------------------------
data Primitive a = Line a a
                 | Triangle a a a
                 | Polygon [a]
                 deriving (Show)

{-# LANGUAGE GADTs #-}
module Gelatin.Geometry (
    -- * Primitive types
    Primitive(..),

    -- * Decomposing primitives
    triangleToLines,
    primitiveToLines,
    primitiveToList,
    bezierToList,
    triangulate,

    -- * Decomposition helpers
    deCasteljau,
    pathHasPoint,

    -- * Creating geometry
    rectangle,
    circle,
    polygon,
    triangles,
    curve,
    bezier,
    lines',
) where

import Linear
import Control.Lens hiding (pre)

--------------------------------------------------------------------------------
-- Building geometry
--------------------------------------------------------------------------------
-- | Specify a polygon rectangle with an upper left point p, width and height.
rectangle :: (R1 f, R2 f, Num a)
          => f a -- ^ The top left of the rectangle
          -> a    -- ^ The width of the rectangle
          -> a    -- ^ The height of the rectangle
          -> Primitive f a
rectangle p w h = Polygon [ tl, tr, br, bl ]
    where tl = p
          tr = p & _x %~ (+w)
          bl = p & _y %~ (+h)
          br = p & _x %~ (+w) & _y %~ (+h)

circle :: (R1 f, R2 f, Additive f, Metric f, Fractional a, Floating a, Num a, Ord a, Enum a)
       => f a -> a -> Primitive f a
circle p r = Polygon bs
    where bs = [ mk (cos t) (sin t) | t <- [0.0,(2*pi/divisions) .. 2*pi] ]
          c = 2 * pi * r
          divisions = c / n
          n = max (logBase 10 c) 1
          mk x y = mv $ zero & _x .~ x & _y .~ y
          mv v = (v ^* r) ^+^ p

-- | Specify a polygon with points.
polygon :: Eq a => [f a] -> Primitive f a
polygon = Polygon

-- | Specify a n-bezier curve rasterized in segments with distance <= 1.
curve :: (R1 f, R2 f, Ord a, Fractional a) => [f a] -> Primitive f a
curve = bezier 1.0

-- | Specify a n-bezier curve rasterized in segments with distance <= `n`.
bezier :: (R1 f, R2 f, Ord a, Fractional a) => a -> [f a] -> Primitive f a
bezier n ps
    | length ps <= 1 = Polygon [] -- Maybe this is bad?
    | p1:p2:[] <- ps = Line p1 p2
    | otherwise = Curve n ps

-- | Turns a list of points representing a polygon into a list of
-- triangle primitives by performing ear clipping.
triangles :: (Metric f, R1 f, R2 f, Ord a, Fractional a, Floating a)
          => [f a] -> [Primitive f a]
triangles = triangulate . Polygon

-- | Turns a list of points representing a polygon int a list of
-- line primitives.
lines' :: Eq a => [f a] -> [Primitive f a]
lines' = pathToLines

-- | Turns a path into a list of line primitives.
pathToLines :: Eq a => [f a] -> [Primitive f a]
pathToLines [] = []
pathToLines ps'@(p':_) = toLines' ps'
    where toLines' [] = []
          toLines' [p] = Line p p':[]
          toLines' (p1:p2:ps) = Line p1 p2  : toLines' (p2:ps)

-- | Turns a list of triangles into a list of lines.
triangleToLines :: Primitive f a -> [Primitive f a]
triangleToLines (Triangle a b c) = [ Line a b
                                   , Line b c
                                   , Line c a
                                   ]
triangleToLines _ = []

-- | Converts any primitive to a list of lines.
primitiveToLines :: (Metric f, R1 f, R2 f, Eq a, Num a, Floating a, Ord a)
                 => Primitive f a -> [Primitive f a]
primitiveToLines (Polygon p)  = pathToLines p
primitiveToLines (Curve bs n) = pathToLines $ bezierToList n bs
primitiveToLines l@(Line _ _) = [l]
primitiveToLines t@(Triangle _ _ _) = triangleToLines t

-- | Converts any primitive to a list of lines.
primitiveToList :: (Metric f, R1 f, R2 f, Num a, Floating a, Ord a)
                => Primitive f a -> [f a]
primitiveToList (Polygon p) = p
primitiveToList (Triangle a b c) = [a, b, c]
primitiveToList (Curve bs n) = bezierToList n bs
primitiveToList (Line a b) = [a,b]

-- | Converts a curve of beziers into a path of points. Uses `n` as the
-- maximum distance between points along the curve.
bezierToList :: (Metric f, R1 f, R2 f, Num a, Floating a, Ord a) => [f a] -> a -> [f a]
bezierToList bs n = [deCasteljau 0 bs] ++ subs ++ [deCasteljau 1 bs]
    where subs = subdivideBezier n 0 1 bs

subdivideBezier :: (Metric f, R1 f, R2 f, Num a, Floating a, Ord a) => a -> a -> a -> [f a] -> [f a]
subdivideBezier n start end bs =
    let p1  = deCasteljau start bs
        p2  = deCasteljau end bs
        d   = distance p1 p2
        p3  = deCasteljau mid bs
        mid = start + (end - start) / 2
        pre = subdivideBezier n start mid bs
        suf = subdivideBezier n mid end bs
        in if d <= n
            then []
            else pre ++ [p3] ++ suf -- Start with the first point and starti = 0, endi = 1
-- get points along the curve at starti, endi
-- add points inbetween until the distance between two new points is less
-- than or equal to n.

--------------------------------------------------------------------------------
-- Helpers
--------------------------------------------------------------------------------
-- | Compute the point at `t` along an N-bezier curve.
deCasteljau :: (Additive f, R1 f, R2 f, Num a) => a -> [f a] -> f a
deCasteljau _ [b] = b
deCasteljau t coefs = deCasteljau t reduced
  where
    reduced = zipWith (flip (lerp t)) coefs (tail coefs)
    --lerpP t' (V3 x0 y0 z0) (V3 x1 y1 z1) = V3 (lerp t' x0 x1) (lerp t' y0 y1) (lerp t' z0 z1)
    --lerp t' a b = t' * b + (1 - t') * a

-- | The dirtiest O(n^3) ear clipping I could write.
triangulate :: (Metric f, R1 f, R2 f, Ord a, Fractional a, Floating a)
         => Primitive f a -> [Primitive f a]
triangulate (Polygon ps) = triangulate' [] ps
    where triangulate' ts ps'
              | (p1:p2:p3:[]) <- ps' = Triangle p1 p2 p3 :ts
              | (p1:p2:p3:rest) <- ps' =
                  if any (pathHasPoint [p1,p2,p3]) rest
                    -- Cycle through and check the next triangle
                    then triangulate' ts $ p2:p3:rest ++ [p1]
                    else triangulate' (Triangle p1 p2 p3 :ts) $ p1:p3:rest
              | otherwise = ts
triangulate t@(Triangle _ _ _) = [t]
triangulate (Curve n bs) = triangulate $ Polygon $ bezierToList bs n
triangulate _ = []

-- | Determine if a point lies within a polygon path using the even/odd
-- rule.
pathHasPoint :: (R1 f, R2 f, Ord a, Fractional a) => [f a] -> f a -> Bool
pathHasPoint [] _ = False
pathHasPoint poly@(p1':_) p' = pointInPath' False p' (poly ++ [p1'])
    where pointInPath' :: (R1 f, R2 f, Ord a, Fractional a) => Bool -> f a -> [f a] -> Bool
          pointInPath' c _ []  = c
          pointInPath' c _ [_] = c
          pointInPath' c p (p1:p2:ps) = pointInPath' (test p p1 p2 $ c) p (p2:ps)
          test :: (R2 f, Ord a, Fractional a) => f a -> f a -> f a -> (Bool -> Bool)
          test p p1 p2 = if t1 p p1 p2 && t2 p p1 p2 then not else id
          t1 :: (R2 f, Ord a) => f a -> f a -> f a -> Bool
          t1 p p1 p2 = (y p2 > y p) /= (y p1 > y p)
          t2 :: (R1 f, R2 f, Ord a, Fractional a) => f a -> f a -> f a -> Bool
          t2 p p1 p2 = x p < (x p1 - x p2) * (y p - y p2) / (y p1 - y p2) + x p2
          x v = v ^. _x
          y v = v ^. _y
--------------------------------------------------------------------------------
-- Instances
--------------------------------------------------------------------------------
--instance Functor (Primitive (f a)) where
--    fmap f (Line p1 p2) = Line (f p1) (f p2)
--    fmap f (Curve bs n) = Curve (fmap (fmap f) bs) n
--    fmap f (Triangle a b c) = Triangle (f a) (f b) (f c)
--    fmap f (Polygon ps) = Polygon $ fmap f ps
--------------------------------------------------------------------------------
-- Classes
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
-- Types
--------------------------------------------------------------------------------
data Primitive f a = Line (f a) (f a)
                   -- ^ A straight line from one point to another.
                   | Curve a [f a]
                   -- ^ A n-bezier curve with a maximum segment distance.
                   -- This distance is used during rasterization as the
                   -- moximum tolerable distance between resolved points on
                   -- the curve. A smaller number will result in a higher
                   -- curve resolution. (So if your curve doesn't look
                   -- smooth, make this number smaller.)
                   | Triangle (f a) (f a) (f a)
                   -- ^ A flat triangle of three points.
                   | Polygon [f a]
                   -- ^ A path defining a flat un-closed polygon. The first point
                   -- should not be repeated as the last.
                   deriving (Show)

{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
module Gelatin.Core.Bezier (
    Bezier(..),
    QuadraticBezier(..),
    CubicBezier(..),
    bez,
    bez3,
    bez4,
    toBeziers,
    bez3ToBez,
    bez3ToBezInner,
    bez3ToBezOuter,
    bez4ToBez,
    bez4ToBezInner,
    bez4ToBezOuter,
    bez4sToPath,
    flipBez4,
    demoteCubic,
    deCasteljau,
    subdivideAdaptive,
    subdivideAdaptive3,
    subdivideAdaptive4,
    cleanSeqDupes
) where

import Gelatin.Core.Transform
import Linear

data Bezier a = Bezier Ordering a a a deriving (Show, Eq)

data QuadraticBezier a = QuadraticBezier a a a deriving (Show, Eq)

data CubicBezier a = CubicBezier a a a a deriving (Show, Eq)

newtype NBezier a = NBezier [a] deriving (Show, Eq)

instance Functor Bezier where
    fmap f (Bezier o a b c) = Bezier o (f a) (f b) (f c)

instance Functor QuadraticBezier where
    fmap f (QuadraticBezier a b c) = QuadraticBezier (f a) (f b) (f c)

instance Functor CubicBezier where
    fmap f (CubicBezier a b c d) = CubicBezier (f a) (f b) (f c) (f d)

instance Transformable Transform a => Transformable Transform (Bezier a) where
    transform = fmap . transform

instance Transformable Transform a => Transformable Transform (QuadraticBezier a) where
    transform = fmap . transform

instance Transformable Transform a => Transformable Transform (CubicBezier a) where
    transform = fmap . transform

-- | Turn a polyline into a list of bezier primitives.
toBeziers :: (Fractional a, Ord a) => [V2 a] -> [Bezier (V2 a)]
toBeziers (a:b:c:ps) = bez a b c : toBeziers (c:ps)
toBeziers _ = []

-- | Create a bezier primitive. The area of the triangle formed by the
-- bezier's three points will be used to determine the orientation.
bez :: (Ord a, Fractional a) => V2 a -> V2 a -> V2 a -> Bezier (V2 a)
bez a b c = Bezier (compare (triangleArea a b c) 0) a b c
    where triangleArea (V2 x2 y2) (V2 x0 y0) (V2 x1 y1) =
            (x1-x0)*(y2-y0)-(x2-x0)*(y1-y0)

-- | Create a quadratic bezier. This is an alias of 'QuadraticBezier'.
bez3 :: V2 a -> V2 a -> V2 a -> QuadraticBezier (V2 a)
bez3 = QuadraticBezier

-- | Create a cubic bezier. This is an alias of 'CubicBezier'.
bez4 :: V2 a -> V2 a -> V2 a -> V2 a -> CubicBezier (V2 a)
bez4 = CubicBezier

flipBez4 :: CubicBezier a -> CubicBezier a
flipBez4 (CubicBezier a b c d) = CubicBezier d c b a

-- | Convert a quadratic bezier into a list of drawable bezier primitives
-- and derives the winding (which determines drawing an inner or outer bez) from
-- the order of control points.
bez3ToBez :: (Ord a, Fractional a) => QuadraticBezier (V2 a) -> [Bezier (V2 a)]
bez3ToBez (QuadraticBezier a b c) = [bez a b c]

-- | Convert a quadratic bezier into a bezier primitive that fills outer.
bez3ToBezOuter :: (Ord a, Fractional a)
               => QuadraticBezier (V2 a) -> [Bezier (V2 a)]
bez3ToBezOuter qbz =
    case bez3ToBez qbz of
        z@[Bezier GT _ _ _] -> z
        [Bezier _ a b c] -> [bez c b a]
        z -> z

-- | Convert a quadratic bezier into a bezier primitive that fills inner.
bez3ToBezInner :: (Ord a, Fractional a)
               => QuadraticBezier (V2 a) -> [Bezier (V2 a)]
bez3ToBezInner qbz =
    case bez3ToBez qbz of
        [Bezier GT a b c] -> [bez c b a]
        b -> b

-- | Convert a cubic bezier into a list of drawable bezier primitives.
bez4ToBez :: (Ord a, Fractional a) => CubicBezier (V2 a) -> [Bezier (V2 a)]
bez4ToBez = concatMap bez3ToBez . demoteCubic

-- | Convert a cubic bezier into a list of drawable bezier primitives that
-- fill the inner bezier.
bez4ToBezInner :: (Ord a, Fractional a) => CubicBezier (V2 a) -> [Bezier (V2 a)]
bez4ToBezInner = concatMap bez3ToBezInner . demoteCubic

-- | Convert a cubic bezier into a list of drawable bezier primitives that
-- fill the inner bezier.
bez4ToBezOuter :: (Ord a, Fractional a) => CubicBezier (V2 a) -> [Bezier (V2 a)]
bez4ToBezOuter = concatMap bez3ToBezOuter . demoteCubic

-- | Convert a list of cubic beziers into a smooth path.
bez4sToPath :: RealFloat a => a -> a -> [CubicBezier (V2 a)] -> [V2 a]
bez4sToPath mScale mAngle bs =
    cleanSeqDupes $ concatMap (subdivideAdaptive4 mScale mAngle) bs

-- | Compute the point at `t` along an N-bezier curve.
deCasteljau :: (Additive f, R1 f, R2 f, Num a) => a -> [f a] -> f a
deCasteljau _ [b] = b
deCasteljau t coefs = deCasteljau t reduced
  where reduced = zipWith (flip (lerp t)) coefs (tail coefs)

curveCollinearityEpsilon :: Double
curveCollinearityEpsilon = 1e-30

curveAngleToleranceEpsilon :: Double
curveAngleToleranceEpsilon = 0.01

curveRecursionLimit :: Int
curveRecursionLimit = 32

-- | Approximate a cubic bezier with a list of four quadratic beziers.
--
-- <<docimages/demoteCubic.png>>
demoteCubic :: Fractional a => CubicBezier (V2 a) -> [QuadraticBezier (V2 a)]
demoteCubic (CubicBezier a b c d) = [q1,q2,q3,q4]
    where mid = lerp 0.5
          m1 = mid a b
          m2 = mid b c
          m3 = mid c d
          m1m2 = mid m1 m2
          m2m3 = mid m2 m3
          mam1 = mid a m1
          h1 = mid mam1 m1
          p2 = mid m1m2 m2m3
          m1m2p2 = mid m1m2 p2
          h2 = mid m1m2 m1m2p2
          mp2m2m3 = mid p2 m2m3
          h3 = mid mp2m2m3 m2m3
          mdm3 = mid d m3
          h4 = mid mdm3 m3
          p1 = mid h1 h2
          --p2 = mid h2 h3
          p3 = mid h3 h4
          q1 = QuadraticBezier a h1 p1
          q2 = QuadraticBezier p1 h2 p2
          q3 = QuadraticBezier p2 h3 p3
          q4 = QuadraticBezier p3 h4 d

-- | Adaptively subdivide the quadratic bezier into a series of points (line
-- segments).
-- i.e. Generate more points along the part of the curve with greater curvature.
-- @see http://www.antigrain.com/research/adaptive_bezier/index.html
-- and http://www.antigrain.com/__code/src/agg_curves.cpp.html
subdivideAdaptive,subdivideAdaptive3 :: RealFloat a
                                     => a -> a -> QuadraticBezier (V2 a)
                                     -> [V2 a]
subdivideAdaptive3 = subdivideAdaptive
subdivideAdaptive mScale mAngle (QuadraticBezier va vb vc) =
    let mDistanceToleranceSquare = (0.5 / mScale) ** 2
    in va : subdivide mDistanceToleranceSquare mAngle 0 va vb vc ++ [vc]

-- | Adaptively subdivide the cubic bezier into a series of points (line
-- segments).
subdivideAdaptive4 :: RealFloat a => a -> a -> CubicBezier (V2 a) -> [V2 a]
subdivideAdaptive4 s a = cleanSeqDupes . concatMap (subdivideAdaptive s a) . demoteCubic

-- | Removes sequential duplicates from a list.
cleanSeqDupes :: Eq a => [a] -> [a]
cleanSeqDupes [] = []
cleanSeqDupes [x] = [x]
cleanSeqDupes (x:y:zs) = if x == y
                         then cleanSeqDupes (y:zs)
                         else x : cleanSeqDupes (y:zs)

subdivide :: RealFloat a
          => a -> a -> Int -> V2 a -> V2 a -> V2 a -> [V2 a]
subdivide mDistanceToleranceSquare mAngleTolerance level
          v1@(V2 x1 y1) v2@(V2 x2 y2) v3@(V2 x3 y3)
    | level > curveRecursionLimit = []
    | otherwise =
            -- calculate the midpoints of the line segments
        let v12 = (v1 + v2) / 2
            v23 = (v2 + v3) / 2
            v123= (v12 + v23) / 2
            V2 dx dy = v3 - v1
            d = abs $ (x2 - x3) * dy - (y2 - y3) * dx
            subdivideFurther = subdivide mDistanceToleranceSquare
                                         mAngleTolerance
                                         (level + 1)
                                         v1 v12 v123
                               ++
                               subdivide mDistanceToleranceSquare
                                         mAngleTolerance
                                         (level + 1)
                                         v123 v23 v3
        in if d > realToFrac curveCollinearityEpsilon
           then -- test regular case
               if (d * d) <= (mDistanceToleranceSquare * (dx*dx + dy*dy))
               then -- if the curvature is within our distance tolerance then
                    -- we're done subdividing
                    if mAngleTolerance < realToFrac curveAngleToleranceEpsilon
                    then [v123]
                    else -- test angle and cusp condition
                         let preDA = abs (atan2 (y3 - y2) (x3 - x2) -
                                     atan2 (y2 - y1) (x2 - x1))
                             da = if preDA >= pi then 2*pi - preDA else preDA
                         in if da < mAngleTolerance
                            then [v123]
                            else subdivideFurther
               else subdivideFurther
           else -- test collinear case
                let da = dx*dx + dy*dy
                    f a b = if quadrance (a - b) < mDistanceToleranceSquare
                            then [v2]
                            else subdivideFurther
                in if da == 0
                   then f v1 v2
                   else let d' = ((x2 - x1) * dx + (y2 - y1) * dy) / da
                        in if d' > 0 && d' < 1
                           then -- this is the simple collinear case, 1-2-3
                                []
                           else f v2 $ if d' <= 0
                                then v1
                                else if d' >= 1
                                     then v3
                                     else V2 (x1 + d'*dx) (y1 + d'*dy)

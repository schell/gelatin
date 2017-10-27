{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-- | Here is a simple bezier algebra.
-- To better server drawing beziers the 'Bezier' type's first
-- record is a boolean representing its fill direction (inner or outer).
module Gelatin.Core.Bezier (
  -- * Types
  Bezier,
  QuadraticBezier,
  CubicBezier,
  -- * Smart Constructors
  bezier,
  bez3,
  bez4,
  -- * Special helpers
  fmapBezier,
  fmapQuadraticBezier,
  fmapCubicBezier,
  transformBezier,
  transformQuadraticBezier,
  transformCubicBezier,
  triangleArea,
  -- * Conversion
  bezToBez3,
  bez3ToBez,
  bez3ToBezInner,
  bez3ToBezOuter,
  bez4ToBez,
  bez4ToBezInner,
  bez4ToBezOuter,
  bez4sToPath,
  flipBez4,
  demoteCubic,
  -- * Subdivision
  deCasteljau,
  subdivideAdaptive,
  subdivideAdaptive3,
  subdivideAdaptive4,
  cleanSeqDupes,
  -- * Shapes
  arcBez4,
  arcBez3,
  ellipseBez4,
  ellipseBez3,
  cornerBez4,
  cornerBez3
) where

import           Gelatin.Core.Transform
import           Linear
import qualified Data.Vector.Unboxed as V
import           Data.Vector.Unboxed (Vector, Unbox)

-- | A bezier that fills internally or externally.
type Bezier a = (Bool, a, a, a)

-- | A simple quadratic bezier with no explicit fill direction.
type QuadraticBezier a = (a, a, a)

-- | A simple cubic bezier with no explicit fill direction.
type CubicBezier a = (a, a, a, a)

fmapBezier :: (a -> b) -> Bezier a -> Bezier b
fmapBezier f (o, a, b, c) = (o, f a, f b, f c)

fmapQuadraticBezier :: (a -> b) -> QuadraticBezier a -> QuadraticBezier b
fmapQuadraticBezier f (a, b, c) = (f a, f b, f c)

fmapCubicBezier :: (a -> b) -> CubicBezier a -> CubicBezier b
fmapCubicBezier f (a, b, c, d) = (f a, f b, f c, f d)

transformBezier :: Num a => M44 a -> Bezier (V2 a) -> Bezier (V2 a)
transformBezier = fmapBezier . transformV2

transformQuadraticBezier :: Num a => M44 a -> QuadraticBezier (V2 a)
                         -> QuadraticBezier (V2 a)
transformQuadraticBezier = fmapQuadraticBezier . transformV2

transformCubicBezier :: Num a => M44 a -> CubicBezier (V2 a)
                     -> CubicBezier (V2 a)
transformCubicBezier = fmapCubicBezier . transformV2

-- | Create a bezier primitive. The area of the triangle formed by the
-- bezier's three points will be used to determine the orientation.
bezier :: (Ord a, Fractional a) => V2 a -> V2 a -> V2 a -> Bezier (V2 a)
bezier a b c = (triangleArea a b c > 0, a, b, c)

triangleArea :: Num a => V2 a -> V2 a -> V2 a -> a
triangleArea (V2 x2 y2) (V2 x0 y0) (V2 x1 y1) =
        (x1-x0)*(y2-y0)-(x2-x0)*(y1-y0)

--bezToPath :: (RealFloat a, Unbox a) => Bezier (V2 a) -> Path (V2 a)
--bezToPath = Path . subdivideAdaptive 100 0 . bezToBez3

-- | Create a quadratic bezier. This is an alias of 'QuadraticBezier'.
bez3 :: V2 a -> V2 a -> V2 a -> QuadraticBezier (V2 a)
bez3 = (,,)

-- | Convert a Bezier to a QuadraticBezier.
bezToBez3 :: Bezier a -> QuadraticBezier a
bezToBez3 (_,a,b,c) = (a,b,c)

-- | Create a cubic bezier. This is an alias of 'CubicBezier'.
bez4 :: V2 a -> V2 a -> V2 a -> V2 a -> CubicBezier (V2 a)
bez4 = (,,,)

flipBez4 :: CubicBezier a -> CubicBezier a
flipBez4 (a,b,c,d) = (d,c,b,a)

-- | Convert a quadratic bezier into a bezier primitive and derives the winding
-- (which determines drawing an inner or outer bez) from the order of control
-- points.
bez3ToBez :: (Ord a, Fractional a, Unbox a)
          => QuadraticBezier (V2 a) -> Bezier (V2 a)
bez3ToBez (a,b,c) = bezier a b c

-- | Convert a quadratic bezier into a bezier primitive that fills outer.
bez3ToBezOuter :: (Ord a, Fractional a, Unbox a)
               => QuadraticBezier (V2 a) -> Bezier (V2 a)
bez3ToBezOuter qbz =
    case bez3ToBez qbz of
        z@(True,_,_,_) -> z
        (_,a,b,c) -> bezier c b a

-- | Convert a quadratic bezier into a bezier primitive that fills inner.
bez3ToBezInner :: (Ord a, Fractional a, Unbox a)
               => QuadraticBezier (V2 a) -> Bezier (V2 a)
bez3ToBezInner qbz =
    case bez3ToBez qbz of
        (True,a,b,c) -> bezier c b a
        b -> b

-- | Convert a cubic bezier into a list of drawable bezier primitives.
bez4ToBez :: (Ord a, Fractional a, Unbox a)
          => CubicBezier (V2 a) -> Vector (Bezier (V2 a))
bez4ToBez = V.map bez3ToBez . demoteCubic

-- | Convert a cubic bezier into a list of drawable bezier primitives that
-- fill the inner bezier.
bez4ToBezInner :: (Ord a, Fractional a, Unbox a)
               => CubicBezier (V2 a) -> Vector (Bezier (V2 a))
bez4ToBezInner = V.map bez3ToBezInner . demoteCubic

-- | Convert a cubic bezier into a list of drawable bezier primitives that
-- fill the inner bezier.
bez4ToBezOuter :: (Ord a, Fractional a, Unbox a)
               => CubicBezier (V2 a) -> Vector (Bezier (V2 a))
bez4ToBezOuter = V.map bez3ToBezOuter . demoteCubic

-- | Convert a list of cubic beziers into a smooth path.
bez4sToPath :: (RealFloat a, Unbox a)
            => a -> a -> Vector (CubicBezier (V2 a)) -> Vector (V2 a)
bez4sToPath mScale mAngle =
  cleanSeqDupes . V.concatMap (subdivideAdaptive4 mScale mAngle)

-- | Compute the point at `t` along an N-bezier curve.
deCasteljau :: (Additive f, R1 f, R2 f, Num a) => a -> [f a] -> f a
deCasteljau _ [b] = b
deCasteljau t coefs = deCasteljau t reduced
  where reduced = zipWith (flip (lerp t)) coefs (Prelude.tail coefs)

curveCollinearityEpsilon :: Double
curveCollinearityEpsilon = 1e-30

curveAngleToleranceEpsilon :: Double
curveAngleToleranceEpsilon = 0.01

curveRecursionLimit :: Int
curveRecursionLimit = 32

-- | Approximate a cubic bezier with a list of four quadratic beziers.
--
-- <<docimages/demoteCubic.png>>
demoteCubic :: (Fractional a, Unbox a)
            => CubicBezier (V2 a) -> Vector (QuadraticBezier (V2 a))
demoteCubic (a,b,c,d) = V.fromList [q1,q2,q3,q4]
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
          q1 = bez3 a h1 p1
          q2 = bez3 p1 h2 p2
          q3 = bez3 p2 h3 p3
          q4 = bez3 p3 h4 d

-- | Adaptively subdivide the quadratic bezier into a series of points (line
-- segments).
-- i.e. Generate more points along the part of the curve with greater curvature.
-- @see http://www.antigrain.com/research/adaptive_bezier/index.html
-- and http://www.antigrain.com/__code/src/agg_curves.cpp.html
subdivideAdaptive,subdivideAdaptive3 :: (RealFloat a, Unbox a)
                                     => a -> a -> QuadraticBezier (V2 a)
                                     -> Vector (V2 a)
subdivideAdaptive3 = subdivideAdaptive
subdivideAdaptive mScale mAngle (va,vb,vc) =
    let mDistanceToleranceSquare = (0.5 / mScale) ** 2
        vs = subdivide mDistanceToleranceSquare mAngle 0 va vb vc
    in va `V.cons` vs V.++ V.singleton vc

-- | Adaptively subdivide the cubic bezier into a series of points (line
-- segments).
subdivideAdaptive4 :: (RealFloat a, Unbox a)
                   => a -> a -> CubicBezier (V2 a) -> Vector (V2 a)
subdivideAdaptive4 s a = cleanSeqDupes . V.concatMap (subdivideAdaptive s a) . demoteCubic

-- | Removes sequential duplicates from a vector.
cleanSeqDupes :: (Eq a, Unbox a) => Vector a -> Vector a
cleanSeqDupes vs
  | V.length vs > 1 = vs1 `V.snoc` V.last vs
  | otherwise = vs
  where vs1 = V.map fst $ V.filter (uncurry (/=)) $ V.zip vs (V.drop 1 vs)

subdivide :: (RealFloat a, Unbox a)
          => a -> a -> Int -> V2 a -> V2 a -> V2 a -> Vector (V2 a)
subdivide mDistanceToleranceSquare mAngleTolerance level
          v1@(V2 x1 y1) v2@(V2 x2 y2) v3@(V2 x3 y3)
    | level > curveRecursionLimit = V.empty
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
                               V.++
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
                    then V.singleton v123
                    else -- test angle and cusp condition
                         let preDA = abs (atan2 (y3 - y2) (x3 - x2) -
                                     atan2 (y2 - y1) (x2 - x1))
                             da = if preDA >= pi then 2*pi - preDA else preDA
                         in if da < mAngleTolerance
                            then V.singleton v123
                            else subdivideFurther
               else subdivideFurther
           else -- test collinear case
                let da = dx*dx + dy*dy
                    f a b = if quadrance (a - b) < mDistanceToleranceSquare
                            then V.singleton v2
                            else subdivideFurther
                in if da == 0
                   then f v1 v2
                   else let d' = ((x2 - x1) * dx + (y2 - y1) * dy) / da
                        in if d' > 0 && d' < 1
                           then -- this is the simple collinear case, 1-2-3
                                V.empty
                           else f v2 $ if d' <= 0
                                then v1
                                else if d' >= 1
                                     then v3
                                     else V2 (x1 + d'*dx) (y1 + d'*dy)
--------------------------------------------------------------------------------
-- Shapes with beziers
--------------------------------------------------------------------------------
kappa :: Fractional a => a
kappa = 0.5522847498307936

cornerBez4 :: RealFloat a => a -> a -> CubicBezier (V2 a)
cornerBez4 xr yr = bez4 (V2 0 yr) (V2 0 y) (V2 x 0) (V2 xr 0)
    where x = xr * kappa
          y = yr * kappa

cornerBez3 :: (RealFloat a, Unbox a)
           => a -> a -> Vector (QuadraticBezier (V2 a))
cornerBez3 xr yr = demoteCubic $ cornerBez4 xr yr

-- | Generate a cubic Bezier representing an arc on the unit circle of total
-- angle `size` radians, beginning `start` radians above the x-axis. Up to four
-- of these curves are combined to make a full arc.
-- See www.joecridge.me/bezier.pdf for an explanation of the method.
acuteArc :: RealFloat a => a -> a -> CubicBezier (V2 a)
acuteArc start size = bez4 a b c d
    where [a,b,c,d] = [V2 ax ay, V2 bx by, V2 cx cy, V2 dx dy]
          ax = cos start
          ay = sin start
          bx = lambda * cosPhi + mu * sinPhi
          by = lambda * sinPhi - mu * cosPhi
          cx = lambda * cosPhi - mu * sinPhi
          cy = lambda * sinPhi + mu * cosPhi
          dx = cos (start + size)
          dy = sin (start + size)
          alpha = size / 2
          cosAlpha = cos alpha
          sinAlpha = sin alpha
          cotAlpha = 1 / tan alpha
          phi = start + alpha
          cosPhi = cos phi
          sinPhi = sin phi
          lambda = (4 - cosAlpha) / 3
          mu = sinAlpha + (cosAlpha - lambda) * cotAlpha

curveEpsilon :: Fractional a => a
curveEpsilon = 0.00001

-- | Create a list of cubic beziers representing an arc along an ellipse with
-- width `w`, height `h` and total angle `stop - start` radians, beginning
-- `start` radians above the x-axis.
arcBez4,arc' :: (Epsilon a, RealFloat a)
             => a -> a -> a -> a -> [CubicBezier (V2 a)]
arcBez4 w h start stop = if stop - start >= 2*pi
                     then close $ arc' w h start (start + 2*pi)
                     else arc' w h start stop
        -- This is a full arc so make sure the first and last points are equal
  where close [c1@(d,_,_,_) ,c2,c3,(a,b,c,_)] =
            [c1,c2,c3,(a,b,c,d)]
        close cs = cs

arc' w h start stop
    | (stop - start) > curveEpsilon = a : arcBez4 w h (start + arcToDraw) stop
    | otherwise = []
        where arcToDraw = min (stop - start) (pi/2)
              mv = affine2Modelview $ Scale (realToFrac <$> V2 w h)
              a = transformCubicBezier mv $ acuteArc start arcToDraw

arcBez3 :: (Epsilon a, RealFloat a, Unbox a)
        => a -> a -> a -> a -> Vector (QuadraticBezier (V2 a))
arcBez3 w h start stop = V.concat $ map demoteCubic $ arcBez4 w h start stop

-- | Create a list of cubic beziers that represent an entire closed
-- ellipse.
ellipseBez4 :: (Epsilon a, RealFloat a) => a -> a -> [CubicBezier (V2 a)]
ellipseBez4 xr yr = arcBez4 xr yr 0 (2*pi)

ellipseBez3 :: (Epsilon a, RealFloat a, Unbox a)
            => a -> a -> Vector (QuadraticBezier (V2 a))
ellipseBez3 xr yr = V.concat $ map demoteCubic $ ellipseBez4 xr yr

-- | @see http://www.antigrain.com/research/adaptive_bezier/index.html
-- and http://www.antigrain.com/__code/src/agg_curves.cpp.html
module Gelatin.Core.Rendering.Bezier where

import Gelatin.Core.Rendering.Types
import Linear

curveDistanceEpsilon :: Double
curveDistanceEpsilon = 1e-30

curveCollinearityEpsilon :: Double
curveCollinearityEpsilon = 1e-30

curveAngleToleranceEpsilon :: Double
curveAngleToleranceEpsilon = 0.01

curveRecursionLimit :: Int
curveRecursionLimit = 32


--subdivideAdaptive_ :: RealFloat a => Double -> Bezier (V2 a) -> [V2 a]
--subdivideAdaptive_ mScale (Bezier _ a@(V2 x1 y1) b@(V2 x2 y2) c@(V2 x3 y3)) =
--    let V2 mStartX mStartY = a
--        V2 mEndX mEndY = c
--        d1@(V2 dx1 dy1) = b - a
--        d2@(V2 dx2 dy2) = c - b
--        len = norm d1 + norm d2
--        mNumSteps = max (round $ realToFrac len * 0.25 * mScale) 4 :: Int
--        subdivideStep = 1.0 / fromIntegral mNumSteps
--        subdivideStep2 = subdivideStep * subdivideStep
--        tmpx = (x1 - x2 * 2.0 + x3) * subdivideStep2
--        tmpy = (y1 - y2 * 2.0 + y3) * subdivideStep2
--    in []

subdivideAdaptive :: RealFloat a => a -> a -> Bezier (V2 a) -> [V2 a]
subdivideAdaptive mScale mAngle (Bezier _ va vb vc) =
    let mDistanceToleranceSquare = (0.5 / mScale) ** 2
    in va : subdivide mDistanceToleranceSquare mAngle 0 va vb vc ++ [vc]

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
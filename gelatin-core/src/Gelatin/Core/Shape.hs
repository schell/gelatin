module Gelatin.Core.Shape where

import Gelatin.Core.Bezier
import Gelatin.Core.Transform
import Linear

kappa :: Fractional a => a
kappa = 0.5522847498307936

corner :: RealFloat a => a -> a -> CubicBezier (V2 a)
corner xr yr = CubicBezier (V2 0 yr) (V2 0 y) (V2 x 0) (V2 xr 0)
    where x = xr * kappa
          y = yr * kappa

-- | Generate a cubic Bezier representing an arc on the unit circle of total
-- angle `size` radians, beginning `start` radians above the x-axis. Up to four
-- of these curves are combined to make a full arc.
-- See www.joecridge.me/bezier.pdf for an explanation of the method.
acuteArc :: RealFloat a => a -> a -> CubicBezier (V2 a)
acuteArc start size = CubicBezier a b c d
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
arc,arc' :: RealFloat a => a -> a -> a -> a -> [CubicBezier (V2 a)]
arc w h start stop = if stop - start >= 2*pi
                     then close $ arc' w h start (start + 2*pi)
                     else arc' w h start stop
        -- This is a full arc so make sure the first and last points are equal
  where close [c1@(CubicBezier d _ _ _) ,c2,c3,CubicBezier a b c _] =
            [c1,c2,c3,CubicBezier a b c d]
        close cs = cs

arc' w h start stop
    | (stop - start) > curveEpsilon = a : arc w h (start + arcToDraw) stop
    | otherwise = []
        where arcToDraw = min (stop - start) (pi/2)
              s = fmap realToFrac $ V2 w h
              a = transform (Transform 0 s 0) $ acuteArc start arcToDraw

-- | Create a list of cubic beziers that represent an entire closed
-- ellipse.
ellipse :: RealFloat a => a -> a -> [CubicBezier (V2 a)]
ellipse xr yr = arc xr yr 0 (2*pi)

-- | Create a closed box path.
box :: Fractional a => V2 a -> [V2 a]
box (V2 w h) = poly
    where poly = [V2 x1 y1, V2 x2 y1, V2 x2 y2, V2 x1 y2, V2 x1 y1]
          (hw,hh) = (w/2,h/2)
          x1 = -hw
          x2 = hw
          y1 = -hh
          y2 = hh

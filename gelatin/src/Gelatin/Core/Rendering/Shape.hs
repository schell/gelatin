module Gelatin.Core.Rendering.Shape where

import Gelatin.Core.Rendering.Bezier
import Gelatin.Core.Rendering.Types
import Linear

kappa :: Fractional a => a
kappa = 0.5522847498307936

corner :: RealFloat a => a -> a -> CubicBezier (V2 a)
corner xr yr = CubicBezier (V2 0 yr) (V2 0 y) (V2 x 0) (V2 xr 0)
    where x = xr * kappa
          y = yr * kappa

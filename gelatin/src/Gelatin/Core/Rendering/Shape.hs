module Gelatin.Core.Rendering.Shape where

import Gelatin.Core.Rendering.Geometrical
import Gelatin.Core.Rendering.Types
import Linear

kappa :: Fractional a => a
kappa = 0.5522847498307936

corner :: RealFloat a => a -> a -> CubicBezier (V2 a)
corner xr yr = CubicBezier (V2 0 yr) (V2 0 y) (V2 x 0) (V2 xr 0)
    where x = xr * kappa
          y = yr * kappa

ellipse :: Float -> Float -> [CubicBezier (V2 Float)]
ellipse xr yr = [c1,c2,c3,c4]
    where c1 = fmap (transformV2 t) $ corner xr yr
          t = translate (-xr) (-yr) mempty
          r axis vec = demoteV3 $ rotateAbout axis pi $ promoteV2 vec
          rx = r $ V3 1 0 0
          ry = r $ V3 0 1 0
          rz = r $ V3 0 0 1
          c2 = fmap ry c1
          c3 = fmap rx c1
          c4 = fmap rz c1

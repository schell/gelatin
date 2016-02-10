{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
module Gelatin.Core.Transform where

import Linear

class Transformable a b where
    -- | Transform a type using the transform type.
    transform :: a -> b -> b

instance Transformable a b => Transformable a [b] where
    transform = map . transform
--------------------------------------------------------------------------------
-- Affine Transformation
--------------------------------------------------------------------------------
type Position = V2 Float
type Scale = V2 Float
type Rotation = Float

data Transform = Transform { tfrmTranslation :: Position
                           , tfrmScale       :: Scale
                           , tfrmRotation    :: Rotation
                           } deriving (Show)

instance Monoid Transform where
    mempty = Transform zero (V2 1 1) 0
    (Transform t1 s1 r1) `mappend` (Transform t2 s2 r2) = Transform (t1 + t2) (s1 * s2) (r1 + r2)

invertTfrm :: Transform -> Transform
invertTfrm (Transform v s r) = Transform (-1*v) ((1/) <$> s) (-1*r)

promoteV2 :: Num a => V2 a -> V3 a
promoteV2 (V2 x y) = V3 x y 0

demoteV3 :: V3 a -> V2 a
demoteV3 (V3 x y _) = V2 x y

toM44 :: RealFloat a => Transform -> M44 a
toM44 (Transform (V2 x y) (V2 w h) r) = fmap (fmap realToFrac) mv
    where mv = mat4Translate txy !*! rot !*! mat4Scale sxy
          sxy = V3 w h 1
          txy = V3 x y 0
          rxy = V3 0 0 1
          rot = if r /= 0 then mat4Rotate r rxy else identity

transformPoly :: Transform -> [V2 Float] -> [V2 Float]
transformPoly t = map (transformV2 t)

transformV2 :: RealFloat a => Transform -> V2 a -> V2 a
transformV2 t (V2 x y) = V2 x' y'
    where V3 x' y' _ = transformV3 t $ realToFrac <$> V3 x y 1

transformV3 :: RealFloat a => Transform -> V3 a -> V3 a
transformV3 t v = m41ToV3 $ toM44 t !*! v3ToM41 v

v3ToM41 :: Num a => V3 a -> V4 (V1 a)
v3ToM41 (V3 x y z) = V4 (V1 x) (V1 y) (V1 z) (V1 1)

m41ToV3 :: V4 (V1 a) -> V3 a
m41ToV3 (V4 (V1 x) (V1 y) (V1 z) _) = V3 x y z

rotateAbout :: (Num a, Epsilon a, Floating a) => V3 a -> a -> V3 a -> V3 a
rotateAbout axis phi = m41ToV3 . (mat4Rotate phi axis !*!) . v3ToM41
--------------------------------------------------------------------------------
-- Matrix helpers
--------------------------------------------------------------------------------
mat4Translate :: Num a => V3 a -> M44 a
mat4Translate = mkTransformationMat identity

mat4Rotate :: (Num a, Epsilon a, Floating a) => a -> V3 a -> M44 a
mat4Rotate phi v = mkTransformation (axisAngle v phi) (V3 0 0 0)

mat4Scale :: Num a => V3 a -> M44 a
mat4Scale (V3 x y z) =
    V4 (V4 x 0 0 0)
       (V4 0 y 0 0)
       (V4 0 0 z 0)
       (V4 0 0 0 1)


instance RealFloat a => Transformable Transform (V2 a) where
    transform = transformV2

instance RealFloat a => Transformable Transform (V3 a) where
    transform = transformV3

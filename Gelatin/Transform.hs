module Gelatin.Transform where

import Control.Monad.Free
import Control.Monad.Free.Church
import Linear
import Graphics.GLUtil.Camera3D (projectionMatrix)

data TransformationDef v next = Translate (V3 v) next
                              | Scale (V3 v) next
                              | Rotate v (V3 v) next

instance Functor (TransformationDef a) where
    fmap f (Translate v n) = Translate v $ f n
    fmap f (Scale v n) = Scale v $ f n
    fmap f (Rotate phi v n) = Rotate phi v $ f n

type Transformation a = F (TransformationDef a)

translate :: V3 a -> Transformation a ()
translate v = liftF $ Translate v ()

scale :: V3 a -> Transformation a ()
scale v = liftF $ Scale v ()

rotate :: a -> V3 a -> Transformation a ()
rotate phi v = liftF $ Rotate phi v ()

compileMatrix :: (Epsilon a, Floating a) => Transformation a () -> M44 a
compileMatrix = compile . fromF
    where compile (Pure ()) = eye4
          compile (Free (Translate v n)) = mkTransformationMat eye3 v !*! compile n
          compile (Free (Rotate phi v n)) = mkTransformation (axisAngle v phi) (V3 0 0 0) !*! compile n
          compile (Free (Scale (V3 x y z) n)) = V4 (V4 x 0 0 0)
                                                   (V4 0 y 0 0)
                                                   (V4 0 0 z 0)
                                                   (V4 0 0 0 1) !*! compile n

mkM44 :: (Epsilon a, Floating a) => Transformation a () -> M44 a
mkM44 = compileMatrix
--------------------------------------------------------------------------------
-- Matrix Helpers
--------------------------------------------------------------------------------
rotateX :: (Epsilon a, Floating a) => a -> Quaternion a
rotateX = axisAngle (V3 1 0 0)

rotateY :: (Epsilon a, Floating a) => a -> Quaternion a
rotateY = axisAngle (V3 0 1 0)

rotateZ :: (Epsilon a, Floating a) => a -> Quaternion a
rotateZ = axisAngle (V3 0 0 1)

transform :: Real a => V3 a -> V3 a -> Quaternion a -> M44 a
transform p (V3 sx sy sz) q = t !*! s !*! q'
    where t  = mkTransformationMat eye3 p
          q' = mkTransformation q $ V3 0 0 0
          s  = V4 (V4 sx 0 0  0)
                  (V4 0 sy 0  0)
                  (V4 0  0 sz 0)
                  (V4 0  0 0  1)

perspective :: (Conjugate a, Epsilon a, RealFloat a)
            => a -> a -> a -> a -> M44 a
perspective = projectionMatrix

ortho :: (Num a, Fractional a) => a -> a -> a -> a -> a -> a -> M44 a
ortho left right top bottom near far =
    V4 (V4 (2/(right-left)) 0 0 (-(right+left)/(right-left)) )
       (V4 0 (2/(top-bottom)) 0 (-(top+bottom)/(top-bottom)) )
       (V4 0 0 (-2/(far-near)) (-(far+near)/(far-near)) )
       (V4 0 0 0 1)

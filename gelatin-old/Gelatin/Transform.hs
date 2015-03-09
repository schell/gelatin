{-# LANGUAGE GADTs #-}
module Gelatin.Transform where

import Control.Monad.Free
import Control.Monad.Free.Church
import Linear hiding (perspective)
import Graphics.GLUtil.Camera3D (projectionMatrix)

--------------------------------------------------------------------------------
-- Types
--------------------------------------------------------------------------------
data TransformationDef next where
    Translate :: (Real a, Fractional a) => V3 a -> next -> TransformationDef next
    Scale :: (Real a, Fractional a) => V3 a -> next -> TransformationDef next
    Rotate :: (Real a , Fractional a) => a -> V3 a -> next -> TransformationDef next

instance Functor TransformationDef where
    fmap f (Translate v n) = Translate v $ f n
    fmap f (Scale v n) = Scale v $ f n
    fmap f (Rotate phi v n) = Rotate phi v $ f n

type Transformation = F TransformationDef
--------------------------------------------------------------------------------
-- Building transformations.
--------------------------------------------------------------------------------
translate :: (Real a, Fractional a) => V3 a -> Transformation ()
translate v = liftF $ Translate v ()

scale :: (Real a, Fractional a) => V3 a -> Transformation ()
scale v = liftF $ Scale v ()

rotate :: (Real a, Fractional a) => a -> V3 a -> Transformation ()
rotate phi v = liftF $ Rotate phi v ()

compileMatrix :: (Epsilon a, Floating a) => Transformation () -> M44 a
compileMatrix = compile . fromF
    where compile (Pure ()) = eye4
          compile (Free (Translate v n)) = let v' = fmap realToFrac v in
              mkTransformationMat eye3 v' !*! compile n
          compile (Free (Rotate phi v n)) = let v'   = fmap realToFrac v
                                                phi' = realToFrac phi in
              mkTransformation (axisAngle v' phi') (V3 0 0 0) !*! compile n
          compile (Free (Scale v n)) = let (V3 x y z) = fmap realToFrac v in
              V4 (V4 x 0 0 0)
                 (V4 0 y 0 0)
                 (V4 0 0 z 0)
                 (V4 0 0 0 1) !*! compile n

mkM44 :: (Epsilon a, Floating a) => Transformation () -> M44 a
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

--perspective :: (Epsilon a, RealFloat a, Conjugate a) => a -> a -> a -> a -> M44 a
--perspective = projectionMatrix

ortho :: (Num a, Fractional a) => a -> a -> a -> a -> a -> a -> M44 a
ortho left right top bottom near far =
    V4 (V4 (2/(right-left)) 0 0 (-(right+left)/(right-left)) )
       (V4 0 (2/(top-bottom)) 0 (-(top+bottom)/(top-bottom)) )
       (V4 0 0 (-2/(far-near)) (-(far+near)/(far-near)) )
       (V4 0 0 0 1)

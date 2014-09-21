{-# LANGUAGE GADTs #-}
module Gelatin.Transform where

import Control.Monad.Free
import Control.Monad.Free.Church
import Linear
import Graphics.GLUtil.Camera3D (projectionMatrix)

--------------------------------------------------------------------------------
-- Types
--------------------------------------------------------------------------------
data TransformationDef next where
    Translate :: V3 Double -> next -> TransformationDef next
    Scale :: V3 Double -> next -> TransformationDef next
    Rotate :: Double -> V3 Double -> next -> TransformationDef next

instance Functor TransformationDef where
    fmap f (Translate v n) = Translate v $ f n
    fmap f (Scale v n) = Scale v $ f n
    fmap f (Rotate phi v n) = Rotate phi v $ f n

type Transformation = F TransformationDef
--------------------------------------------------------------------------------
-- Building transformations.
--------------------------------------------------------------------------------
translate :: V3 Double -> Transformation ()
translate v = liftF $ Translate v ()

scale :: V3 Double -> Transformation ()
scale v = liftF $ Scale v ()

rotate :: Double -> V3 Double -> Transformation ()
rotate phi v = liftF $ Rotate phi v ()

compileMatrix :: Transformation () -> M44 Double
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

mkM44 :: Transformation () -> M44 Double
mkM44 = compileMatrix
--------------------------------------------------------------------------------
-- Matrix Helpers
--------------------------------------------------------------------------------
rotateX :: Double -> Quaternion Double
rotateX = axisAngle (V3 1 0 0)

rotateY :: Double -> Quaternion Double
rotateY = axisAngle (V3 0 1 0)

rotateZ :: Double -> Quaternion Double
rotateZ = axisAngle (V3 0 0 1)

perspective :: Double -> Double -> Double -> Double -> M44 Double
perspective = projectionMatrix

ortho :: (Num a, Fractional a) => a -> a -> a -> a -> a -> a -> M44 a
ortho left right top bottom near far =
    V4 (V4 (2/(right-left)) 0 0 (-(right+left)/(right-left)) )
       (V4 0 (2/(top-bottom)) 0 (-(top+bottom)/(top-bottom)) )
       (V4 0 0 (-2/(far-near)) (-(far+near)/(far-near)) )
       (V4 0 0 0 1)

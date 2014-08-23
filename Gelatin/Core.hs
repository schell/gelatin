module Gelatin.Core where

import Linear
import Graphics.Rendering.OpenGL hiding (Color, Fill)
import Control.Monad.Free.Church

--------------------------------------------------------------------------------
-- Types
--------------------------------------------------------------------------------

data Transform a = Transform (V3 a) (V3 a) (Quaternion a) deriving (Show, Eq, Ord)

type Color a = Color4 a

data DrawCommand a next = Fill (Color a) [V3 a] next
                        | Gradient [Color a] [V3 a] next
                        | WithTransform (Transform a) (Drawing a ()) next

type Drawing a = F (DrawCommand a)

--------------------------------------------------------------------------------
-- Classes
--------------------------------------------------------------------------------

class Embedable a where
    embed  :: Fractional b => a -> V3 b

--------------------------------------------------------------------------------
-- Instances
--------------------------------------------------------------------------------

instance Functor (DrawCommand a) where
    fmap f (Fill c vs n) = Fill c vs $ f n
    fmap f (Gradient cs vs n) = Gradient cs vs $ f n
    fmap f (WithTransform t d n) = WithTransform t d $ f n

instance (Real a, Fractional a) => Embedable (V1 a) where
    embed (V1 x) = V3 (realToFrac x) 0 0

instance Real a => Embedable (V2 a) where
    embed (V2 x y) = V3 (realToFrac x) (realToFrac y) 0

instance Real a => Embedable (V3 a) where
    embed = fmap realToFrac

--------------------------------------------------------------------------------
-- Building geometry
--------------------------------------------------------------------------------

-- | Given two points, creates a pair of triangles representing
-- a rectangle.
rectangle :: Num a
          => V2 a -- ^ The top left of the rectangle
          -> V2 a -- ^ The width and height of the rectangle
          -> [V2 a]
rectangle (V2 x y) (V2 w h) = [ tl, bl, br, tl, tr, br ]
    where tl = V2 x y
          tr = V2 (x+w) y
          bl = V2 x (y+h)
          br = V2 (x+w) (y+h)

--------------------------------------------------------------------------------
-- Building transforms
--------------------------------------------------------------------------------

idTransform :: (Epsilon a, Num a, Floating a) => Transform a
idTransform = Transform (V3 0 0 0) (V3 1 1 1) $ axisAngle (V3 0 0 1) 0

tfrm :: (Embedable v, Fractional a) => v -> v -> Quaternion a -> Transform a
tfrm p s q = Transform (embed p) (embed s) q

rotateZ :: (Epsilon a, Floating a) => a -> Quaternion a
rotateZ = axisAngle (V3 0 0 1)

setPosition :: (Embedable v, Fractional a) => Transform a -> v -> Transform a
setPosition (Transform _ s q) p = Transform (embed p) s q

setScale :: (Embedable v, Fractional a) => Transform a -> v -> Transform a
setScale (Transform p _ q) s = Transform p (embed s) q

setRotation :: Transform a -> Quaternion a -> Transform a
setRotation (Transform p s _) q = Transform p s q

--------------------------------------------------------------------------------
-- Building a drawing
--------------------------------------------------------------------------------

fill :: (Embedable v, Fractional a) => Color a -> [v] -> Drawing a ()
fill c vs = liftF $ Fill c (map embed vs) ()

gradient :: (Embedable v, Fractional a) => [Color a] -> [v] -> Drawing a ()
gradient cs vs = liftF $ Gradient cs (map embed vs) ()

withTransform :: Transform a -> Drawing a () -> Drawing a ()
withTransform t d = liftF $ WithTransform t d ()

withPosition :: (Embedable v, Fractional a, Epsilon a, Floating a)
             => v -> Drawing a () -> Drawing a ()
withPosition = withTransform . setPosition idTransform

withScale :: (Embedable v, Fractional a, Epsilon a, Floating a)
          => v -> Drawing a () -> Drawing a ()
withScale = withTransform . setScale idTransform

withRotation :: (Fractional a, Epsilon a, Floating a)
             => Quaternion a -> Drawing a () -> Drawing a ()
withRotation = withTransform . setRotation idTransform

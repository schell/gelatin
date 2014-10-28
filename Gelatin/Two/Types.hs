{-# LANGUAGE GADTs #-}
module Gelatin.Two.Types (
    Rendering2d,
    TwoCommand(..),
    ColorMapping(..),
    clear,
    fill,
    outline,
    solid,
    gradient,
    texture,
    withSize,
    withTransform,
    withPosition,
    withScale,
    withRotation,
) where

import Gelatin.Transform
import Gelatin.Geometry
import Gelatin.Core.TextureCommands
import Control.Monad.Free
import Control.Monad.Free.Church
import Linear hiding (rotate)
--------------------------------------------------------------------------------
-- Building a Rendering2d
--------------------------------------------------------------------------------
-- | TODO: I'm pretty sure we'd like to be able to do things like
--    withTexture (Relative "img/quantum-foam.jpg") $ fill (rectangle 0 0 100 100)
-- What that does I'm not sure of yet.
-- http://hackage.haskell.org/package/Rasterific-0.3/docs/Graphics-Rasterific-Texture.html

clear :: Rendering2d ()
clear = liftF $ Clear ()

fill :: (Real a, Fractional a) => ColorMapping a -> [Primitive (V2 a)] -> Rendering2d ()
fill c ts = liftF $ Fill c ts ()

outline :: (Real a, Fractional a) => ColorMapping a -> [Primitive (V2 a)] -> Rendering2d ()
outline c ps = liftF $ Outline c ps ()

solid :: V4 a -> ColorMapping a
solid = Color

gradient :: (V2 a -> V4 a) -> ColorMapping a
gradient = ColorMapping

texture :: TextureSrc -> (V2 a -> V2 a) -> ColorMapping a
texture = TextureMapping

withSize :: Int -> Int -> Rendering2d () -> Rendering2d ()
withSize w h r = liftF $ Size2d w h r ()

withTransform :: Transformation () -> Rendering2d () -> Rendering2d ()
withTransform t d = liftF $ WithTransform t d ()

withPosition :: (Real a, Fractional a) => V2 a -> Rendering2d () -> Rendering2d ()
withPosition = withTransform . translate . (\(V2 x y) -> V3 x y 0)

withScale :: (Real a, Fractional a) => V2 a -> Rendering2d () -> Rendering2d ()
withScale = withTransform . scale . (\(V2 x y) -> V3 x y 1)

withRotation :: Double -> Rendering2d () -> Rendering2d ()
withRotation r t = withTransform (rotate r $ V3 0 0 1) t
--------------------------------------------------------------------------------
-- Helpers
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
-- Instances
--------------------------------------------------------------------------------
instance Functor TwoCommand where
    fmap f (Clear n) = Clear $ f n
    fmap f (Size2d w h r n) = Size2d w h r $ f n
    fmap f (WithTransform t d n) = WithTransform t d $ f n
    fmap f (Fill c ps n) = Fill c ps $ f n
    fmap f (Outline c ps n) = Outline c ps $ f n
--------------------------------------------------------------------------------
-- Types
--------------------------------------------------------------------------------
data ColorMapping a = Color (V4 a)
                    | ColorMapping (V2 a -> V4 a)
                    | TextureMapping TextureSrc (V2 a -> V2 a)

data TwoCommand next where
    Clear :: next -> TwoCommand next
    Size2d :: Int -> Int -> Rendering2d () -> next -> TwoCommand next
    WithTransform :: Transformation () -> Rendering2d () -> next -> TwoCommand next
    Fill :: (Real a, Fractional a) => ColorMapping a -> [Primitive (V2 a)] -> next -> TwoCommand next
    Outline :: (Real a, Fractional a) => ColorMapping a -> [Primitive (V2 a)] -> next -> TwoCommand next

type Rendering2d = F TwoCommand

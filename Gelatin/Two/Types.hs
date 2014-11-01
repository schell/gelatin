{-# LANGUAGE GADTs #-}
module Gelatin.Two.Types where

import Gelatin.Transform
import Gelatin.Geometry
import Gelatin.Core.TextureCommands
import Control.Monad.Free.Church
import Linear hiding (rotate)

--------------------------------------------------------------------------------
-- Helpers
--------------------------------------------------------------------------------
-- | TODO: Add ColorMapping gradient functions for convenience.
-- linear
-- radial
-- colorAtPoint
--------------------------------------------------------------------------------
-- Instances
--------------------------------------------------------------------------------
instance Functor TwoCommand where
    fmap f (Render2d io n) = Render2d io $ f n
    fmap f (Clear n) = Clear $ f n
    fmap f (WithSize w h r n) = WithSize w h r $ f n
    fmap f (WithTransform t d n) = WithTransform t d $ f n
    fmap f (Fill c ps n) = Fill c ps $ f n
    fmap f (Outline c ps n) = Outline c ps $ f n
    --fmap f (Stroke c ps n) = Stroke c ps $ f n
--------------------------------------------------------------------------------
-- Types
--------------------------------------------------------------------------------
data ColorMapping a = Color (V4 a)
                    | ColorMapping (V2 a -> V4 a)
                    | TextureMapping TextureSrc (V2 a -> V2 a)

data TwoCommand next where
    Clear :: next -> TwoCommand next
    Render2d :: IO () -> next -> TwoCommand next
    WithSize :: Int -> Int -> Rendering2d () -> next -> TwoCommand next
    WithTransform :: Transformation () -> Rendering2d () -> next -> TwoCommand next
    Fill :: (Real a, Fractional a, Floating a) => ColorMapping a -> [Primitive V2 a] -> next -> TwoCommand next
    Outline :: (Real a, Fractional a, Floating a) => ColorMapping a -> [Primitive V2 a] -> next -> TwoCommand next
    --Stroke :: (Real a, Fractional a) => ColorMapping a -> [Primitive (V2 a)] -> next -> TwoCommand next

type Rendering2d = F TwoCommand

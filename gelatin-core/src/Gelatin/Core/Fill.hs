{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Gelatin.Core.Fill (
    -- * Smart Constructors
    solid,
    fromColors,
    fromUVs,
    -- * Quering colors/uvs from a map
    lookupColor,
    -- * Types
    Fill(..),
    ColorMap(..),
    TextureMap(..),
) where

import Gelatin.Core.Color
import Gelatin.Core.Bounds
import Gelatin.Core.Transform
import Data.Hashable
import qualified Data.Map.Strict as M
import Data.Map (Map)
import Data.List (sortBy, sort)
import GHC.Generics
import Linear
--------------------------------------------------------------------------------
-- Types
--------------------------------------------------------------------------------
newtype ColorMap = ColorMap { unColorMap :: Map (V2 Float) (V4 Float) }
                 deriving (Show, Eq, Generic)

instance Hashable ColorMap where
    hashWithSalt s m = s `hashWithSalt` M.toList (unColorMap m)

newtype TextureMap = TextureMap { unTextureMap :: Map (V2 Float) (V2 Float) }
                   deriving (Show, Eq, Generic)

instance Hashable TextureMap where
    hashWithSalt s m = s `hashWithSalt` M.toList (unTextureMap m)

data Fill = FillColor ColorMap
          | FillTexture FilePath TextureMap
          deriving (Show, Eq, Generic)
instance Hashable Fill

instance Transformable Transform Fill where
    transform t (FillColor cm) = 
        FillColor $ ColorMap $ M.mapKeys (transform t) $ unColorMap cm
    transform t (FillTexture fp tm) = 
        FillTexture fp $ TextureMap $ M.mapKeys (transform t) $ unTextureMap tm

--------------------------------------------------------------------------------
-- Helpers
--------------------------------------------------------------------------------
lowerBoundary :: Ord a => V2 a -> (V2 a -> a) -> [V2 a] -> Maybe (V2 a)
lowerBoundary v f vs = 
    case sortBy (flip compare) $ filter (\v' -> f v' <= f v) vs of
        [] -> Nothing
        a:_ -> Just a

upperBoundary :: Ord a => V2 a -> (V2 a -> a) -> [V2 a] -> Maybe (V2 a)
upperBoundary v f vs =
    case sort $ filter (\v' -> f v' >= f v) vs of
        [] -> Nothing
        a:_ -> Just a

lookupColorComp :: (Additive f, Fractional a, Floating a, Ord a)
                => Map (V2 a) (f a) -> V2 a -> (V2 a -> a) 
                -> Maybe (f a)
lookupColorComp m v f = mc
    where vs = M.keys m 
          vlow  = lowerBoundary v f vs
          vhigh = upperBoundary v f vs 
          mc = case (vlow,vhigh) of
                   (Nothing,Just high)  -> M.lookup high m
                   (Just low,Nothing)   -> M.lookup low m
                   (Just low,Just high) -> lerp (distance low v / distance low high)
                                                <$> M.lookup low m
                                                <*> M.lookup high m
--------------------------------------------------------------------------------
-- Making an interpolated color/uv lookup
--------------------------------------------------------------------------------
lookupColor :: (Additive f, Fractional a, Floating a, Ord a)
            => Map (V2 a) (f a) -> V2 a -> Maybe (f a)
lookupColor m v = lerp 0.5 <$> lookupColorComp m v x
                           <*> lookupColorComp m v y
    where x (V2 a _) = a
          y (V2 _ a) = a
--------------------------------------------------------------------------------
-- Smart constructors
--------------------------------------------------------------------------------
solid :: Color -> Fill
solid = FillColor . ColorMap . M.fromList . (:[]) . (V2 0 0,)

fromColors :: [(V2 Float, V4 Float)] -> Fill
fromColors = FillColor . ColorMap . M.fromList

fromUVs :: FilePath -> [(V2 Float, V2 Float)] -> Fill
fromUVs fp = FillTexture fp . TextureMap . M.fromList 

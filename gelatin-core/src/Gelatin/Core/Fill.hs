module Gelatin.Core.Fill where

import Gelatin.Core.Color
import Gelatin.Core.Common
import Data.Hashable
import Linear
--------------------------------------------------------------------------------
-- Types
--------------------------------------------------------------------------------
data Fill a = FillColor { fillUid :: Uid
                        , fillColorMap :: (V2 Float -> V4 Float)
                        }
            | FillTexture { fillUid :: Uid
                          , fillTexture :: a
                          , fillTextureMap :: (V2 Float -> V2 Float)
                          }

instance Show a => Show (Fill a) where
    show (FillColor uid _) = "FillColor{ " ++ show uid ++ ", V2 Float -> V4 Float }"
    show (FillTexture uid fp _) = "FillTexture{ " ++ show uid ++ ", " ++ show fp ++ " (V2 Float -> V2 Float) }"

instance Hashable a => Hashable (Fill a) where
  hashWithSalt s (FillColor uid _) = hashWithSalt s uid
  hashWithSalt s (FillTexture uid t _) = s `hashWithSalt` uid `hashWithSalt` t
--------------------------------------------------------------------------------
-- Smart constructors
--------------------------------------------------------------------------------

solid :: Color -> Fill a
solid = FillColor (Uid 0) . const

module Gelatin.Core.Fill where

import Gelatin.Core.Color
import Gelatin.Core.Bounds
import Gelatin.Core.Transform
import Data.ByteString (ByteString)
import Data.Hashable
import qualified Data.Map.Strict as M
import Data.Map (Map)
import Data.List (sortBy, sort)
import GHC.Generics
import Linear

--------------------------------------------------------------------------------
-- Types
--------------------------------------------------------------------------------
data Fill a = FillColor (V2 Float -> V4 Float)
            | FillTexture a (V2 Float -> V2 Float)

instance Show a => Show (Fill a) where
    show (FillColor _) = "FillColor{ V2 Float -> V4 Float }"
    show (FillTexture fp _) = "FillTexture{ " ++ show fp ++ " (V2 Float -> V2 Float) }"
--------------------------------------------------------------------------------
-- Smart constructors
--------------------------------------------------------------------------------

solid :: Color -> Fill a
solid = FillColor . const

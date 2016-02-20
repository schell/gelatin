{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveGeneric #-}
module Gelatin.GL (
    fontyData,
    -- * Re-exports
    module G,
    module Picture,
    module GL,
    module Linear,
    module Renderable,
) where

import Gelatin.GL.Renderer as G
import Gelatin.GL.Shader as G
import Gelatin.GL.Primitives as G
import Gelatin.GL.Common as G
import Gelatin.Picture as Picture
import Gelatin.PicturePrimitives as Picture
import Control.Monad
import Control.Arrow (second)
import Data.Renderable as Renderable
import Data.Hashable
import Graphics.Text.TrueType 
import Graphics.GL.Types as GL
import Graphics.GL.Core33 as GL
import Linear hiding (rotate)
import System.Exit
import GHC.Generics

instance Primitive (Painted Primitives) where
    type PrimM (Painted Primitives) = IO
    type PrimR (Painted Primitives) = Rez
    type PrimT (Painted Primitives) = Transform
    canAllocPrimitive _ (Stroked _ p) = not $ null $ primToPaths p
    canAllocPrimitive _ _ = True 
    compilePrimitive = renderPaintedPrimitives 
    
deriving instance Generic FontStyle
instance Hashable FontStyle
deriving instance Generic FontDescriptor
instance Hashable FontDescriptor

-- | Provide a FontData for a given FontyFruity TrueType Font.
fontyData :: Font -> FontData
fontyData font = FontData { fontStringBoundingBox = boundingBox
                          , fontStringCurves = fontCurves font
                          , fontStringGeom = fontGeom font
                          , fontHash = \s -> hashWithSalt s $ descriptorOf font 
                          , fontShow = show $ descriptorOf font
                          }
    where boundingBox dpi px str = unBox $ stringBoundingBox font dpi 
                                                                  (PointSize px) 
                                                                  str
          unBox (BoundingBox xn yn xx yx _) = (V2 xn yn, V2 xx yx) 

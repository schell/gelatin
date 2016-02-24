{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Gelatin.GL (
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
import Linear hiding (rotate, trace)
import System.Exit

instance Primitive PaintedPrimitives where
    type PrimM PaintedPrimitives = IO
    type PrimR PaintedPrimitives = Rez
    type PrimT PaintedPrimitives = Transform
    canAllocPrimitive _ (Stroked _ p) = not $ null $ primToPaths p
    canAllocPrimitive _ _ = True
    compilePrimitive = renderPaintedPrimitives

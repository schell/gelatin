{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveGeneric #-}
module Gelatin.GL (
    Rez(..),
    fontyData,
    -- * Re-exports
    module G,
    module Picture,
    module GL,
    module Linear,
    module Renderable,
) where

import Gelatin.Picture as Picture
import Gelatin.PicturePrimitives as Picture
import Gelatin.GL.Renderer as G
import Gelatin.GL.Shader as G
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

data Rez = Rez { rezShader  :: SumShader
               , rezContext :: Context
               }

instance Primitive (Painted Primitives) where
    type PrimM (Painted Primitives) = IO
    type PrimR (Painted Primitives) = Rez
    type PrimT (Painted Primitives) = Transform
    canAllocPrimitive _ (Stroked _ p) = not $ null $ primToPaths p
    canAllocPrimitive _ _ = True 

    compilePrimitive (Rez sh win) (Stroked (Stroke c cs w f cp) p) = do
        let ps = primToPaths p
            shader = shProjectedPolyline sh
        rs <- forM ps $ \(Path vs) -> do
            let cs' = if null cs then repeat c else gradient
                gradient = [deCasteljau n cs | n <- map (/len) [0..len - 1]]
                len = realToFrac $ length vs
            projectedPolylineRenderer win shader w f cp vs cs'
        return $ foldl appendRenderer emptyRenderer rs

    compilePrimitive (Rez sh win) (Filled fill (TextPrims font dpi px str)) = do
            let gsh = shGeometry sh
                bsh = shBezier sh
                clr = if null (fillColors fill) 
                      then fillColor fill
                      else head $ fillColors fill
            -- | TODO: Fix the fill situation. It sucks.
            colorFontRenderer win gsh bsh font dpi px str clr
    compilePrimitive (Rez sh win) (Filled fill (BezierPrims bs)) = do
        let bsh = shBezier sh
        filledBezierRenderer win bsh bs fill
    compilePrimitive (Rez sh win) (Filled fill (TrianglePrims ts)) = do
        let gsh = shGeometry sh
        filledTriangleRenderer win gsh ts fill
    compilePrimitive (Rez sh win) (Filled fill (PathPrims ps)) = do
        -- We use a filled concave polygon technique instead of
        -- triangulating the path.
        -- http://www.glprogramming.com/red/chapter14.html#name13
        let gsh = shGeometry sh
            tss = map path2ConcavePoly ps
        rs <- forM tss $ \ts -> do
            (c,f) <- filledTriangleRenderer win gsh ts fill
            return (c,\t -> stencilMask (f t) (f t))
        return $ foldl appendRenderer emptyRenderer rs

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

--
--instance Hashable Font where
--    hashWithSalt s = hashWithSalt s . descriptorOf
--
--instance FontClass Font where
--    stringBoundingBox font dpi px str = (V2 nx ny, V2 xx xy)
--        where BoundingBox nx ny xx xy _ =
--                TT.stringBoundingBox font dpi (PointSize px) str

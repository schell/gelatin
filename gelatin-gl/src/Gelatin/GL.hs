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
    -- * Renderable
    paintedPrimitivesRenderStrategy
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

renderPaintedPrimitives :: Rez -> PaintedPrimitives -> IO GLRenderer
renderPaintedPrimitives (Rez sh win) (Stroked (Stroke sf w f cp) p) = do
        let ps = primToPaths p
            shader = shProjectedPolyline sh
        rs <- forM ps $ \(Path vs) ->
            filledPolylineRenderer win shader sf w f cp vs
        return $ foldl appendRenderer emptyRenderer rs
renderPaintedPrimitives (Rez sh win) (Filled fill (TextPrims font dpi px str)) = do
    let gsh = shGeometry sh
        bsh = shBezier sh
    filledFontRenderer win gsh bsh font dpi px str fill
renderPaintedPrimitives (Rez sh win) (Filled fill (BezierPrims bs)) = do
    let bsh = shBezier sh
    filledBezierRenderer win bsh bs fill
renderPaintedPrimitives (Rez sh win) (Filled fill (TrianglePrims ts)) = do
    let gsh = shGeometry sh
    filledTriangleRenderer win gsh ts fill
renderPaintedPrimitives (Rez sh win) (Filled fill (PathPrims ps)) = do
    -- Here we use a filled concave polygon technique instead of
    -- triangulating the path.
    -- http://www.glprogramming.com/red/chapter14.html#name13
    let gsh = shGeometry sh
        tss = map path2ConcavePoly ps
    rs <- forM tss $ \ts -> do
        (c,f) <- filledTriangleRenderer win gsh ts fill
        return (c,\t -> stencilMask (f t) (f t))
    return $ foldl appendRenderer emptyRenderer rs

canAlloc :: Rez -> PaintedPrimitives -> Bool
canAlloc _ (Stroked _ p) = not $ null $ primToPaths p
canAlloc _ _ = True

compile :: Rez -> PaintedPrimitives -> IO (Renderer IO Transform)
compile = renderPaintedPrimitives

paintedPrimitivesRenderStrategy :: RenderStrategy IO Transform Rez PaintedPrimitives
paintedPrimitivesRenderStrategy = RenderStrategy
    { canAllocPrimitive = canAlloc
    , compilePrimitive = compile
    }

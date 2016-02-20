module Gelatin.GL.Primitives where

import Gelatin.Picture
import Gelatin.GL.Renderer
import Gelatin.GL.Shader
import Gelatin.GL.Common
import Control.Monad
import Data.Renderable

renderPaintedPrimitives :: Rez -> Painted Primitives -> IO GLRenderer 
renderPaintedPrimitives (Rez sh win) (Stroked (Stroke c cs w f cp) p) = do
        let ps = primToPaths p
            shader = shProjectedPolyline sh
        rs <- forM ps $ \(Path vs) -> do
            let cs' = if null cs then repeat c else gradient
                gradient = [deCasteljau n cs | n <- map (/len) [0..len - 1]]
                len = realToFrac $ length vs
            projectedPolylineRenderer win shader w f cp vs cs'
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


{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
module Gelatin.GLFW where

import Gelatin.GL.Renderer
import Gelatin.GL.Shader
import Gelatin.Core
import Control.Monad
import Data.Renderable
import Graphics.Text.TrueType
import Graphics.UI.GLFW
import Linear

data Rez = Rez { rezShader :: SumShader
               , rezWindow :: Window
               }

stringRenderer :: Window -> GeomShader -> BezShader -> Font
                -> String -> Color -> (Float,Float) -> IO GLRenderer
stringRenderer win geom bz font str fc xy = do
    -- Some docs
    let mult = 2 :: Float
        movv = 1/4 :: Double
        movh = 1/2 :: Double
        px   = mult*16
        fstr = FontString font px xy str
        fc'  = fc `alpha` 0.25
        t    = Transform 0 (V2 (1/mult) (1/mult)) 0
    (c1, r1) <- colorFontRenderer win geom bz fstr $ const fc'
    (c2, r2) <- colorFontRenderer win geom bz fstr $ const fc
    let f t' = do r1 (translate (-movh) 0 t')
                  r1 (translate movh 0 t')
                  r1 (translate 0 movv t')
                  r1 (translate 0 (-movv) t')
                  r2 t'
    return $ transformRenderer t (c1 >> c2, f)

toPaths :: PathPrimitives Font -> [Path (V2 Float)]
toPaths (Paths ps) = ps
toPaths (PathText f px str) =
    let qs = fontCurves 72 f px str
        sub = subdivideAdaptive 100 0
        mkPath = Path . cleanSeqDupes . concat . fmap sub
        in concat $ fmap (fmap mkPath) qs

instance Primitive (Stroked (PathPrimitives Font)) where
    type PrimM (Stroked (PathPrimitives Font)) = IO
    type PrimR (Stroked (PathPrimitives Font)) = Rez
    type PrimT (Stroked (PathPrimitives Font)) = Transform
    canAllocPrimitive _ (Stroked _ p) = not $ null $ toPaths p
    compilePrimitive (Rez sh win) (Stroked (Stroke c cs w f cp) p) = do
        let ps = toPaths p
            shader = shProjectedPolyline sh
        rs <- forM ps $ \(Path vs) -> do
            let cs' = if null cs then repeat c else gradient
                gradient = [deCasteljau n cs | n <- map (/len) [0..len - 1]]
                len = realToFrac $ length vs
            projectedPolylineRenderer win shader w f cp vs cs'
        return $ foldl appendRenderer emptyRenderer rs


instance Primitive (FillPrimitives Font) where
    type PrimM (FillPrimitives Font) = IO
    type PrimR (FillPrimitives Font) = Rez
    type PrimT (FillPrimitives Font) = Transform
    canAllocPrimitive _ _ = True
    compilePrimitive (Rez sh win) (FillText fill font px str)
        | FillColor f <- fill = do
            let gsh = shGeometry sh
                bsh = shBezier sh
            colorFontRenderer win gsh bsh (FontString font px (0,0) str) f
        -- TODO: FillText with texture fill
        | otherwise = return (return (), const $ return ())
    compilePrimitive (Rez sh win) (FillBeziers fill bs) = do
        let bsh = shBezier sh
        filledBezierRenderer win bsh bs fill
    compilePrimitive (Rez sh win) (FillTriangles fill ts) = do
        let gsh = shGeometry sh
        filledTriangleRenderer win gsh ts fill
    compilePrimitive (Rez sh win) (FillPaths fill ps) = do
        -- We use a filled concave polygon technique instead of
        -- triangulating the path.
        -- http://www.glprogramming.com/red/chapter14.html#name13
        let gsh = shGeometry sh
            tss = map path2ConcavePoly ps
        rs <- forM tss $ \ts -> do
            (c,f) <- filledTriangleRenderer win gsh ts fill
            return (c,\t -> stencilMask (f t) (f t))
        return $ foldl appendRenderer emptyRenderer rs

module Gelatin.GLFW where

import Gelatin.GL.Renderer
import Gelatin.GL.Shader
import Gelatin.Core
import Data.Renderable
import Graphics.Text.TrueType
import Graphics.UI.GLFW
import Linear

data Rez = Rez { rezShader :: SumShader
               , rezWindow :: Window
               }

stringRendering :: Window -> GeomShader -> BezShader -> Font
                -> String -> Color -> (Float,Float) -> IO Rendering
stringRendering win geom bz font str fc xy = do
    -- Some docs
    let mult = 2 :: Float
        movv = 1/4 :: Double
        movh = 1/2 :: Double
        px   = mult*16
        fstr = FontString font px xy str
        fc'  = fc `alpha` 0.25
        t    = Transform 0 (V2 (1/mult) (1/mult)) 0
    (c1, r1) <- colorFontRendering win geom bz fstr $ const fc'
    (c2, r2) <- colorFontRendering win geom bz fstr $ const fc
    let f t' = do r1 (translate (-movh) 0 t')
                  r1 (translate movh 0 t')
                  r1 (translate 0 movv t')
                  r1 (translate 0 (-movv) t')
                  r2 t'
    return $ transformRendering t $ (c1 >> c2, f)

instance Primitive (R2Primitives Font) where
    type PrimM (R2Primitives Font) = IO
    type PrimR (R2Primitives Font) = Rez
    type PrimT (R2Primitives Font) = Transform
    canAllocPrimitive _ (Stroked _ p) = not $ null $ toPaths p
    compilePrimitive (Rez sh win) (Stroked (Stroke c cs w f cp) p) = do
        let ps = toPaths p
            shader = _shProjectedPolyline sh
        rs <- forM ps $ \(Path vs) -> do
            let cs' = if null cs then repeat c else gradient
                gradient = [deCasteljau n cs | n <- map (/len) [0..len - 1]]
                len = realToFrac $ length vs
            projectedPolylineRendering win shader w f cp vs cs'
        let Rendering a b = foldl (<>) mempty rs
        return (b, a)


instance Primitive FillPrimitives where
    type PrimM FillPrimitives = IO
    type PrimR FillPrimitives = Rez
    type PrimT FillPrimitives = Transform
    canAllocPrimitive _ _ = True
    compilePrimitive (Rez sh win) (FillText fill font px str)
        | FillColor f <- fill = do
            let gsh = _shGeometry sh
                bsh = _shBezier sh
            Rendering r c <- colorFontRendering win gsh bsh
                                                (FontString font px (0,0) str)
                                                f
            return (c, r)
        -- TODO: FillText with texture fill
        | otherwise = return (return (), const $ return ())
    compilePrimitive (Rez sh win) (FillBeziers fill bs) = do
        let bsh = _shBezier sh
        Rendering f c <- filledBezierRendering win bsh bs fill
        return (c, f)
    compilePrimitive (Rez sh win) (FillTriangles fill ts) = do
        let gsh = _shGeometry sh
        Rendering f c <- filledTriangleRendering win gsh ts fill
        return (c, f)
    compilePrimitive (Rez sh win) (FillPaths fill ps) = do
        -- We use a filled concave polygon technique instead of
        -- triangulating the path.
        -- http://www.glprogramming.com/red/chapter14.html#name13
        let gsh = _shGeometry sh
            tss = map path2ConcavePoly ps
        rs <- forM tss $ \ts -> do
            Rendering f c <- filledTriangleRendering win gsh ts fill
            return $ Rendering (\t -> stencilMask (f t) (f t)) c
        let Rendering f c = foldl (<>) mempty rs
        return (c,f)

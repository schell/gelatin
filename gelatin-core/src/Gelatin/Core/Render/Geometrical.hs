module Gelatin.Core.Render.Geometrical (
    toLines,
    toArrows,
    toBeziers,
    scale,
    translate,
    rotate,
    mat4Translate,
    mat4Rotate,
    mat4Scale
) where

import Gelatin.Core.Triangulation.Common
import Gelatin.Core.Render.Types

toLines :: [a] -> [Line a]
toLines (a:b:cs) = Line a b : toLines (b:cs)
toLines _ = []

toArrows :: Floating a => [V2 a] -> [Line (V2 a)]
toArrows (a:b:cs) = arrow ++ toArrows (b:cs)
    where arrow = [ Line a b
                  , Line (b - u*l + n * w) b
                  , Line (b - u*l + n * (-w)) b
                  ]
            where n = signorm $ perp $ b - a
                  u = signorm $ b - a
                  l = 5 -- head length
                  w = 3 -- head width
toArrows _ = []

toBeziers :: (Fractional a, Ord a) => [V2 a] -> [Bezier (V2 a)]
toBeziers (a:b:c:ps) = Bezier (compare (triangleArea a b c) 0) a b c : toBeziers (c:ps)
toBeziers _ = []

--------------------------------------------------------------------------------
-- Transformation helpers
--------------------------------------------------------------------------------
scale :: RealFrac a => a -> a -> Transform -> Transform
scale sx sy t@Transform{tfrmScale = V2 x y} =
    t{tfrmScale = V2 (sx'*x) (sy'*y)}
        where [sx',sy'] = map realToFrac [sx,sy]

translate :: RealFrac a => a -> a -> Transform -> Transform
translate tx ty t@Transform{tfrmTranslation = V2 x y} =
    t{tfrmTranslation = V2 (x+tx') (y+ty')}
        where [tx',ty'] = map realToFrac [tx,ty]

rotate :: RealFrac a => a -> Transform -> Transform
rotate r' t@Transform{tfrmRotation = r} = t{tfrmRotation = r + realToFrac r'}

--------------------------------------------------------------------------------
-- Matrix helpers
--------------------------------------------------------------------------------
mat4Translate :: Num a => V3 a -> M44 a
mat4Translate = mkTransformationMat identity

mat4Rotate :: (Num a, Epsilon a, Floating a) => a -> V3 a -> M44 a
mat4Rotate phi v = mkTransformation (axisAngle v phi) (V3 0 0 0)

mat4Scale :: Num a => V3 a -> M44 a
mat4Scale (V3 x y z) =
    V4 (V4 x 0 0 0)
       (V4 0 y 0 0)
       (V4 0 0 z 0)
       (V4 0 0 0 1)


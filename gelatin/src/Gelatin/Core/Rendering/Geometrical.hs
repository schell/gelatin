module Gelatin.Core.Rendering.Geometrical (
    toLines,
    toArrows,
    trisToComp,
    triPoints
) where

import Gelatin.Core.Triangulation.Common
import Gelatin.Core.Rendering.Types
import Linear hiding (rotate)

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


trisToComp :: [Triangle (V2 a)] -> [V2 a]
trisToComp = concatMap triPoints

triPoints :: Triangle (V2 a) -> [V2 a]
triPoints (Triangle a b c) = [a, b, c]



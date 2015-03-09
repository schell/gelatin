module Triangulation.EarClipping where

import Types
import Triangulation.Common
import Linear hiding (trace)

triangulate :: [V2 Float] -> [Triangle Float]
triangulate ps = triangulate' [] $ clean ps
    where triangulate' ts ps'
              | (p1:p2:p3:[]) <- ps' = Triangle p1 p2 p3 :ts
              | (p1:p2:p3:rest) <- ps' =
                  let isReflex = area p1 p2 p3 >= 0
                  in if isReflex && (not $ any (`pointInside` [p1,p2,p3]) rest)
                     then triangulate' (ts ++ [Triangle p1 p2 p3]) $ p1:p3:rest
                     -- Cycle through and check the next triangle
                     else triangulate' ts $ p2:p3:rest ++ [p1]
              | otherwise = ts
          clean = removeHeadTail . removeColinears

removeHeadTail :: Eq a => [a] -> [a]
removeHeadTail xs = if head xs == last xs then init xs else xs

removeColinears :: (Fractional a, Eq a) => [V2 a] -> [V2 a]
removeColinears (a:b:c:ds) = if area a b c == 0
                             then a: (removeColinears $ c:ds)
                             else a:b: (removeColinears $ c:ds)
removeColinears vs = vs

area :: Fractional a => V2 a -> V2 a -> V2 a -> a
area (V2 ax ay) (V2 bx by) (V2 cx cy) =
    0.5 * det33 (V3 (V3 ax ay 1)
                    (V3 bx by 1)
                    (V3 cx cy 1))


module Triangulation.Common where

import Linear
import Control.Lens

type Poly = [V2 Float]

signedArea :: Num a => [V2 a] -> a
signedArea = signedAreaOfPoints

signedAreaOfPoints :: Num a => [V2 a] -> a
signedAreaOfPoints lst =
  sum [x1 * y2 - x2 * y1 | (V2 x1 y1, V2 x2 y2) <- zip lst $ rotateLeft lst]

rotateLeft :: [a] -> [a]
rotateLeft [] = []
rotateLeft (x:xs) = xs ++ [x]

-- | returns True iff the first point of the first polygon is inside the second poylgon
insidePoly :: Poly -> Poly -> Bool
insidePoly poly1 poly2 | null poly1 = False
                       | null poly2 = False
                       | otherwise  = and $ map (`pointInside` poly2) poly1

-- | A point is inside a polygon if it has an odd number of intersections with the boundary (Jordan Curve theorem)
pointInside :: (V2 Float) -> Poly -> Bool
pointInside = flip pathHasPoint

-- | Determine if a point lies within a polygon path using the even/odd
-- rule.
pathHasPoint :: (R1 f, R2 f, Ord a, Fractional a) => [f a] -> f a -> Bool
pathHasPoint [] _ = False
pathHasPoint poly@(p1':_) p' = pointInPath' False p' (poly ++ [p1'])
    where pointInPath' :: (R1 f, R2 f, Ord a, Fractional a) => Bool -> f a -> [f a] -> Bool
          pointInPath' c _ []  = c
          pointInPath' c _ [_] = c
          pointInPath' c p (p1:p2:ps) = pointInPath' (test p p1 p2 $ c) p (p2:ps)
          test :: (R2 f, Ord a, Fractional a) => f a -> f a -> f a -> (Bool -> Bool)
          test p p1 p2 = if t1 p p1 p2 && t2 p p1 p2 then not else id
          t1 :: (R2 f, Ord a) => f a -> f a -> f a -> Bool
          t1 p p1 p2 = (y p2 > y p) /= (y p1 > y p)
          t2 :: (R1 f, R2 f, Ord a, Fractional a) => f a -> f a -> f a -> Bool
          t2 p p1 p2 = x p < (x p1 - x p2) * (y p - y p2) / (y p1 - y p2) + x p2
          x v = v ^. _x
          y v = v ^. _y


-- |return a list containing lists of every element with its neighbour
-- i.e. [e1,e2,e3] -> [ [e1,e2], [e2,e3], [e3, e1] ]
cycleNeighbours :: [a] -> [[a]]
cycleNeighbours xs | null xs = []
                   | otherwise = cycleN (head xs) xs

cycleN :: a -> [a] -> [[a]]
cycleN f xs | length xs >= 2 = cons ([head xs, head (tail xs)]) (cycleN f (tail xs))
            | otherwise      = [[head xs, f]] -- if the upper doesn't match close cycle


triangleArea :: Fractional a => V2 a -> V2 a -> V2 a -> a
triangleArea (V2 x2 y2) (V2 x0 y0) (V2 x1 y1) = (x1-x0)*(y2-y0)-(x2-x0)*(y1-y0)


module Gelatin.Geometry.Triangulation where

import Linear
import Control.Lens
import Gelatin.Geometry.Types

-- | The dirtiest O(n^3) ear clipping I could write.
clipEars :: (Ord a, Fractional a) => [V2 a] -> [Triangle a]
clipEars = clipEars' []
    where clipEars' ts ps'
              | (p1:p2:p3:[]) <- ps' = (p1,p2,p3):ts
              | (p1:p2:p3:ps) <- ps' =
                  if any (pathHasPoint [p1,p2,p3]) ps
                    -- Cycle through and check the next triangle
                    then clipEars' ts $ p2:p3:ps ++ [p1]
                    else clipEars' ((p1,p2,p3):ts) $ p1:p3:ps
              | otherwise = ts

-- | Determine if a point lies within a polygon path using the even/odd
-- rule.
pathHasPoint :: (Ord a, Fractional a) => [V2 a] -> V2 a -> Bool
pathHasPoint [] _ = False
pathHasPoint poly@(p1':_) p' = pointInPath' False p' (poly ++ [p1'])
    where pointInPath' c _ []  = c
          pointInPath' c _ [_] = c
          pointInPath' c p (p1:p2:ps) = pointInPath' (test p p1 p2 $ c) p (p2:ps)
          test p p1 p2 = if t1 p p1 p2 && t2 p p1 p2 then not else id
          t1 p p1 p2 = (y p2 > y p) /= (y p1 > y p)
          t2 p p1 p2 = x p < (x p1 - x p2) * (y p - y p2) / (y p1 - y p2) + x p2
          x v = v ^. _x
          y v = v ^. _y

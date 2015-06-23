module Gelatin.Core.Render.Polylines where

import Gelatin.Core.Render.Types
import Gelatin.Core.Render.Geometrical
import Linear hiding (trace)
import Debug.Trace

-- | The outline at a given thickness of a given line.
polyOutline :: [V2 Float] -> Float -> [V2 Float]
polyOutline ps t = exs ++ reverse ins ++ h
    where js = joins ps t
          (exs,ins) = unzip $ map (uncurry miterLine) $ zip js ps
          h = case exs of
                  h':_ -> [h']
                  _    -> []

polyline :: LineJoin -> Float -> [V2 Float] -> [Triangle (V2 Float)]
polyline LineJoinBevel t ps = polyline LineJoinMiter t ps'
    where ps' = smooth ps
          smooth [] = []
          smooth [a] = [a]
          smooth (a:b:[]) = [a,b]
          smooth (a:b:c:xs) = let (b',b'') = tangentialSmooth t a b c
                              in a:b': (smooth $ b'':c:xs)
polyline LineJoinMiter t ps = concatMap (uncurry miterTriangles) jps'
    where js   = joins ps t
          jps  = zip js ps
          jps' = zip jps $ tail jps

-- | Smooths out a line by emitting new points along the tangents of
-- elbows.
tangentialSmooth :: Float -> V2 Float -> V2 Float -> V2 Float -> (V2 Float, V2 Float)
tangentialSmooth t a b c = (b', b'')
    where tng = tangentOf a b c
          b' = b - (tng ^* t)
          b'' = b + (tng ^* t)

-- | Generates some triangles using a miter join strategy.
miterTriangles :: (Join, V2 Float) -> (Join, V2 Float) -> [Triangle (V2 Float)]
miterTriangles (j1, p1) (j2, p2) = [t1, t2]
    where (a,b) = miterLine j1 p1
          (c,d) = miterLine j2 p2
          t1 = Triangle a b c
          t2 = Triangle b c d

-- | Finds the miter line through a midpoint for a given join.
miterLine :: Join -> V2 Float -> (V2 Float, V2 Float)
miterLine (Join v l _) p = (a,b)
    where a  = p + v'
          b  = p - v'
          v' = (v ^* l)

-- | Finds the cap and miter joins of a polyline. For a polyline of N points
-- there will be N joins.
joins :: [V2 Float] -> Float -> [Join]
joins []  _ = []
joins [_] _ = []
joins [n1,n2] t = let c = capJoin n1 n2 t in [c,c]
joins ps t = start : mid ++ [end]
    where start     = capJoin n1 n2 t
          end       = capJoin n1' n2' t
          mid       = miters ps t
          [n1,n2]   = take 2 ps
          [n1',n2'] = reverse $ take 2 $ reverse $ ps

-- | Finds the miters of a polyline. For a polyline of N points there will
-- be N-2 miters.
miters :: [V2 Float] -> Float -> [Join]
miters (a:b:c:ps) t = join a b c t : miters (b:c:ps) t
miters _ _ = []

-- | Finds the joint of three points with a thickness.
-- A join with a positive angle denotes an elbow that bends
-- counter-clockwise. A join with a negative angle denotes an elbow that
-- bends clockwise.
-- The join with an angle == 0 is the join of two parallel lines.
-- The join with an angle == pi is the join of two opposite but parallel
-- lines, which is used to denote a line cap.
join :: V2 Float -> V2 Float -> V2 Float -> Float -> Join
join a b c t = Join v ln th
    where th = angleBetween (b - a) (c - b)
          tgnt = tangentOf a b c
          v = perp tgnt
          ln = t / (v `dot` n)
          Join n _ _ = capJoin a b t

-- | Finds the cap join of a line with a thickness.
capJoin :: V2 Float -> V2 Float -> Float -> Join
capJoin a b t = Join v t $ angleBetween a b
    where v = signorm $ perp $ b - a

-- | Finds the tangent of an elbow.
tangentOf :: V2 Float -> V2 Float -> V2 Float -> V2 Float
tangentOf a b c = signorm $ (signorm l2) + (signorm l1)
    where l1 = b - a
          l2 = c - b

-- | Finds the angle between two vectors.
angleBetween :: V2 Float -> V2 Float -> Float
angleBetween v1 v2 = a - b
    where V2 x1 y1 = signorm v1
          V2 x2 y2 = signorm v2
          a = atan2 y1 x1
          b = atan2 y2 x2

-- | A join is the angle between two lines, along with another line that
-- runs through the shared point of the two lines - perpendicular to their
-- tangent.
data Join = Join { joinVector :: V2 Float
                 , joinLength :: Float
                 , joinAngle  :: Float
                 } deriving (Show, Eq)


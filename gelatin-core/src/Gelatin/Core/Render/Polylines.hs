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

polyline :: [V2 Float] -> Float -> [Triangle (V2 Float)]
polyline ps t = concatMap (uncurry polylineTriangles) jps'
    where js   = joins ps t
          jps  = zip js ps
          jps' = zip jps $ tail jps

polylineTriangles :: (Join, V2 Float) -> (Join, V2 Float) -> [Triangle (V2 Float)]
polylineTriangles (j1, p1) (j2, p2) = [t1, t2]
    where (a,b) = miterLine j1 p1
          (c,d) = miterLine j2 p2
          t1 = Triangle a b c
          t2 = Triangle b c d

miterLine :: Join -> V2 Float -> (V2 Float, V2 Float)
miterLine (Join v l) p = (a,b)
    where a  = p + v'
          b  = p - v'
          v' = (v ^* l)

-- | Find the cap and miter joins of a polyline. For a polyline of N points
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
miters (a:b:c:ps) t = miterJoin a b c t : miters (b:c:ps) t
miters _ _ = []

-- | Finds the miter line of three points with a thickness.
miterJoin :: V2 Float -> V2 Float -> V2 Float -> Float -> Join
miterJoin a b c t = Join v ln
    where l1 = b - a
          l2 = c - b
          tgnt = signorm $ (signorm l2) + (signorm l1)
          v = perp tgnt
          ln = t / (v `dot` n)
          Join n _ = capJoin a b t

capJoin :: V2 Float -> V2 Float -> Float -> Join
capJoin a b t = Join v t
    where v = signorm $ perp $ b - a

data Join = Join { joinVector :: V2 Float
                 , joinLength :: Float
                 } deriving (Show, Eq)

--------------------------------------------------------------------------------
-- Try #1
--------------------------------------------------------------------------------

testPoints :: [V2 Float]
testPoints = [ V2 0 0
             , V2 100 100
             , V2 0 200
             , V2 100 300
             ]

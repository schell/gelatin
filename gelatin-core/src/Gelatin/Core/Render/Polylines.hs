module Gelatin.Core.Render.Polylines where

import Gelatin.Core.Render.Types
import Gelatin.Core.Render.Geometrical
import Linear hiding (trace)
import Debug.Trace

test :: [Triangle (V2 Float)]
test =
    let vs = [ V2 0 0
             , V2 100 100
             , V2 0 200
             , V2 100 300
             ]
    in case polylineToJoins 5 $ toLines vs of
        Nothing -> []
        Just js -> joinsToTris EndCapButt LineJoinMiter js

joinsToTris :: Fractional a
            => EndCap -> LineJoin -> [Join (V2 a)] -> [Triangle (V2 a)]
joinsToTris _ _ [] = []
joinsToTris _ _ [_] = []
joinsToTris c j (j1:j2:js) = ts ++ joinsToTris c j (j2:js)
    where (a,b) = exitPoints j1
          ts = [Triangle a b m1, Triangle b m1 m2]
          (m1,m2) = exitPoints j2

exitPoints :: Fractional a => Join (V2 a) -> (V2 a, V2 a)
exitPoints e@(Elbow{}) = (a, b)
    where b = p + (d *^ m)
          a = p - (d *^ m)
          d = realToFrac $ elbowMiterLength e
          m = elbowMiterLine e
          (l1,_) = elbowLines e
          (_,p) = lineMetricsPoints l1
exitPoints (Start lm) = (a,b)
    where (a,b,_,_) = lineMetricsCorners lm
exitPoints (End lm) = (c,d)
    where (_,_,c,d) = lineMetricsCorners lm

startCap :: EndCap -> LineMetrics (V2 a) -> [Triangle (V2 a)]
startCap EndCapButt _ = []
startCap _ _ = undefined

endCap :: EndCap -> LineMetrics (V2 a) -> [Triangle (V2 a)]
endCap EndCapButt _ = []
endCap _ _ = undefined

polylineToJoins :: (Eq a, Floating a, Real a)
                 => a -> [Line (V2 a)] -> Maybe [Join (V2 a)]
polylineToJoins t ps@(l1:_)=  (:) <$> start <*> rest
    where start = Just $ End $ lineMetrics t l1
          rest = segmentToJoins t ps
polylineToJoins _ _ = Just []

segmentToJoins :: (Eq a, Floating a, Real a)
          => a -> [Line (V2 a)] -> Maybe [Join (V2 a)]
segmentToJoins t (l1:[]) = Just [End $ lineMetrics t l1]
segmentToJoins t (l1:l2:[]) =
    (:) <$> elbow
        <*> end
        where elbow = joinMetrics (lineMetrics t l1) (lineMetrics t l2)
              end   = Just [End $ lineMetrics t l2]
segmentToJoins t (l1:l2:ls) =
    (:) <$> elbow
        <*> rest
        where elbow = joinMetrics (lineMetrics t l1) (lineMetrics t l2)
              rest  = segmentToJoins t $ l2:ls
segmentToJoins _ _ = Just []

-- | Calculate the join metrics of two line metrics. May result in
-- `Nothing` if the two lines cannot be joined.
joinMetrics :: (Eq a, Floating a, Real a)
            => LineMetrics (V2 a) -> LineMetrics (V2 a)
            -> Maybe (Join (V2 a))
joinMetrics a b =
    case lineMetricsAreConnected a b of
        False -> Nothing
        True  -> Just $ Elbow ls t m ln
            where ls = (a,b)
                  t  = signorm $ l1 + l2
                  l1 = signorm $ lineMetricsLine a
                  l2 = signorm $ lineMetricsLine b
                  m  = perp t
                  d  = realToFrac $ m `dot` (lineMetricsNorm a)
                  ln = (lineMetricsThickness a) / d

lineMetricsAreConnected :: Eq a => LineMetrics a -> LineMetrics a -> Bool
lineMetricsAreConnected a b = p1 == p2
    where p1 = snd $ lineMetricsPoints a
          p2 = fst $ lineMetricsPoints b

data Join a = Elbow { elbowLines       :: (LineMetrics a, LineMetrics a)
                    , elbowTangent     :: a
                    , elbowMiterLine   :: a
                    , elbowMiterLength :: Double
                    }
            | Start { startLine :: LineMetrics a }
            | End { endLine :: LineMetrics a }
            deriving (Show, Eq)

-- | Calculate the bounding box metrics of this line given a thickness.
--
--   b --- d
--   |     |<--thickness/2
--   p0----p1
--   |     |
--   a --- c
lineMetrics :: (Floating a, Real a) => a -> Line (V2 a) -> LineMetrics (V2 a)
lineMetrics thickn (Line p0 p1) = LineMetrics ps l n cs $ realToFrac thickn
    where ps = (p0,p1)
          l = p1 - p0
          n = signorm l
          t = thickn *^ n
          a = p0 - t
          b = p0 + t
          c = p1 - t
          d = p1 + t
          cs = (a,b,c,d)

data LineMetrics a = LineMetrics { lineMetricsPoints    :: (a, a)
                                 , lineMetricsLine      :: a
                                 , lineMetricsNorm      :: a
                                 , lineMetricsCorners   :: (a, a, a, a)
                                 , lineMetricsThickness :: Double
                                 } deriving (Show, Eq)


module Gelatin.Geometry.Triangulation where

import Gelatin.Geometry.Types
import Linear
import Control.Lens

strokePath :: [V2 a] -> [Primitive V2 a]
strokePath = undefined

strokePathWith :: (Ord a, Fractional a, Floating a)
               => a -> StrokeFold a -> [V2 a ] -> [Primitive V2 a]
strokePathWith w f (p1:p2:ps) = strokedGeom $ foldl (f w) i $ toJoins ps
    where i = (t,b, [])
          (t,b,_,_) = perpPoints w p1 p2
strokePathWith _ _ _ = [];

toJoins :: (Ord a, Fractional a) => [V2 a] -> [Join a]
toJoins ps@(a:b:_) = Cap a b : joins ps
    where joins (a':b':c':ps') = elbow a' b' c' : joins (b':c':ps')
          joins (a':b':[]) = [Cap b' a']
          joins _ = []
toJoins _ = []

--------------------------------------------------------------------------------
-- Ways to stroke, names assume a clockwise ordering. Always start with the
-- interior.
--------------------------------------------------------------------------------
squareStroke :: (Ord a, Fractional a) => StrokeFold a
squareStroke w (i,e,prims) (CWElbow p1 p2 p3) = undefined
    where -- The acute lines


--------------------------------------------------------------------------------
-- Helpers
--------------------------------------------------------------------------------
-- | The intersection of the elbow lines displaced `w` units along the
-- perpendicular vector that point towards the outside of the elbow.
-- This assumes that the area is negative for a CCWElbow.
obtuseIntersection :: (Num a, Ord a, Fractional a, Floating a)
                   => a -> Join a -> Maybe (V2 a)
obtuseIntersection _ (Cap _ _) = Nothing
obtuseIntersection w (CCWElbow a b c) = acuteIntersection w $ CWElbow a b c
obtuseIntersection w (CWElbow a b c) = acuteIntersection w $ CWElbow c b a

-- | The intersection of the elbow lines `w` units displaced from the elbow
-- along their perpendicular vector. This assumes that the area of elbow is
-- negative for a CCWElbow.
acuteIntersection :: (Num a, Ord a, Fractional a, Floating a)
                  => a -> Join a -> Maybe (V2 a)
acuteIntersection _ (Cap _ _) = Nothing
acuteIntersection w (CCWElbow a b c) = acuteIntersection w $ CWElbow c b a
acuteIntersection w (CWElbow a b c) =
    lineIntersection (intaba, intabb) (intbcb, intbcc)
      where perpab = w *^ (signorm $ perp $ b - a)
            perpbc = w *^ (signorm $ perp $ c - b)
            intaba = a ^+^ perpab
            intabb = b ^+^ perpab
            intbcb = b ^+^ perpbc
            intbcc = c ^+^ perpbc

-- | The elbow lines `w` units displaced from the elbow along their
-- perpendicular vector. This assumes that the area of elbow is negative for a
-- CCWElbow.
acuteLines :: (Floating a) => a -> Join a -> (Segment a, Segment a)
acuteLines _ (Cap a b) = ((a,b), (b,a))
acuteLines w (CCWElbow a b c) = acuteLines w $ CWElbow c b a
acuteLines w (CWElbow a b c) = ((intaba, intabb), (intbcb, intbcc))
    where perpab = w *^ (signorm $ perp $ b - a)
          perpbc = w *^ (signorm $ perp $ c - b)
          intaba = a ^+^ perpab
          intabb = b ^+^ perpab
          intbcb = b ^+^ perpbc
          intbcc = c ^+^ perpbc


unJoin :: Join a -> [V2 a]
unJoin (CWElbow a b c) = [a, b, c]
unJoin (CCWElbow a b c) = [a, b, c]
unJoin (Cap a b) = [a, b]

elbow :: (Ord a, Fractional a) => V2 a -> V2 a -> V2 a -> Join a
elbow a b c =
    if triangleArea a b c >= 0
    then CWElbow a b c
    else CCWElbow a b c

lineIntersection :: (Num a, Ord a, Fractional a)
                 => Segment a -> Segment a -> Maybe (V2 a)
lineIntersection (V2 x1 y1, V2 x2 y2) (V2 x3 y3, V2 x4 y4) =
    if d == 0 then Nothing else Just $ V2 x y
      where x = ((x1*y2 - y1*x2) * (x3 - x4) - (x1 - x2) * (x3*y4 - y3*x4)) / d
            y = ((x1*y2 - y1*x2) * (y3 - y4) - (y1 - y2) * (x3*y4 - y3*x4)) / d
            d = (x1 - x2) * (y3 - y4) - (y1 - y2) * (x3 - x4)

triangleArea :: (R1 f, R2 f, Fractional a) => f a -> f a -> f a -> a
triangleArea a b c = 0.5 * det33 (V3 (embedWith a 1)
                                     (embedWith b 1)
                                     (embedWith c 1))

perpPoints :: (Fractional a, Floating a) => a -> V2 a -> V2 a -> (V2 a, V2 a, V2 a, V2 a)
perpPoints w a b = (t1, b1, t2, b2)
    where b1 = a ^+^ (w/2 *^ pv)
          b2 = b ^+^ (w/2 *^ pv)
          t1 = a ^+^ (-w/2 *^ pv)
          t2 = b ^+^ (-w/2 *^ pv)
          pv = perp $ signorm $ b - a

interiorAngle :: (R1 f, R2 f, Num a, Ord a, RealFloat a)
              => f a -> f a -> f a -> a
interiorAngle a b c = if ng < 0 then 2 * pi - ng else ng
    where ng = signedAngle a b c

exteriorAngle :: (R1 f, R2 f, Num a, Ord a, RealFloat a)
              => f a -> f a -> f a -> a
exteriorAngle a b c = (2 * pi -) $ interiorAngle a b c

angleTo :: (R1 f, R2 f, RealFloat a) => f a -> f a -> a
angleTo v v' = if isNaN angl then 0 else angl
    where crss = v1 `cross` v2
          sina = norm crss
          cosa = v1 `dot` v2
          angl = atan2 sina cosa
          v1 = signorm $ embed v
          v2 = signorm $ embed v'

signedAngle :: (R1 f, R2 f, Num a, Ord a, RealFloat a)
            => f a -> f a -> f a -> a
signedAngle p1' p2' p3' = sign * angl
    where crss = v1 `cross` v2
          sina = norm crss
          cosa = v1 `dot` v2
          angl = atan2 sina cosa
          sign = if V3 0 0 1 `dot` crss >= 0 then 1 else (-1)
          v1 = signorm $ p2 - p1
          v2 = signorm $ p3 - p2
          [p1,p2,p3] = map embed [p1',p2',p3']

embed :: (R1 f, R2 f, Num a) => f a -> V3 a
embed v = embedWith v 0

embedWith :: (R1 f, R2 f, Num a) => f a -> a -> V3 a
embedWith v a = V3 (v ^. _x) (v ^. _y) a

--------------------------------------------------------------------------------
-- Types
--------------------------------------------------------------------------------
type StrokeFold a = a -> Stroke a -> Join a -> Stroke a

strokedGeom :: Stroke a -> [Primitive V2 a]
strokedGeom (_,_,ps) = ps

type Stroke a = (V2 a, V2 a, [Primitive V2 a])

data Join a = Cap (V2 a) (V2 a)
            | CWElbow (V2 a) (V2 a) (V2 a)
            | CCWElbow (V2 a) (V2 a) (V2 a)
            deriving (Show)

type Segment a = (V2 a, V2 a)

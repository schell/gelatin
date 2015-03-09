module Gelatin.Geometry.Triangulation where

import Gelatin.Geometry.Types
import Linear
import Control.Lens
import Data.Maybe

--------------------------------------------------------------------------------
-- Stroking
--------------------------------------------------------------------------------
strokeWith :: (Num a, Ord a, Fractional a)
           => CapFunc a -> JoinFunc a -> a -> [V2 a] -> [Primitive V2 a]
strokeWith cf jf w ps = joinStrokes $ start ++ middle ++ end
    where start  = startCapWith cf w elbows
          middle = concatMap (jf w) elbows
          end    = endCapWith cf w elbows
          elbows = toElbows ps

toElbows :: (Ord a, Fractional a) => [V2 a] -> [Elbow a]
toElbows (a':b':c':ps') = elbow a' b' c' : toElbows (b':c':ps')
toElbows (_:[_]) = []
toElbows _ = []
--------------------------------------------------------------------------------
-- Joining and capping.
--------------------------------------------------------------------------------
joinStrokes :: Stroke a -> [Primitive V2 a]
joinStrokes [] = []
joinStrokes [_] = []
joinStrokes ((a,b):(c,d):xs) = [Triangle a b c, Triangle b c d] ++ joinStrokes ((c,d):xs)

--        :: a -> Elbow a -> ([Primitive V2 a], V2 a, V2 a)
bevelJoin :: (Ord a, Num a, Fractional a, Floating a) => JoinFunc a
bevelJoin w e = [l1,l2]
    where ((_,o2),(o3,_)) = obtuseLines w e
          iscw  = isCW e
          l1 = if iscw then (i, o2) else (o2, i)
          l2 = if iscw then (i, o3) else (o3, i)
          i = fromJust $ acuteIntersection w e

startCapWith :: Num a => CapFunc a -> a -> [Elbow a] -> Stroke a
startCapWith _ _ [] = []
startCapWith _ _ [_] = []
startCapWith f w (a:_) = f w $ reverseElbow a

endCapWith :: CapFunc a -> a -> [Elbow a] -> Stroke a
endCapWith _ _ [] = []
endCapWith f w [a] = f w a
endCapWith f w (_:ps) = endCapWith f w ps

-- | Adds a flat cap of width `w` at line ab. Assumes that ab points to the
-- cap.
flatCap :: Floating a => CapFunc a
flatCap w e = [(acute, obtuse)]
    where acute = a4
          obtuse = o4
          ((_,_),(_,o4)) = obtuseLines w e
          ((_,_),(_,a4)) = acuteLines w e

squareCap :: Floating a => CapFunc a
squareCap w e = [(acute, obtuse)]
    where acute = a4 ^+^ w *^ (signorm $ a4 - a3)
          obtuse = o4 ^+^ w *^ (signorm $ o4 - o3)
          ((_,_),(o3,o4)) = obtuseLines w e
          ((_,_),(a3,a4)) = acuteLines w e

triangleCap :: Floating a => CapFunc a
triangleCap w e = [(a, b),(a, c)]
    where [_, p2, p3] = unElbow e
          a = a4
          (b,c) = if isCW e then (obtuse, apex) else (apex, obtuse)
          apex = p3 ^+^ w *^ (signorm $ p3 - p2)
          obtuse = o4
          ((_,_),(_,o4)) = obtuseLines w e
          ((_,_),(_,a4)) = acuteLines w e
--------------------------------------------------------------------------------
-- Helpers
--------------------------------------------------------------------------------
-- | The intersection of the elbow lines displaced `w` units along the
-- perpendicular vector that point towards the outside of the elbow.
-- This assumes that the area is negative for a CCWElbow.
obtuseIntersection :: (Num a, Ord a, Fractional a, Floating a)
                   => a -> Elbow a -> Maybe (V2 a)
--obtuseIntersection _ (Cap _ _) = Nothing
obtuseIntersection w (CCWElbow a b c) = acuteIntersection w $ CWElbow a b c
obtuseIntersection w (CWElbow a b c) = acuteIntersection w $ CWElbow c b a

-- | The intersection of the elbow lines `w` units displaced from the elbow
-- along their perpendicular vector. This assumes that the area of elbow is
-- negative for a CCWElbow.
acuteIntersection :: (Num a, Ord a, Fractional a, Floating a)
                  => a -> Elbow a -> Maybe (V2 a)
--acuteIntersection _ (Cap _ _) = Nothing
acuteIntersection w (CCWElbow a b c) = acuteIntersection w $ CWElbow c b a
acuteIntersection w e = lineIntersection la lb
      where (la,lb) = acuteLines w e

reverseLines :: ((a,a),(a,a)) -> ((a,a),(a,a))
reverseLines ((la,lb),(lc,ld)) = ((ld,lc),(lb,la))

-- | The elbow lines `w` units displaced from the elbow along their
-- perpendicular vector that points outward. This assumes that the area of
-- elbow is negative for a CCWElbow.
obtuseLines :: (Floating a) => a -> Elbow a -> (Segment a, Segment a)
obtuseLines w (CCWElbow a b c) = acuteLines w $ CWElbow a b c
obtuseLines w (CWElbow a b c) = reverseLines $ acuteLines w $ CWElbow c b a

-- | The elbow lines `w` units displaced from the elbow along their
-- perpendicular vector. This assumes that the area of elbow is negative for a
-- CCWElbow.
acuteLines :: (Floating a) => a -> Elbow a -> (Segment a, Segment a)
--acuteLines _ (Cap a b) = ((a,b),(b,a))
acuteLines w (CCWElbow a b c) = reverseLines $ acuteLines w $ CWElbow c b a
acuteLines w (CWElbow a b c) = ((intaba, intabb), (intbcb, intbcc))
    where perpab = w *^ (signorm $ perp $ b - a)
          perpbc = w *^ (signorm $ perp $ c - b)
          intaba = a ^+^ perpab
          intabb = b ^+^ perpab
          intbcb = b ^+^ perpbc
          intbcc = c ^+^ perpbc

isCW :: Elbow a -> Bool
isCW (CWElbow _ _ _) = True
isCW _ = False

reverseElbow :: Elbow a -> Elbow a
reverseElbow (CCWElbow a b c) = CWElbow c b a
reverseElbow (CWElbow a b c) = CCWElbow c b a

unElbow :: Elbow a -> [V2 a]
unElbow (CWElbow a b c) = [a, b, c]
unElbow (CCWElbow a b c) = [a, b, c]

elbow :: (Ord a, Fractional a) => V2 a -> V2 a -> V2 a -> Elbow a
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
type StrokeFunc a = a -> Stroke a -> Elbow a -> Stroke a

type JoinFunc a = a -> Elbow a -> Stroke a

type CapFunc a = a -> Elbow a -> Stroke a

type Stroke a = [(V2 a, V2 a)]
--type Stroke a = (V2 a, V2 a, [Primitive V2 a])

data Elbow a = CWElbow (V2 a) (V2 a) (V2 a)
             | CCWElbow (V2 a) (V2 a) (V2 a)
             deriving (Show)

type Segment a = (V2 a, V2 a)

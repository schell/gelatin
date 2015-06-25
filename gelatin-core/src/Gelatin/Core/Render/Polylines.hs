module Gelatin.Core.Render.Polylines where

import Gelatin.Core.Render.Types
import Gelatin.Core.Triangulation.Common (triangleArea)
import Linear hiding (trace)

-- | The outline at a given thickness of a given line.
--polyOutline :: [V2 Float] -> Float -> [V2 Float]
--polyOutline ps t = exs ++ reverse ins ++ h
--    where js = joins t ps
--          (exs,ins) = unzip $ map (uncurry miterLine) $ zip js ps
--          h = case exs of
--                  h':_ -> [h']
--                  _    -> []

polyline :: EndCap -> LineJoin -> Float -> [V2 Float] -> [Triangle (V2 Float)]
polyline c j t ps = triangulate $ joints c j t ps

triangulate :: [Joint] -> [Triangle (V2 Float)]
-- start
triangulate (j@Cap{}:j':js) = cap ++ arm ++ (triangulate $ j':js)
    where cap   = triangulateCap j
          arm   = triangulateArm j j'
-- end
triangulate [j, j'@Cap{}] = arm ++ bow ++ cap
    where arm = triangulateArm j j'
          bow = triangulateElbow j
          cap = triangulateCap j'
triangulate (j:j':js) = arm ++ bow ++ (triangulate $ j':js)
    where arm   = triangulateArm j j'
          bow   = triangulateElbow j
triangulate _ = []

exitLine :: Joint -> (V2 Float, V2 Float)
exitLine (Cap _ ps) = (head ps, head $ reverse ps)
exitLine (Elbow _ l []) = l
exitLine (Elbow Clockwise (p,_) ps) = (p, head $ reverse ps)
exitLine (Elbow CounterCW (_,n) ps) = (head $ reverse ps, n)

entryLine :: Joint -> (V2 Float, V2 Float)
entryLine (Cap _ ps) = (head $ reverse ps, head ps)
entryLine (Elbow _ l []) = l
entryLine (Elbow Clockwise (p,_) ps) = (p, head ps)
entryLine (Elbow CounterCW (_,n) ps) = (head ps, n)

triangulateElbow :: Joint -> [Triangle (V2 Float)]
triangulateElbow (Elbow Clockwise (p,_) ps) = map (uncurry $ Triangle p) $ zip ps $ tail ps
triangulateElbow (Elbow CounterCW (_,n) ps) = map (uncurry $ Triangle n) $ zip ps $ tail ps
triangulateElbow _ = []

triangulateArm :: Joint -> Joint -> [Triangle (V2 Float)]
triangulateArm j j' = [Triangle a b c, Triangle b c d]
    where (a,b) = exitLine j
          (c,d) = entryLine j'

triangulateCap :: Joint -> [Triangle (V2 Float)]
-- This is a butt cap so do nothing.
triangulateCap (Cap p ps) = map (uncurry $ Triangle p) $ zip ps $ tail ps
triangulateCap _ = []

joints :: EndCap -> LineJoin -> Float -> [V2 Float] -> [Joint]
joints _ _ _ [] = []
joints _ _ _ [_] = []
joints c j t ps@(a:b:_) = start : mid ++ [end]
    where start = capFunc c t a b
          end   = capFunc c t z y
          mid   = miters j t ps
          [z,y] = take 2 $ reverse ps

capFunc :: EndCap -> Float -> V2 Float -> V2 Float -> Joint
capFunc EndCapButt t a b = Cap a [lp,hp]
    where (lp,hp) = miterLine (capJoin t a b) a
capFunc EndCapBevel t a b = Cap a [lp,p,hp]
    where (lp,hp) = miterLine (capJoin t a b) a
          p       = a + (signorm $ a - b) ^* t
capFunc EndCapSquare t a b = Cap a [lp,p'',p',hp]
    where (lp,hp) = miterLine (capJoin t a b) a
          p       = a + (signorm $ a - b) ^* t
          p'      = p + ((signorm $ hp - a) ^* t)
          p''     = p + ((signorm $ lp - a) ^* t)
capFunc EndCapRound t a b = Cap a ps
    where ps     = map f [(pi/2) + r + (d * pi/180) | d <- [0..180]]
          V2 x y = signorm $ b - a
          r      = atan2 y x
          f th   = a + (V2 (cos th) (sin th) ^* t)

miters :: LineJoin -> Float -> [V2 Float] -> [Joint]
miters j t (a:b:c:ps) = miterFunc j t a b c : (miters j t $ b:c:ps)
miters _ _ _ = []

miterFunc :: LineJoin -> Float -> V2 Float -> V2 Float -> V2 Float -> Joint
miterFunc LineJoinMiter = miterJoint
miterFunc LineJoinBevel = bevelJoint
--miterFunc LineJoinRound = roundJoint
--
--roundJoint :: Float -> V2 Float -> V2 Float -> V2 Float -> Joint
--roundJoint t a b c =
--    if triangleArea a b c > 0
--    then Elbow Clockwise (p,n) ps
--    else Elbow CounterCW (p,n) $ reverse ps
--    where j       = join t a b c
--          (p,n)   = miterLine j b
--          v'      = t *^ (perp ab)
--          v''     = t *^ (perp bc)
--          ps     = map f [r + d | d <- [0, pi/2, pi]]
--          V2 x y = signorm $ v'' - v'
--          r      = atan2 y x
--          f th   = b + (V2 (cos th) (sin th) ^* (0.5 * distance v'' v'))
--          ab     = signorm $ b - a
--          bc     = signorm $ c - b

bevelJoint :: Float -> V2 Float -> V2 Float -> V2 Float -> Joint
bevelJoint t a b c =
    if triangleArea a b c >= 0
    then Elbow Clockwise (p,n) [b - v', b - v'']
    else Elbow CounterCW (p,n) [b + v', b + v'']
    where j       = join t a b c
          (p,n) = miterLine j b
          v'      = t *^ (perp ab)
          v''     = t *^ (perp bc)
          ab      = signorm $ b - a
          bc      = signorm $ c - b

miterJoint :: Float -> V2 Float -> V2 Float -> V2 Float -> Joint
miterJoint t a b c =
    if (abs $ angleBetween (b - a) (c - b)) < 1
    then bevelJoint t a b c
    else if triangleArea a b c >= 0
         then Elbow Clockwise (ptan,ntan) []
         else Elbow CounterCW (ptan,ntan) []
         where j       = join t a b c
               (ptan,ntan) = miterLine j b

-- | Finds the miter line through a midpoint for a given join.
miterLine :: Join -> V2 Float -> (V2 Float, V2 Float)
miterLine (Join v l) p = (ptan,ntan)
    -- ptan is the point on the miterline in the direction the
    -- perpendicular tangent is pointing. ntan is in the opposite
    -- direction. This means that for clockwise winding elbows ptan
    -- will lie within the bend, on the inside of the elbow, while
    -- ntan will lie outside. This is reversed for elbows winding
    -- counter-clockwise.
    where ptan = p + v'
          ntan = p - v'
          v'   = (v ^* l)

-- | Finds the joint of three points with a thickness.
-- A join with a positive angle denotes an elbow that bends
-- counter-clockwise. A join with a negative angle denotes an elbow that
-- bends clockwise.
-- The join with an angle == 0 is the join of two parallel lines.
-- The join with an angle == pi is the join of two opposite but parallel
-- lines, which is used to denote a line cap.
join :: Float -> V2 Float -> V2 Float -> V2 Float -> Join
join t a b c = Join v ln
    where tgnt = tangentOf a b c
          v = perp tgnt
          ln = min d $ t / (v `dot` n)
          n = signorm $ perp $ b - a
          d = min (distance (c - b) zero) (distance (b - a) zero)

-- | Finds the join of a start or end line with a thickness.
capJoin :: Float -> V2 Float -> V2 Float -> Join
capJoin t a b = Join v t
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

-- | A join is the 'miter line' that runs through the shared point of two lines
-- perpendicular to their tangent.
data Join = Join { joinVector :: V2 Float
                 , joinLength :: Float
                 } deriving (Show, Eq)

{-# LANGUAGE GADTs #-}
module Gelatin.Geometry (
    -- * Primitive types
    Primitive(..),

    -- * Decomposing primitives
    triangleToLines,
    primitiveToLines,
    primitiveToList,
    bezierToList,
    triangulate,

    -- * Tesselation
    strokePrimitives,
    strokeLine,
    minkowskiConvolution,

    -- * Decomposition helpers
    deCasteljau,
    pathHasPoint,

    -- * Composing geometry
    path,
    closePath,
    rectangle,
    circle,
    circlePath,
    polygon,
    triangles,
    curve,
    bezier,
    line,
    lines',
) where

import Linear hiding (trace)
import Control.Lens hiding (pre)
import Debug.Trace

--------------------------------------------------------------------------------
-- Building geometry
--------------------------------------------------------------------------------
-- | Specify a polygon rectangle with an upper left point p, width and height.
rectangle :: (R1 f, R2 f, Num a)
          => f a -- ^ The top left of the rectangle
          -> a    -- ^ The width of the rectangle
          -> a    -- ^ The height of the rectangle
          -> Primitive f a
rectangle p w h = Path [ tl, tr, br, bl, tl ]
    where tl = p
          tr = p & _x %~ (+w)
          bl = p & _y %~ (+h)
          br = p & _x %~ (+w) & _y %~ (+h)

circle :: (R1 f, R2 f, Additive f, Metric f, Fractional a, Floating a, Num a, Ord a, Enum a)
       => f a -> a -> Primitive f a
circle p r = Path $ circlePath p r n
    where n = c / flatness
          c  = 2 * pi * r
          flatness = max (logBase 10 c) 1

circlePath :: (R1 f, R2 f, Additive f, Metric f, Fractional a, Floating a, Num a, Ord a, Enum a)
           => f a -> a -> a -> [f a]
circlePath p r n = bs
    where bs = [ mk (cos t) (sin t) | t <- [0.0,(2*pi/n') .. 2*pi] ]
          n' = max 3 n
          mk x y = mv $ zero & _x .~ x & _y .~ y
          mv v = (v ^* r) ^+^ p

-- | Specify a line from a to b.
line :: f a -> f a -> Primitive f a
line = Line

-- | Specify a path.
path :: [f a] -> Primitive f a
path = Path

-- | Specify a polygon with points.
polygon :: Eq (f a) => [f a] -> Primitive f a
polygon = Path . closePath

closePath :: Eq (f a) => [f a] -> [f a]
closePath [] = []
closePath ps
    | last ps == head ps = ps
    | otherwise = ps ++ [head ps]

-- | Specify a n-bezier curve rasterized in segments with distance <= 1.
curve :: (R1 f, R2 f, Ord a, Fractional a) => [f a] -> Primitive f a
curve = bezier 1.0

-- | Specify a n-bezier curve rasterized in segments with distance <= `n`.
bezier :: (R1 f, R2 f, Ord a, Fractional a) => a -> [f a] -> Primitive f a
bezier n ps
    | length ps <= 1 = Path [] -- Maybe this is bad?
    | p1:p2:[] <- ps = Line p1 p2
    | otherwise = Curve n ps

-- | Turns a list of points representing a polygon into a list of
-- triangle primitives by performing ear clipping.
triangles :: (Metric f, R1 f, R2 f, Ord a, Fractional a, Floating a)
          => [f a] -> [Primitive f a]
triangles = triangulate . Path

-- | Turns a list of points representing a polygon int a list of
-- line primitives.
lines' :: Eq a => [f a] -> [Primitive f a]
lines' = pathToLines

--------------------------------------------------------------------------------
-- Tesselating geometry
--------------------------------------------------------------------------------
strokePrimitives :: (Eq a, Num a, Floating a, Ord a)
                 => a -> [Primitive V2 a] -> [Primitive V2 a]
strokePrimitives w = concatMap (strokeLine w) . concatMap primitiveToLines

strokeLine :: (Eq a, Num a, Floating a, Ord a)
           => a -> Primitive V2 a -> [Primitive V2 a]
strokeLine w (Line a b) = [Triangle p1 p2 p3, Triangle p2 p3 p4]
    where v   = b - a
          u   = signorm $ perp v
          hw  = w * 0.5
          _hw = (-hw)
          {- p1 -- p2 -}
          {- p3 -- p4 -}
          [p1, p2, p3, p4] = [hw *^ u ^+^ a, hw *^ u ^+^ b, _hw *^ u ^+^ a, _hw *^ u ^+^ b]
strokeLine _ _ = []

-- | https://www.dropbox.com/s/iifje9obdvs1bcd/siggraph_realistic2d.pdf?dl=0
type Pen a = [V2 a]
type Shape a = [V2 a]
type Convolution a = [V2 a]

minkowskiConvolution :: (Floating a, Ord a) => Pen a -> Shape a -> Convolution a
minkowskiConvolution pen shape = conv pen $ shape ++ (drop 1 $ reverse shape)
    -- Convolve there and back again
    where cap  = map (last shape +) $ drop hpen pen
          hpen = floor $ (fromIntegral (length pen)) / 2
          conv ps ss@(s1:s2:_) = convolve (cycleToActivePen ps s1 s2) ss []
          conv ps _ = ps

cycleToActivePen :: (Ord a, Floating a) => Pen a -> V2 a -> V2 a -> Pen a
cycleToActivePen ps@(p:p':p'':_) v v' = npen
    where -- The exit tangent
          tExit = signorm $ v' - v
          -- The pen tangent
          tPen = signorm $ p'- p
          -- The next pen tangent
          ntPen = signorm $ p'' - p'
          -- The active region
          a = (180/pi) * (acos $ dot ntPen tPen)
          t = (180/pi) * (acos $ dot tExit tPen)
          c = clk a t
          plen = length ps
          npen = case c of
                     CW  -> take plen $ drop 1 $ cycle ps
                     CCW -> take plen $ drop (plen - 1) $ cycle ps
                     CN  -> ps
cycleToActivePen ps _ _ = ps

convolve :: (Floating a, Ord a)
          => Pen a   -- ^ The pen polygon/path.
          -> Shape a -- ^ The shape path.
          -> Convolution a -- ^ The accumulated convolution.
          -> Convolution a
convolve ps ss cv
    | length ps < 3 = trace "pen limit" cv
    | length ss < 2 = trace "shape limit" cv
    | (p:p':p'':_) <- ps
    , (v:v':shape) <- ss =
    let -- The exit tangent
        tExit = signorm $ v' - v
        -- The pen tangent
        tPen = signorm $ p'- p
        -- The next pen tangent
        ntPen = signorm $ p'' - p'
        -- The active region
        a = (180/pi) * (acos $ dot ntPen tPen)
        t = (180/pi) * (acos $ dot tExit tPen)
        c = clk a t
        plen = length ps
        npen = case c of
                   CW  -> take plen $ drop 1 $ cycle ps
                   CCW -> take plen $ drop (plen - 1) $ cycle ps
                   CN  -> ps
        nshape = if c == CN then v':shape else ss

    in trace "convolve" $ convolve npen nshape $ cv ++ [v + p']
    | otherwise = trace "otherwise" cv

data Clk = CW | CCW | CN deriving Eq

clk :: (Num a, Ord a, Fractional a) => a -> a -> Clk
clk ba ca = if ca < ba
              then CN
              else if ca > t
                     then CCW
                     else CW
    where t = 180 + (ba/2)
--------------------------------------------------------------------------------
-- Decomposing geometry
--------------------------------------------------------------------------------
-- | Turns a path into a list of line primitives.
pathToLines :: Eq a => [f a] -> [Primitive f a]
pathToLines [] = []
pathToLines ps' = toLines' ps'
    where toLines' [] = []
          toLines' [_] = []
          toLines' (p1:p2:ps) = Line p1 p2  : toLines' (p2:ps)

-- | Turns a list of triangles into a list of lines.
triangleToLines :: Primitive f a -> [Primitive f a]
triangleToLines (Triangle a b c) = [ Line a b
                                   , Line b c
                                   , Line c a
                                   ]
triangleToLines _ = []

-- | Converts any primitive to a list of lines.
primitiveToLines :: (Metric f, R1 f, R2 f, Eq a, Num a, Floating a, Ord a)
                 => Primitive f a -> [Primitive f a]
primitiveToLines (Path p)  = pathToLines p
primitiveToLines (Curve n bs) = pathToLines $ bezierToList n bs
primitiveToLines l@(Line _ _) = [l]
primitiveToLines t@(Triangle _ _ _) = triangleToLines t

-- | Converts any primitive to a list of lines.
primitiveToList :: (Metric f, R1 f, R2 f, Num a, Floating a, Ord a)
                => Primitive f a -> [f a]
primitiveToList (Path p) = p
primitiveToList (Triangle a b c) = [a, b, c]
primitiveToList (Curve n bs) = bezierToList n bs
primitiveToList (Line a b) = [a,b]

-- | Converts a curve of beziers into a path of points. Uses `n` as the
-- maximum distance between points along the curve. Another way to say this
-- is that `n` is the "flatness" rating. A larger `n` means bigger line
-- segments.
bezierToList :: (Metric f, R1 f, R2 f, Num a, Floating a, Ord a) => a -> [f a] -> [f a]
bezierToList n bs = [deCasteljau 0 bs] ++ subs ++ [deCasteljau 1 bs]
    where subs = subdivideBezier n 0 1 bs

subdivideBezier :: (Metric f, R1 f, R2 f, Num a, Floating a, Ord a) => a -> a -> a -> [f a] -> [f a]
subdivideBezier n start end bs =
    let p1  = deCasteljau start bs
        p2  = deCasteljau end bs
        d   = distance p1 p2
        p3  = deCasteljau mid bs
        mid = start + (end - start) / 2
        pre = subdivideBezier n start mid bs
        suf = subdivideBezier n mid end bs
        in if d <= n
            then []
            else pre ++ [p3] ++ suf
-- Start with interpoltions at start = 0, end = 1
-- Get the points along the curve at start and end
-- Add points inbetween until the distance between two new points is less
--   than or equal to n.

-- | Compute the point at `t` along an N-bezier curve.
deCasteljau :: (Additive f, R1 f, R2 f, Num a) => a -> [f a] -> f a
deCasteljau _ [b] = b
deCasteljau t coefs = deCasteljau t reduced
  where
    reduced = zipWith (flip (lerp t)) coefs (tail coefs)
    --lerpP t' (V3 x0 y0 z0) (V3 x1 y1 z1) = V3 (lerp t' x0 x1) (lerp t' y0 y1) (lerp t' z0 z1)
    --lerp t' a b = t' * b + (1 - t') * a

-- | The dirtiest O(n^3) ear clipping I could write.
triangulate :: (Metric f, R1 f, R2 f, Ord a, Fractional a, Floating a)
         => Primitive f a -> [Primitive f a]
triangulate (Path ps) = triangulate' [] ps
    where triangulate' ts ps'
              | (p1:p2:p3:[]) <- ps' = Triangle p1 p2 p3 :ts
              | (p1:p2:p3:rest) <- ps' =
                  if any (pathHasPoint [p1,p2,p3]) rest
                    -- Cycle through and check the next triangle
                    then triangulate' ts $ p2:p3:rest ++ [p1]
                    else triangulate' (Triangle p1 p2 p3 :ts) $ p1:p3:rest
              | otherwise = ts
triangulate t@(Triangle _ _ _) = [t]
triangulate (Curve n bs) = triangulate $ Path $ bezierToList n bs
triangulate _ = []

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
--------------------------------------------------------------------------------
-- Types
--------------------------------------------------------------------------------
data Primitive f a = Line (f a) (f a)
                   -- ^ A straight line from one point to another.
                   | Curve a [f a]
                   -- ^ A n-bezier curve with a maximum segment distance.
                   -- This distance is used during rasterization as the
                   -- moximum tolerable distance between resolved points on
                   -- the curve. A smaller number will result in a higher
                   -- curve resolution. (So if your curve doesn't look
                   -- smooth, make this number smaller.)
                   | Triangle (f a) (f a) (f a)
                   -- ^ A flat triangle of three points.
                   | Path [f a]
                   -- ^ A list of points defining a path .
                   deriving (Show)

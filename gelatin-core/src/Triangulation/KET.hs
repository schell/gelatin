-- |
-- Module    : Triangulation.KET
-- Copyright :(C) 1997, 1998, 2008 Joern Dinkla, www.dinkla.net
--
-- Updates by Schell Scivally
--
-- Triangulation of simple polygons after Kong, Everett, Toussaint 91
-- with some changes by T.Vogt: return indices instead of coordinates of triangles and Data.Vector instead of lists
--
-- see
--     Joern Dinkla, Geometrische Algorithmen in Haskell, Diploma Thesis,
--     University of Bonn, Germany, 1998.
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Triangulation.KET (triangulate) where

import Render.Types
import Linear
import Data.Vector (Vector)
import qualified Data.Vector as V
import Data.List ( (\\) )

type V2i = (V2 Float,Int)

toV2 = V.map (\(x,_) -> x)

triangulate :: [V2 Float] -> [Triangle Float]
triangulate vs = map (\(a,b,c) -> Triangle (vec V.! a) (vec V.! b) (vec V.! c)) ndxs
    where vec  = V.fromList vs
          ndxs = triangulation vec

triangulation :: Vector (V2 Float) -> [(Int,Int,Int)]
triangulation points | (V.length vertices) > 3 = scan vs stack rs
              | otherwise = []
  where vertices = V.zip points (V.generate (V.length points) id)
        [p1,p2,p3] = V.toList (V.take 3 vertices)
        qs         = V.drop 3 vertices
        vs         = qs V.++ (V.singleton p1)
        stack      = V.fromList [p3, p2, p1, V.last vertices]
        rs         = reflexVertices (angles vertices)

scan :: Vector V2i -> Vector V2i -> Vector V2i -> [(Int,Int,Int)]
scan vs stack rs | V.null vs            = []
                 | V.length vs == 1     = [(snd (V.head stack), snd (V.head (V.tail stack)), snd (V.head vs))]
                 | V.length stack == 3  = scan (V.tail vs) (V.cons (V.head vs) stack) rs
                 | isEar rs x_m x_i x_p = (snd x_p, snd x_i, snd x_m) : (scan vs (V.cons x_p ss') rs')
                 | otherwise            = scan (V.tail vs) (V.cons (V.head vs) stack) rs
  where [x_p, x_i, x_m] = V.toList (V.take 3 stack)
        ss' = V.drop 2 stack
        rs'   = V.fromList $ (V.toList rs) \\ (isConvex x_m x_p (V.head vs) ++
                                               isConvex (V.head (V.tail ss')) x_m x_p)
        isConvex (im,_) (i,ii) (ip,_) = if isLeftTurn im i ip then [(i,ii)] else []

isEar :: Vector V2i -> V2i -> V2i -> V2i -> Bool
isEar rs (m,_) (x,_) (p,_) | V.null rs = True
                           | otherwise = isLeftTurn m x p && not (V.any ( (m,x,p) `containsBNV`) (toV2 rs))

reflexVertices  :: Vector (V2i,V2i,V2i) -> Vector V2i
reflexVertices as | V.null as             = V.empty
                  | isRightTurnOrOn m x p = V.cons (x,xi) $ reflexVertices (V.tail as)
                  | otherwise             =                 reflexVertices (V.tail as)
  where ((m,_),(x,xi),(p,_)) = V.head as

containsBNV (s,t,v) p    = (a==b && b==c)
  where a                = isLeftTurn s t p
        b                = isLeftTurn t v p
        c                = isLeftTurn v s p

angles :: Vector a -> Vector (a,a,a)
angles xs = V.zip3 (rotateR xs) xs (rotateL xs)

rotateL xs = (V.tail xs) V.++ (V.singleton (V.head xs))
rotateR xs = (V.singleton (V.last xs)) V.++ (V.init xs)

isRightTurnOrOn m x p = (area2 m x p) <= 0
isLeftTurn :: (V2 Float) -> (V2 Float) -> (V2 Float) -> Bool
isLeftTurn      m x p = (area2 m x p) > 0
area2 (V2 x2 y2) (V2 x0 y0) (V2 x1 y1) = (x1-x0)*(y2-y0)-(x2-x0)*(y1-y0)

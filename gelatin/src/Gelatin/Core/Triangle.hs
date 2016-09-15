{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
module Gelatin.Core.Triangle where

import           Gelatin.Core.Bounds
import           Gelatin.Core.Utils
import           Gelatin.Core.Bezier
import           Linear
import qualified Data.Vector.Unboxed as V
import           Data.Vector.Unboxed (Vector, Unbox)

type Triangle a = (a,a,a)

trisToComp :: Unbox a => Vector (Triangle (V2 a)) -> Vector (V2 a)
trisToComp = V.concatMap triPoints

triPoints :: Unbox a => Triangle (V2 a) -> Vector (V2 a)
triPoints (a,b,c) = V.fromList [a, b, c]

bezToTri :: Bezier a -> Triangle a
bezToTri (_,a,b,c) = (a,b,c)

triToPath :: Unbox a => Triangle a -> Vector a
triToPath (a,b,c) = V.fromList [a,b,c]

fmapTriangle :: (t -> t1) -> (t, t, t) -> (t1, t1, t1)
fmapTriangle f (a,b,c) = (f a, f b, f c)

triBounds :: Triangle (V2 Float) -> BBox
triBounds (a,b,c) = polyBounds $ V.fromList [a,b,c]
--------------------------------------------------------------------------------
-- Decomposing things into triangles
--------------------------------------------------------------------------------
sizeToTris :: V2 Float -> Vector (Triangle (V2 Float))
sizeToTris (V2 w h) = V.fromList [(a,b,c), (a,c,d)]
    where [a,b,c,d] = [V2 (-hw) (-hh), V2 hw (-hh), V2 hw hh, V2 (-hw) hh]
          (hw,hh) = (w/2,h/2)

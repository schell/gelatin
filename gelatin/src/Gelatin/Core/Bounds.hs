{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
module Gelatin.Core.Bounds where

import           Control.Arrow          (Arrow, first, second, (>>>))
import           Data.Vector.Unboxed    (Unbox, Vector)
import qualified Data.Vector.Unboxed    as V
import           Gelatin.Core.Transform
import           Linear

type BBox = (V2 Float, V2 Float)

type BCube = (V3 Float, V3 Float)

--------------------------------------------------------------------------------
-- 3d
--------------------------------------------------------------------------------
boundingCube :: (Unbox a, Real a) => Vector (V3 a) -> BCube
boundingCube vs
  | V.null vs = (0,0)
  | otherwise = V.foldl' f (br,tl) vs
  where mn a = min a . realToFrac
        mx a = max a . realToFrac
        f (a, b) c = (mn <$> a <*> c, mx <$> b <*> c)
        inf = 1/0
        ninf = (-1)/0
        tl = V3 ninf ninf ninf
        br = V3 inf inf inf

listToCube :: [V3 Float] -> BCube
listToCube = boundingCube . V.fromList

foldIntoCube :: Vector BCube -> BCube
foldIntoCube = boundingCube . uncurry (V.++) . V.unzip

pointInCube :: V2 Float -> BBox -> Bool
pointInCube (V2 px py) (V2 minx miny, V2 maxx maxy) =
  (px >= minx && px <= maxx) && (py >= miny && py <= maxy)

applyTfrmToCube :: M44 Float -> BBox -> BBox
applyTfrmToCube t (tl,br) = listToBox [transformV2 t tl, transformV2 t br]
--------------------------------------------------------------------------------
-- 2d
--------------------------------------------------------------------------------
both :: Arrow a => a d c -> a (d, d) (c, c)
both f = first f >>> second f

boundingBox :: (Unbox a, Real a) => Vector (V2 a) -> BBox
boundingBox = second demoteV3 . first demoteV3 . boundingCube . V.map promoteV2

listToBox :: [V2 Float] -> BBox
listToBox = boundingBox . V.fromList

foldIntoBox :: Vector BBox -> BBox
foldIntoBox = boundingBox . uncurry (V.++) . V.unzip

pointInBox :: V2 Float -> BBox -> Bool
pointInBox (V2 px py) (V2 minx miny, V2 maxx maxy) =
  (px >= minx && px <= maxx) && (py >= miny && py <= maxy)

applyTfrmToBox :: M44 Float -> BBox -> BBox
applyTfrmToBox t (tl,br) = listToBox [transformV2 t tl, transformV2 t br]

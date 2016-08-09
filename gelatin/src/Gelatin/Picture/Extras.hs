
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module Gelatin.Picture.Extras where

import           Gelatin.Core
import           Gelatin.Core.Bezier
import           Gelatin.Picture.Raw
import           Data.Vector.Unboxed (Vector, Unbox)
import qualified Data.Vector.Unboxed as V
import qualified Data.Vector as B
import           Control.Monad.State.Strict
import           Control.Lens hiding (to)
import           Control.Arrow (first, second)
import           Linear hiding (rotate)
import qualified Linear as L
--------------------------------------------------------------------------------
-- Mapping Vertices
--------------------------------------------------------------------------------
class HasVertices f where
  mapVertices :: (Unbox a, Unbox b) => (a -> b) -> f a -> f b

instance HasVertices Vector where
  mapVertices = V.map

instance HasVertices B.Vector where
  mapVertices = B.map

instance HasVertices Vertices where
  mapVertices f = Vertices . put . mapVertices f . runVertices

instance HasVertices RawGeometry where
  mapVertices f (RawTriangles vs) = RawTriangles $ V.map f vs
  mapVertices f (RawBeziers vs) = RawBeziers $ V.map f vs
  mapVertices f (RawTriangleStrip vs) = RawTriangleStrip $ V.map f vs
  mapVertices f (RawTriangleFan vs) = RawTriangleFan $ V.map f vs
  mapVertices f (RawLine vs) = RawLine $ V.map f vs

instance HasVertices Geometry where
  mapVertices f = Geometry . put . B.map (mapVertices f) . runGeometry

color :: (Unbox a, HasVertices f) => V4 Float -> f a -> f (a, V4 Float)
color c = mapVertices (,c)

recolor :: (Unbox a, HasVertices f) => V4 Float -> f (a, V4 Float)
        -> f (a, V4 Float)
recolor c = mapVertices (\(v,_) -> (v,c))
--------------------------------------------------------------------------------
-- Transforming (Helpers)
--------------------------------------------------------------------------------
doAround :: Monad m => m a -> m b -> m a
doAround f g = do
  x <- f
  void g
  return x

rotateV2 :: (RealFloat a, Conjugate a, Epsilon a) => a -> V2 a -> V2 a
rotateV2 r (V2 x y) =
  let V3 x1 y1 _ = L.rotate (axisAngle (V3 0 0 1) r) (V3 x y 0)
  in V2 x1 y1
--------------------------------------------------------------------------------
-- Transforming (Spatially)
--------------------------------------------------------------------------------
class HasAffine a where
  move     :: V2 Float -> a -> a
  scale    :: V2 Float -> a -> a
  rotate   :: Float -> a -> a

instance (RealFloat a, Epsilon a, Conjugate a) => HasAffine (V2 a) where
  move v a   = (realToFrac <$> v) + a
  scale v a  = (realToFrac <$> v) * a
  rotate r = rotateV2 (realToFrac r)

instance (RealFloat a, Epsilon a, Conjugate a) => HasAffine (V2 a, b) where
  move  v  = first (move v)
  scale v  = first (scale v)
  rotate r = first (rotate r)

instance (HasAffine a, Unbox a) => HasAffine (Vertices a) where
  move   v = mapVertices (move v)
  scale  v = mapVertices (scale v)
  rotate r = mapVertices (rotate r)

instance (HasAffine a, Unbox a) => HasAffine (Geometry a) where
  move   v = mapVertices (move v)
  scale  v = mapVertices (scale v)
  rotate r = mapVertices (rotate r)

instance (HasAffine a, Unbox a) => HasAffine (RawGeometry a) where
  move   v = mapVertices (move v)
  scale  v = mapVertices (scale v)
  rotate r = mapVertices (rotate r)
--------------------------------------------------------------------------------
-- Transforming (Perceptually)
--------------------------------------------------------------------------------
class HasColoring a where
  alpha    :: Float -> a -> a
  multiply :: V4 Float -> a -> a

instance Fractional a => HasColoring (V4 a) where
  alpha t    = (realToFrac t *^)
  multiply c = ((realToFrac <$> c) *)

instance Fractional b => HasColoring (a, V4 b) where
  alpha t    = second (alpha t)
  multiply c = second (multiply c)

instance (HasColoring a, Unbox a, Fractional a) => HasColoring (Vertices a) where
  alpha v     = mapVertices (alpha v)
  multiply v  = mapVertices (multiply v)

instance (HasColoring a, Unbox a, Fractional a) => HasColoring (Geometry a) where
  alpha v     = mapVertices (alpha v)
  multiply v  = mapVertices (multiply v)
--------------------------------------------------------------------------------
-- Shapes
--------------------------------------------------------------------------------
curve :: (Monad m, RealFloat a, Unbox a, Enum a)
      => V2 a -> V2 a -> V2 a
      -> StateT (Vector (V2 a)) m ()
curve a b c =
  let vs  = subdivideAdaptive 100 0 $ bez3 a b c
  in modify (V.++ vs)

corner :: (Monad m, RealFloat a, Unbox a, Enum a)
      => a -> a -> StateT (Vector (V2 a)) m ()
corner xr yr =
  let vs = cleanSeqDupes $ V.concatMap (subdivideAdaptive 100 0) $ cornerBez3 xr yr
  in modify (V.++ vs)

arc :: (Monad m, Unbox a, RealFloat a)
    => a -> a -> a -> a -> StateT (Vector (V2 a)) m ()
arc w h start stop =
  let vs = cleanSeqDupes $ V.concatMap (subdivideAdaptive 100 0) $ arcBez3 w h start stop
  in modify (V.++ vs)
--------------------------------------------------------------------------------
-- Measuring
--------------------------------------------------------------------------------
v2Bounds :: (Unbox (f a), Additive f, Unbox a, Num a, Ord a, Fractional a)
           => Vector (V2 a, f a) -> (V2 a, V2 a)
v2Bounds vs
  | V.null vs = (0,0)
  | otherwise = V.foldl' f (br,tl) vs
  where f (V2 nx ny, V2 xx xy) (V2 x y, _) = ( V2 (min nx x) (min ny y)
                                             , V2 (max xx x) (max xy y)
                                             )
        inf = 1/0
        ninf = (-1)/0
        tl = V2 ninf ninf
        br = V2 inf inf

geomBounds :: (Monad m, Unbox (f a), Additive f, Unbox a, Num a, Ord a, Fractional a)
           => StateT (RawGeometry (V2 a, f a)) m (V2 a, V2 a)
geomBounds = do
  let f (RawTriangles vs)     = vs
      f (RawBeziers vs)       = vs
      f (RawTriangleStrip vs) = vs
      f (RawTriangleFan vs)   = vs
      f (RawLine vs)          = vs
  vs <- gets f
  return $ v2Bounds vs
--------------------------------------------------------------------------------
-- An Example
--------------------------------------------------------------------------------
plainGeom :: Geometry (V2 Float)
plainGeom = geometry $ do
  add $ triangles $ withVertices $ do
    tri 0 1 (V2 0 1)
    tri 1 2 (V2 1 2)
  add $ beziers $ withVertices $ bez 0 10 (V2 0 10)
  add $ line $ withVertices $ do
    to 10
    to 20
    to (V2 20 0)

redGeom :: Geometry (V2 Float, V4 Float)
redGeom = color red plainGeom

greenGeom :: Geometry (V2 Float, V4 Float)
greenGeom = recolor green redGeom

movedGeom :: Geometry (V2 Float, V4 Float)
movedGeom = move 100 greenGeom

texturedGeom :: Geometry (V2 Float, V2 Float)
texturedGeom = mapVertices (\(v,_) -> (v, v/10)) movedGeom

mixedPicture :: Picture Int
mixedPicture = picture $ do
  draw $ do
    drawStroke   .= [StrokeWidth 3, StrokeFeather 1, StrokeCaps (LineCapRound,LineCapSquare)]
    drawGeometry .= colored [redGeom, greenGeom, movedGeom]

  draw $ do
    drawTexture  .= Just 0
    drawGeometry .= textured [texturedGeom]

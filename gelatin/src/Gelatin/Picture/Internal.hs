{-# LANGUAGE GADTs #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleContexts #-}
module Gelatin.Picture.Internal where

import           Gelatin.Core
import qualified Data.Vector as B
import qualified Data.Vector.Unboxed as V
import           Data.Vector.Unboxed (Vector, Unbox)
import           Data.Maybe (fromMaybe)
import           Control.Monad.State.Strict
import           Control.Lens hiding (to)
import           Linear hiding (rotate)

--------------------------------------------------------------------------------
-- General Geometry Types
--------------------------------------------------------------------------------
newtype Vertices a = Vertices { unVertices :: State (Vector a) () }
--------------------------------------------------------------------------------
-- Pretty General Operators
--------------------------------------------------------------------------------
snoc3 :: Unbox a => Vector a -> a -> a -> a -> Vector a
snoc3 v a b = V.snoc (V.snoc (V.snoc v a) b)

tri :: Unbox a => a -> a -> a -> State (Vector a) ()
tri a b c = modify $ \v -> snoc3 v a b c

bez :: Unbox a => a -> a -> a -> State (Vector a) ()
bez = tri

to :: Unbox a => a -> State (Vector a) ()
to = modify . flip V.snoc

segment :: Unbox a => a -> a -> State (Vector a) ()
segment a b = to a >> to b

vertices :: Unbox a => State (Vector a) () -> Vertices a
vertices = Vertices

mapVertices :: (Unbox a, Unbox b) => (a -> b) -> Vertices a -> Vertices b
mapVertices f = Vertices . put . V.map f . runVertices

runVertices :: Unbox a => Vertices a -> Vector a
runVertices = flip execState V.empty . unVertices
--------------------------------------------------------------------------------
-- Mixing drawing types and transforming them
--------------------------------------------------------------------------------
data RawGeometry a = RawTriangles (Vector a)
                   | RawBeziers (Vector a)
                   | RawTriangleStrip (Vector a)
                   | RawTriangleFan (Vector a)
                   | RawLine (Vector a)

mapRawGeometry :: (Unbox a, Unbox b) => (a -> b) -> RawGeometry a -> RawGeometry b
mapRawGeometry f (RawTriangles vs) = RawTriangles $ V.map f vs
mapRawGeometry f (RawBeziers vs) = RawBeziers $ V.map f vs
mapRawGeometry f (RawTriangleStrip vs) = RawTriangleStrip $ V.map f vs
mapRawGeometry f (RawTriangleFan vs) = RawTriangleFan $ V.map f vs
mapRawGeometry f (RawLine vs) = RawLine $ V.map f vs

newtype Geometry a = Geometry { unGeometry :: State (B.Vector (RawGeometry a)) ()}

add :: a -> State (B.Vector a) ()
add a = modify (`B.snoc` a)

triangles :: Unbox a => Vertices a -> RawGeometry a
triangles = RawTriangles . runVertices

beziers :: Unbox a => Vertices a -> RawGeometry a
beziers = RawBeziers . runVertices

strip :: Unbox a => Vertices a -> RawGeometry a
strip = RawTriangleStrip . runVertices

fan :: Unbox a => Vertices a -> RawGeometry a
fan = RawTriangleFan . runVertices

line :: Unbox a => Vertices a -> RawGeometry a
line = RawLine . runVertices

geometry :: State (B.Vector (RawGeometry a)) () -> Geometry a
geometry = Geometry

mapGeometry :: (Unbox a, Unbox b) => (a -> b) -> Geometry a -> Geometry b
mapGeometry f = Geometry . put . B.map (mapRawGeometry f) . runGeometry

runGeometry :: Geometry a -> B.Vector (RawGeometry a)
runGeometry = flip execState B.empty . unGeometry

vertexData :: Unbox v
           => RawGeometry v -> Vector v
vertexData (RawTriangles vs)     = vs
vertexData (RawBeziers vs)       = vs
vertexData (RawTriangleStrip vs) = vs
vertexData (RawTriangleFan vs)   = vs
vertexData (RawLine vs)          = vs
--------------------------------------------------------------------------------
-- Special Rendering Options
--------------------------------------------------------------------------------
data RenderingOption = StencilMaskOption
--------------------------------------------------------------------------------
-- Affine Transformation
--------------------------------------------------------------------------------
data Affine a r = Translate a
                | Scale a
                | Rotate r
                deriving (Show, Eq)
--------------------------------------------------------------------------------
-- Picture Data
--------------------------------------------------------------------------------
data PictureData texture spatial rotation vertex =
  PictureData { _picDataGeometry  :: B.Vector (RawGeometry vertex)
              -- ^ This picture's vertex data.
              , _picDataAlpha     :: Float
              -- ^ This picture's alpha value.
              , _picDataMultiply  :: V4 Float
              -- ^ This picture's multiply color.
              , _picDataStroke    :: [StrokeAttr]
              -- ^ The stroke attributes to use for drawing lines.
              , _picDataAffine    :: [Affine spatial rotation]
              -- ^ A list of affine transformations to apply to the picture and
              -- its children
              , _picDataToSpatial :: vertex -> spatial
              -- ^ A function that converts a vertex into a spatial coord.
              , _picDataBounds    :: Maybe (spatial, spatial)
              -- ^ The pre-computed bounding spatial coordinates.
              -- If this value is Nothing then it has not yet been computed.
              , _picDataCalcBounds :: PictureData texture spatial rotation vertex
                                   -> (PictureData texture spatial rotation vertex, (spatial,spatial))
              -- ^ A function that calculates the spatial boundary of the
              -- picture's data and all its children, placing the bounds in the
              -- _picDataBounds record and returning a tuple of the updated data
              -- and bounds.
              , _picDataTextures  :: [texture]
              -- ^ All the textures needed to render this picture's vertex data.
              , _picDataOptions   :: [RenderingOption]
              -- ^ Any special drawing options to apply when rendering this
              -- picture.
              , _picDataChildren  :: B.Vector (PictureData texture spatial rotation vertex)
              -- ^ Embedded picture data that should be transformed by this
              -- picture's affine transformation, alpha, multiply and should be
              -- contained within this picture's spatial boundary.
              }
makeLenses ''PictureData
--------------------------------------------------------------------------------
-- Picture Construction & Combination
--------------------------------------------------------------------------------
type Picture t s r v = State (PictureData t s r v)

invalidateBoundary :: Picture t s r v ()
invalidateBoundary = picDataBounds .= Nothing

runPicture :: Monoid (PictureData t s r v) => Picture t s r v a -> (a, PictureData t s r v)
runPicture = flip runState mempty

embed :: Monoid (PictureData t s r v) => Picture t s r v () -> Picture t s r v ()
embed p = do
  invalidateBoundary
  let dat = snd $ runPicture p
  picDataChildren %= (`B.snoc` dat)

overlay :: Monoid (PictureData t s r v)
        => Picture t s r v () -> Picture t s r v () -> Picture t s r v ()
overlay a b = do
  invalidateBoundary
  embed a >> embed b
--------------------------------------------------------------------------------
-- Getters & Setters
--------------------------------------------------------------------------------
setRawGeometry :: B.Vector (RawGeometry v) -> Picture t s r v ()
setRawGeometry vs = do
  picDataGeometry .= vs
  invalidateBoundary

getRawGeometry :: Picture t s r v (B.Vector (RawGeometry v))
getRawGeometry = use picDataGeometry

setGeometry :: Geometry v -> Picture t s r v ()
setGeometry = setRawGeometry . runGeometry

applyAffine :: Affine s r -> Picture t s r v ()
applyAffine f = do
  invalidateBoundary
  picDataAffine %= (f:)

move :: Num s => s -> Picture t s r v ()
move = applyAffine . Translate

scale :: (Num s, Fractional s) => s -> Picture t s r v ()
scale = applyAffine . Scale

rotate :: Num r => r -> Picture t s r v ()
rotate = applyAffine . Rotate

setAlpha :: Float -> Picture t s r v ()
setAlpha = (picDataAlpha .=)

getAlpha :: Picture t s r v Float
getAlpha = use picDataAlpha

alpha :: Float -> Picture t s r v ()
alpha = (picDataAlpha %=) . (*)

setMultiply :: V4 Float -> Picture t s r v ()
setMultiply = (picDataMultiply .=)

getMultiply :: Picture t s r v (V4 Float)
getMultiply = use picDataMultiply

multiply :: V4 Float -> Picture t s r v ()
multiply = (picDataMultiply %=) . (*)

setStroke :: [StrokeAttr] -> Picture t s r v ()
setStroke = (picDataStroke .=)

getStroke :: Picture t s r v [StrokeAttr]
getStroke = use picDataStroke

setTextures :: [t] -> Picture t s r v ()
setTextures = (picDataTextures .=)

getTextures :: Picture t s r v [t]
getTextures = use picDataTextures

setRenderingOptions :: [RenderingOption] -> Picture t s r v ()
setRenderingOptions = (picDataOptions .=)

getRenderingOptions :: Picture t s r v [RenderingOption]
getRenderingOptions = use picDataOptions


pictureBounds :: (Num s, Unbox v, Monoid (PictureData t s r v)) => Picture t s r v (s, s)
pictureBounds = use picDataBounds >>= \case
  Nothing -> do
    dat <- get
    let (newDat, bounds) = _picDataCalcBounds dat dat
    put newDat
    return bounds
  Just bb -> return bb

pictureDataBounds :: (Num s, Unbox v, Monoid (PictureData t s r v))
                  => PictureData t s r v -> (s,s)
pictureDataBounds = evalState pictureBounds

pictureBounds' :: (Num s, Unbox v, Monoid (PictureData t s r v))
               => Picture t s r v a -> (s, s)
pictureBounds' = fst . runPicture . (>> pictureBounds)

pictureSize :: (Num s, Unbox v, Monoid (PictureData t s r v))  => Picture t s r v s
pictureSize = do
  (tl,br) <- pictureBounds
  return $ br - tl

pictureSize' :: (Num s, Unbox v, Monoid (PictureData t s r v))
             => Picture t s r v a -> s
pictureSize' = fst . runPicture . (>> pictureSize)

pictureOrigin :: (Num s, Unbox v, Monoid (PictureData t s r v)) => Picture t s r v s
pictureOrigin = fst <$> pictureBounds

pictureOrigin' :: (Num s, Unbox v, Monoid (PictureData t s r v))
               => Picture t s r v a -> s
pictureOrigin' = fst . runPicture . (>> pictureOrigin)

pictureCenter :: (Num s, Fractional s, Unbox v, Monoid (PictureData t s r v))
              => Picture t s r v s
pictureCenter = do
  (tl,br) <- pictureBounds
  return $ tl + (br - tl)/2

pictureCenter' :: (Num s, Fractional s, Unbox v, Monoid (PictureData t s r v))
               => Picture t s r v a -> s
pictureCenter' = fst . runPicture . (>> pictureCenter)

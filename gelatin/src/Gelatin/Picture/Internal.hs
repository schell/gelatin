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
newtype VerticesT a m b = Vertices { unVertices :: StateT (Vector a) m b }
type Vertices a = VerticesT a Identity ()
--------------------------------------------------------------------------------
-- Pretty General Operators
--------------------------------------------------------------------------------
snoc3 :: Unbox a => Vector a -> a -> a -> a -> Vector a
snoc3 v a b = V.snoc (V.snoc (V.snoc v a) b)

tri :: (Monad m, Unbox a) => a -> a -> a -> StateT (Vector a) m ()
tri a b c = modify $ \v -> snoc3 v a b c

bez :: (Monad m, Unbox a) => a -> a -> a -> StateT (Vector a) m ()
bez = tri

to :: (Monad m, Unbox a) => a -> StateT (Vector a) m ()
to = modify . flip V.snoc

segment :: (Monad m, Unbox a) => a -> a -> StateT (Vector a) m ()
segment a b = to a >> to b

vertices :: (Monad m, Unbox a) => StateT (Vector a) m b -> VerticesT a m b
vertices = Vertices

runVerticesT :: (Monad m, Unbox a) => VerticesT a m b -> m (Vector a)
runVerticesT = flip execStateT V.empty . unVertices

runVertices :: Unbox a => Vertices a -> Vector a
runVertices = runIdentity . runVerticesT

mapVertices :: (Monad m, Unbox a, Unbox c)
            => (a -> c) -> VerticesT a m b -> VerticesT c m ()
mapVertices f s = Vertices $ do
  vs <- lift $ runVerticesT s
  put $ V.map f vs
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

newtype GeometryT a m b = Geometry { unGeometry :: StateT (B.Vector (RawGeometry a)) m b}
type Geometry a = GeometryT a Identity ()

add :: Monad m => a -> StateT (B.Vector a) m ()
add a = modify (`B.snoc` a)

triangles :: (Monad m, Unbox a) => VerticesT a m b -> m (RawGeometry a)
triangles = (RawTriangles <$>) . runVerticesT

beziers :: (Monad m, Unbox a) => VerticesT a m b -> m (RawGeometry a)
beziers = (RawBeziers <$>) . runVerticesT

strip :: (Monad m, Unbox a) => VerticesT a m b -> m (RawGeometry a)
strip = (RawTriangleStrip <$>) . runVerticesT

fan :: (Monad m, Unbox a) => VerticesT a m b -> m (RawGeometry a)
fan = (RawTriangleFan <$>) . runVerticesT

line :: (Monad m, Unbox a) => VerticesT a m b -> m (RawGeometry a)
line = (RawLine <$>) . runVerticesT

geometry :: StateT (B.Vector (RawGeometry a)) m b -> GeometryT a m b
geometry = Geometry

runGeometryT :: Monad m => GeometryT a m b -> m (B.Vector (RawGeometry a))
runGeometryT = flip execStateT B.empty . unGeometry

runGeometry :: Geometry a -> B.Vector (RawGeometry a)
runGeometry = runIdentity . runGeometryT

mapGeometry :: (Monad m, Unbox a, Unbox c)
            => (a -> c) -> GeometryT a m b -> GeometryT c m ()
mapGeometry f s = Geometry $ do
  gs <- lift $ runGeometryT s
  put $ B.map (mapRawGeometry f) gs

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
-- Picture Construction
--------------------------------------------------------------------------------
type PictureT t s r v = StateT (PictureData t s r v)

runPictureT :: (Monad m, Monoid (PictureData t s r v))
            => PictureT t s r v m a -> m (a, PictureData t s r v)
runPictureT = flip runStateT mempty
--------------------------------------------------------------------------------
-- Identity Parameterized Pictures
--------------------------------------------------------------------------------
type Picture t s r v = PictureT t s r v Identity

runPicture :: Monoid (PictureData t s r v)
           => Picture t s r v a -> (a, PictureData t s r v)
runPicture = runIdentity . runPictureT
--------------------------------------------------------------------------------
-- Picture Combination & Operations
--------------------------------------------------------------------------------
invalidateBoundary :: Monad m =>  PictureT t s r v m ()
invalidateBoundary = picDataBounds .= Nothing

embed :: (Monad m, Monoid (PictureData t s r v))
      => PictureT t s r v m () -> PictureT t s r v m ()
embed p = do
  invalidateBoundary
  dat <- snd <$> lift (runPictureT p)
  picDataChildren %= (`B.snoc` dat)

overlay :: (Monad m, Monoid (PictureData t s r v))
        => PictureT t s r v m () -> PictureT t s r v m ()
        -> PictureT t s r v m ()
overlay a b = do
  invalidateBoundary
  embed a >> embed b

setRawGeometry :: Monad m => B.Vector (RawGeometry v) -> PictureT t s r v m ()
setRawGeometry vs = do
  picDataGeometry .= vs
  invalidateBoundary

getRawGeometry :: Monad m => PictureT t s r v m (B.Vector (RawGeometry v))
getRawGeometry = use picDataGeometry

setGeometry :: Monad m => Geometry v -> PictureT t s r v m ()
setGeometry = setRawGeometry . runGeometry

applyAffine :: Monad m => Affine s r -> PictureT t s r v m ()
applyAffine f = do
  invalidateBoundary
  picDataAffine %= (f:)

move :: (Monad m, Num s ) => s -> PictureT t s r v m ()
move = applyAffine . Translate

scale :: (Monad m, Num s, Fractional s) => s -> PictureT t s r v m ()
scale = applyAffine . Scale

rotate :: (Monad m, Num r ) => r -> PictureT t s r v m ()
rotate = applyAffine . Rotate

setAlpha :: Monad m => Float -> PictureT t s r v m ()
setAlpha = (picDataAlpha .=)

getAlpha :: Monad m => PictureT t s r v m Float
getAlpha = use picDataAlpha

alpha :: Monad m => Float -> PictureT t s r v m ()
alpha = (picDataAlpha %=) . (*)

setMultiply :: Monad m => V4 Float -> PictureT t s r v m ()
setMultiply = (picDataMultiply .=)

getMultiply ::Monad m => PictureT t s r v m (V4 Float)
getMultiply = use picDataMultiply

multiply :: Monad m => V4 Float -> PictureT t s r v m ()
multiply = (picDataMultiply %=) . (*)

setStroke :: Monad m => [StrokeAttr] -> PictureT t s r v m ()
setStroke = (picDataStroke .=)

getStroke :: Monad m => PictureT t s r v m [StrokeAttr]
getStroke = use picDataStroke

setTextures :: Monad m => [t] -> PictureT t s r v m ()
setTextures = (picDataTextures .=)

getTextures :: Monad m => PictureT t s r v m [t]
getTextures = use picDataTextures

setRenderingOptions :: Monad m => [RenderingOption] -> PictureT t s r v m ()
setRenderingOptions = (picDataOptions .=)

getRenderingOptions :: Monad m => PictureT t s r v m [RenderingOption]
getRenderingOptions = use picDataOptions

pictureBounds :: (Monad m, Num s, Unbox v, Monoid (PictureData t s r v))
              => PictureT t s r v m (s, s)
pictureBounds = use picDataBounds >>= \case
  Nothing -> do
    dat <- get
    let (newDat, bounds) = _picDataCalcBounds dat dat
    put newDat
    return bounds
  Just bb -> return bb

pictureSize :: ((Monad m, Num s, Unbox v, Monoid (PictureData t s r v))  )
            => PictureT t s r v m s
pictureSize = do
  (tl,br) <- pictureBounds
  return $ br - tl

pictureOrigin :: ((Monad m, Num s, Unbox v, Monoid (PictureData t s r v)) ) => PictureT t s r v m s
pictureOrigin = fst <$> pictureBounds

pictureCenter :: (Monad m, Num s, Fractional s, Unbox v, Monoid (PictureData t s r v))
              => PictureT t s r v m s
pictureCenter = do
  (tl,br) <- pictureBounds
  return $ tl + (br - tl)/2
--------------------------------------------------------------------------------
-- Conveniences for measuring pictures from outside the PictureT monad
--------------------------------------------------------------------------------
pictureDataBounds :: (Num s, Unbox v, Monoid (PictureData t s r v))
                  => PictureData t s r v -> (s,s)
pictureDataBounds = evalState pictureBounds

runPictureBoundsT :: (Monad m, Num s, Unbox v, Monoid (PictureData t s r v))
                  => PictureT t s r v m a -> m (s, s)
runPictureBoundsT = (fst <$>) . runPictureT . (>> pictureBounds)


runPictureSizeT :: (Monad m, Num s, Unbox v, Monoid (PictureData t s r v))
                => PictureT t s r v m a -> m s
runPictureSizeT = (fst <$>) . runPictureT . (>> pictureSize)

runPictureOriginT :: (Monad m, Num s, Unbox v, Monoid (PictureData t s r v))
                  => PictureT t s r v m a -> m s
runPictureOriginT = (fst <$>) . runPictureT . (>> pictureOrigin)

runPictureCenterT :: (Monad m, Num s, Fractional s, Unbox v, Monoid (PictureData t s r v))
                  => PictureT t s r v m a -> m s
runPictureCenterT = (fst <$>) . runPictureT . (>> pictureCenter)
--------------------------------------------------------------------------------
-- Conveniences for measuring pictures from outside the Picture monad
--------------------------------------------------------------------------------
runPictureBounds :: (Num s, Unbox v, Monoid (PictureData t s r v))
                 => Picture t s r v a -> (s, s)
runPictureBounds = runIdentity . runPictureBoundsT

runPictureSize :: (Num s, Unbox v, Monoid (PictureData t s r v))
               => Picture t s r v a -> s
runPictureSize = runIdentity . runPictureSizeT

runPictureOrigin :: (Num s, Unbox v, Monoid (PictureData t s r v))
                 => Picture t s r v s -> s
runPictureOrigin = runIdentity . runPictureOriginT

runPictureCenter :: (Num s, Fractional s, Unbox v, Monoid (PictureData t s r v))
                 => Picture t s r v a -> s
runPictureCenter = runIdentity . runPictureCenterT

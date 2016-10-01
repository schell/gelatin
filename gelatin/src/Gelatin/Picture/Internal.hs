{-# LANGUAGE GADTs #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
module Gelatin.Picture.Internal where

import           Gelatin.Core
import qualified Data.Vector as B
import qualified Data.Vector.Unboxed as V
import           Data.Vector.Unboxed (Vector, Unbox)
import           Data.Foldable (foldr')
import           Control.Monad.Trans.Class
import           Control.Monad.IO.Class
import           Control.Monad.State.Strict
import           Control.Lens hiding (to)
import           Control.Arrow
import           Linear hiding (rotate)

--------------------------------------------------------------------------------
-- A Monad for defining vertex data
--------------------------------------------------------------------------------
newtype VerticesT a m b = Vertices { unVertices :: StateT (Vector a) m b }
type Vertices a = VerticesT a Identity ()

instance Functor m => Functor (VerticesT a m) where
  fmap f (Vertices s) = Vertices $ fmap f s

instance Monad m => Applicative (VerticesT a m) where
  pure = Vertices . pure
  (Vertices f) <*> (Vertices x) = Vertices $ f <*> x

instance Monad m => Monad (VerticesT a m) where
  return = pure
  (Vertices m) >>= f = Vertices $ m >>= unVertices . f

instance MonadTrans (VerticesT a) where
  lift = Vertices . lift

instance MonadIO m => MonadIO (VerticesT a m) where
  liftIO = lift . liftIO
--------------------------------------------------------------------------------
-- Pretty General Operators
--------------------------------------------------------------------------------
snoc3 :: Unbox a => Vector a -> a -> a -> a -> Vector a
snoc3 v a b = V.snoc (V.snoc (V.snoc v a) b)

tri :: (Monad m, Unbox a) => a -> a -> a -> VerticesT a m ()
tri a b c = Vertices $ modify $ \v -> snoc3 v a b c

bez :: (Monad m, Unbox a) => a -> a -> a -> VerticesT a m ()
bez = tri

to :: (Monad m, Unbox a) => a -> VerticesT a m ()
to = Vertices . modify . flip V.snoc

segment :: (Monad m, Unbox a) => a -> a -> VerticesT a m ()
segment a b = to a >> to b

addVertexList :: (Monad m, Unbox a) => [a] -> VerticesT a m ()
addVertexList ys = Vertices $ do
  xs <- get
  put $ xs V.++ V.fromList ys

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
--------------------------------------------------------------------------------
-- A Monad for defining geometry
--------------------------------------------------------------------------------
newtype GeometryT a m b =
  Geometry { unGeometry :: StateT (B.Vector (RawGeometry a)) m b}
type Geometry a = GeometryT a Identity ()

instance Functor m => Functor (GeometryT a m) where
  fmap f (Geometry s) = Geometry $ fmap f s

instance Monad m => Applicative (GeometryT a m) where
  pure = Geometry . pure
  (Geometry f) <*> (Geometry x) = Geometry $ f <*> x

instance Monad m => Monad (GeometryT a m) where
  return = pure
  (Geometry m) >>= f = Geometry $ m >>= unGeometry . f

instance MonadTrans (GeometryT a) where
  lift = Geometry . lift

instance MonadIO m => MonadIO (GeometryT a m) where
  liftIO = lift . liftIO

add :: Monad m => RawGeometry a -> StateT (B.Vector (RawGeometry a)) m ()
add a = modify (`B.snoc` a)

triangles :: (Unbox a, Monad m) => VerticesT a m () -> GeometryT a m ()
triangles vs = Geometry $ do
  v <- lift $ runVerticesT vs
  add $ RawTriangles v

beziers :: (Monad m, Unbox a) => VerticesT a m () -> GeometryT a m ()
beziers vs = Geometry $ do
  v <- lift $ runVerticesT vs
  add $ RawBeziers v

strip :: (Monad m, Unbox a) => VerticesT a m () -> GeometryT a m ()
strip vs = Geometry $ do
  v <- lift $ runVerticesT vs
  add $ RawTriangleStrip v

fan :: (Monad m, Unbox a) => VerticesT a m () -> GeometryT a m ()
fan vs = Geometry $ do
  v <- lift $ runVerticesT vs
  add $ RawTriangleFan v

line :: (Monad m, Unbox a) => VerticesT a m () -> GeometryT a m ()
line vs = Geometry $ do
  v <- lift $ runVerticesT vs
  add $ RawLine v

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
-- Picture Data
--------------------------------------------------------------------------------
data PictureData texture vertex =
  PictureData { _picDataGeometry  :: B.Vector (RawGeometry vertex)
              -- ^ This picture's vertex data.
              -- , _picDataAlpha     :: Float
              -- -- ^ This picture's alpha value.
              -- , _picDataMultiply  :: V4 Float
              -- -- ^ This picture's multiply color.
              -- , _picDataReplaceColor :: Maybe (V4 Float)
              -- -- ^ This picture's replacement color value.
              -- -- | TODO: Explain what replacement color does (hint - it lets you
              -- --         change the color of text drawn with an atlas texture
              , _picDataStroke    :: [StrokeAttr]
              -- ^ The stroke attributes to use for drawing lines.
              -- , _picDataAffine    :: [Affine spatial rotation]
              -- -- ^ A list of affine transformations to apply to the picture and
              -- -- its children
              -- , _picDataToSpatial :: vertex -> spatial
              -- -- ^ A function that converts a vertex into a spatial coord.
              -- , _picDataBounds    :: Maybe (spatial, spatial)
              -- -- ^ The pre-computed bounding spatial coordinates.
              -- -- If this value is Nothing then it has not yet been computed.
              -- , _picDataCalcBounds :: PictureData texture spatial rotation vertex raster
              --                      -> (PictureData texture spatial rotation vertex raster, (spatial,spatial))
              -- ^ A function that calculates the spatial boundary of the
              -- picture's data and all its children, placing the bounds in the
              -- _picDataBounds record and returning a tuple of the updated data
              -- and bounds.
              , _picDataTextures  :: [texture]
              -- ^ All the textures needed to render this picture's vertex data.
              , _picDataOptions   :: [RenderingOption]
              -- ^ Any special drawing options to apply when rendering this
              -- picture.
              -- , _picDataChildren  :: B.Vector (PictureData texture spatial rotation vertex raster)
              -- ^ Embedded picture data that should be transformed by this
              -- picture's affine transformation, alpha, multiply and should be
              -- contained within this picture's spatial boundary.
              }
makeLenses ''PictureData
--------------------------------------------------------------------------------
-- Helpers for Common Picture Types
--------------------------------------------------------------------------------
emptyPictureData :: PictureData t v
emptyPictureData =
    PictureData { _picDataGeometry   = B.empty
                -- , _picDataAffine     = []
                -- , _picDataCalcBounds = calculateBoundsV2VX
                -- , _picDataAlpha     = 1
                -- , _picDataMultiply  = 1
                -- , _picDataReplaceColor = Nothing
                , _picDataStroke    = []
                -- , _picDataToSpatial = fst
                -- , _picDataBounds    = Nothing
                , _picDataTextures  = []
                , _picDataOptions   = []
                -- , _picDataChildren  = B.empty
                }

--embedPictureData :: Monoid (PictureData t v r)
--                 => [PictureData t v r] -> PictureData t s r v
--embedPictureData ps = mempty { _picDataChildren = B.fromList ps }

bothToFrac :: (Real a, Fractional b) => (V2 a, V2 a) -> (V2 b, V2 b)
bothToFrac= second (fmap realToFrac) . first (fmap realToFrac)

--calcV2VX_applyTfrm :: Num a => V4 (V4 a) -> V2 a -> V2 a
--calcV2VX_applyTfrm mv = demoteV3 . m41ToV3 . (mv !*!) . v3ToM41 . promoteV2
--{-# INLINE calcV2VX_applyTfrm #-}
--
--calcV2VX_mv :: (Real a, Floating a, Epsilon a)
--            => PictureData t (V2 a) a v -> M44 Float
--calcV2VX_mv dat = (fmap realToFrac) <$> affine2sModelview (_picDataAffine dat)
--
--calcV2VX_extractSpatial :: (Unbox a, Unbox b, Real a, Floating a, Epsilon a)
--                        => PictureData t (V2 a) a (V2 a,b) -> V.Vector (V2 Float)
--calcV2VX_extractSpatial dat =
--  let gs = _picDataGeometry dat
--      mv = calcV2VX_mv dat
--      extractAndTfrm = calcV2VX_applyTfrm mv . fmap realToFrac . fst
--      f = V.map extractAndTfrm . vertexData . (gs B.!)
--  in V.concatMap f $ V.enumFromTo 0 (B.length gs - 1)
--
--calcV2VX_kids :: (RealFrac a, Unbox a, Unbox b, Floating a, Epsilon a)
--              => PictureData t (V2 a) a (V2 a, b)
--              -> ( B.Vector (PictureData t (V2 a) a (V2 a, b))
--                 , V.Vector (V2 Float, V2 Float)
--                 )
--calcV2VX_kids dat =
--  let (ks, kbs) = B.unzip $ B.map calculateBoundsV2VX $ _picDataChildren dat
--      kidBounds = V.map bothToFrac $ B.convert kbs
--  in (ks, kidBounds)

--calculateBoundsV2VX :: (Unbox a, Unbox b, RealFrac a, Floating a, Epsilon a)
--                    => PictureData t (V2 a) a (V2 a, b)
--                    -> (PictureData t (V2 a) a (V2 a, b), (V2 a, V2 a))
--calculateBoundsV2VX dat =
--  let vs :: V.Vector (V2 Float)
--      vs = calcV2VX_extractSpatial dat
--      (ks, kidBounds) = calcV2VX_kids dat
--      bounds    = boundsBounds (polyBounds vs `V.cons` kidBounds)
--      boundsFin :: Fractional a => (V2 a, V2 a)
--      boundsFin = bothToFrac bounds
--  in (dat{ _picDataBounds = Just boundsFin, _picDataChildren = ks }, boundsFin)

--instance (Unbox a, Unbox b, RealFrac a, Floating a, Epsilon a)
--  => Monoid (PictureData t (V2 a) a (V2 a, b) x) where
--  mempty = emptyPictureDataV2VX
--  mappend a b = embedPictureData [a,b]
--  mconcat = embedPictureData
--------------------------------------------------------------------------------
-- Picture Construction
--------------------------------------------------------------------------------
type PictureT tex vert = StateT (PictureData tex vert)

runPictureT :: PictureT t v m a -> m (a, PictureData t v)
runPictureT = flip runStateT emptyPictureData
--------------------------------------------------------------------------------
-- Identity Parameterized Pictures
--------------------------------------------------------------------------------
type Picture t v = PictureT t v Identity

runPicture :: Picture t v a -> (a, PictureData t v)
runPicture = runIdentity . runPictureT
--------------------------------------------------------------------------------
-- Picture Combination & Operations
--------------------------------------------------------------------------------
invalidateBoundary :: Monad m =>  PictureT t v m ()
--invalidateBoundary = picDataBounds .= Nothing
invalidateBoundary = return ()

--embed :: (Monad m, Monoid (PictureData t v r))
--      => PictureT t v m b -> PictureT t v m b
--embed p = do
--  invalidateBoundary
--  (b,dat) <- lift (runPictureT p)
--  picDataChildren %= (`B.snoc` dat)
--  return b
--
--embedAt :: (Monad m, Monoid (PictureData t v r))
--          => Int -> PictureT t v m b -> PictureT t v m b
--embedAt k p = do
--  invalidateBoundary
--  (b,dat)  <- lift (runPictureT p)
--  cs <- use picDataChildren
--  let xs = B.take k cs
--      ys = B.cons dat $ B.drop k cs
--  picDataChildren .= (xs B.++ ys)
--  return b
--
--overlay :: (Monad m, Monoid (PictureData t v r))
--        => PictureT t v m () -> PictureT t v m ()
--        -> PictureT t v m ()
--overlay a b = do
--  invalidateBoundary
--  embed a >> embed b

setRawGeometry :: Monad m => B.Vector (RawGeometry v) -> PictureT t v m ()
setRawGeometry vs = do
  picDataGeometry .= vs
  invalidateBoundary

getRawGeometry :: Monad m => PictureT t v m (B.Vector (RawGeometry v))
getRawGeometry = use picDataGeometry

setGeometry :: Monad m => GeometryT v m () -> PictureT t v m ()
setGeometry gs = do
  v <- lift $ runGeometryT gs
  setRawGeometry v

setStroke :: Monad m => [StrokeAttr] -> PictureT t v m ()
setStroke = (picDataStroke .=)

getStroke :: Monad m => PictureT t v m [StrokeAttr]
getStroke = use picDataStroke

setTextures :: Monad m => [t] -> PictureT t v m ()
setTextures = (picDataTextures .=)

getTextures :: Monad m => PictureT t v m [t]
getTextures = use picDataTextures

setRenderingOptions :: Monad m => [RenderingOption] -> PictureT t v m ()
setRenderingOptions = (picDataOptions .=)

getRenderingOptions :: Monad m => PictureT t v m [RenderingOption]
getRenderingOptions = use picDataOptions
--------------------------------------------------------------------------------
-- Measuring pictures
--------------------------------------------------------------------------------
extractSpatial :: (Monad m, Unbox v, Unbox s)
               => (v -> s) -> PictureT t v m (V.Vector s)
extractSpatial vert2space = do
  gs <- use picDataGeometry
  let --extract = fmap realToFrac . vert2space
      f       = V.map vert2space . vertexData . (gs B.!)
  return $ V.concatMap f $ V.enumFromTo 0 (B.length gs - 1)

pictureBounds2 :: (Monad m, Unbox v)
               => (v -> V2 Float) -> PictureT t v m (V2 Float, V2 Float)
pictureBounds2 = (polyBounds <$>) . extractSpatial

pictureSize2 :: (Monad m, Unbox v)
             => (v -> V2 Float) -> PictureT t v m (V2 Float)
pictureSize2 vert2space = do
  (tl,br) <- pictureBounds2 vert2space
  return $ br - tl

pictureOrigin2 :: (Monad m, Unbox v)
               => (v -> V2 Float) -> PictureT t v m (V2 Float)
pictureOrigin2 = (fst <$>) . pictureBounds2

pictureCenter2 :: (Monad m, Unbox v)
               => (v -> V2 Float) -> PictureT t v m (V2 Float)
pictureCenter2 vert2space = do
  (tl,br) <- pictureBounds2 vert2space
  return $ tl + (br - tl)/2
--------------------------------------------------------------------------------
-- Conveniences for measuring pictures from outside the PictureT monad
--------------------------------------------------------------------------------
--pictureDataBounds :: (Num s, Unbox v, Monoid (PictureData t v r))
--                  => PictureData t v r -> (s,s)
--pictureDataBounds = evalState pictureBounds
--
--runPictureBoundsT :: (Monad m, Num s, Unbox v, Monoid (PictureData t v r))
--                  => PictureT t v m a -> m (s, s)
--runPictureBoundsT = (fst <$>) . runPictureT . (>> pictureBounds)
--
--
--runPictureSizeT :: (Monad m, Num s, Unbox v, Monoid (PictureData t v r))
--                => PictureT t v m a -> m s
--runPictureSizeT = (fst <$>) . runPictureT . (>> pictureSize)
--
--runPictureOriginT :: (Monad m, Num s, Unbox v, Monoid (PictureData t v r))
--                  => PictureT t v m a -> m s
--runPictureOriginT = (fst <$>) . runPictureT . (>> pictureOrigin)
--
--runPictureCenterT :: (Monad m, Num s, Fractional s, Unbox v, Monoid (PictureData t v r))
--                  => PictureT t v m a -> m s
--runPictureCenterT = (fst <$>) . runPictureT . (>> pictureCenter)
--------------------------------------------------------------------------------
-- Conveniences for measuring pictures from outside the Picture monad
--------------------------------------------------------------------------------
--runPictureBounds :: (Num s, Unbox v, Monoid (PictureData t v r))
--                 => Picture t v r a -> (s, s)
--runPictureBounds = runIdentity . runPictureBoundsT
--
--runPictureSize :: (Num s, Unbox v, Monoid (PictureData t v r))
--               => Picture t v r a -> s
--runPictureSize = runIdentity . runPictureSizeT
--
--runPictureOrigin :: (Num s, Unbox v, Monoid (PictureData t v r))
--                 => Picture t v r s -> s
--runPictureOrigin = runIdentity . runPictureOriginT
--
--runPictureCenter :: (Num s, Fractional s, Unbox v, Monoid (PictureData t v r))
--                 => Picture t v r a -> s
--runPictureCenter = runIdentity . runPictureCenterT

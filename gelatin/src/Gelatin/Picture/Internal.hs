{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TupleSections        #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Gelatin.Picture.Internal where

import           Control.Arrow
import           Control.Lens               hiding (to)
import           Control.Monad.IO.Class
import           Control.Monad.State.Strict
import qualified Data.Vector                as B
import           Data.Vector.Unboxed        (Unbox, Vector)
import qualified Data.Vector.Unboxed        as V
import           Gelatin.Core
import           Linear                     hiding (rotate)

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
mapRawGeometry f (RawTriangles vs)     = RawTriangles $ V.map f vs
mapRawGeometry f (RawBeziers vs)       = RawBeziers $ V.map f vs
mapRawGeometry f (RawTriangleStrip vs) = RawTriangleStrip $ V.map f vs
mapRawGeometry f (RawTriangleFan vs)   = RawTriangleFan $ V.map f vs
mapRawGeometry f (RawLine vs)          = RawLine $ V.map f vs
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

vertexData :: RawGeometry v -> Vector v
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
  PictureData { _picDataGeometry :: B.Vector (RawGeometry vertex)
              -- ^ This picture's vertex data.
              , _picDataStroke   :: [StrokeAttr]
              -- ^ The stroke attributes to use for drawing lines.
              , _picDataTextures :: [texture]
              -- ^ All the textures needed to render this picture's vertex data.
              , _picDataOptions  :: [RenderingOption]
              -- ^ Any special drawing options to apply when rendering this
              -- picture.
              }
makeLenses ''PictureData
--------------------------------------------------------------------------------
-- Helpers for Common Picture Types
--------------------------------------------------------------------------------
emptyPictureData :: PictureData t v
emptyPictureData =
    PictureData { _picDataGeometry   = B.empty
                , _picDataStroke    = []
                , _picDataTextures  = []
                , _picDataOptions   = []
                }

bothToFrac :: (Real a, Fractional b) => (V2 a, V2 a) -> (V2 b, V2 b)
bothToFrac= second (fmap realToFrac) . first (fmap realToFrac)
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

setRawGeometry :: Monad m => B.Vector (RawGeometry v) -> PictureT t v m ()
setRawGeometry vs = picDataGeometry .= vs

getRawGeometry :: Monad m => PictureT t v m (B.Vector (RawGeometry v))
getRawGeometry = use picDataGeometry

setGeometry :: Monad m => GeometryT v m () -> PictureT t v m ()
setGeometry = (setRawGeometry =<<) . lift . runGeometryT

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
extractSpatial vertToSpace = do
  gs <- use picDataGeometry
  let f = V.map vertToSpace . vertexData . (gs B.!)
  return $ V.concatMap f $ V.enumFromTo 0 (B.length gs - 1)

pictureBounds2 :: (Monad m, Unbox v)
               => (v -> V2 Float) -> PictureT t v m (V2 Float, V2 Float)
pictureBounds2 = (boundingBox <$>) . extractSpatial

pictureBounds3 :: (Monad m, Unbox v)
               => (v -> V3 Float) -> PictureT t v m BCube
pictureBounds3 = (boundingCube <$>) . extractSpatial

pictureSize2 :: (Monad m, Unbox v)
             => (v -> V2 Float) -> PictureT t v m (V2 Float)
pictureSize2 = pictureBounds2 >=> (return . uncurry (flip (-)))

pictureSize3 :: (Monad m, Unbox v)
             => (v -> V3 Float) -> PictureT t v m (V3 Float)
pictureSize3 = pictureBounds3 >=> (return . uncurry (flip (-)))

pictureOrigin2 :: (Monad m, Unbox v)
               => (v -> V2 Float) -> PictureT t v m (V2 Float)
pictureOrigin2 = (fst <$>) . pictureBounds2

pictureOrigin3 :: (Monad m, Unbox v)
               => (v -> V3 Float) -> PictureT t v m (V3 Float)
pictureOrigin3 = (fst <$>) . pictureBounds3

pictureCenter2 :: (Monad m, Unbox v)
               => (v -> V2 Float) -> PictureT t v m (V2 Float)
pictureCenter2 vertToSpace = do
  (tl,br) <- pictureBounds2 vertToSpace
  return $ tl + (br - tl)/2

pictureCenter3 :: (Monad m, Unbox v)
               => (v -> V3 Float) -> PictureT t v m (V3 Float)
pictureCenter3 vertToSpace = do
  (tl,br) <- pictureBounds3 vertToSpace
  return $ tl + (br - tl)/2

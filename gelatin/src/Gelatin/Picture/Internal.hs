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
-- | A monad transformer for defining geometry.
newtype VerticesT a m b = Vertices { unVertices :: StateT (Vector a) m b }
-- | A pure context for defining geometry.
-- This is 'VerticesT' parameterized over 'Identity'.
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
-- | Append three elements to a 'Vector'.
-- /O(n + 3)/
snoc3 :: Unbox a => Vector a -> a -> a -> a -> Vector a
snoc3 v a b c = V.fromList [a,b,c] V.++ v

-- | Add a triangle of vertices.
tri :: (Monad m, Unbox a) => a -> a -> a -> VerticesT a m ()
tri a b c = Vertices $ modify $ \v -> snoc3 v a b c

-- | Add a bezier of vertices.
-- This is an alias of 'tri' but looks better in the context
-- of drawing beziers.
bez :: (Monad m, Unbox a) => a -> a -> a -> VerticesT a m ()
bez = tri

-- | Add one vertex.
to :: (Monad m, Unbox a) => a -> VerticesT a m ()
to = Vertices . modify . flip V.snoc

-- | Add two vertices.
segment :: (Monad m, Unbox a) => a -> a -> VerticesT a m ()
segment a b = to a >> to b

-- | Add vertices from a list.
addVertexList :: (Monad m, Unbox a) => [a] -> VerticesT a m ()
addVertexList ys = Vertices $ do
  xs <- get
  put $ xs V.++ V.fromList ys

-- | Extract the raw 'Vector' of vertices monadically.
runVerticesT :: (Monad m, Unbox a) => VerticesT a m b -> m (Vector a)
runVerticesT = flip execStateT V.empty . unVertices

-- | Extract the raw 'Vector' of vertices.
runVertices :: Unbox a => Vertices a -> Vector a
runVertices = runIdentity . runVerticesT

-- | Map all the vertices in the computation.
mapVertices :: (Monad m, Unbox a, Unbox c)
            => (a -> c) -> VerticesT a m b -> VerticesT c m ()
mapVertices f s = Vertices $ do
  vs <- lift $ runVerticesT s
  put $ V.map f vs
--------------------------------------------------------------------------------
-- Mixing drawing types and transforming them
--------------------------------------------------------------------------------
-- | Mixed drawing types roughly corresponding to OpenGL's draw modes.
data RawGeometry a = RawTriangles (Vector a)
                   -- ^ A collection of points known to be triangles.
                   | RawBeziers (Vector a)
                   -- ^ A collection of points known to be beziers.
                   | RawTriangleStrip (Vector a)
                   -- ^ A collection of points known to be a triangle strip.
                   | RawTriangleFan (Vector a)
                   -- ^ A collection of points known to be a triangle fan.
                   | RawLine (Vector a)
                   -- ^ A collection of points known to be a polyline.
                   -- *Note* that in the future polylines will be expressed in
                   -- terms of the other constructors.



-- | Map all the vertices within a 'RawGeometry'.
mapRawGeometry :: (Unbox a, Unbox b) => (a -> b) -> RawGeometry a -> RawGeometry b
mapRawGeometry f (RawTriangles vs)     = RawTriangles $ V.map f vs
mapRawGeometry f (RawBeziers vs)       = RawBeziers $ V.map f vs
mapRawGeometry f (RawTriangleStrip vs) = RawTriangleStrip $ V.map f vs
mapRawGeometry f (RawTriangleFan vs)   = RawTriangleFan $ V.map f vs
mapRawGeometry f (RawLine vs)          = RawLine $ V.map f vs
--------------------------------------------------------------------------------
-- A Monad for defining geometry
--------------------------------------------------------------------------------
-- | A monad transformer for defining collections of geometries, specifically
-- mixed collections of triangles, beziers, strips, fans and polylines.
newtype GeometryT a m b =
  Geometry { unGeometry :: StateT (B.Vector (RawGeometry a)) m b}
-- | A pure context for defining collections of geometry.
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

-- | Add some geometry.
add :: Monad m => RawGeometry a -> StateT (B.Vector (RawGeometry a)) m ()
add a = modify (`B.snoc` a)

-- | Define and add some triangles.
triangles :: (Unbox a, Monad m) => VerticesT a m () -> GeometryT a m ()
triangles vs = Geometry $ do
  v <- lift $ runVerticesT vs
  add $ RawTriangles v

-- | Define and add some beziers.
beziers :: (Monad m, Unbox a) => VerticesT a m () -> GeometryT a m ()
beziers vs = Geometry $ do
  v <- lift $ runVerticesT vs
  add $ RawBeziers v

-- | Define and add a triangle strip.
strip :: (Monad m, Unbox a) => VerticesT a m () -> GeometryT a m ()
strip vs = Geometry $ do
  v <- lift $ runVerticesT vs
  add $ RawTriangleStrip v

-- | Define and add a triangle fan.
fan :: (Monad m, Unbox a) => VerticesT a m () -> GeometryT a m ()
fan vs = Geometry $ do
  v <- lift $ runVerticesT vs
  add $ RawTriangleFan v

-- | Define and add a polyline.
line :: (Monad m, Unbox a) => VerticesT a m () -> GeometryT a m ()
line vs = Geometry $ do
  v <- lift $ runVerticesT vs
  add $ RawLine v

-- | Extract the raw 'Vector' of geometries monadically.
runGeometryT :: Monad m => GeometryT a m b -> m (B.Vector (RawGeometry a))
runGeometryT = flip execStateT B.empty . unGeometry

-- | Extract the raw 'Vector' of geometries.
runGeometry :: Geometry a -> B.Vector (RawGeometry a)
runGeometry = runIdentity . runGeometryT

-- | Map all the vertices within all geometries in the computation.
mapGeometry :: (Monad m, Unbox a, Unbox c)
            => (a -> c) -> GeometryT a m b -> GeometryT c m ()
mapGeometry f s = Geometry $ do
  gs <- lift $ runGeometryT s
  put $ B.map (mapRawGeometry f) gs

-- | Extract only the raw 'Vector' of vertices within the geometry.
vertexData :: RawGeometry v -> Vector v
vertexData (RawTriangles vs)     = vs
vertexData (RawBeziers vs)       = vs
vertexData (RawTriangleStrip vs) = vs
vertexData (RawTriangleFan vs)   = vs
vertexData (RawLine vs)          = vs
--------------------------------------------------------------------------------
-- Special Rendering Options
--------------------------------------------------------------------------------
-- | Some special rendering options. Not much to see here.
data RenderingOption = StencilMaskOption
--------------------------------------------------------------------------------
-- Picture Data
--------------------------------------------------------------------------------
-- | Underlying picture data used to accumulate a visible picture.
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
-- | The empty 'PictureData'.
emptyPictureData :: PictureData t v
emptyPictureData =
    PictureData { _picDataGeometry   = B.empty
                , _picDataStroke    = []
                , _picDataTextures  = []
                , _picDataOptions   = []
                }

-- | Map 'realToFrac' over both.
bothToFrac :: (Real a, Fractional b) => (V2 a, V2 a) -> (V2 b, V2 b)
bothToFrac= second (fmap realToFrac) . first (fmap realToFrac)
--------------------------------------------------------------------------------
-- Picture Construction
--------------------------------------------------------------------------------
-- | A monad transformer computation that defines a picture.
type PictureT tex vert = StateT (PictureData tex vert)

-- | Extract the result and 'PictureData' from a 'PictureT' computation.
runPictureT :: PictureT t v m a -> m (a, PictureData t v)
runPictureT = flip runStateT emptyPictureData
--------------------------------------------------------------------------------
-- Identity Parameterized Pictures
--------------------------------------------------------------------------------
-- | 'PictureT' parameterized over 'Identity'.
type Picture t v = PictureT t v Identity

-- | Extract the result and 'PictureData' of a pure 'Picture' computation.
runPicture :: Picture t v a -> (a, PictureData t v)
runPicture = runIdentity . runPictureT

-- | Set the geometries of the 'PictureT' with a 'Vector' explicitly.
setRawGeometry :: Monad m => B.Vector (RawGeometry v) -> PictureT t v m ()
setRawGeometry vs = picDataGeometry .= vs

-- | Extract the current geometries of the 'PictureT' as a 'Vector'.
getRawGeometry :: Monad m => PictureT t v m (B.Vector (RawGeometry v))
getRawGeometry = use picDataGeometry

-- | Define and set the geometries of the 'PictureT'.
setGeometry :: Monad m => GeometryT v m () -> PictureT t v m ()
setGeometry = (setRawGeometry =<<) . lift . runGeometryT

-- | Set the stroke attributes of the 'PictureT'.
setStroke :: Monad m => [StrokeAttr] -> PictureT t v m ()
setStroke = (picDataStroke .=)

-- | Get the current stroke attributes of the 'PictureT'.
getStroke :: Monad m => PictureT t v m [StrokeAttr]
getStroke = use picDataStroke

-- | Set the textures contained within the 'PictureT'.
-- These textures @[t]@ are backend dependent.
setTextures :: Monad m => [t] -> PictureT t v m ()
setTextures = (picDataTextures .=)

-- | Get the current textures within the 'PictureT'.
getTextures :: Monad m => PictureT t v m [t]
getTextures = use picDataTextures

-- | Set any special rendering options. Nothing to see here.
setRenderingOptions :: Monad m => [RenderingOption] -> PictureT t v m ()
setRenderingOptions = (picDataOptions .=)

-- | Get any special rendering options. Nothing to see here.
getRenderingOptions :: Monad m => PictureT t v m [RenderingOption]
getRenderingOptions = use picDataOptions
--------------------------------------------------------------------------------
-- Measuring pictures
--------------------------------------------------------------------------------
-- | Evaluates the current geometry in the 'PictureT', mapping each vertex.
mapPictureVertices
  :: (Monad m, Unbox v, Unbox s)
  => (v -> s)
  -> PictureT t v m (V.Vector s)
mapPictureVertices mapper = do
  gs <- use picDataGeometry
  let f = V.map mapper . vertexData . (gs B.!)
  return $ V.concatMap f $ V.enumFromTo 0 (B.length gs - 1)

-- | Determines the bounds of a 'PictureT' defined in 2d space.
pictureBounds2 :: (Monad m, Unbox v)
               => (v -> V2 Float) -> PictureT t v m (V2 Float, V2 Float)
pictureBounds2 = (boundingBox <$>) . mapPictureVertices

-- | Determines the bounds of a 'PictureT' defined in 3d space.
pictureBounds3 :: (Monad m, Unbox v)
               => (v -> V3 Float) -> PictureT t v m BCube
pictureBounds3 = (boundingCube <$>) . mapPictureVertices

-- | Determines the size of a 'PictureT' defined in 2d space.
pictureSize2 :: (Monad m, Unbox v)
             => (v -> V2 Float) -> PictureT t v m (V2 Float)
pictureSize2 = pictureBounds2 >=> (return . uncurry (flip (-)))

-- | Determines the size of a 'PictureT' defined in 3d space.
pictureSize3 :: (Monad m, Unbox v)
             => (v -> V3 Float) -> PictureT t v m (V3 Float)
pictureSize3 = pictureBounds3 >=> (return . uncurry (flip (-)))

-- | Determines the origin of a 'PictureT' defined in 2d space.
pictureOrigin2 :: (Monad m, Unbox v)
               => (v -> V2 Float) -> PictureT t v m (V2 Float)
pictureOrigin2 = (fst <$>) . pictureBounds2

-- | Determines the origin of a 'PictureT' defined in 3d space.
pictureOrigin3 :: (Monad m, Unbox v)
               => (v -> V3 Float) -> PictureT t v m (V3 Float)
pictureOrigin3 = (fst <$>) . pictureBounds3

-- | Determines the center point of a 'PictureT' defined in 2d space.
pictureCenter2 :: (Monad m, Unbox v)
               => (v -> V2 Float) -> PictureT t v m (V2 Float)
pictureCenter2 vertToSpace = do
  (tl,br) <- pictureBounds2 vertToSpace
  return $ tl + (br - tl)/2

-- | Determines the center point of a 'PictureT' defined in 3d space.
pictureCenter3 :: (Monad m, Unbox v)
               => (v -> V3 Float) -> PictureT t v m (V3 Float)
pictureCenter3 vertToSpace = do
  (tl,br) <- pictureBounds3 vertToSpace
  return $ tl + (br - tl)/2

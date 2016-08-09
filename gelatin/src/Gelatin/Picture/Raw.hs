{-# LANGUAGE GADTs #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
module Gelatin.Picture.Raw where

import           Gelatin.Core
import qualified Data.Vector as B
import qualified Data.Vector.Unboxed as V
import           Data.Vector.Unboxed (Vector, Unbox)
import           Control.Monad.State.Strict
import           Control.Lens hiding (to)
import           Linear
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

vertices,withVertices :: Unbox a => State (Vector a) () -> Vertices a
vertices = Vertices
withVertices = vertices

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

runGeometry :: Geometry a -> B.Vector (RawGeometry a)
runGeometry = flip execState B.empty . unGeometry
--------------------------------------------------------------------------------
-- Mixing concrete vertex types
--------------------------------------------------------------------------------
type V2V4 = (V2 Float, V4 Float)
type V2V2 = (V2 Float, V2 Float)

data SupportedGeometry = ColorGeometry (B.Vector (RawGeometry V2V4))
                       | TextureGeometry (B.Vector (RawGeometry V2V2))
                       | NoGeometry

data SupportedOptions = StencilMaskOption

data DrawingData t = DrawingData { _drawGeometry :: SupportedGeometry
                                 , _drawStroke   :: [StrokeAttr]
                                 , _drawTexture  :: Maybe t
                                 , _drawOptions  :: [SupportedOptions]
                                 }
makeLenses ''DrawingData

colored :: [Geometry V2V4] -> SupportedGeometry
colored = ColorGeometry . B.concat . map runGeometry

coloredGeometry :: State (B.Vector (RawGeometry V2V4)) () -> SupportedGeometry
coloredGeometry = colored . (:[]) . geometry

usingColoredGeometry :: State (B.Vector (RawGeometry V2V4)) ()
                     -> State (DrawingData t) ()
usingColoredGeometry = (drawGeometry .=) . coloredGeometry

textured :: [Geometry V2V2] -> SupportedGeometry
textured = TextureGeometry . B.concat . map runGeometry

texturedGeometry :: State (B.Vector (RawGeometry V2V2)) () -> SupportedGeometry
texturedGeometry = textured . (:[]) . geometry

usingTexturedGeometry :: State (B.Vector (RawGeometry V2V2)) ()
                      -> State (DrawingData t) ()
usingTexturedGeometry = (drawGeometry .=) . texturedGeometry

usingStroke :: [StrokeAttr] -> State (DrawingData t) ()
usingStroke = (drawStroke .=)

usingTexture :: t -> State (DrawingData t) ()
usingTexture = (drawTexture .=) . Just

emptyDrawingData :: DrawingData t
emptyDrawingData = DrawingData NoGeometry [] Nothing []

newtype Drawing t = Drawing { unDrawing :: State (DrawingData t) () }

drawing :: State (DrawingData t) () -> Drawing t
drawing = Drawing

runDrawing :: Drawing t -> DrawingData t
runDrawing = flip execState emptyDrawingData . unDrawing

draw :: State (DrawingData t) () -> State (B.Vector (DrawingData t)) ()
draw = add . runDrawing . drawing

newtype Picture t = Picture { unPicture :: State (B.Vector (DrawingData t)) () }

picture :: State (B.Vector (DrawingData t)) () -> Picture t
picture = Picture

runPicture :: Picture t -> B.Vector (DrawingData t)
runPicture = flip execState B.empty . unPicture

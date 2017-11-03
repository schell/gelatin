{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Gelatin.Compiler where

import qualified Data.Vector as B
import           Data.Vector.Unboxed (Vector)
import           Data.Functor.Identity
import           Data.Foldable (foldl')
import           Linear (V4(..), V2(..), M44, identity, (!*!))
import           Control.Monad.IO.Class

import           Gelatin.Core
import           Gelatin.Picture.Internal
--------------------------------------------------------------------------------
-- Compilation results in a Renderer
--------------------------------------------------------------------------------
data RenderTransform v r s = Spatial (Affine v r)
                           | Special s

extractSpatial :: [RenderTransform v r s] -> [Affine v r]
extractSpatial = concatMap f
  where f (Spatial x) = [x]
        f _ = []

type Renderer v r s = (IO (), [RenderTransform v r s] -> IO ())
--------------------------------------------------------------------------------
-- Renderers can be transformed with many things, but here are some concrete
-- examples.
--------------------------------------------------------------------------------
data Raster = Alpha Float
            | Multiply (V4 Float)
            | ColorReplacement (V4 Float)
            deriving (Show, Eq)

type RenderTransform2 = RenderTransform (V2 Float) Float Raster
type Renderer2        = Renderer        (V2 Float) Float Raster
--------------------------------------------------------------------------------
-- Transformation Helpers
--------------------------------------------------------------------------------
unwrapTransforms :: [RenderTransform2]
                 -> (M44 Float, Float, V4 Float, Maybe (V4 Float))
unwrapTransforms = foldl' f (identity, 1, white, Nothing)
  where f (mv, alph, mlt, rep) (Spatial a) =
          (mv !*! affine2Modelview a, alph, mlt, rep)
        f (mv, alph, mlt, rep) (Special (Alpha a)) =
          (mv, alph * a, mlt, rep)
        f (mv, alph, mlt, rep) (Special (Multiply a)) =
          (mv, alph, mlt * a, rep)
        f (mv, alph, mlt, _) (Special (ColorReplacement a)) =
          (mv, alph, mlt, Just a)
--------------------------------------------------------------------------------
-- Conveniences for creating transformations
--------------------------------------------------------------------------------
move :: Float -> Float -> RenderTransform2
move x y = Spatial $ Translate $ V2 x y

moveV2 :: V2 Float -> RenderTransform2
moveV2 (V2 x y) = move x y

scale :: Float -> Float -> RenderTransform2
scale x y = Spatial $ Scale $ V2 x y

scaleV2 :: V2 Float -> RenderTransform2
scaleV2 (V2 x y) = scale x y

rotate :: Float -> RenderTransform2
rotate = Spatial . Rotate

alpha :: Float -> RenderTransform2
alpha = Special . Alpha

multiply :: Float -> Float -> Float -> Float -> RenderTransform2
multiply r g b a = Special $ Multiply $ V4 r g b a

multiplyV4 :: V4 Float -> RenderTransform2
multiplyV4 (V4 r g b a) = multiply r g b a

redChannelReplacement :: Float -> Float -> Float -> Float -> RenderTransform2
redChannelReplacement r g b a = Special $ ColorReplacement $ V4 r g b a

redChannelReplacementV4 :: V4 Float -> RenderTransform2
redChannelReplacementV4 (V4 r g b a) = redChannelReplacement r g b a
--------------------------------------------------------------------------------
-- Making compiling easier through types
--------------------------------------------------------------------------------
data VertexType = VertexTriangles
                | VertexBeziers
                | VertexStrip
                | VertexFan
                deriving (Show, Eq)

data GeometryCompiler vx v r s = GeometryCompiler
  { compileShapes :: VertexType -> Vector vx -> IO (Renderer v r s)
  , compileLine   :: Stroke     -> Vector vx -> IO (Renderer v r s)
  }

type MakeCompiler z vx v r s = z -> GeometryCompiler vx v r s
--------------------------------------------------------------------------------
-- Specifying the backend
--------------------------------------------------------------------------------
data BackendOps tex event = BackendOps
  { backendOpGetFramebufferSize :: IO (V2 Int)
  , backendOpGetWindowSize      :: IO (V2 Int)
  , backendOpClearWindow        :: IO ()
  , backendOpUpdateWindow       :: IO ()
  , backendOpSetClearColor      :: V4 Float -> IO ()
  , backendOpAllocTexture       :: FilePath -> IO (Maybe (tex, V2 Int))
  , backendOpBindTextures       :: [tex] -> IO () -> IO ()
  , backendOpGetEvents          :: IO [event]
  }
data BackendCompiler vert spatial rot rast = BackendComp
  { backendCompApplyOption :: Renderer spatial rot rast -> RenderingOption
                           -> Renderer spatial rot rast
  , backendCompCompiler    :: GeometryCompiler vert spatial rot rast
  }
data Backend tex event vert spatial rot rast = Backend
  { backendOps      :: BackendOps tex event
  , backendCompiler :: BackendCompiler vert spatial rot rast
  }

compiler :: Backend tex event vert spatial rot rast
         -> GeometryCompiler vert spatial rot rast
compiler = backendCompCompiler . backendCompiler

applyCompilerOption :: Backend tex event vert spatial rot rast
                    -> Renderer spatial rot rast
                    -> RenderingOption
                    -> Renderer spatial rot rast
applyCompilerOption b = backendCompApplyOption $ backendCompiler b

bindTextures :: Backend tex event vert spatial rot rast -> [tex] -> IO () -> IO ()
bindTextures b = backendOpBindTextures $ backendOps b

allocTexture :: Backend tex event vert spatial rot rast -> FilePath
                   -> IO (Maybe (tex, V2 Int))
allocTexture b = backendOpAllocTexture $ backendOps b

clearWindow :: Backend tex event vert spatial rot rast -> IO ()
clearWindow = backendOpClearWindow . backendOps

updateWindow :: Backend tex event vert spatial rot rast -> IO ()
updateWindow = backendOpUpdateWindow . backendOps

getEvents :: Backend tex event vert spatial rot rast -> IO [event]
getEvents = backendOpGetEvents . backendOps
--------------------------------------------------------------------------------
-- Compiling Concrete Picture Types
--------------------------------------------------------------------------------
compilePictureT :: MonadIO m
                => Backend tex event vert spatial rot rast
                -> PictureT tex vert m a
                -> m (a, Renderer spatial rot rast)
compilePictureT b pic = do
  (a, dat) <- runPictureT pic
  glr      <- compilePictureData b dat
  return (a, glr)

compilePicture :: MonadIO m
               => Backend tex event vert spatial rot rast
               -> Picture tex vert a
               -> m (a, Renderer spatial rot rast)
compilePicture b pic = do
  let (a, dat) = runIdentity $ runPictureT pic
  glr <- compilePictureData b dat
  return (a, glr)

compileGeometry :: GeometryCompiler vx v r s -> [StrokeAttr] -> RawGeometry vx
                -> IO (Renderer v r s)
compileGeometry GeometryCompiler{..} _ (RawTriangles v) =
  compileShapes VertexTriangles v
compileGeometry GeometryCompiler{..} _ (RawBeziers v) =
  compileShapes VertexBeziers v
compileGeometry GeometryCompiler{..} _ (RawTriangleStrip v) =
  compileShapes VertexStrip v
compileGeometry GeometryCompiler{..} _ (RawTriangleFan v) =
  compileShapes VertexFan v
compileGeometry GeometryCompiler{..} ss (RawLine v) =
  compileLine (strokeWith ss) v

compilePictureData :: MonadIO m
                   => Backend tex event vert spatial rot rast
                   -> PictureData tex vert
                   -> m (Renderer spatial rot rast)
compilePictureData b PictureData{..} = do
  let compile = liftIO . compileGeometry (compiler b) _picDataStroke
  glrs <- B.mapM compile _picDataGeometry
  let render rs = bindTextures b _picDataTextures $ mapM_ (($ rs) . snd) glrs
      clean = mapM_ fst glrs
      glr   = foldl (applyCompilerOption b) (clean, render) _picDataOptions
  return glr

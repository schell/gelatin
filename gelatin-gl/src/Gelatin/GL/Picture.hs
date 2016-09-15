{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Gelatin.GL.Picture where

import           Data.Monoid
import qualified Data.Vector as B
import qualified Data.Vector.Unboxed as V
import           Data.Vector.Unboxed (Vector, Unbox)
import           Linear as L
import           Control.Lens
import           Control.Monad.IO.Class
import           Graphics.GL.Types
import           Graphics.GL.Core33

import           Gelatin
import           Gelatin.Picture.Internal
import           Gelatin.GL.Common
import           Gelatin.GL.Renderer
--------------------------------------------------------------------------------
-- Concrete Picture Types
--------------------------------------------------------------------------------
type V2V4 = (V2 Float, V4 Float)
type ColorPictureData = PictureData GLuint (V2 Float) Float (V2 Float, V4 Float)
type ColorPictureT = PictureT GLuint (V2 Float) Float (V2 Float, V4 Float)
type ColorPicture = ColorPictureT Identity

type V2V2 = (V2 Float, V2 Float)
type TexturePictureData = PictureData GLuint (V2 Float) Float (V2 Float, V2 Float)
type TexturePictureT = PictureT GLuint (V2 Float) Float (V2 Float, V2 Float)
type TexturePicture = TexturePictureT Identity
--------------------------------------------------------------------------------
-- Making compiling easier through types
--------------------------------------------------------------------------------
data GeometryCompiler v uv = GeometryCompiler
  { compileTris :: Vector v -> Vector uv -> IO GLRenderer
  , compileBezs :: Vector v -> Vector uv -> IO GLRenderer
  , compileStrip:: Vector v -> Vector uv -> IO GLRenderer
  , compileFan  :: Vector v -> Vector uv -> IO GLRenderer
  , compileLine :: Stroke   -> Vector v  -> Vector uv -> IO GLRenderer
  }

type MakeCompiler v uv = Rez -> GeometryCompiler v uv

rgbaCompiler :: Rez -> GeometryCompiler (V2 Float) (V4 Float)
rgbaCompiler Rez{..} =
  GeometryCompiler
    (colorRenderer rezContext rezShader GL_TRIANGLES)
    (colorBezRenderer rezContext rezShader)
    (colorRenderer rezContext rezShader GL_TRIANGLE_STRIP)
    (colorRenderer rezContext rezShader GL_TRIANGLE_FAN)
    (\ Stroke{..} -> colorPolylineRenderer rezContext rezShader
                      strokeWidth strokeFeather strokeLineCaps)

uvCompiler :: Rez -> GeometryCompiler (V2 Float) (V2 Float)
uvCompiler Rez{..} =
  GeometryCompiler
    (textureRenderer rezContext rezShader GL_TRIANGLES)
    (textureBezRenderer rezContext rezShader)
    (textureRenderer rezContext rezShader GL_TRIANGLE_STRIP)
    (textureRenderer rezContext rezShader GL_TRIANGLE_FAN)
    (\ Stroke{..} -> texPolylineRenderer rezContext rezShader
                      strokeWidth strokeFeather strokeLineCaps)
--------------------------------------------------------------------------------
-- Compiling Concrete Picture Types
--------------------------------------------------------------------------------
compileGeometry :: (Unbox v, Unbox uv)
                => MakeCompiler v uv -> Rez -> [StrokeAttr] -> RawGeometry (v,uv)
                -> IO GLRenderer
compileGeometry mk rz _ (RawTriangles v) =
  let GeometryCompiler{..} = mk rz in uncurry compileTris $ V.unzip v
compileGeometry mk rz _ (RawBeziers v) =
  let GeometryCompiler{..} = mk rz in uncurry compileBezs $ V.unzip v
compileGeometry mk rz _ (RawTriangleStrip v) =
  let GeometryCompiler{..} = mk rz in uncurry compileStrip $ V.unzip v
compileGeometry mk rz _ (RawTriangleFan v) =
  let GeometryCompiler{..} = mk rz in uncurry compileFan $ V.unzip v
compileGeometry mk rz ss (RawLine v) =
  let s = strokeWith ss
      GeometryCompiler{..} = mk rz in uncurry (compileLine s) $ V.unzip v

extractTransformData :: PictureData t (V2 Float) Float v -> [RenderTransform]
extractTransformData PictureData{..} =
  let afs = map Spatial _picDataAffine
      ts  = Alpha _picDataAlpha : Multiply _picDataMultiply : afs
  in case _picDataReplaceColor of
       Nothing -> ts
       Just c  -> ColorReplacement c : ts

compilePictureData :: Unbox uv
                   => MakeCompiler (V2 Float) uv -> Rez
                   -> PictureData GLuint (V2 Float) Float (V2 Float, uv)
                   -> IO GLRenderer
compilePictureData mk rz pic@PictureData{..}
  | StencilMaskOption:ops <- _picDataOptions = do
    (c,r) <- compilePictureData mk rz pic{_picDataOptions=ops}
    return (c, \rs -> stencilMask (r rs) (r rs))
  | otherwise = do
    geomglrs <- B.mapM (compileGeometry mk rz _picDataStroke) _picDataGeometry
    chldglrs <- B.mapM (compilePictureData mk rz) _picDataChildren
    let qs = extractTransformData pic
        render rs = do bindTexsAround _picDataTextures $
                         flip mapM_ geomglrs $ \(_,glr) -> glr $ qs ++ rs
                       flip mapM_ chldglrs $ \(_,glr) -> glr $ qs ++ rs
        clean = do mapM_ fst geomglrs
                   mapM_ fst chldglrs
    return (clean, render)

compileColorPictureData :: Rez -> ColorPictureData -> IO GLRenderer
compileColorPictureData = compilePictureData rgbaCompiler

compileTexturePictureData :: Rez -> TexturePictureData -> IO GLRenderer
compileTexturePictureData = compilePictureData uvCompiler
--------------------------------------------------------------------------------
-- Top level compilation functions
--------------------------------------------------------------------------------
compileColorPictureT :: MonadIO m => Rez -> ColorPictureT m a -> m (a, GLRenderer)
compileColorPictureT rz pic = do
  (a, dat) <- runPictureT pic
  glr <- liftIO $ compileColorPictureData rz dat
  return (a,glr)

compileTexturePictureT :: MonadIO m => Rez -> TexturePictureT m a -> m (a, GLRenderer)
compileTexturePictureT rz pic = do
  (a, dat) <- runPictureT pic
  glr <- liftIO $ compileTexturePictureData rz dat
  return (a,glr)

compileColorPicture :: MonadIO m => Rez -> ColorPicture a -> m (a, GLRenderer)
compileColorPicture rz pic = do
  let (a, dat) = runPicture pic
  glr <- liftIO $ compileColorPictureData rz dat
  return (a,glr)

compileTexturePicture :: MonadIO m => Rez -> TexturePicture a -> m (a, GLRenderer)
compileTexturePicture rz pic = do
  let (a, dat) = runPicture pic
  glr <- liftIO $ compileTexturePictureData rz dat
  return (a,glr)

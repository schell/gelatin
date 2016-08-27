{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Gelatin.GL.Picture where

import           Data.Monoid
import qualified Data.Vector as B
import qualified Data.Vector.Unboxed as V
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
type ColorPictureData = PictureData () (V2 Float) Float (V2 Float, V4 Float)
type ColorPictureT = PictureT () (V2 Float) Float (V2 Float, V4 Float)
type ColorPicture = ColorPictureT Identity

type V2V2 = (V2 Float, V2 Float)
type TexturePictureData = PictureData GLuint (V2 Float) Float (V2 Float, V2 Float)
type TexturePictureT = PictureT GLuint (V2 Float) Float (V2 Float, V2 Float)
type TexturePicture = TexturePictureT Identity
--------------------------------------------------------------------------------
-- Compiling Concrete Picture Types
--------------------------------------------------------------------------------
compileColorGeometry :: Rez -> Stroke -> RawGeometry V2V4 -> IO GLRenderer
compileColorGeometry Rez{..} _ (RawTriangles v) =
  let (vs, cs) = V.unzip v
  in colorRenderer rezContext rezShader GL_TRIANGLES vs cs
compileColorGeometry Rez{..} _ (RawBeziers v) =
  let (vs, cs) = V.unzip v
  in colorBezRenderer rezContext rezShader vs cs
compileColorGeometry Rez{..} _ (RawTriangleStrip v) =
  let (vs, cs) = V.unzip v
  in colorRenderer rezContext rezShader GL_TRIANGLE_STRIP vs cs
compileColorGeometry Rez{..} _ (RawTriangleFan v) =
  let (vs, cs) = V.unzip v
  in colorRenderer rezContext rezShader GL_TRIANGLE_FAN vs cs
compileColorGeometry Rez{..} Stroke{..} (RawLine v) =
  let (vs, cs) = V.unzip v
  in colorPolylineRenderer rezContext rezShader strokeWidth strokeFeather
                           strokeLineCaps vs cs

compileTextureGeometry :: Rez -> Stroke -> RawGeometry V2V2 -> IO GLRenderer
compileTextureGeometry Rez{..} _ (RawTriangles v) =
  let (vs, cs) = V.unzip v
  in textureRenderer rezContext rezShader GL_TRIANGLES vs cs
compileTextureGeometry Rez{..} _ (RawBeziers v) =
  let (vs, cs) = V.unzip v
  in textureBezRenderer rezContext rezShader vs cs
compileTextureGeometry Rez{..} _ (RawTriangleStrip v) =
  let (vs, cs) = V.unzip v
  in textureRenderer rezContext rezShader GL_TRIANGLE_STRIP vs cs
compileTextureGeometry Rez{..} _ (RawTriangleFan v) =
  let (vs, cs) = V.unzip v
  in textureRenderer rezContext rezShader GL_TRIANGLE_FAN vs cs
compileTextureGeometry Rez{..} Stroke{..} (RawLine v) =
  let (vs, cs) = V.unzip v
  in texPolylineRenderer rezContext rezShader strokeWidth strokeFeather
                         strokeLineCaps vs cs

picDataToPicTransform :: PictureData t (V2 Float) Float v -> PictureTransform
picDataToPicTransform p@PictureData{..} =
  PictureTransform (calcV2VX_mv p) _picDataAlpha _picDataMultiply _picDataReplaceColor

compileColorPictureData :: Rez -> ColorPictureData -> IO GLRenderer
compileColorPictureData rz p = compileColorPictureData' rz p mempty

compileColorPictureData' :: Rez -> ColorPictureData -> PictureTransform -> IO GLRenderer
compileColorPictureData' rz pic@PictureData{..} parentTfrm
  | StencilMaskOption:ops <- _picDataOptions = do
    (c,r) <- compileColorPictureData' rz pic{ _picDataOptions = ops } parentTfrm
    return (c, \t -> stencilMask (r t) (r t))
  | otherwise = do
    r <- transformRenderer parentTfrm <$> B.foldM fc mempty _picDataGeometry
    let t = picDataToPicTransform pic
    r1 <- B.foldM (fg t) mempty _picDataChildren
    return $ r <> r1
  where fc r0 g = (r0 <>) <$> compileColorGeometry rz (strokeWith _picDataStroke) g
        fg t r0 p = (r0 <>) <$> compileColorPictureData' rz p (parentTfrm <> t)

compileTexturePictureData :: Rez -> TexturePictureData -> IO GLRenderer
compileTexturePictureData rz pic@PictureData{..}
  | StencilMaskOption:ops <- _picDataOptions = do
    (c,r) <- compileTexturePictureData rz pic{ _picDataOptions = ops }
    return (c, \t -> stencilMask (r t) (r t))
  | otherwise = do
    (c0,r0) <- B.foldM fc mempty _picDataGeometry
    (c1,r1) <- B.foldM fg mempty _picDataChildren
    let t = picDataToPicTransform pic
        rfin tfrm = bindTexsAround _picDataTextures (r0 tfrm) >> r1 tfrm
    return (c0 >> c1, rfin . (t<>))
  where fc r0 g = (r0 <>) <$> compileTextureGeometry rz (strokeWith _picDataStroke) g
        fg r0 p = (r0 <>) <$> compileTexturePictureData rz p
--------------------------------------------------------------------------------
-- Top level compilation functions
--------------------------------------------------------------------------------
compileColorPicture :: Rez -> ColorPicture a -> IO GLRenderer
compileColorPicture rz = compileColorPictureData rz . snd . runPicture

compileTexturePicture :: Rez -> TexturePicture a -> IO GLRenderer
compileTexturePicture rz = compileTexturePictureData rz . snd . runPicture

compileColorPictureT :: MonadIO m => Rez -> ColorPictureT m a -> m GLRenderer
compileColorPictureT rz pic = do
  dat <- snd <$> runPictureT pic
  liftIO $ compileColorPictureData rz dat

compileTexturePictureT :: MonadIO m => Rez -> TexturePictureT m a -> m GLRenderer
compileTexturePictureT rz pic = do
  dat <- snd <$> runPictureT pic
  liftIO $ compileTexturePictureData rz dat

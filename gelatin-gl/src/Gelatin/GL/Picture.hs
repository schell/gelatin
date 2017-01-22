{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Gelatin.GL.Picture where

import           Control.Lens               hiding (op)
import           Control.Monad              ((>=>))
import           Data.Bits                  ((.|.))
import qualified Data.Vector.Unboxed        as V
import           Graphics.GL.Core33
import           Graphics.GL.Types
import           Linear                     as L

import           Gelatin
import           Gelatin.GL.Renderer.Common
import qualified Gelatin.GL.Renderer.R2     as R2
import qualified Gelatin.GL.Renderer.R3     as R3
--------------------------------------------------------------------------------
-- 2D Picture Types
--------------------------------------------------------------------------------
type V2V4 = (V2 Float, V4 Float)
type ColorPictureData = PictureData GLuint (V2 Float, V4 Float)
type ColorPictureT = PictureT GLuint (V2 Float, V4 Float)
type ColorPicture = ColorPictureT Identity

type V2V2 = (V2 Float, V2 Float)
type TexturePictureData = PictureData GLuint (V2 Float, V2 Float)
type TexturePictureT = PictureT GLuint (V2 Float, V2 Float)
type TexturePicture = TexturePictureT Identity
--------------------------------------------------------------------------------
-- 3D Picture Types
--------------------------------------------------------------------------------
type V3V4 = (V3 Float, V4 Float)
type ColorPictureData3 = PictureData GLuint (V3 Float, V4 Float)
type ColorPictureT3 = PictureT GLuint (V3 Float, V4 Float)
type ColorPicture3 = ColorPictureT3 Identity

type V3V2 = (V3 Float, V2 Float)
type TexturePictureData3 = PictureData GLuint (V3 Float, V2 Float)
type TexturePictureT3 = PictureT GLuint (V3 Float, V2 Float)
type TexturePicture3 = TexturePictureT3 Identity

rgbaCompiler2 :: Rez
             -> GeometryCompiler V2V4 (V2 Float) Float Raster
rgbaCompiler2 Rez{..} = GeometryCompiler s l
  where s VertexTriangles =
          uncurry (R2.colorRenderer rezContext rezShader GL_TRIANGLES) . V.unzip
        s VertexStrip =
          uncurry (R2.colorRenderer rezContext rezShader GL_TRIANGLE_STRIP) . V.unzip
        s VertexFan =
          uncurry (R2.colorRenderer rezContext rezShader GL_TRIANGLE_FAN) . V.unzip
        s VertexBeziers =
          uncurry (R2.colorBezRenderer rezContext rezShader) . V.unzip
        l Stroke{..} =
            uncurry (R2.colorPolylineRenderer rezContext rezShader strokeWidth
                      strokeFeather strokeLineCaps) . V.unzip

rgbaCompiler3 :: Rez
              -> GeometryCompiler V3V4 (V3 Float) (Quaternion Float) Raster
rgbaCompiler3 Rez{..} = GeometryCompiler s l
  where s VertexTriangles =
          uncurry (R3.colorRenderer rezContext rezShader GL_TRIANGLES) . V.unzip
        s VertexStrip =
          uncurry (R3.colorRenderer rezContext rezShader GL_TRIANGLE_STRIP) . V.unzip
        s VertexFan =
          uncurry (R3.colorRenderer rezContext rezShader GL_TRIANGLE_FAN) . V.unzip
        s VertexBeziers =
          uncurry (R3.colorRenderer rezContext rezShader GL_TRIANGLES) . V.unzip
        -- Lines are a non-op
        l Stroke{..} = const $ return (return (), const $ return ())

uvCompiler2 :: Rez -> GeometryCompiler V2V2 (V2 Float) Float Raster
uvCompiler2 Rez{..} = GeometryCompiler s l
  where s VertexTriangles =
          uncurry (R2.textureRenderer rezContext rezShader GL_TRIANGLES) . V.unzip
        s VertexStrip =
          uncurry (R2.textureRenderer rezContext rezShader GL_TRIANGLE_STRIP) . V.unzip
        s VertexFan =
          uncurry (R2.textureRenderer rezContext rezShader GL_TRIANGLE_FAN) . V.unzip
        s VertexBeziers =
          uncurry (R2.textureBezRenderer rezContext rezShader) . V.unzip
        l Stroke{..} =
            uncurry (R2.texPolylineRenderer rezContext rezShader strokeWidth
                      strokeFeather strokeLineCaps) . V.unzip

uvCompiler3 :: Rez -> GeometryCompiler V3V2 (V3 Float) (Quaternion Float) Raster
uvCompiler3 Rez{..} = GeometryCompiler s l
  where s VertexTriangles =
          uncurry (R3.textureRenderer rezContext rezShader GL_TRIANGLES) . V.unzip
        s VertexStrip =
          uncurry (R3.textureRenderer rezContext rezShader GL_TRIANGLE_STRIP) . V.unzip
        s VertexFan =
          uncurry (R3.textureRenderer rezContext rezShader GL_TRIANGLE_FAN) . V.unzip
        s VertexBeziers =
          uncurry (R3.textureRenderer rezContext rezShader GL_TRIANGLES) . V.unzip
        l Stroke{..} = const $ return (return (), const $ return ())

applyOption :: (c, rs -> IO ()) -> RenderingOption -> (c, rs -> IO ())
applyOption (c, r) StencilMaskOption = (c, \rs -> stencilMask (r rs) (r rs))

glV2V4Compiler :: Rez -> BackendCompiler V2V4 (V2 Float) Float Raster
glV2V4Compiler rz = BackendComp
  { backendCompApplyOption = applyOption
  , backendCompCompiler = rgbaCompiler2 rz
  }

glV2V2Compiler :: Rez -> BackendCompiler V2V2 (V2 Float) Float Raster
glV2V2Compiler rz = BackendComp
  { backendCompApplyOption = applyOption
  , backendCompCompiler = uvCompiler2 rz
  }

glV3V4Compiler :: Rez -> BackendCompiler V3V4 (V3 Float) (Quaternion Float) Raster
glV3V4Compiler rz = BackendComp
  { backendCompApplyOption = applyOption
  , backendCompCompiler = rgbaCompiler3 rz
  }

glV3V2Compiler :: Rez -> BackendCompiler V3V2 (V3 Float) (Quaternion Float) Raster
glV3V2Compiler rz = BackendComp
  { backendCompApplyOption = applyOption
  , backendCompCompiler = uvCompiler3 rz
  }

glOps :: Rez -> IO () -> IO [a] -> (M44 Float -> IO ()) -> BackendOps GLuint a
glOps Rez{..} windowUpdate getEvs projectionUpdate = BackendOps
  { backendOpGetFramebufferSize = uncurry V2 <$> ctxFramebufferSize rezContext
  , backendOpGetWindowSize = uncurry V2 <$> ctxWindowSize rezContext
  , backendOpClearWindow = do
      (fbw,fbh) <- ctxFramebufferSize rezContext
      glViewport 0 0 (fromIntegral fbw) (fromIntegral fbh)
      glClear $ GL_COLOR_BUFFER_BIT .|. GL_DEPTH_BUFFER_BIT
  , backendOpUpdateWindow = windowUpdate
  , backendOpUpdateProjection = projectionUpdate
  , backendOpSetClearColor = \(V4 r g b a) -> glClearColor r g b a
  , backendOpAllocTexture = loadImage >=> \case
     Nothing -> return Nothing
     Just (sz, tex) -> return $ Just (tex, sz)
  , backendOpBindTextures = bindTexsAround
  , backendOpGetEvents = getEvs
  }

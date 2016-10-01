{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Gelatin.GL.Picture where

import qualified Data.Vector.Unboxed as V
import           Data.Bits ((.|.))
import           Linear as L
import           Control.Lens hiding (op)
import           Control.Monad ((>=>))
import           Graphics.GL.Types
import           Graphics.GL.Core33

import           Gelatin
import           Gelatin.GL.Common
import           Gelatin.GL.Renderer
--------------------------------------------------------------------------------
-- Concrete Picture Types
--------------------------------------------------------------------------------
type V2V4 = (V2 Float, V4 Float)
type ColorPictureData = PictureData GLuint (V2 Float, V4 Float)
type ColorPictureT = PictureT GLuint (V2 Float, V4 Float)
type ColorPicture = ColorPictureT Identity

type V2V2 = (V2 Float, V2 Float)
type TexturePictureData = PictureData GLuint (V2 Float, V2 Float)
type TexturePictureT = PictureT GLuint (V2 Float, V2 Float)
type TexturePicture = TexturePictureT Identity

rgbaCompiler :: Rez
             -> GeometryCompiler V2V4 (V2 Float) Float Raster
rgbaCompiler Rez{..} = GeometryCompiler s l
  where s VertexTriangles =
          uncurry (colorRenderer rezContext rezShader GL_TRIANGLES) . V.unzip
        s VertexStrip =
          uncurry (colorRenderer rezContext rezShader GL_TRIANGLE_STRIP) . V.unzip
        s VertexFan =
          uncurry (colorRenderer rezContext rezShader GL_TRIANGLE_FAN) . V.unzip
        s VertexBeziers =
          uncurry (colorBezRenderer rezContext rezShader) . V.unzip
        l Stroke{..} =
            uncurry (colorPolylineRenderer rezContext rezShader strokeWidth
                      strokeFeather strokeLineCaps) . V.unzip

uvCompiler :: Rez -> GeometryCompiler V2V2 (V2 Float) Float Raster
uvCompiler Rez{..} = GeometryCompiler s l
  where s VertexTriangles =
          uncurry (textureRenderer rezContext rezShader GL_TRIANGLES) . V.unzip
        s VertexStrip =
          uncurry (textureRenderer rezContext rezShader GL_TRIANGLE_STRIP) . V.unzip
        s VertexFan =
          uncurry (textureRenderer rezContext rezShader GL_TRIANGLE_FAN) . V.unzip
        s VertexBeziers =
          uncurry (textureBezRenderer rezContext rezShader) . V.unzip
        l Stroke{..} =
            uncurry (texPolylineRenderer rezContext rezShader strokeWidth
                      strokeFeather strokeLineCaps) . V.unzip

applyOption :: (c, rs -> IO ()) -> RenderingOption -> (c, rs -> IO ())
applyOption (c, r) StencilMaskOption = (c, \rs -> stencilMask (r rs) (r rs))

glV2V4Compiler :: Rez -> BackendCompiler V2V4 (V2 Float) Float Raster
glV2V4Compiler rz = BackendComp
  { backendCompApplyOption = applyOption
  , backendCompCompiler = rgbaCompiler rz
  }

glV2V2Compiler :: Rez -> BackendCompiler V2V2 (V2 Float) Float Raster
glV2V2Compiler rz = BackendComp
  { backendCompApplyOption = applyOption
  , backendCompCompiler = uvCompiler rz
  }

glOps :: Rez -> IO () -> (IO [a]) -> BackendOps GLuint a
glOps Rez{..} windowUpdate getEvents = BackendOps
  { backendOpGetFramebufferSize = uncurry V2 <$> ctxFramebufferSize rezContext
  , backendOpGetWindowSize = uncurry V2 <$> ctxWindowSize rezContext
  , backendOpClearWindow = do
      (fbw,fbh) <- ctxFramebufferSize rezContext
      glViewport 0 0 (fromIntegral fbw) (fromIntegral fbh)
      glClear $ GL_COLOR_BUFFER_BIT .|. GL_DEPTH_BUFFER_BIT
  , backendOpUpdateWindow = windowUpdate
  , backendOpSetClearColor = \(V4 r g b a) -> glClearColor r g b a
  , backendOpAllocTexture = loadImage >=> \case
     Nothing -> return Nothing
     Just (sz, tex) -> return $ Just (tex, sz)
  , backendOpBindTextures = bindTexsAround
  , backendOpGetEvents = getEvents
  }

{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Gelatin.WebGL.Common where

import           Control.Monad.Reader
import           Control.Monad.Trans.Either
import           GHCJS.DOM.Types

import           Gelatin
import           Gelatin.Shaders

type V2V4 = (V2 Float, V4 Float)
type ColorPictureData = PictureData WebGLTexture (V2 Float, V4 Float)
type ColorPictureT = PictureT WebGLTexture (V2 Float, V4 Float)

type V2V2 = (V2 Float, V2 Float)
type TexturePictureData = PictureData WebGLTexture (V2 Float, V2 Float)
type TexturePictureT = PictureT WebGLTexture (V2 Float, V2 Float)

type WebGLV2V2 =
  Backend WebGLTexture () V2V2 (V2 Float) Float Raster

type WebGLV2V4 =
  Backend WebGLTexture () V2V4 (V2 Float) Float Raster

data WebGLBackends = WebGLBackends { backendV2V4    :: WebGLV2V4
                                   , backendV2V2    :: WebGLV2V2
                                   , backendContext :: WebGLRenderingContextBase
                                   , backendCanvas  :: HTMLCanvasElement
                                   }

type WGLShaderDef    = ShaderDef GLenum Simple2DAttrib
type WGLShader       = Shader WebGLProgram WebGLUniformLocation

data GelatinContext = GelatinContext
  { gelCanvas           :: HTMLCanvasElement
  , gelRenderingContext :: WebGLRenderingContextBase
  }

type WebGLT m = EitherT String (ReaderT WebGLRenderingContextBase m)

type Gelatin = ReaderT GelatinContext IO

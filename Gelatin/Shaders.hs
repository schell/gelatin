{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Gelatin.Shaders where

import           Linear
import           Data.Vinyl.Universe
import           Graphics.Rendering.OpenGL hiding (Color, color)
import           Graphics.GLUtil
import qualified Data.ByteString.Char8 as BS

instance HasVariableType (Color4 GLfloat) where variableType _ = FloatVec4

colorShaderVertSrc :: BS.ByteString
colorShaderVertSrc = BS.pack $ unlines
    [ "uniform mat4 projection;"
    , "uniform mat4 modelview;"
    , "attribute vec3 vertex;"
    , "attribute vec4 color;"
    , "varying vec4 fcolor;"
    , "void main(void) {"
    , " fcolor = color;"
    , " gl_Position = projection * modelview * vec4(vertex, 1.0);"
    , "}"
    ]

colorShaderFragSrc :: BS.ByteString
colorShaderFragSrc = BS.pack $ unlines
    [ "varying vec4 fcolor;"
    , "void main(void) {"
    , " gl_FragColor = fcolor;"
    , "}"
    ]

type Projection = "projection" ::: M44 GLfloat
type Modelview  = "modelview" ::: M44 GLfloat
type V3gl  = "vertex" ::: V3 GLfloat
type V4gl = "color" ::: V4 GLfloat

vertex :: SField V3gl
vertex = SField

color :: SField V4gl
color = SField

projection :: SField Projection
projection = SField

modelview :: SField Modelview
modelview = SField

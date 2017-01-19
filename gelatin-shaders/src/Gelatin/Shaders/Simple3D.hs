{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}
module Gelatin.Shaders.Simple3D where

import           Gelatin

import           Gelatin.Shaders.Common
import           Gelatin.Shaders.TypeLevel

type APosition = Attribute "position" (V3 Float) 0
type AColor    = Attribute "color"    (V4 Float) 1
type AUV       = Attribute "uv"       (V2 Float) 2
type ANormal   = Attribute "normal"   (V3 Float) 3

type Simple3DAttribs = '[APosition, AColor, AUV, ANormal]
type Simple3DAttribToggles = TypeMap AttributeToggling Simple3DAttribs
type Simple3DAttribBuffers = TypeMap AttributeBuffering Simple3DAttribs

type UProjection         = Uniform "projection"         (M44 Float)
type UModelView          = Uniform "modelview"          (M44 Float)
type USampler            = Uniform "sampler"            Int
type UHasUV              = Uniform "hasUV"              Bool
type UAlpha              = Uniform "alpha"              Float
type UMult               = Uniform "mult"               (V4 Float)
type UShouldReplaceColor = Uniform "shouldColorReplace" Bool
type UReplaceColor       = Uniform "replaceColor"       (V4 Float)

type Simple3DUniforms = '[ UProjection
                         , UModelView
                         , USampler
                         , UHasUV
                         , UAlpha
                         , UMult
                         , UShouldReplaceColor
                         , UReplaceColor
                         ]

type Simple3DShaders = '[VertexShader, FragmentShader]

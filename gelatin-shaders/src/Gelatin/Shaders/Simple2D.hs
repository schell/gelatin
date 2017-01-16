{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}
module Gelatin.Shaders.Simple2D where

import           Gelatin

import           Gelatin.Shaders.Common
import           Gelatin.Shaders.TypeLevel

-- | TODO: Most of this stuff is shader specific and shoud be moved to a
-- different part of the repo.

--------------------------------------------------------------------------------
-- $layout
-- Attributes layout locations are unique and global.
--------------------------------------------------------------------------------
type APosition = Attribute "position" (V2 Float) 0
type AColor    = Attribute "color"    (V4 Float) 1
type AUV       = Attribute "uv"       (V2 Float) 2
type ABez      = Attribute "bez"      (V3 Float) 3
type ABezUV    = Attribute "bezuv"    (V2 Float) 4
type APrev     = Attribute "prev"     (V2 Float) 5
type ANext     = Attribute "next"     (V2 Float) 6

type Simple2DAttribs = '[APosition, AColor, AUV, ABez, ABezUV, APrev, ANext]
type Simple2DAttribToggles = TypeMap AttributeToggling Simple2DAttribs
type Simple2DAttribBuffers = TypeMap AttributeBuffering Simple2DAttribs

--------------------------------------------------------------------------------
-- $uniforms
-- Uniform Helper Types
--------------------------------------------------------------------------------
data PrimType = PrimTri
              | PrimBez
              | PrimLine
              | PrimMask
              deriving (Show, Eq, Enum, Ord, Bounded)
--------------------------------------------------------------------------------
-- Updating uniforms
--------------------------------------------------------------------------------
type UPrimType           = Uniform "primitive"          PrimType
type UProjection         = Uniform "projection"         (M44 Float)
type UModelView          = Uniform "modelview"          (M44 Float)
type UThickness          = Uniform "thickness"          Float
type UFeather            = Uniform "feather"            Float
type USumLength          = Uniform "sumlength"          Float
type ULineCaps           = Uniform "cap"                (LineCap,LineCap)
type UHasUV              = Uniform "hasUV"              Bool
type USampler            = Uniform "sampler"            Int
type UMainTex            = Uniform "mainTex"            Int
type UMaskTex            = Uniform "maskTex"            Int
type UAlpha              = Uniform "alpha"              Float
type UMult               = Uniform "mult"               (V4 Float)
type UShouldReplaceColor = Uniform "shouldColorReplace" Bool
type UReplaceColor       = Uniform "replaceColor"       (V4 Float)

type Simple2DUniforms = '[ UPrimType
                         , UProjection
                         , UModelView
                         , UThickness
                         , UFeather
                         , USumLength
                         , ULineCaps
                         , UHasUV
                         , USampler
                         , UMainTex
                         , UMaskTex
                         , UAlpha
                         , UMult
                         , UShouldReplaceColor
                         , UReplaceColor
                         ]

type Simple2DShaders = '[VertexShader, FragmentShader]

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Gelatin.Shaders.Core where

import Graphics.Rendering.OpenGL hiding (Color, color, position)
import Data.Vinyl.Universe
import Linear
import Gelatin.ShaderCommands

--------------------------------------------------------------------------------
-- Types
--------------------------------------------------------------------------------
type V2tx = "texcoord" ::: V2 GLfloat
type V3pos = "position" ::: V3 GLfloat
type V4clr = "color" ::: V4 GLfloat
----------------------------------------------------------------------------------
---- Vinyl
----------------------------------------------------------------------------------
position :: SField V3pos
position = SField

color :: SField V4clr
color = SField

texcoord :: SField V2tx
texcoord = SField

setProjection :: Real a => M44 a -> ShaderCommand ()
setProjection = setUniform . uniformM44GLfloat "projection"

setModelview :: Real a => M44 a -> ShaderCommand ()
setModelview = setUniform . uniformM44GLfloat "modelview"

setSampler :: Integral a => a -> ShaderCommand ()
setSampler = setUniform . uniformGLint "sampler"

uniformM44GLfloat :: Real a => String -> M44 a -> ShaderUniform (M44 GLfloat)
uniformM44GLfloat s = ShaderUniform s . fmap (fmap realToFrac)

uniformGLint :: Integral a => String -> a -> ShaderUniform GLint
uniformGLint s = ShaderUniform s . fromIntegral

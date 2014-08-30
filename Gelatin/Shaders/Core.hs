{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Gelatin.Shaders.Core where

import Graphics.Rendering.OpenGL hiding (Color, color, position)
import Graphics.GLUtil
import Data.Vinyl.Universe
import Linear

--------------------------------------------------------------------------------
-- Types
--------------------------------------------------------------------------------

type UniformType u t = u ::: t

data Uniform = PJ (SField ("projection" ::: M44 GLfloat))
             | MV (SField ("modelview" ::: M44 GLfloat))

type UniformM44 u = u ::: M44 GLfloat
type Projection = "projection" ::: M44 GLfloat
type Modelview  = "modelview" ::: M44 GLfloat
type Sampler = "sampler" ::: GLint

type V2tx = "texcoord" ::: V2 GLfloat
type V3pos = "position" ::: V3 GLfloat
type V4clr = "color" ::: V4 GLfloat

--------------------------------------------------------------------------------
-- Vinyl
--------------------------------------------------------------------------------

position :: SField V3pos
position = SField

color :: SField V4clr
color = SField

texcoord :: SField V2tx
texcoord = SField

projection :: SField Projection
projection = SField

modelview :: SField Modelview
modelview = SField

sampler :: SField Sampler
sampler = SField

--------------------------------------------------------------------------------
-- Instances
--------------------------------------------------------------------------------

instance HasVariableType (Color4 GLfloat) where variableType _ = FloatVec4

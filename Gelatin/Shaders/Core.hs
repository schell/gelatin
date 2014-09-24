{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Gelatin.Shaders.Core where

import Graphics.Rendering.OpenGL hiding (Color, color, position, VertexComponent)
import Data.Vinyl.Universe
import Linear
import Foreign
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
--position :: SField V3pos
--position = SField

--color :: SField V4clr
--color = SField

--texcoord :: SField V2tx
--texcoord = SField

position :: Real a => [V3 a] -> VertexComponent (V3 GLfloat) [Float]
position = attrVecVF "position" 3

color :: Real a => [V4 a] -> VertexComponent (V4 GLfloat) [Float]
color = attrVecVF "color" 4

texcoord :: Real a => [V2 a] -> VertexComponent (V2 GLfloat) [Float]
texcoord = attrVecVF "texcoord" 2

attrVecVF :: (Real a, Functor f, Fractional b) => String -> NumComponents -> [f a] -> VertexComponent (f b) a1
attrVecVF s n vs = VertexComponent s vs' ToFloat $ VertexArrayDescriptor n Float 0 nullPtr
    where vs' = map (fmap realToFrac) vs

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

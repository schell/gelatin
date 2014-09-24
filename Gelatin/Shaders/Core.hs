{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Gelatin.Shaders.Core where

import Graphics.Rendering.OpenGL hiding (Color, color, position, VertexComponent)
import Linear
import Foreign
import Gelatin.ShaderCommands

--------------------------------------------------------------------------------
-- Helpers
--------------------------------------------------------------------------------
(.+) :: (Storable a) => VertexBufferCommand () -> VertexComponent a b -> VertexBufferCommand ()
(.+) m xs = m >> addComponent xs
--------------------------------------------------------------------------------
-- Vertex Attributes & Uniforms
--------------------------------------------------------------------------------
position :: Real a => [V3 a] -> VertexComponent (V3 GLfloat) [Float]
position = avecfv "position" 3

color :: Real a => [V4 a] -> VertexComponent (V4 GLfloat) [Float]
color = avecfv "color" 4

texcoord :: Real a => [V2 a] -> VertexComponent (V2 GLfloat) [Float]
texcoord = avecfv "texcoord" 2

avecfv :: (Real a, Functor f, Fractional b) => String -> NumComponents -> [f a] -> VertexComponent (f b) a1
avecfv s n vs = VertexComponent s vs' ToFloat $ VertexArrayDescriptor n Float 0 nullPtr
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

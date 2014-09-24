module Gelatin.Shaders.Core where

import Gelatin.ShaderCommands
import Foreign
import Linear
import Graphics.Rendering.OpenGL hiding (VertexComponent)

--------------------------------------------------------------------------------
-- Included Vertex Attributes & Uniforms
--------------------------------------------------------------------------------
position :: Real a => [V3 a] -> VertexComponent (V3 GLfloat) [Float]
position = avecfv "position" 3

color :: Real a => [V4 a] -> VertexComponent (V4 GLfloat) [Float]
color = avecfv "color" 4

texcoord :: Real a => [V2 a] -> VertexComponent (V2 GLfloat) [Float]
texcoord = avecfv "texcoord" 2

setProjection :: Real a => M44 a -> ShaderCommand ()
setProjection = setUniform . uniformM4f "projection"

setModelview :: Real a => M44 a -> ShaderCommand ()
setModelview = setUniform . uniformM4f "modelview"

setSampler :: Integral a => a -> ShaderCommand ()
setSampler = setUniform . uniformi "sampler"
--------------------------------------------------------------------------------
-- Uniform Helpers
--------------------------------------------------------------------------------
-- | TODO: Add more of these uniform helpers!
uniformM4f :: Real a => String -> M44 a -> ShaderUniform (M44 GLfloat)
uniformM4f s = ShaderUniform s . fmap (fmap realToFrac)

uniformi :: Integral a => String -> a -> ShaderUniform GLint
uniformi s = ShaderUniform s . fromIntegral
--------------------------------------------------------------------------------
-- Vertex Attribute Helpers
--------------------------------------------------------------------------------
-- | TODO: Add more of these attribute helpers!
avecfv :: (Real a, Functor f, Fractional b) => String -> NumComponents -> [f a] -> VertexComponent (f b) a1
avecfv s n vs = VertexComponent s vs' ToFloat $ VertexArrayDescriptor n Float 0 nullPtr
    where vs' = map (fmap realToFrac) vs

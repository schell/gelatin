module Gelatin.Shaders.Core where

import Gelatin.Core.ShaderCommands
import Foreign
import Linear
import Graphics.Rendering.OpenGL hiding (VertexComponent)

--------------------------------------------------------------------------------
-- Included Vertex Attributes & Uniforms
--------------------------------------------------------------------------------
position3 :: Real a => [V3 a] -> VertexComponent (V3 GLfloat) [Float]
position3 = avecfv "position" 3

position2 :: Real a => [V2 a] -> VertexComponent (V2 GLfloat) [Float]
position2 = avecfv "position" 2

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

uniformV4f :: Real a => String -> V4 a -> ShaderUniform (V4 GLfloat)
uniformV4f s = ShaderUniform s . fmap realToFrac

uniformV4fv :: Real a => String -> [V4 a] -> ShaderUniform [V4 GLfloat]
uniformV4fv s = ShaderUniform s . fmap (fmap realToFrac)

uniformi :: Integral a => String -> a -> ShaderUniform GLint
uniformi s = ShaderUniform s . fromIntegral

uniformb :: String -> Bool -> ShaderUniform GLint
uniformb s b = ShaderUniform s i
    where i = if b then 1 else 0
--------------------------------------------------------------------------------
-- Vertex Attribute Helpers
--------------------------------------------------------------------------------
-- | TODO: Add more of these attribute helpers!
avecfv :: (Real a, Functor f, Fractional b) => String -> NumComponents -> [f a] -> VertexComponent (f b) a1
avecfv s n vs = VertexComponent s vs' ToFloat $ VertexArrayDescriptor n Float 0 nullPtr
    where vs' = map (fmap realToFrac) vs

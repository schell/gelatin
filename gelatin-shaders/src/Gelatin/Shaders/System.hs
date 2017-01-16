{-# LANGUAGE #-}
module Gelatin.Shader.System where

class ShaderSystem t where
  compileSources :: ShaderSteps

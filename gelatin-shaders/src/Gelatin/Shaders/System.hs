{-# LANGUAGE TypeFamilies #-}
module Gelatin.Shader.System where

-- | This is some future work in progress.
class MonadShader a where
  data M a :: * -> *

  data Program a
  -- | Reads the shader program.
  readProgram :: (M a) (Program a)

  data UniformType a
  -- | Updates a specific uniform.
  updateUniforms :: [UniformType a] -> (M a) ()

  data AttributeType a
  -- | Enables a number of attribute types
  enableAttributes  :: [AttributeType a] -> (M a) ()
  disableAttributes :: [AttributeType a] -> (M a) ()

  data AttributeData a
  bufferAttribute :: AttributeData a -> (M a) ()

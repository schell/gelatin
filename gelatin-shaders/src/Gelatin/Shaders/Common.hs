{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE TypeSynonymInstances  #-}
module Gelatin.Shaders.Common
  ( VertexShader
  , FragmentShader
  , Uniform
  , Attribute
  , AttributeToggling
  , AttributeBuffering
  , IsShaderType(..)
  , ShaderSteps(..)
  ) where

import           Data.Proxy                (Proxy (..))
--------------------------------------------------------------------------------
import           Gelatin.Shaders.TypeLevel
--------------------------------------------------------------------------------
-- $shader Defining shaders using (mostly) just types
--------------------------------------------------------------------------------
data VertexShader
data FragmentShader

data ShaderType = ShaderTypeVertex | ShaderTypeFragment

class IsShaderType a b where
  getShaderType :: Proxy a -> b

instance IsShaderType VertexShader ShaderType where
  getShaderType _ = ShaderTypeVertex

instance IsShaderType FragmentShader ShaderType where
  getShaderType _ = ShaderTypeFragment

instance (IsShaderType t b, IsShaderType ts [b])
  => IsShaderType (t ': ts) [b] where
  getShaderType _ = getShaderType (Proxy :: Proxy t) : getShaderType (Proxy :: Proxy ts)

instance IsShaderType '[] [x] where
  getShaderType _ = []

-- | A glsl uniform type.
data Uniform name val

instance GetLits name String => GetLits (Uniform name val) String where
  getSymbols _ = getSymbols (Proxy :: Proxy name)

-- | A glsl attribute type.
data Attribute name val loc

instance (GetLits name String, GetLits loc Integer)
  => GetLits (Attribute name val loc) (String, Integer) where
  getSymbols _ =
    let name  = getSymbols (Proxy :: Proxy name)
        loc   = getSymbols (Proxy :: Proxy loc)
    in (name, loc)

-- | Used to resolve typeclass instances for generating enable/disable attribute
-- functions.
data AttributeToggling a

-- | Used to resolve typeclass instances for generating attribute buffering
-- functions.
data AttributeBuffering a

-- | A shader step is a step in the shader compilation process. This means that
-- `ShaderSteps '[VertexShader, FragmentShader] [ByteString]` is a list of
-- vertex and fragment shader source code that needs to be compiled.
-- `ShaderSteps '[VertexShader, FragmentShader] GLuint` most likely means a list
-- of vertex and fragment shaders that need to be linked.
data ShaderSteps t v = ShaderSteps { unShaderSteps :: [v] }

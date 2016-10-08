module Gelatin.Shaders.Common where

import Gelatin
import Data.ByteString.Char8 (ByteString)

--------------------------------------------------------------------------------
-- $shader
--------------------------------------------------------------------------------
-- | A Shader is a compiled shader program along with a list of uniform
-- locations.
data Shader program uniform = Shader { shProgram  :: program
                                     , shUniforms :: [(String, uniform)]
                                     }

-- | A ShaderDef is the definition of a shader, which is used to compile a
-- Shader.
data ShaderDef shadertype attribloc =
      ShaderDefFP { defShaderPaths :: [(String, shadertype)]
                  , defUniforms :: [String]
                  , defAttribs  :: [attribloc]
                  }
    | ShaderDefBS { defShaderSrcs :: [(ByteString, shadertype)]
                  , defUniforms   :: [String]
                  , defAttribs    :: [attribloc]
                  }

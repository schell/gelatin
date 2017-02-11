module Gelatin.Shaders.Error where

import           Graphics.GL

lookupError :: GLenum -> Maybe String
lookupError n =
  lookup n [ (GL_INVALID_OPERATION, "invalid operation")
           , (GL_INVALID_VALUE,     "invalid value")
           ]

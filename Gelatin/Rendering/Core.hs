{-# LANGUAGE GADTs #-}
module Gelatin.Rendering.Core where

import Gelatin.Shaders.Core
import Graphics.Rendering.OpenGL
import Graphics.VinylGL
import Graphics.GLUtil
import Control.Monad.Free.Church
import Data.Vinyl.Universe
import Data.Vinyl.Derived
import Linear

--------------------------------------------------------------------------------
-- Types
--------------------------------------------------------------------------------

--data GLSLType = Mat44 (M44 GLfloat)

data ShaderDef a next = WithUniform a next

--data SetUniform a next = SetUniform (PlainFieldRec a) GLSLType next
--
--type SetUniformCommand = F SetUniform

--type VertexBufferDef u v = F (VertexCompCommand u v ())

data OpenGLCommand next = UsingDepthFunc ComparisonFunction (OpenGL ()) next
                        | UsingShader ShaderProgram (OpenGL ()) next
--                        | UsingVertexBuffer (VertexBufferDef u v) (Rendering ()) next
--                        -- TODO: Add a bunch more...

type OpenGL = F OpenGLCommand

module Gelatin.Rendering where

import Gelatin.ShaderCommands as S
import Graphics.GLUtil
import Graphics.Rendering.OpenGL
import Control.Monad.Free.Church

--------------------------------------------------------------------------------
-- Types
--------------------------------------------------------------------------------
data Render next = UsingDepthFunc ComparisonFunction (Rendering ()) next
                 | UsingShader ShaderProgram (ShaderCommand ()) next

type Rendering = F Render
--------------------------------------------------------------------------------
-- Instances
--------------------------------------------------------------------------------
instance Functor Render where
    fmap f (UsingDepthFunc cf r next) = UsingDepthFunc cf r $ f next
    fmap f (UsingShader p sc next) = UsingShader p sc $ f next
--------------------------------------------------------------------------------
-- User API
--------------------------------------------------------------------------------
usingDepthFunc :: ComparisonFunction -> Rendering () -> Rendering ()
usingDepthFunc cf r = liftF $ UsingDepthFunc cf r ()

usingShader :: ShaderProgram -> ShaderCommand () -> Rendering ()
usingShader p sc = liftF $ UsingShader p sc ()

{-# LANGUAGE GADTs #-}
module Gelatin.Core.Types where

import Gelatin.Core.ShaderCommands
import Gelatin.Core.TextureCommands
import Graphics.GLUtil
import qualified Graphics.Rendering.OpenGL as GL
import Control.Monad.Free.Church
import Linear as L hiding (rotate)
--------------------------------------------------------------------------------
-- Instances
--------------------------------------------------------------------------------
instance Functor Render where
    fmap f (RenderM m n) = RenderM m $ f n
    fmap f (SetViewport l t w h next) = SetViewport l t w h $ f next
    fmap f (SetDepthFunc mcf next) = SetDepthFunc mcf $ f next
    fmap f (UsingShader p sc next) = UsingShader p sc $ f next
    fmap f (UsingTextures t ts ccmd rcmd next) = UsingTextures t ts ccmd rcmd $ f next
    fmap f (ClearColorWith c next) = ClearColorWith c $ f next
    fmap f (ClearDepth next) = ClearDepth $ f next
--------------------------------------------------------------------------------
-- Types
--------------------------------------------------------------------------------
data Render next where
    RenderM :: IO () -> next -> Render next
    SetViewport :: Integral a => a -> a -> a -> a -> next -> Render next
    SetDepthFunc :: Maybe GL.ComparisonFunction -> next -> Render next
    ClearDepth :: next -> Render next
    ClearColorWith :: (Real a, Fractional a) => (V4 a) -> next -> Render next
    UsingShader :: ShaderProgram -> (ShaderCommand ()) -> next -> Render next
    UsingTextures :: ( GL.ParameterizedTextureTarget t
                     , GL.BindableTextureTarget t )
                  => t -> [TextureSrc] -> (TextureCommand ()) -> (Rendering ()) -> next -> Render next

type Rendering = F Render

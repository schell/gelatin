{-# LANGUAGE GADTs #-}
module Gelatin.Rendering where

import Gelatin.ShaderCommands
import Gelatin.TextureCommands
import Graphics.GLUtil
import Graphics.Rendering.OpenGL
import Control.Monad.Free.Church
import Linear (V4(..))
import Data.Monoid

--------------------------------------------------------------------------------
-- Types
--------------------------------------------------------------------------------
data Render next where
    SetViewport :: Integral a => a -> a -> a -> a -> next -> Render next
    SetDepthFunc :: Maybe ComparisonFunction -> next -> Render next
    ClearDepth :: next -> Render next
    ClearColorWith :: (Real a, Fractional a) => (V4 a) -> next -> Render next
    UsingShader :: ShaderProgram -> (ShaderCommand ()) -> next -> Render next
    UsingTextures :: ( ParameterizedTextureTarget t
                     , BindableTextureTarget t
                     )
                  => t -> [TextureSrc] -> (TextureCommand ()) -> (Rendering ()) -> next -> Render next

type Rendering = F Render
data CompiledRendering = Compiled { render :: IO ()
                                  , cleanup :: IO ()
                                  }
--------------------------------------------------------------------------------
-- Instances
--------------------------------------------------------------------------------
instance Functor Render where
    fmap f (SetViewport l t w h next) = SetViewport l t w h $ f next
    fmap f (SetDepthFunc mcf next) = SetDepthFunc mcf $ f next
    fmap f (UsingShader p sc next) = UsingShader p sc $ f next
    fmap f (UsingTextures t ts ccmd rcmd next) = UsingTextures t ts ccmd rcmd $ f next
    fmap f (ClearColorWith c next) = ClearColorWith c $ f next
    fmap f (ClearDepth next) = ClearDepth $ f next

instance Monoid CompiledRendering where
    mempty = Compiled (return ()) (return ())
    (Compiled a b) `mappend` (Compiled c d) = Compiled (c >> a) (d >> b)
--------------------------------------------------------------------------------
-- User API
--------------------------------------------------------------------------------
setViewport :: Integral a => a -> a -> a -> a -> Rendering ()
setViewport l t w h = liftF $ SetViewport l t w h ()

setDepthFunc :: Maybe ComparisonFunction -> Rendering ()
setDepthFunc mcf = liftF $ SetDepthFunc mcf ()

usingDepthFunc :: ComparisonFunction -> Rendering () -> Rendering ()
usingDepthFunc cf r = setDepthFunc (Just cf) >> r >> setDepthFunc Nothing

usingShader :: ShaderProgram -> ShaderCommand () -> Rendering ()
usingShader p sc = liftF $ UsingShader p sc ()

usingTextures :: (ParameterizedTextureTarget t, BindableTextureTarget t)
              => t -> [TextureSrc] -> TextureCommand () -> Rendering ()
              -> Rendering ()
usingTextures t ts ccmd rcmd = liftF $ UsingTextures t ts ccmd rcmd ()

usingTexture :: (ParameterizedTextureTarget t, BindableTextureTarget t)
             => t -> TextureSrc -> TextureCommand () -> Rendering ()
             -> Rendering ()
usingTexture t tex = usingTextures t [tex]

clearDepth :: Rendering ()
clearDepth = liftF $ ClearDepth ()

clearColorWith :: (Real a, Fractional a) => V4 a -> Rendering ()
clearColorWith v = liftF $ ClearColorWith v ()
--------------------------------------------------------------------------------
-- Help with compiling
--------------------------------------------------------------------------------
setRender :: CompiledRendering -> IO () -> CompiledRendering
setRender c io = c { render = io }

setCleanup :: CompiledRendering -> IO () -> CompiledRendering
setCleanup c io = c { cleanup = io }

prefixRender :: IO () -> CompiledRendering -> CompiledRendering
prefixRender io (Compiled io' c)  = Compiled (io >> io') c

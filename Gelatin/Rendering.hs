{-# LANGUAGE GADTs #-}
module Gelatin.Rendering (
    -- * User API
    Rendering,
    traceLn,
    setViewport,
    setDepthFunc,
    usingDepthFunc,
    usingShader,
    usingTextures,
    usingTexture,
    clearDepth,
    clearColorWith,
    -- * For compiling
    Render(..),
) where

import Gelatin.ShaderCommands
import Gelatin.TextureCommands
import Graphics.GLUtil
import qualified Graphics.Rendering.OpenGL as GL
import Control.Monad.Free.Church
import Linear as L hiding (rotate)

--------------------------------------------------------------------------------
-- User API
--------------------------------------------------------------------------------
setViewport :: Integral a => a -> a -> a -> a -> Rendering ()
setViewport l t w h = liftF $ SetViewport l t w h ()

setDepthFunc :: Maybe GL.ComparisonFunction -> Rendering ()
setDepthFunc mcf = liftF $ SetDepthFunc mcf ()

usingDepthFunc :: GL.ComparisonFunction -> Rendering () -> Rendering ()
usingDepthFunc cf r = setDepthFunc (Just cf) >> r >> setDepthFunc Nothing

usingShader :: ShaderProgram -> ShaderCommand () -> Rendering ()
usingShader p sc = liftF $ UsingShader p sc ()

usingTextures :: (GL.ParameterizedTextureTarget t, GL.BindableTextureTarget t)
              => t -> [TextureSrc] -> TextureCommand () -> Rendering ()
              -> Rendering ()
usingTextures t ts ccmd rcmd = liftF $ UsingTextures t ts ccmd rcmd ()

usingTexture :: (GL.ParameterizedTextureTarget t, GL.BindableTextureTarget t)
             => t -> TextureSrc -> TextureCommand () -> Rendering ()
             -> Rendering ()
usingTexture t tex = usingTextures t [tex]

clearDepth :: Rendering ()
clearDepth = liftF $ ClearDepth ()

clearColorWith :: (Real a, Fractional a) => V4 a -> Rendering ()
clearColorWith v = liftF $ ClearColorWith v ()

traceLn :: String -> Rendering ()
traceLn str = liftF $ TraceLn str ()
--------------------------------------------------------------------------------
-- Instances
--------------------------------------------------------------------------------
instance Functor Render where
    fmap f (TraceLn s n) = TraceLn s $ f n
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
    TraceLn :: String -> next -> Render next
    SetViewport :: Integral a => a -> a -> a -> a -> next -> Render next
    SetDepthFunc :: Maybe GL.ComparisonFunction -> next -> Render next
    ClearDepth :: next -> Render next
    ClearColorWith :: (Real a, Fractional a) => (V4 a) -> next -> Render next
    UsingShader :: ShaderProgram -> (ShaderCommand ()) -> next -> Render next
    UsingTextures :: ( GL.ParameterizedTextureTarget t
                     , GL.BindableTextureTarget t
                     )
                  => t -> [TextureSrc] -> (TextureCommand ()) -> (Rendering ()) -> next -> Render next

type Rendering = F Render

{-# LANGUAGE GADTs #-}
module Gelatin.Core (
    -- * User API
    --Rendering,
    --CompiledRendering(..),
    --runRendering,
    --renderOnce,
    --intoTexture,
    --renderIO,
    --setViewport,
    --setDepthFunc,
    --usingDepthFunc,
    --usingShader,
    --usingTextures,
    --usingTexture,
    --clearDepth,
    --clearColorWith,
) where

import Gelatin.Core.Compiling
import Gelatin.Core.ShaderCommands
import Gelatin.Core.TextureCommands
import Gelatin.Core.Texture
import Gelatin.Core.Types
import Graphics.GLUtil hiding (setUniform)
import Graphics.Rendering.OpenGL hiding (position, clearDepth, translate, scale)
import Control.Monad.Free.Church
import Linear

-- | Compiles a Rendering. The resulting type can be used to render
-- a frame and clean up and contains resources used that frame.
--runRendering :: Rendering () -> IO CompiledRendering
--runRendering = compileRenderCommand . fromF
--
---- | Renders a Rendering once and cleans up after.
--renderOnce :: Rendering () -> IO ()
--renderOnce r = do
--    r' <- runRendering r
--    render r'
--    cleanup r'
--
---- | Compiles and draws a Rendering into a texture and returns that
---- texture.
--intoTexture :: Int -> Int -> Rendering () -> IO TextureObject
--intoTexture w h = renderToTexture (Size w' h') RGBA' . renderOnce
--    where [w', h'] = map fromIntegral [w, h]
--
---- | Perform an arbitrary IO during compilation.
--renderIO :: IO () -> Rendering ()
--renderIO m = liftF $ RenderIO m ()
--
---- | Set the viewport of the current context.
--setViewport :: Integral a => a -> a -> a -> a -> Rendering ()
--setViewport l t w h = liftF $ SetViewport l t w h ()
--
---- | Set the depth function to use in culling.
--setDepthFunc :: Maybe ComparisonFunction -> Rendering ()
--setDepthFunc mcf = liftF $ SetDepthFunc mcf ()
--
---- | Specify a rendering using a certain depth function.
--usingDepthFunc :: ComparisonFunction -> Rendering () -> Rendering ()
--usingDepthFunc cf r = setDepthFunc (Just cf) >> r >> setDepthFunc Nothing
--
---- | Specify a rendering using the given shader program.
--usingShader :: ShaderProgram -> ShaderCommand () -> Rendering ()
--usingShader p sc = liftF $ UsingShader p sc ()
--
---- | Specify a rendering using a number of shaders.
--usingTextures :: (ParameterizedTextureTarget t, BindableTextureTarget t)
--              => t -> [TextureSrc] -> TextureCommand () -> Rendering ()
--              -> Rendering ()
--usingTextures t ts ccmd rcmd = liftF $ UsingTextures t ts ccmd rcmd ()
--
---- | Specify a rendering using one shader.
--usingTexture :: (ParameterizedTextureTarget t, BindableTextureTarget t)
--             => t -> TextureSrc -> TextureCommand () -> Rendering ()
--             -> Rendering ()
--usingTexture t tex = usingTextures t [tex]
--
---- | Clear the depth buffer.
--clearDepth :: Rendering ()
--clearDepth = liftF $ ClearDepth ()
--
---- | Clear the color buffer and set the color cleared to.
--clearColorWith :: (Real a, Fractional a) => V4 a -> Rendering ()
--clearColorWith v = liftF $ ClearColorWith v ()

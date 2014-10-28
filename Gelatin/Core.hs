{-# LANGUAGE GADTs #-}
module Gelatin.Core (
    -- * User API
    Rendering,
    intoTexture,
    textureRenderer,
    renderOnce,
    renderM,
    setViewport,
    setDepthFunc,
    usingDepthFunc,
    usingShader,
    usingTextures,
    usingTexture,
    clearDepth,
    clearColorWith,
) where

import Gelatin.Core.Compiling
import Gelatin.Core.ShaderCommands
import Gelatin.Core.TextureCommands
import Gelatin.Core.Texture
import Gelatin.Core.Types
import Gelatin.Shaders
import Gelatin.Transform
import Graphics.GLUtil hiding (setUniform)
import Graphics.Rendering.OpenGL hiding (position, clearDepth, translate, scale)
import Control.Monad.Free.Church
import Linear

-- | Return a function that will render a texture.
textureRenderer :: ShaderProgram -> IO (TextureObject -> V2 Int -> Int -> Int -> IO ())
textureRenderer shdr = do
    let verts = do addComponent $ position2 ps
                   addComponent $ texcoord uvs
        ps = [ V2 0 0, V2 1 0
             , V2 0 1, V2 1 1
             ] :: [V2 GLfloat]
        uvs = [ V2 0 1, V2 1 1
              , V2 0 0, V2 1 0
              ] :: [V2 GLfloat]
        nds = [0,1,2,1,2,3] :: [GLint]
        r_  = return ()
    g <- runRendering $ usingShader shdr $ withVertices verts $
             drawIndexedTriangles nds 2
    return $ \tex (V2 x y) w h -> do
        let [x',y',w',h'] = map fromIntegral [x,y,w,h]
            m44           = mkM44 $ do scale $ (V3 w' h' 1 :: V3 GLfloat)
                                       translate $ V3 x' y' 0
        f <- runRendering $ usingTexture Texture2D (Loaded tex) r_ $ usingShader shdr $ do
            setSampler (0 :: Int)
            setModelview (m44 :: M44 GLfloat)
            shaderM $ render g
        render f

-- | Compiles and draws a Rendering into a texture and returns that
-- texture.
intoTexture :: Int -> Int -> Rendering () -> IO TextureObject
intoTexture w h = renderToTexture (Size w' h') RGBA' . renderOnce
    where [w', h'] = map fromIntegral [w, h]

-- | Renders a Rendering once and cleans up after.
renderOnce :: Rendering () -> IO ()
renderOnce r = do
    r' <- runRendering r
    render r'
    cleanup r'
--------------------------------------------------------------------------------
-- Rendering user API
--------------------------------------------------------------------------------
renderM :: IO () -> Rendering ()
renderM m = liftF $ RenderM m ()

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

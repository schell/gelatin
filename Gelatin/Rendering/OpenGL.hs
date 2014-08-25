{-# LANGUAGE DataKinds #-}
module Gelatin.Rendering.OpenGL (
    renderDrawing
) where

import Graphics.Rendering.OpenGL hiding (Fill, Color, color, position)
import Graphics.GLUtil.ShaderProgram
import Graphics.GLUtil.JuicyTextures
import Graphics.GLUtil.Textures
import Graphics.VinylGL
import Gelatin.Core
import Gelatin.Shaders
import Data.Vinyl
import Linear
import Control.Monad.Free
import Control.Monad.Free.Church
import System.Exit
import System.Directory
import System.FilePath ( (</>) )

transform2Mat :: Num a => Transform a -> M44 a
transform2Mat (Transform p (V3 sx sy sz) q) = t !*! s !*! q'
    where t  = mkTransformationMat eye3 p
          q' = mkTransformation q $ V3 0 0 0
          s  = V4 (V4 sx 0 0  0)
                  (V4 0 sy 0  0)
                  (V4 0  0 sz 0)
                  (V4 0  0 0  1)

color2V4 :: Color4 a -> V4 a
color2V4 (Color4 r b g a) = V4 r g b a

loadTextureSrc :: TextureSrc -> IO (Either String TextureObject)
loadTextureSrc (Local fp) = readTexture fp
loadTextureSrc (Relative fp) = do
    fp' <- fmap (</> fp) getCurrentDirectory
    readTexture fp'

renderCommand :: Real a => M44 a -> M44 a -> Renderer -> Free (DrawCommand a) ()
              -> IO (IO (), IO ())
renderCommand _ _ _ (Pure ()) = return (return (), return ())
renderCommand pj mv rndr (Free (WithTransform t d n)) = do
    (draw,clean)   <- renderCommand pj (mv !*! transform2Mat t) rndr (fromF d)
    (draw',clean') <- renderCommand pj mv rndr n
    return (draw >> draw', clean >> clean')
renderCommand pj mv rndr (Free (Fill vs (Color4 r g b a) n)) = do
    let vs' = zipWith (<+>) (map (position =:) $ (fmap.fmap) realToFrac vs)
                            (map (color =:) cs)
        len  = length vs
        cs   = replicate len $ fmap realToFrac $ V4 r g b a
        s    = colorShader rndr
    vbo <- bufferVertices vs'
    let draw = do currentProgram $= (Just $ program $ colorShader rndr)
                  setUniforms s (projection =: (fmap.fmap) realToFrac pj <+>
                                 modelview =: (fmap.fmap) realToFrac mv)
                  bindVertices vbo
                  enableVertices' s vbo
                  drawArrays Triangles 0 (fromIntegral len)
        clean = deleteVertices vbo
    (draw', clean') <- renderCommand pj mv rndr n
    return (draw >> draw', clean >> clean')
renderCommand pj mv rndr (Free (Gradient vs cs n)) = do
    let vs' = zipWith (<+>) (map (position =:) $ (fmap.fmap) realToFrac vs)
                            (map (color =:) $ fmap (fmap realToFrac.color2V4) cs)
        len = fromIntegral $ length vs
        s    = colorShader rndr
    vbo <- bufferVertices vs'
    let fd = do currentProgram $= (Just $ program $ colorShader rndr)
                setUniforms s (projection =: (fmap.fmap) realToFrac pj <+>
                               modelview =: (fmap.fmap) realToFrac mv)
                bindVertices vbo
                enableVertices' s vbo
                drawArrays Triangles 0 len
        fc = deleteVertices vbo
    (fd', fc') <- renderCommand pj mv rndr n
    return (fd >> fd', fc >> fc')
renderCommand pj mv rndr (Free (WithTexture src d n)) = do
    et <- loadTextureSrc src
    case et of
        Left err -> do putStrLn err
                       exitFailure
        Right t  -> do (fd', fc') <- renderCommand pj mv rndr $ fromF d
                       let pg = program $ textureShader rndr
                           fd = do currentProgram $= (Just pg)
                                   texture Texture2D $= Enabled
                                   textureFilter   Texture2D   $= ((Nearest, Nothing), Nearest)
                                   textureWrapMode Texture2D S $= (Repeated, Clamp)
                                   textureWrapMode Texture2D T $= (Repeated, Clamp)
                                   withTextures2D [t] fd'
                       (fd'',fc'') <- renderCommand pj mv rndr n
                       return (fd >> fd'', fc' >> fc'')
renderCommand pj mv rndr (Free (TexTris vs ts n)) = do
    let vs' = zipWith (<+>) (map (position =:) $ (fmap.fmap) realToFrac vs)
                            (map (texcoord =:) $ (fmap.fmap) realToFrac ts)
        len = fromIntegral $ length vs
        t   = textureShader rndr
    vbo <- bufferVertices vs'
    let fd = do currentProgram $= (Just $ program t)
                setUniforms t (projection =: (fmap.fmap) realToFrac pj <+>
                               modelview =: (fmap.fmap) realToFrac mv <+>
                               sampler =: 0)
                bindVertices vbo
                enableVertices' t vbo
                drawArrays Triangles 0 len
        fc = deleteVertices vbo
    (fd', fc') <- renderCommand pj mv rndr n
    return (fd >> fd', fc >> fc')

-- | Render a drawing down to a set of functions. The first draws into the
-- OpenGL context and the second cleans up resources used for that drawing.
renderDrawing :: (Num a, Real a) => M44 a -> M44 a -> Renderer -> Drawing a () -> IO (IO (), IO ())
renderDrawing pj mv r = renderCommand pj mv r . fromF

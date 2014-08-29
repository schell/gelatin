{-# LANGUAGE DataKinds #-}
module Gelatin.Rendering.OpenGL (
    --renderDrawing,
    transform2Mat
) where

import Graphics.Rendering.OpenGL hiding (Fill, Color, color, position)
import Graphics.GLUtil.ShaderProgram
import Graphics.GLUtil.JuicyTextures
import Graphics.GLUtil.Textures
import Graphics.GLUtil.Camera3D
import Graphics.VinylGL
import Gelatin.Core
import Gelatin.Shaders
import Data.Vinyl
import Linear
import Control.Monad.Reader
import Control.Monad.Free
import Control.Monad.Free.Church
import System.Exit
import System.Directory
import System.FilePath ( (</>) )

transform2Mat :: Transform -> M44 Double
transform2Mat (Transform p (V3 sx sy sz) q) = t !*! s !*! q'
    where t  = mkTransformationMat eye3 p
          q' = mkTransformation q $ V3 0 0 0
          s  = V4 (V4 sx 0 0  0)
                  (V4 0 sy 0  0)
                  (V4 0  0 sz 0)
                  (V4 0  0 0  1)

loadTextureSrc :: TextureSrc -> IO (Either String TextureObject)
loadTextureSrc (Local fp) = readTexture fp
loadTextureSrc (Relative fp) = do
    fp' <- fmap (</> fp) getCurrentDirectory
    readTexture fp'

combineRenderPairs :: RenderPair a -> RenderPair a -> RenderPair a
combineRenderPairs (fd, fc) (fd', fc') = (fd >> fd', fc >> fc')

askForPJMV :: Render a (M44 Double, M44 Double)
askForPJMV = liftM2 (,) (asks reProjection) (asks reModelview)

setMatsInBothShaders :: Renderer -> M44 Double -> M44 Double -> IO ()
setMatsInBothShaders rndr pj mv = do
    let [pj',mv'] = map (fmap (fmap realToFrac)) [pj,mv]
        cs = colorShader rndr
        ts = textureShader rndr
    forM_ [cs, ts] $ \s -> do
        currentProgram $= (Just $ program s)
        setUniforms s (projection =: pj' <+>
                       modelview =: mv')

setMatrices :: Renderer -> Render a ()
setMatrices rndr = do
    (pj,mv) <- askForPJMV
    liftIO $ setMatsInBothShaders rndr pj mv

--renderCommand :: Renderer -> Free (DrawCommand a) ()
--              -> IO (RenderPair a)
--renderCommand _ (Pure ()) = return (return (), return ())
--renderCommand rndr (Free (WithTransform t d n)) = do
--    (draw,clean)   <- renderCommand rndr (fromF d)
--    -- Replace the draw command with one using a modified modelview mat.
--    let draw' = do mv  <- asks reModelview
--                   local (\env -> env{ reModelview = (mv !*! transform2Mat t) })
--                         (setMatrices rndr >> draw)
--        r1 = (draw', clean)
--    r2 <- renderCommand rndr n
--    return $ combineRenderPairs r1 r2
--renderCommand rndr (Free (WithProjection pj d n)) = do
--    (draw,clean)   <- renderCommand rndr (fromF d)
--    -- Replace the draw command with one using the given projection mat and
--    -- reset the matrices.
--    let draw' = do local (\env -> env{ reProjection = pj, reModelview = eye4 })
--                         (setMatrices rndr >> draw)
--        r1 = (draw', clean)
--    r2 <- renderCommand rndr n
--    return $ combineRenderPairs r1 r2
--renderCommand rndr (Free (Fill vs c n)) = do
--    let vs' = zipWith (<+>) (map (position =:) $ fmap (fmap realToFrac) vs)
--                            (map (color =:) cs)
--        len  = length vs
--        cs   = replicate len $ fmap realToFrac c
--        s    = colorShader rndr
--    vbo <- bufferVertices vs'
--    let draw  = do currentProgram $= (Just $ program $ colorShader rndr)
--                   bindVertices vbo
--                   enableVertices' s vbo
--                   drawArrays Triangles 0 (fromIntegral len)
--        clean = deleteVertices vbo
--        r1    = (liftIO draw, clean)
--    r2 <- renderCommand rndr n
--    return $ combineRenderPairs r1 r2
--renderCommand rndr (Free (Gradient vs cs n)) = do
--    let vs' = zipWith (<+>) (map (position =:) $ (fmap.fmap) realToFrac vs)
--                            (map (color =:) $ fmap (fmap realToFrac) cs)
--        len = fromIntegral $ length vs
--        s    = colorShader rndr
--    vbo <- bufferVertices vs'
--    let fd = do currentProgram $= (Just $ program $ colorShader rndr)
--                bindVertices vbo
--                enableVertices' s vbo
--                drawArrays Triangles 0 len
--        fc = deleteVertices vbo
--        r1 = (liftIO fd,fc)
--    r2 <- renderCommand rndr n
--    return $ combineRenderPairs r1 r2
--renderCommand rndr (Free (WithTexture src d n)) = do
--    et <- loadTextureSrc src
--    case et of
--        Left err -> do putStrLn err
--                       exitFailure
--        Right t  -> do textureFilter   Texture2D   $= ((Nearest, Nothing), Nearest)
--                       textureWrapMode Texture2D S $= (Repeated, Clamp)
--                       textureWrapMode Texture2D T $= (Repeated, Clamp)
--                       (fd', fc') <- renderCommand rndr $ fromF d
--                       let fd = do env <- ask
--                                   liftIO $ withTextures2D [t] $ runReaderT fd' env
--                           r1 = (fd, fc')
--                       r2 <- renderCommand rndr n
--                       return $ combineRenderPairs r1 r2
--renderCommand rndr (Free (TexTris vs ts n)) = do
--    let vs' = zipWith (<+>) (map (position =:) $ (fmap.fmap) realToFrac vs)
--                            (map (texcoord =:) $ (fmap.fmap) realToFrac ts)
--        len = fromIntegral $ length vs
--        t   = textureShader rndr
--    vbo <- bufferVertices vs'
--    let fd = do currentProgram $= (Just $ program t)
--                setUniforms t (sampler =: 0)
--                bindVertices vbo
--                enableVertices' t vbo
--                drawArrays Triangles 0 len
--        fc = deleteVertices vbo
--        r1 = (liftIO fd, fc)
--    r2 <- renderCommand rndr n
--    return $ combineRenderPairs r1 r2
--renderCommand rndr (Free (RawRendering f n)) = do
--    pair  <- f rndr
--    pair' <- renderCommand rndr n
--    return $ combineRenderPairs pair pair'
--
--defaultRootDraw :: Renderer -> RenderPair a
--defaultRootDraw rndr = (draw, return ())
--    where draw = do (V2 ww wh)   <- asks reWindowSize
--                    (V2 fbw fbh) <- asks reFrameBufferSize
--                    liftIO $ do
--                        viewport $= (Position 0 0, Size (fromIntegral fbw) (fromIntegral fbh))
--                        clearColor $= Color4 0 0 0 1
--                        clear [ColorBuffer, DepthBuffer]
--                        -- Use a standard 2d projection where the top left
--                        -- (0,0) is the upper left of the window.
--                        let pj = orthoMatrix 0 (fromIntegral ww) 0 (fromIntegral wh) 0 1
--                            mv = eye4
--                        setMatsInBothShaders rndr pj mv
--
--
---- | Render a drawing down to a set of functions. The first draws into the
---- OpenGL context and the second cleans up resources used for that drawing.
--renderDrawing :: Renderer -> Drawing a () -> IO (RenderPair a)
--renderDrawing rndr d = do
--    texture Texture2D $= Enabled
--    depthFunc $= Nothing
--    blend $= Enabled
--    blendEquationSeparate $= (FuncAdd, FuncAdd)
--    blendFuncSeparate $= ((SrcAlpha, OneMinusSrcAlpha), (One, Zero))
--    r2 <- renderCommand rndr $ fromF d
--    return $ combineRenderPairs (defaultRootDraw rndr) r2


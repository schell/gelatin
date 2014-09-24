{-# LANGUAGE ScopedTypeVariables #-}
module Gelatin.Compiling where

import Gelatin.Rendering
import Gelatin.ShaderCommands
import Gelatin.TextureCommands
import Graphics.VinylGL
import Graphics.GLUtil hiding (Elem, setUniform)
import qualified Graphics.GLUtil as GLU
import Graphics.Rendering.OpenGL hiding (Color, position, color, VertexComponent)
import qualified Graphics.Rendering.OpenGL as GL
import Control.Monad
import Control.Monad.Free
import Control.Monad.Free.Church
import Data.Monoid
import Data.Either
import Data.List (intercalate)
import qualified Data.Map as M
import Foreign
import Linear (V4(..))

data RenderingDefaults = RenderingDefaults { rdefDepthFunc     :: Maybe ComparisonFunction
                                           , rdefShaderProgram :: Maybe ShaderProgram
                                           , rdefClearColor    :: Color4 GLfloat
                                           }

sizeOfList :: forall a. Storable a => [a] -> GLsizeiptr
sizeOfList vs = fromIntegral $ (length vs) * sizeOf (undefined :: a)

--------------------------------------------------------------------------------
-- Compiling/Running
--------------------------------------------------------------------------------
compileVertexBufferCommand :: ShaderProgram -> Free VertexBufferOp () -> IO CompiledRendering
compileVertexBufferCommand _ (Pure ()) = return mempty
compileVertexBufferCommand s (Free (AddComponent v n)) = do
    nxt <- compileVertexBufferCommand s n
    let vname = vertexName v
        vdesc = vertexDescriptor v
        vdata = vertexData v
        vinth = vertexHandling v
        aloc  = getAttrib s vname
    b <- genObjectName
    bindBuffer ArrayBuffer $= Just b
    vertexAttribPointer aloc $= (vinth, vdesc)
    vertexAttribArray aloc $= Enabled
    withArray vdata $ \ptr ->
        bufferData ArrayBuffer $= (sizeOfList vdata, ptr, StaticDraw)
    bindBuffer ArrayBuffer $= Nothing
    let io = do bindBuffer ArrayBuffer $= Just b
                vertexAttribPointer aloc $= (vinth, vdesc)
                vertexAttribArray aloc $= Enabled
        cu = deleteObjectName b
    return $ nxt `mappend`  Compiled io cu

compileDrawElementsCommand :: Free DrawElements () -> IO CompiledRendering
compileDrawElementsCommand (Pure ()) = return mempty
compileDrawElementsCommand (Free (DrawElements n mode next)) =
    fmap (prefixRender $ GL.drawElements mode n UnsignedInt nullPtr) $
        compileDrawElementsCommand next

compileShaderCommand :: ShaderProgram -> Free ShaderOp () -> IO CompiledRendering
compileShaderCommand _ (Pure ()) = return mempty
compileShaderCommand s (Free (SetUniform u next)) = do
    let uname = uniformName u
        udata = uniformData u
        muLoc = fmap fst $ M.lookup uname $ uniforms s
    case muLoc of
        Nothing  -> error $ unwords [ "Could not find uniform '" ++ uname ++ "'."
                                    , " Available uniforms include "
                                    , intercalate ", " $ M.keys $ uniforms s
                                    , "."
                                    ]
        Just _ -> fmap (prefixRender $ GLU.setUniform s uname udata) $ compileShaderCommand s next
compileShaderCommand s (Free (WithVertexBuffer vb cmd next)) = do
    sub <- compileShaderCommand s $ fromF cmd
    nxt <- compileShaderCommand s next
    vbs <- compileVertexBufferCommand s $ fromF vb
    let io = do render vbs
                render sub
                bindBuffer ArrayBuffer $= Nothing
        cu = return ()
    return $ nxt `mappend` Compiled io cu
compileShaderCommand s (Free (WithVertices vs cmd next)) = do
    sub <- compileShaderCommand s $ fromF cmd
    nxt <- compileShaderCommand s next
    vbo <- bufferVertices vs
    let io = do bindVertices vbo
                enableVertices' s vbo
                render sub
        cu = do cleanup sub
                deleteVertices vbo
    return $ nxt `mappend` Compiled io cu
compileShaderCommand s (Free (WithIndices ns cmd next)) = do
    sub <- compileDrawElementsCommand $ fromF cmd
    nxt <- compileShaderCommand s next
    ebo <- bufferIndices $ map fromIntegral ns
    let io = do bindBuffer ElementArrayBuffer $= Just ebo
                render sub
        cu = do cleanup sub
                bindBuffer ElementArrayBuffer $= Nothing
    return $ nxt `mappend` Compiled io cu
compileShaderCommand s (Free (DrawArrays mode i next)) =
    fmap (prefixRender $ GL.drawArrays mode 0 $ fromIntegral i) $ compileShaderCommand s next

compileTextureCommand :: ParameterizedTextureTarget t
                      => t -> Free TextureOp () -> IO CompiledRendering
compileTextureCommand _ (Pure ()) = return mempty
compileTextureCommand t (Free (SetFilter mn mg n)) =
    fmap (prefixRender $ textureFilter t $= (mn, mg)) $
        compileTextureCommand t n
compileTextureCommand t (Free (SetWrapMode c rp clamp n)) =
    fmap (prefixRender $ textureWrapMode t c $= (rp, clamp)) $
        compileTextureCommand t n

compileRenderCommand :: Free Render () -> IO CompiledRendering
compileRenderCommand (Pure ()) = return mempty
compileRenderCommand (Free (SetViewport x y w h n)) = do
    let [x', y', w', h'] = map fromIntegral [x, y, w, h]
    nxt <- compileRenderCommand n
    return $ nxt `mappend` Compiled (viewport $= (Position x' y', Size w' h')) (return ())
compileRenderCommand (Free (SetDepthFunc mfunc next)) = do
    nxt <- compileRenderCommand next
    return $ nxt `mappend` Compiled (depthFunc $= mfunc) (return ())
compileRenderCommand (Free (UsingShader s sc next)) = do
    sub <- compileShaderCommand s $ fromF sc
    nxt <- compileRenderCommand next
    let io = do currentProgram $= (Just $ program s)
                render sub
        cu = do cleanup sub
                currentProgram $= Nothing
    return $ nxt `mappend` Compiled io cu
compileRenderCommand (Free (ClearDepth next)) = do
    fmap (prefixRender $ clear [DepthBuffer]) $ compileRenderCommand next
compileRenderCommand (Free (ClearColorWith c next)) = do
    fmap (prefixRender $ clearColor $= (toColor4 c) >> clear [ColorBuffer]) $
        compileRenderCommand next
compileRenderCommand (Free (UsingTextures t ts cmd r n)) = do
    ts' <- putStrLn "Loading textures..." >> loadTextures t (fromF cmd) ts
    sub <- compileRenderCommand $ fromF r
    nxt <- compileRenderCommand n
    let io = withTextures t ts' $ render sub
        cu = do cleanup sub
    return $ nxt `mappend` Compiled io cu

loadTextures :: ( BindableTextureTarget t
                , ParameterizedTextureTarget t
                )
             => t -> TextureCommand () -> [TextureSrc] -> IO [TextureObject]
loadTextures t cmd srcs = do
    texparams <- compileTextureCommand t $ fromF cmd
    texture t $= Enabled
    fmap rights $ forM srcs $ loadAndInitTex texparams

loadAndInitTex :: CompiledRendering -> TextureSrc -> IO (Either String TextureObject)
loadAndInitTex r src = do
    eT <- loadTextureSrc src
    case eT of
        Right t  -> render r >> return (Right t)
        Left err -> putStrLn err >> return (Left err)

toColor4 :: Real a => V4 a -> Color4 GLfloat
toColor4 v = Color4 r g b a
    where (V4 r g b a) = fmap realToFrac v

compileRendering :: Rendering () -> IO CompiledRendering
compileRendering = compileRenderCommand . fromF

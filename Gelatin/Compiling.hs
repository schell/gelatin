{-# LANGUAGE ScopedTypeVariables #-}
module Gelatin.Compiling where

import Gelatin.Rendering
import Gelatin.ShaderCommands
import Gelatin.TextureCommands
import Graphics.GLUtil hiding (Elem, setUniform)
import qualified Graphics.GLUtil as GLU
import Graphics.Rendering.OpenGL as GL
import Control.Monad
import Control.Monad.Free
import Control.Monad.Free.Church
import Data.Monoid
import Data.Either
import Data.List (intercalate)
import qualified Data.Map as M
import Foreign
import Linear

-- | Compiles a Rendering. The resulting type can be used to render
-- a frame and clean up and contains resources used that frame.
compileRendering :: Rendering () -> IO CompiledRendering
compileRendering = compileRenderCommand . fromF

-- | Renders a Rendering once and cleans up after.
renderOnce :: Rendering () -> IO ()
renderOnce r = do
    r' <- compileRendering r
    render r'
    cleanup r'
--------------------------------------------------------------------------------
-- Types
--------------------------------------------------------------------------------
data CompiledRendering = Compiled { render  :: IO ()
                                  , cleanup :: IO ()
                                  }
--------------------------------------------------------------------------------
-- Instances
--------------------------------------------------------------------------------
instance Monoid CompiledRendering where
    mempty = Compiled (return ()) (return ())
    (Compiled r1 c1) `mappend` (Compiled r2 c2) = Compiled (r1 >> r2) (c1 >> c2)
--------------------------------------------------------------------------------
-- Compiling/Running
--------------------------------------------------------------------------------
compileVertexBufferCommand :: ShaderProgram -> Free VertexBufferOp ()
                           -> IO CompiledRendering
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
    return $ Compiled io cu `mappend` nxt

compileDrawElementsCommand :: Free DrawElements () -> IO CompiledRendering
compileDrawElementsCommand (Pure ()) = return mempty
compileDrawElementsCommand (Free (DrawElements n mode next)) = do
    nxt <- compileDrawElementsCommand next
    return $ mappend (onlyRender $ GL.drawElements mode n UnsignedInt nullPtr)
                     nxt
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
        Just _ -> do nxt <- compileShaderCommand s next
                     return $ mappend (onlyRender $ GLU.setUniform s uname udata) nxt
compileShaderCommand s (Free (WithVertices vb cmd next)) = do
    sub <- compileShaderCommand s $ fromF cmd
    nxt <- compileShaderCommand s next
    vbs <- compileVertexBufferCommand s $ fromF vb
    let io = do render vbs
                render sub
                bindBuffer ArrayBuffer $= Nothing
        cu = return ()
    return $ Compiled io cu `mappend` nxt
compileShaderCommand s (Free (WithIndices ns cmd next)) = do
    sub <- compileDrawElementsCommand $ fromF cmd
    nxt <- compileShaderCommand s next
    ebo <- bufferIndices $ map fromIntegral ns
    let io = do bindBuffer ElementArrayBuffer $= Just ebo
                render sub
        cu = do cleanup sub
                bindBuffer ElementArrayBuffer $= Nothing
    return $ Compiled io cu `mappend` nxt
compileShaderCommand s (Free (DrawArrays mode i next)) = do
    nxt <- compileShaderCommand s next
    return $ mappend (onlyRender $ GL.drawArrays mode 0 $ fromIntegral i) nxt

compileTextureCommand :: ParameterizedTextureTarget t
                      => t -> Free TextureOp () -> IO CompiledRendering
compileTextureCommand _ (Pure ()) = return mempty
compileTextureCommand t (Free (SetFilter mn mg n)) = do
    nxt <- compileTextureCommand t n
    return $ mappend (onlyRender $ textureFilter t $= (mn, mg)) nxt
compileTextureCommand t (Free (SetWrapMode c rp clamp n)) = do
    nxt <- compileTextureCommand t n
    return $ mappend (onlyRender $ textureWrapMode t c $= (rp, clamp)) nxt

compileRenderCommand :: Free Render () -> IO CompiledRendering
compileRenderCommand (Pure ()) = return mempty
compileRenderCommand (Free (SetViewport x y w h n)) = do
    let [x', y', w', h'] = map fromIntegral [x, y, w, h]
    nxt <- compileRenderCommand n
    return $ onlyRender (viewport $= (Position x' y', Size w' h')) `mappend` nxt
compileRenderCommand (Free (SetDepthFunc mfunc next)) = do
    nxt <- compileRenderCommand next
    return $ mappend (onlyRender $ depthFunc $= mfunc) nxt
compileRenderCommand (Free (UsingShader s sc next)) = do
    sub <- compileShaderCommand s $ fromF sc
    nxt <- compileRenderCommand next
    let io = do currentProgram $= (Just $ program s)
                render sub
        cu = do cleanup sub
                currentProgram $= Nothing
    return $ mappend (Compiled io cu) nxt
compileRenderCommand (Free (ClearDepth next)) = do
    nxt <- compileRenderCommand next
    return $ mappend (onlyRender $ clear [DepthBuffer]) nxt
compileRenderCommand (Free (ClearColorWith c next)) = do
    nxt <- compileRenderCommand next
    return $ mappend (onlyRender $ clearColor $= (toColor4 c) >> clear [ColorBuffer]) nxt
compileRenderCommand (Free (UsingTextures t ts cmd r n)) = do
    ts' <- putStrLn "Loading textures..." >> loadTextures t (fromF cmd) ts
    sub <- compileRenderCommand $ fromF r
    nxt <- compileRenderCommand n
    let io  = withTextures t ts' $ render sub
        cu  = do cleanup sub
                 deleteObjectNames ts'
    return $ Compiled io cu `mappend` nxt

loadTextures :: ( BindableTextureTarget t
                , ParameterizedTextureTarget t
                )
             => t -> TextureCommand () -> [TextureSrc] -> IO [TextureObject]
loadTextures t cmd ts = do
    texparams <- compileTextureCommand t $ fromF cmd
    texture t $= Enabled
    ts' <- forM ts $ \src -> do eT <- loadTextureSrc src
                                case eT of
                                    Left err -> putStrLn err
                                    Right _  -> render texparams
                                return eT
    return $ rights ts'

toColor4 :: Real a => V4 a -> Color4 GLfloat
toColor4 v = Color4 r g b a
    where (V4 r g b a) = fmap realToFrac v

sizeOfList :: forall a. Storable a => [a] -> GLsizeiptr
sizeOfList vs = fromIntegral $ (length vs) * sizeOf (undefined :: a)

onlyRender :: IO () -> CompiledRendering
onlyRender r = Compiled r (return ())

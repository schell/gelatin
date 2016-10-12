{-# LANGUAGE LambdaCase #-}
module Gelatin.WebGL.Shaders where

import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader
import Control.Monad (unless, forM_)
import GHCJS.Types
import GHCJS.DOM.Types
import GHCJS.DOM.JSFFI.Generated.WebGLRenderingContextBase
import Foreign.Storable
import Gelatin.Shaders
import Gelatin.WebGL.Common

type WGLShaderProgram = WebGLProgram
--type WGLShader = Shader
--------------------------------------------------------------------------------
-- Compiling shaders and programs
--------------------------------------------------------------------------------
wglCompileShader :: (MonadIO m, ToJSString s) => s -> GLenum -> WebGLT m WebGLShader
wglCompileShader source shaderType = do
  gl <- lift ask
  shader <- createShader gl shaderType >>= \case
    Nothing -> error "Could not create a shader"
    Just sh -> return sh
  shaderSource gl (Just shader) source
  compileShader gl (Just shader)
  jsval <- getShaderParameter gl (Just shader) COMPILE_STATUS
  success <- liftIO $ peek $ toPtr jsval
  if success
    then return shader
    else getShaderInfoLog gl (Just shader) >>= \case
      Nothing  -> error "Encountered an unreadable error while compiling a shader."
      Just err -> error $ "Could not compile shader: " ++ fromJSString err

wglCompileProgram :: (MonadIO m) => [WebGLShader] -> [Simple2DAttrib]
                  -> WebGLT m WGLShaderProgram
wglCompileProgram shaders attribs = do
  gl <- lift ask
  program <- createProgram gl >>= \case
    Nothing -> error "Could not create a shader program."
    Just p -> return p

  forM_ shaders (attachShader gl (Just program) . Just)
  forM_ attribs $ \attrib ->
    bindAttribLocation gl (Just program) (attribToGLuint attrib)
                                         (simple2DAttribIdentifier attrib)
  linkProgram gl (Just program)

  success <- do
    ptr <- toPtr <$> getProgramParameter gl (Just program) LINK_STATUS
    liftIO $ peek ptr

  unless success $ getProgramInfoLog gl (Just program) >>= \case
    Nothing -> error "Could not link program for some unreadable reason."
    Just jsstr -> error $ "Could not link program: " ++ fromJSString jsstr

  forM_ shaders (deleteShader gl . Just)
  return program
--------------------------------------------------------------------------------
-- Loading shaders
--------------------------------------------------------------------------------
-- | Compile all shader programs and return a "sum renderer".
--loadSumShader :: IO SumShader
--loadSumShader = do
--  vertName <- getDataFileName $ "shaders" </> "master.vert"
--  fragName <- getDataFileName $ "shaders" </> "master.frag"
--  SumShader <$> loader vertName fragName
--  where loader a b = loadGLShader $ ShaderDefFP [(a, VERTEX_SHADER)
--                                                ,(b, FRAGMENT_SHADER)
--                                                ] uniforms attribs
--        uniforms = P.map simple2DUniformIdentifier allSimple2DUniforms
--        attribs = allAttribs
--
--loadGLShader :: GLShaderDef -> IO GLShader
--loadGLShader (ShaderDefBS ss uniforms attribs) = do
--    shaders <- mapM (uncurry compileShader) ss
--    program <- compileProgram shaders attribs
--    glUseProgram program
--    ulocs <- forM uniforms $ \u -> do
--        loc <- withCString u $ glGetUniformLocation program
--        if loc == (-1)
--        then do P.putStrLn $ "Warning! Could not find the uniform " ++ show u
--                return Nothing
--        else return $ Just (u, loc)
--    let sh = Shader program (catMaybes ulocs)
--    --updateUniform (UniformMultiplierColor 1) sh
--    return sh
--loadGLShader (ShaderDefFP fps uniforms attribs) = do
--    srcs <- forM fps $ \(fp, shaderType) -> do
--        src <- B.readFile fp
--        return (src, shaderType)
--    loadGLShader $ ShaderDefBS srcs uniforms attribs

attribToGLuint :: Simple2DAttrib -> GLuint
attribToGLuint = fromIntegral . fromEnum

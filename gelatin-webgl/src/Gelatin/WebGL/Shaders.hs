{-# LANGUAGE LambdaCase #-}
module Gelatin.WebGL.Shaders where

import           Gelatin.Shaders
import           Gelatin.WebGL.Common

import           Control.Arrow                                       (first)
import           Control.Concurrent                                  (threadDelay)
import           Control.Monad                                       (forM,
                                                                      forM_,
                                                                      unless)
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Reader
import qualified Data.ByteString.Char8                               as C8
import           Data.Function                                       (fix)
import           Data.Maybe                                          (catMaybes)
import           Foreign.Storable
import           GHCJS.DOM.JSFFI.Generated.Enums
import           GHCJS.DOM.JSFFI.Generated.WebGLRenderingContextBase
import           GHCJS.DOM.JSFFI.Generated.XMLHttpRequest            hiding
                                                                      (error)
import           GHCJS.DOM.Types
import           GHCJS.Types

type WGLShaderDef = ShaderDef GLenum Simple2DAttrib
type WGLShader    = Shader WebGLProgram WebGLUniformLocation
newtype WGLSumShader = WGLSumShader { unShader :: WGLShader }
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
  success <- liftIO . peek . toPtr =<< getShaderParameter gl (Just shader)
                                                          COMPILE_STATUS
  if success
    then return shader
    else getShaderInfoLog gl (Just shader) >>= \case
      Nothing  -> error "Encountered an unreadable error while compiling a shader."
      Just err -> error $ "Could not compile shader: " ++ fromJSString err

wglCompileProgram :: (MonadIO m) => [WebGLShader] -> [Simple2DAttrib]
                  -> WebGLT m WebGLProgram
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
loadGLShader :: MonadIO m => WGLShaderDef -> WebGLT m WGLShader
loadGLShader (ShaderDefBS ss uniforms attribs) = do
  gl      <- lift ask
  shaders <- mapM (uncurry wglCompileShader . first C8.unpack) ss
  program <- wglCompileProgram shaders attribs
  useProgram gl $ Just program
  ulocs <- forM uniforms $ \u -> getUniformLocation gl (Just program) u >>= \case
    Nothing  -> do
      liftIO $ putStrLn $ "Warning! Could not find the uniform " ++ show u
      return Nothing
    Just loc -> return $ Just (u, loc)
  return $ Shader program (catMaybes ulocs)
loadGLShader (ShaderDefFP fps uniforms attribs) = do
    srcs <- forM fps $ \(fp, shaderType) -> do
        src <- C8.pack <$> getShaderFileSource fp
        return (src, shaderType)
    loadGLShader $ ShaderDefBS srcs uniforms attribs
  where getShaderFileSource fp = do
          req <- newXMLHttpRequest
          open req "GET" fp True "" ""
          send req
          fix $ \loop -> getStatus req >>= \case
            0 -> liftIO (threadDelay 1) >> loop
            _ -> return ()
          getResponseText req >>= \case
            Nothing  -> fail $ "Could not get shader source from" ++ show fp
            Just src -> return src

attribToGLuint :: Simple2DAttrib -> GLuint
attribToGLuint = fromIntegral . fromEnum

-- | Compile all shader programs and return a "sum renderer".
loadSumShaderRemote :: MonadIO m => FilePath -> FilePath -> WebGLT m WGLSumShader
loadSumShaderRemote vertPath fragPath = WGLSumShader <$> loader vertPath fragPath
  where loader a b = loadGLShader $ ShaderDefFP [(a, VERTEX_SHADER)
                                                ,(b, FRAGMENT_SHADER)
                                                ] uniforms attribs
        uniforms = map simple2DUniformIdentifier allSimple2DUniforms
        attribs = allAttribs

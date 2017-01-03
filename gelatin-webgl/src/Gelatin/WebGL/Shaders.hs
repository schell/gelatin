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
import           GHCJS.DOM.JSFFI.Generated.WebGLRenderingContextBase
import           GHCJS.DOM.JSFFI.Generated.XMLHttpRequest            hiding
                                                                      (error)
import           GHCJS.DOM.Types
import           GHCJS.Marshal
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
  liftIO $ putStrLn "Created shader"
  shaderSource gl (Just shader) source
  liftIO $ putStrLn "Added shader source"
  compileShader gl (Just shader)
  liftIO $ putStrLn "Compiled shader source"
  let toBool :: MonadIO m => JSVal -> m (Maybe Bool)
      toBool val
        | isNull val      = return Nothing
        | isUndefined val = return Nothing
        | otherwise       = liftIO $ do
            putStrLn "Got compile status...reading it"
            fromJSVal val

  liftIO $ putStrLn "Checking shader compile status"
  getShaderParameter gl (Just shader) COMPILE_STATUS >>= toBool >>= \case
    Nothing -> do
      liftIO $ putStrLn "Could not check compile status...assuming it's all good."
      return shader
    Just success
      | success -> do
        liftIO $ putStrLn "Shader compiled successfully"
        return shader
      | otherwise -> do
          liftIO $ putStrLn "Checking shader info log"
          getShaderInfoLog gl (Just shader) >>= \case
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

  success <- fmap (\case
      Nothing -> False
      Just s  -> s) $
        getProgramParameter gl (Just program) LINK_STATUS >>= liftIO . fromJSVal

  unless success $ getProgramInfoLog gl (Just program) >>= \case
    Nothing    -> error "Could not link program for some unreadable reason."
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
  liftIO $ putStrLn "Compiled shaders"
  program <- wglCompileProgram shaders attribs
  liftIO $ putStrLn "Created and linked programs"
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

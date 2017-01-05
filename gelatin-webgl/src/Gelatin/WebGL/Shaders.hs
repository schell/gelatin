{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase       #-}
module Gelatin.WebGL.Shaders where

import           Gelatin.Shaders
import           Gelatin.WebGL.Common

import           Control.Arrow                                       (first)
import           Control.Concurrent                                  (threadDelay)
import           Control.Monad                                       (forM,
                                                                      forM_,
                                                                      unless)
import           Control.Monad.IO.Class
import           Control.Monad.Reader
import           Data.ByteString.Char8                               (ByteString)
import qualified Data.ByteString.Char8                               as C8
import           Data.Function                                       (fix)
import           Data.Maybe                                          (catMaybes)
import           GHCJS.DOM.JSFFI.Generated.WebGLRenderingContextBase
import           GHCJS.DOM.JSFFI.Generated.XMLHttpRequest            hiding
                                                                      (error)
import           GHCJS.DOM.Types
import           GHCJS.Marshal
import           GHCJS.Types

wglCompileShader :: ToJSString s => s -> GLenum -> Gelatin WebGLShader
wglCompileShader source shaderType = do
  gl     <- asks gelRenderingContext
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

wglCompileProgram :: [WebGLShader] -> [Simple2DAttrib] -> Gelatin WebGLProgram
wglCompileProgram shaders attribs = do
  gl      <- asks gelRenderingContext
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
loadGLShader :: WGLShaderDef -> Gelatin WGLShader
loadGLShader (ShaderDefBS ss uniforms attribs) = do
  gl <- asks gelRenderingContext
  void $ getExtension gl "OES_standard_derivatives"
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

-- | Compile a shader program from a remote source
loadShaderRemote :: FilePath -> FilePath -> Gelatin WGLShader
loadShaderRemote a b = do
  gl <- asks gelRenderingContext

  ext <- getExtension gl "OES_standard_derivatives"
  when (isNull ext || isUndefined ext) $
    liftIO $ putStrLn "Could not load a needed extension: OES_standard_derivatives"

  let ufms = map simple2DUniformIdentifier allSimple2DUniforms
      atts = allAttribs
      shdef = ShaderDefFP [(a, VERTEX_SHADER),(b, FRAGMENT_SHADER)] ufms atts
  loadGLShader shdef

-- | Compile a shader program from a local, in memory source
loadShaderMemory :: ByteString -> ByteString -> Gelatin WGLShader
loadShaderMemory a b = do
  gl <- asks gelRenderingContext

  ext <- getExtension gl "OES_standard_derivatives"
  when (isNull ext || isUndefined ext) $
    liftIO $ putStrLn "Could not load a needed extension: OES_standard_derivatives"

  let ufms = map simple2DUniformIdentifier allSimple2DUniforms
      atts = allAttribs
      shdef = ShaderDefBS [(a, VERTEX_SHADER),(b, FRAGMENT_SHADER)] ufms atts
  loadGLShader shdef

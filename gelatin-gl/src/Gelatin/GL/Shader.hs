{-# LANGUAGE OverloadedStrings #-}
module Gelatin.GL.Shader (
    -- * Loading shaders
    loadSumShader,
    loadGLShader,
    -- * GLShader types
    GLShaderProgram,
    GLShader,
    GLShaderDef,
    SumShader(..),
    -- * GLShader prims
    PrimType(..),
    -- * Uniforms
    Simple2DUniform(..),
    -- * Updating uniforms for specific kinds of rendering
    updateUniformsForTris,
    updateUniformsForBezs,
    updateUniformsForLines,
    updateUniformsForMask,
    applyAlpha,
    applyMult,
    -- * Free form uniform updates
    updateUniform,
    updateUniforms,
    -- * Attributes
    -- $layout
    Simple2DAttrib(..),
    locToGLuint,
    -- * Enabling attribs for specific kinds of rendering
    enableAttribsForTris,
    enableAttribsForBezs,
    enableAttribsForLines,
    enableAttribsForMask,
    -- * Enabling and disabling any attribs
    onlyEnableAttribs,
    -- * GLShader compilation
    compileShader,
    compileProgram,
) where

import Prelude hiding (init)
import Prelude as P
import Graphics.GL.Core33
import Graphics.GL.Types
import Gelatin
import Gelatin.Shaders
import Control.Monad
import System.Exit
import System.FilePath
import Foreign.Ptr
import Foreign.C.String
import Foreign.Marshal.Array
import Foreign.Marshal.Utils
import Foreign.Storable
import Data.ByteString.Char8 as B
import Data.Maybe

type GLShaderProgram = GLuint

type GLShader = Shader GLuint GLint
type GLShaderDef = ShaderDef GLuint Simple2DAttrib
newtype SumShader = SumShader { unShader :: GLShader }
--------------------------------------------------------------------------------
-- Loading shaders.
--------------------------------------------------------------------------------
-- | Compile all shader programs and return a "sum renderer".
loadSumShader :: IO SumShader
loadSumShader = do
  vertName <- simple2dVertFilePath
  fragName <- simple2dFragFilePath
  SumShader <$> loader vertName fragName
  where loader a b = loadGLShader $ ShaderDefFP [(a, GL_VERTEX_SHADER)
                                              ,(b, GL_FRAGMENT_SHADER)
                                              ] uniforms attribs
        uniforms = P.map simple2DUniformIdentifier allSimple2DUniforms
        attribs = allAttribs

loadGLShader :: GLShaderDef -> IO GLShader
loadGLShader (ShaderDefBS ss uniforms attribs) = do
    shaders <- mapM (uncurry compileShader) ss
    program <- compileProgram shaders attribs
    glUseProgram program
    ulocs <- forM uniforms $ \u -> do
        loc <- withCString u $ glGetUniformLocation program
        if loc == (-1)
        then do P.putStrLn $ "Warning! Could not find the uniform " ++ show u
                return Nothing
        else return $ Just (u, loc)
    let sh = Shader program (catMaybes ulocs)
    --updateUniform (UniformMultiplierColor 1) sh
    return sh
loadGLShader (ShaderDefFP fps uniforms attribs) = do
    srcs <- forM fps $ \(fp, shaderType) -> do
        src <- B.readFile fp
        return (src, shaderType)
    loadGLShader $ ShaderDefBS srcs uniforms attribs
--------------------------------------------------------------------------------
-- Updating shader uniforms
--------------------------------------------------------------------------------
-- | Updates uniforms for rendering triangles.
updateUniformsForTris :: GLShader -> M44 Float -> M44 Float -> Bool -> Float
                      -> V4 Float -> Maybe (V4 Float) -> IO ()
updateUniformsForTris sh pj mv hasUV a m mr =
  updateUniforms (uniformsForTris pj mv hasUV a m mr) sh
{-# INLINE updateUniformsForTris #-}

-- | Updates uniforms for rendering loop-blinn beziers.
updateUniformsForBezs :: GLShader -> M44 Float -> M44 Float -> Bool -> Float
                      -> V4 Float -> Maybe (V4 Float) -> IO ()
updateUniformsForBezs sh pj mv hasUV a m mr =
  updateUniforms (uniformsForBezs pj mv hasUV a m mr) sh
{-# INLINE updateUniformsForBezs #-}

-- | Updates uniforms for rendering projected polylines.
updateUniformsForLines :: GLShader -> M44 Float -> M44 Float -> Bool -> Float
                       -> V4 Float -> Maybe (V4 Float) -> Float -> Float -> Float
                       -> (LineCap,LineCap) -> IO ()
updateUniformsForLines sh pj mv hasUV a m mr thickness feather sumlength caps =
  let us = uniformsForLines pj mv hasUV a m mr thickness feather sumlength caps
  in updateUniforms us sh
{-# INLINE updateUniformsForLines #-}

-- | Updates uniforms for rendering alpha masking.
updateUniformsForMask :: GLShader -> M44 Float -> M44 Float -> Float -> V4 Float
                      -> GLuint -> GLuint -> IO ()
updateUniformsForMask sh pj mv a m main mask =
  updateUniforms (uniformsForMask pj mv a m main mask) sh
{-# INLINE updateUniformsForMask #-}

updateUniforms :: [Simple2DUniform] -> GLShader -> IO ()
updateUniforms us s = mapM_ (`updateUniform` s) us
{-# INLINE updateUniforms #-}

updateUniform :: Simple2DUniform -> GLShader -> IO ()
updateUniform u s = withUniform (simple2DUniformIdentifier u) s $ \p loc -> do
  glUseProgram p
  uniformUpdateFunc u loc
{-# INLINE updateUniform #-}

uniformUpdateFunc :: Simple2DUniform -> GLint -> IO ()
uniformUpdateFunc (UniformPrimType p) u =
  glUniform1i u $ fromIntegral $ fromEnum p
uniformUpdateFunc (UniformProjection m44) u =
  with m44 $ glUniformMatrix4fv u 1 GL_TRUE . castPtr
uniformUpdateFunc (UniformModelView m44) u =
  with m44 $ glUniformMatrix4fv u 1 GL_TRUE . castPtr
uniformUpdateFunc (UniformThickness t) u = glUniform1f u t
uniformUpdateFunc (UniformFeather f) u = glUniform1f u f
uniformUpdateFunc (UniformSumLength l) u = glUniform1f u l
uniformUpdateFunc (UniformLineCaps (capx, capy)) u =
  let [x,y] = P.map (fromIntegral . fromEnum) [capx,capy] in glUniform2f u x y
uniformUpdateFunc (UniformHasUV has) u = glUniform1i u $ if has then 1 else 0
uniformUpdateFunc (UniformSampler s) u = glUniform1i u $ fromIntegral s
uniformUpdateFunc (UniformMainTex t) u = glUniform1i u $ fromIntegral t
uniformUpdateFunc (UniformMaskTex t) u = glUniform1i u $ fromIntegral t
uniformUpdateFunc (UniformAlpha a) u = glUniform1f u $ realToFrac a
uniformUpdateFunc (UniformMult v) u =
  let (V4 r g b a) = realToFrac <$> v in glUniform4f u r g b a
uniformUpdateFunc (UniformShouldReplaceColor s) u =
  glUniform1i u $ if s then 1 else 0
uniformUpdateFunc (UniformReplaceColor c) u =
  let (V4 r g b a) = realToFrac <$> c in glUniform4f u r g b a
{-# INLINE uniformUpdateFunc #-}

withUniform :: String -> GLShader -> (GLuint -> GLint -> IO ()) -> IO ()
withUniform name (Shader p ls) f =
    case lookup name ls of
        Nothing  -> do P.putStrLn $ "could not find uniform " ++ name
                       exitFailure
        Just loc -> f p loc
{-# INLINE withUniform #-}
--------------------------------------------------------------------------------
-- $layout
-- Attributes layout locations are unique and global.
--------------------------------------------------------------------------------
locToGLuint :: Simple2DAttrib -> GLuint
locToGLuint = fromIntegral . fromEnum

-- | Enables the provided attributes and disables all others.
onlyEnableAttribs :: [Simple2DAttrib] -> IO ()
onlyEnableAttribs atts = do
   mapM_ (glDisableVertexAttribArray . locToGLuint) allAttribs
   mapM_ (glEnableVertexAttribArray . locToGLuint) atts

enableAttribsForTris :: Bool -> IO ()
enableAttribsForTris True = onlyEnableAttribs [PositionLoc,UVLoc]
enableAttribsForTris False = onlyEnableAttribs [PositionLoc,ColorLoc]

enableAttribsForBezs :: Bool -> IO ()
enableAttribsForBezs True = onlyEnableAttribs [PositionLoc,UVLoc,BezLoc]
enableAttribsForBezs False = onlyEnableAttribs [PositionLoc,ColorLoc,BezLoc]

enableAttribsForLines :: Bool -> IO ()
enableAttribsForLines True =
  onlyEnableAttribs [PositionLoc,UVLoc,BezUVLoc,NextLoc,PrevLoc]
enableAttribsForLines False =
  onlyEnableAttribs [PositionLoc,ColorLoc,BezUVLoc,NextLoc,PrevLoc]

enableAttribsForMask :: IO ()
enableAttribsForMask = onlyEnableAttribs [PositionLoc,UVLoc]
--------------------------------------------------------------------------------
-- GLShader compilation
--------------------------------------------------------------------------------
compileShader :: ByteString -> GLuint -> IO GLuint
compileShader src sh = do
    shader <- glCreateShader sh
    when (shader == 0) $ do
        B.putStrLn "could not create shader"
        exitFailure

    withCString (B.unpack src) $ \ptr ->
       with ptr $ \ptrptr -> glShaderSource shader 1 ptrptr nullPtr

    glCompileShader shader
    success <- with (0 :: GLint) $ \ptr -> do
        glGetShaderiv shader GL_COMPILE_STATUS ptr
        peek ptr

    when (success == GL_FALSE) $ do
        B.putStrLn "could not compile shader:\n"
        B.putStrLn src
        infoLog <- with (0 :: GLint) $ \ptr -> do
            glGetShaderiv shader GL_INFO_LOG_LENGTH ptr
            logsize <- peek ptr
            allocaArray (fromIntegral logsize) $ \logptr -> do
                glGetShaderInfoLog shader logsize nullPtr logptr
                peekArray (fromIntegral logsize) logptr
        P.putStrLn $ P.map (toEnum . fromEnum) infoLog
        exitFailure

    return shader

compileProgram :: [GLuint] -> [Simple2DAttrib] -> IO GLuint
compileProgram shaders attribs = do
    program <- glCreateProgram

    forM_ shaders (glAttachShader program)
    forM_ attribs $ \attrib ->
        withCString (simple2DAttribIdentifier attrib) (glBindAttribLocation program (locToGLuint attrib))
    glLinkProgram program

    success <- with (0 :: GLint) $ \ptr -> do
        glGetProgramiv program GL_LINK_STATUS ptr
        peek ptr

    when (success == GL_FALSE) $ do
        B.putStrLn "could not link program"
        infoLog <- with (0 :: GLint) $ \ptr -> do
            glGetProgramiv program GL_INFO_LOG_LENGTH ptr
            logsize <- peek ptr
            allocaArray (fromIntegral logsize) $ \logptr -> do
                glGetProgramInfoLog program logsize nullPtr logptr
                peekArray (fromIntegral logsize) logptr
        P.putStrLn $ P.map (toEnum . fromEnum) infoLog
        exitFailure

    forM_ shaders glDeleteShader
    return program

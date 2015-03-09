module Shader where

import Prelude hiding (init)
import Graphics.GL.Core33
import Graphics.GL.Types
import Control.Monad
import System.Exit
import Foreign.Ptr
import Foreign.C.String
import Foreign.Marshal.Array
import Foreign.Marshal.Utils
import Foreign.Storable

positionLoc :: GLuint
positionLoc = 0

colorLoc :: GLuint
colorLoc = 1

uvLoc :: GLuint
uvLoc = 2

compileShader :: String -> GLuint -> IO GLuint
compileShader src sh = do
    shader <- glCreateShader sh
    when (shader == 0) $ do
        putStrLn "could not create shader"
        exitFailure

    withCString src $ \ptr ->
       with ptr $ \ptrptr -> glShaderSource shader 1 ptrptr nullPtr

    glCompileShader shader
    success <- with (0 :: GLint) $ \ptr -> do
        glGetShaderiv shader GL_COMPILE_STATUS ptr
        peek ptr

    when (success == GL_FALSE) $ do
        putStrLn "could not compile shader:\n"
        putStrLn src
        infoLog <- with (0 :: GLint) $ \ptr -> do
            glGetShaderiv shader GL_INFO_LOG_LENGTH ptr
            logsize <- peek ptr
            allocaArray (fromIntegral logsize) $ \logptr -> do
                glGetShaderInfoLog shader logsize nullPtr logptr
                peekArray (fromIntegral logsize) logptr
        putStrLn $ map (toEnum . fromEnum) infoLog
        exitFailure

    return shader

compileProgram :: [GLuint] -> IO GLuint
compileProgram shaders = do
    program <- glCreateProgram

    forM_ shaders (glAttachShader program)
    glLinkProgram program

    success <- with (0 :: GLint) $ \ptr -> do
        glGetProgramiv program GL_LINK_STATUS ptr
        peek ptr

    when (success == GL_FALSE) $ do
        putStrLn "could not link program"
        infoLog <- with (0 :: GLint) $ \ptr -> do
            glGetProgramiv program GL_INFO_LOG_LENGTH ptr
            logsize <- peek ptr
            allocaArray (fromIntegral logsize) $ \logptr -> do
                glGetProgramInfoLog program logsize nullPtr logptr
                peekArray (fromIntegral logsize) logptr
        putStrLn $ map (toEnum . fromEnum) infoLog
        exitFailure

    forM_ shaders glDeleteShader
    return program

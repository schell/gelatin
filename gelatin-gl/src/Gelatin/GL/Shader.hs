{-# LANGUAGE OverloadedStrings #-}
module Gelatin.GL.Shader (
    -- * Loading shaders
    loadSumShader,
    loadShader,
    -- * Shader types
    ShaderProgram,
    Shader(..),
    ShaderDef(..),
    SumShader(..),
    -- * Shader prims
    PrimType(..),
    -- * Uniforms
    Uniform(..),
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
    AttribLoc(..),
    locToGLuint,
    -- * Enabling attribs for specific kinds of rendering
    enableAttribsForTris,
    enableAttribsForBezs,
    enableAttribsForLines,
    enableAttribsForMask,
    -- * Enabling and disabling any attribs
    onlyEnableAttribs,
    -- * Shader compilation
    compileShader,
    compileProgram,
) where

import Prelude hiding (init)
import Prelude as P
import Graphics.GL.Core33
import Graphics.GL.Types
import Gelatin.Core
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
import Linear
import Paths_gelatin_gl

type ShaderProgram = GLuint

data Shader = Shader { shProgram :: ShaderProgram
                     , shUniforms :: [(String, GLint)]
                     } deriving (Show)

data ShaderDef = ShaderDefFP { defShaderPaths :: [(String, GLuint)]
                             -- ^ [("path/to/shader.vert", GL_VERTEX_SHADER)]
                             , defUniforms :: [String]
                             , defAttribs :: [AttribLoc]
                             }
               | ShaderDefBS { defShaderSrcs :: [(ByteString, GLuint)]
                             , defUniforms :: [String]
                             , defAttribs :: [AttribLoc]
                             } deriving (Show, Eq, Ord)

newtype SumShader = SumShader { unShader :: Shader }
--------------------------------------------------------------------------------
-- Loading built shaders.
--------------------------------------------------------------------------------
-- | Compile all shader programs and return a "sum renderer".
loadSumShader :: IO SumShader
loadSumShader = do
  vertName <- getDataFileName $ "shaders" </> "master.vert"
  fragName <- getDataFileName $ "shaders" </> "master.frag"
  SumShader <$> loader vertName fragName
  where loader a b = loadShader $ ShaderDefFP [(a, GL_VERTEX_SHADER)
                                              ,(b, GL_FRAGMENT_SHADER)
                                              ] uniforms attribs
        uniforms = P.map glslUniformIdentifier allUniforms
        attribs = allAttribs

loadShader :: ShaderDef -> IO Shader
loadShader (ShaderDefBS ss uniforms attribs) = do
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
loadShader (ShaderDefFP fps uniforms attribs) = do
    srcs <- forM fps $ \(fp, shaderType) -> do
        src <- B.readFile fp
        return (src, shaderType)
    loadShader $ ShaderDefBS srcs uniforms attribs
--------------------------------------------------------------------------------
-- Uniform Helper Types
--------------------------------------------------------------------------------
data PrimType = PrimTri
              | PrimBez
              | PrimLine
              | PrimMask
              deriving (Show, Eq, Enum, Ord, Bounded)
--------------------------------------------------------------------------------
-- Updating uniforms
--------------------------------------------------------------------------------
data Uniform = UniformPrimType PrimType
             | UniformProjection (M44 Float)
             | UniformModelView (M44 Float)
             | UniformThickness Float
             | UniformFeather Float
             | UniformSumLength Float
             | UniformLineCaps (LineCap,LineCap)
             | UniformHasUV Bool
             | UniformSampler GLuint
             | UniformMainTex GLuint
             | UniformMaskTex GLuint
             | UniformAlpha Float
             | UniformMult (V4 Float)
             deriving (Show, Ord, Eq)

allUniforms :: [Uniform]
allUniforms = [UniformPrimType PrimTri
              ,UniformProjection 0
              ,UniformModelView 0
              ,UniformThickness 0
              ,UniformFeather 0
              ,UniformSumLength 0
              ,UniformLineCaps (LineCapNone, LineCapNone)
              ,UniformHasUV False
              ,UniformSampler 0
              ,UniformMainTex 0
              ,UniformMaskTex 0
              ,UniformAlpha 1
              ,UniformMult 1
              ]

applyAlpha :: Float -> [Uniform] -> [Uniform]
applyAlpha a us = UniformAlpha a : P.filter f us
  where f (UniformAlpha _) = False
        f _ = True

applyMult :: V4 Float -> [Uniform] -> [Uniform]
applyMult v us = UniformMult v : P.filter f us
  where f (UniformMult _) = False
        f _ = True

glslUniformIdentifier :: Uniform -> String
glslUniformIdentifier (UniformPrimType _)   = "primitive"
glslUniformIdentifier (UniformProjection _) = "projection"
glslUniformIdentifier (UniformModelView _)  = "modelview"
glslUniformIdentifier (UniformThickness _)  = "thickness"
glslUniformIdentifier (UniformFeather _)    = "feather"
glslUniformIdentifier (UniformSumLength _)  = "sumlength"
glslUniformIdentifier (UniformLineCaps _)   = "cap"
glslUniformIdentifier (UniformHasUV _)      = "hasUV"
glslUniformIdentifier (UniformSampler _)    = "sampler"
glslUniformIdentifier (UniformMainTex _)    = "mainTex"
glslUniformIdentifier (UniformMaskTex _)    = "maskTex"
glslUniformIdentifier (UniformAlpha _)      = "alpha"
glslUniformIdentifier (UniformMult _)       = "mult"

uniformsForTris :: M44 Float -> M44 Float -> Bool -> Float -> V4 Float
                -> [Uniform]
uniformsForTris pj mv hasUV a m = P.map f allUniforms
  where f (UniformPrimType _) = UniformPrimType PrimTri
        f (UniformProjection _) = UniformProjection pj
        f (UniformModelView _) = UniformModelView mv
        f (UniformHasUV _) = UniformHasUV hasUV
        f (UniformAlpha _) = UniformAlpha a
        f (UniformMult _) = UniformMult m
        f x = x
{-# INLINE uniformsForTris #-}

uniformsForBezs :: M44 Float -> M44 Float -> Bool -> Float -> V4 Float
                -> [Uniform]
uniformsForBezs pj mv hasUV a m = P.map f $ uniformsForTris pj mv hasUV a m
  where f (UniformPrimType _) = UniformPrimType PrimBez
        f x = x
{-# INLINE uniformsForBezs #-}

uniformsForLines :: M44 Float -> M44 Float -> Bool -> Float -> V4 Float
                 -> Float -> Float -> Float -> (LineCap,LineCap) -> [Uniform]
uniformsForLines pj mv hasUV a m thickness feather sumlength caps =
  P.map f $ uniformsForTris pj mv hasUV a m
    where f (UniformPrimType _) = UniformPrimType PrimLine
          f (UniformThickness _) = UniformThickness thickness
          f (UniformFeather _) = UniformFeather feather
          f (UniformSumLength _) = UniformSumLength sumlength
          f (UniformLineCaps _) = UniformLineCaps caps
          f x = x
{-# INLINE uniformsForLines #-}

uniformsForMask :: M44 Float -> M44 Float -> Float -> V4 Float -> GLuint -> GLuint
                -> [Uniform]
uniformsForMask pj mv a m main mask = P.map f $ uniformsForTris pj mv True a m
  where f (UniformPrimType _) = UniformPrimType PrimMask
        f (UniformMainTex _) = UniformMainTex main
        f (UniformMaskTex _) = UniformMaskTex mask
        f x = x
{-# INLINE uniformsForMask #-}

-- | Updates uniforms for rendering triangles.
updateUniformsForTris :: Shader -> M44 Float -> M44 Float -> Bool -> Float
                      -> V4 Float -> IO ()
updateUniformsForTris sh pj mv hasUV a m =
  updateUniforms (uniformsForTris pj mv hasUV a m) sh
{-# INLINE updateUniformsForTris #-}

-- | Updates uniforms for rendering loop-blinn beziers.
updateUniformsForBezs :: Shader -> M44 Float -> M44 Float -> Bool -> Float
                      -> V4 Float -> IO ()
updateUniformsForBezs sh pj mv hasUV a m =
  updateUniforms (uniformsForBezs pj mv hasUV a m) sh
{-# INLINE updateUniformsForBezs #-}

-- | Updates uniforms for rendering projected polylines.
updateUniformsForLines :: Shader -> M44 Float -> M44 Float -> Bool -> Float
                       -> V4 Float -> Float -> Float -> Float
                       -> (LineCap,LineCap) -> IO ()
updateUniformsForLines sh pj mv hasUV a m thickness feather sumlength caps =
  let us = uniformsForLines pj mv hasUV a m thickness feather sumlength caps
  in updateUniforms us sh
{-# INLINE updateUniformsForLines #-}

-- | Updates uniforms for rendering alpha masking.
updateUniformsForMask :: Shader -> M44 Float -> M44 Float -> Float -> V4 Float
                      -> GLuint -> GLuint -> IO ()
updateUniformsForMask sh pj mv a m main mask =
  updateUniforms (uniformsForMask pj mv a m main mask) sh
{-# INLINE updateUniformsForMask #-}

updateUniforms :: [Uniform] -> Shader -> IO ()
updateUniforms us s = mapM_ (`updateUniform` s) us
{-# INLINE updateUniforms #-}

updateUniform :: Uniform -> Shader -> IO ()
updateUniform u s = withUniform (glslUniformIdentifier u) s $ \p loc -> do
    glUseProgram p
    uniformUpdateFunc u loc
{-# INLINE updateUniform #-}

uniformUpdateFunc :: Uniform -> GLint -> IO ()
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
{-# INLINE uniformUpdateFunc #-}

withUniform :: String -> Shader -> (GLuint -> GLint -> IO ()) -> IO ()
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
data AttribLoc = PositionLoc
               | ColorLoc
               | UVLoc
               | BezLoc
               | NextLoc
               | PrevLoc
               | BezUVLoc
               deriving (Show, Eq, Ord, Enum, Bounded)

allAttribs :: [AttribLoc]
allAttribs = [minBound..maxBound]

glslAttribIdentifier :: AttribLoc -> String
glslAttribIdentifier PositionLoc = "position"
glslAttribIdentifier ColorLoc = "color"
glslAttribIdentifier UVLoc = "uv"
glslAttribIdentifier BezLoc = "bez"
glslAttribIdentifier NextLoc = "next"
glslAttribIdentifier PrevLoc = "previous"
glslAttribIdentifier BezUVLoc = "bezuv"

locToGLuint :: AttribLoc -> GLuint
locToGLuint = fromIntegral . fromEnum

-- | Enables the provided attributes and disables all others.
onlyEnableAttribs :: [AttribLoc] -> IO ()
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
-- Shader compilation
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

compileProgram :: [GLuint] -> [AttribLoc] -> IO GLuint
compileProgram shaders attribs = do
    program <- glCreateProgram

    forM_ shaders (glAttachShader program)
    forM_ attribs $ \attrib ->
        withCString (glslAttribIdentifier attrib) (glBindAttribLocation program (locToGLuint attrib))
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

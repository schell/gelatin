{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Gelatin.GL.Shader (
    -- * Loading shaders
    loadShaders,
    loadBezShader,
    loadGeomShader,
    loadMaskShader,
    loadProjectedPolylineShader,
    loadShader,
    -- * Shader types
    ShaderProgram,
    Shader(..),
    ShaderDef(..),
    ProjectedPolylineShader(..),
    GeomShader(..),
    BezShader(..),
    MaskShader(..),
    SumShader(..),
    -- * Uniforms
    Uniform(..),
    updateUniform,
    updateUniforms,
    -- * Attributes
    -- $layout
    AttribLoc(..),
    locToGLuint,
    -- * Enabling and disabling attribs
    onlyEnableAttribs,
    -- * Shader source files
    vertSourceProjPoly,
    fragSourceProjPoly,
    vertSourceGeom,
    fragSourceGeom,
    vertSourceBezier,
    fragSourceBezier,
    vertSourceMask,
    fragSourceMask,
    -- * Shader compilation
    compileShader,
    compileProgram,
) where

import Prelude hiding (init)
import Prelude as P
import Graphics.GL.Core33
import Graphics.GL.Types
import Gelatin.Picture
import Control.Monad
import System.Exit
import System.Directory
import Foreign.Ptr
import Foreign.C.String
import Foreign.Marshal.Array
import Foreign.Marshal.Utils
import Foreign.Storable
import Data.ByteString.Char8 as B
import Data.FileEmbed
import Data.Maybe
import Linear

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

newtype ProjectedPolylineShader = PPRS Shader
newtype GeomShader = GRS Shader
newtype BezShader = BRS Shader
newtype MaskShader = MRS Shader
data SumShader = SRS { shProjectedPolyline :: ProjectedPolylineShader
                     , shGeometry :: GeomShader
                     , shBezier   :: BezShader
                     , shMask     :: MaskShader
                     }
--------------------------------------------------------------------------------
-- Loading built shaders.
--------------------------------------------------------------------------------
-- | Compile all shader programs and return a "sum renderer".
loadShaders :: IO SumShader
loadShaders = SRS <$> loadProjectedPolylineShader
                  <*> loadGeomShader
                  <*> loadBezShader
                  <*> loadMaskShader

-- | Compile a shader program and link attributes for rendering screen space
-- projected expanded polylines.
loadProjectedPolylineShader :: IO ProjectedPolylineShader
loadProjectedPolylineShader = do
    let nocaps = (LineCapNone,LineCapNone)
        uniforms = P.map glslUniformIdentifier [UniformProjection 0
                                               ,UniformModelView 0
                                               ,UniformThickness 0
                                               ,UniformFeather 0
                                               ,UniformSumLength 0
                                               ,UniformLineCaps nocaps
                                               ,UniformSampler 0
                                               ,UniformHasUV False
                                               ]
        attribs = [PositionLoc,ColorLoc,BezUVLoc,NextLoc,PrevLoc,UVLoc]
        def = ShaderDefBS[(vertSourceProjPoly, GL_VERTEX_SHADER)
                         ,(fragSourceProjPoly, GL_FRAGMENT_SHADER)
                         ] uniforms attribs
    PPRS <$> loadShader def

-- | Loads a new shader program and attributes for rendering geometry.
loadGeomShader :: IO GeomShader
loadGeomShader = do
    let uniforms = P.map glslUniformIdentifier [UniformProjection 0
                                               ,UniformModelView 0
                                               ,UniformSampler 0
                                               ,UniformHasUV False
                                               ]
        attribs = [PositionLoc,ColorLoc,UVLoc]
        def = ShaderDefBS [(vertSourceGeom, GL_VERTEX_SHADER)
                          ,(fragSourceGeom, GL_FRAGMENT_SHADER)
                          ] uniforms attribs
    GRS <$> loadShader def

-- | Loads a new shader progarm and attributes for rendering beziers.
loadBezShader :: IO BezShader
loadBezShader = do
    let uniforms = P.map glslUniformIdentifier [UniformProjection 0
                                               ,UniformModelView 0
                                               ,UniformSampler 0
                                               ,UniformHasUV False
                                               ]
        attribs = [PositionLoc,ColorLoc,UVLoc,BezLoc]
        def = ShaderDefBS [(vertSourceBezier, GL_VERTEX_SHADER)
                          ,(fragSourceBezier, GL_FRAGMENT_SHADER)
                          ] uniforms attribs
    BRS <$> loadShader def

-- | Loads a new shader program and attributes for masking textures.
loadMaskShader :: IO MaskShader
loadMaskShader = do
    let uniforms = P.map glslUniformIdentifier [UniformProjection 0
                                               ,UniformModelView 0
                                               ,UniformMainTex 0
                                               ,UniformMaskTex 0
                                               ]
        attribs = [PositionLoc,UVLoc]
        def = ShaderDefBS [(vertSourceMask, GL_VERTEX_SHADER)
                          ,(fragSourceMask, GL_FRAGMENT_SHADER)
                          ] uniforms attribs
    MRS <$> loadShader def

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
    return $ Shader program (catMaybes ulocs)
loadShader (ShaderDefFP fps uniforms attribs) = do
    cwd <- getCurrentDirectory
    srcs <- forM fps $ \(fp, shaderType) -> do
        src <- B.readFile $ cwd ++ "/" ++ fp
        return (src, shaderType)
    loadShader $ ShaderDefBS srcs uniforms attribs
--------------------------------------------------------------------------------
-- Updating uniforms
--------------------------------------------------------------------------------
data Uniform = UniformProjection (M44 Float)
             | UniformModelView (M44 Float)
             | UniformThickness Float
             | UniformFeather Float
             | UniformSumLength Float
             | UniformLineCaps (LineCap,LineCap)
             | UniformHasUV Bool
             | UniformSampler Int
             | UniformMainTex Int
             | UniformMaskTex Int
             deriving (Show, Ord, Eq)

glslUniformIdentifier :: Uniform -> String
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

updateUniforms :: [Uniform] -> Shader -> IO ()
updateUniforms us s = mapM_ (`updateUniform` s) us

updateUniform :: Uniform -> Shader -> IO ()
updateUniform u s = withUniform (glslUniformIdentifier u) s $ \p loc -> do
    glUseProgram p
    uniformUpdateFunc u loc

uniformUpdateFunc :: Uniform -> GLint -> IO ()
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

withUniform :: String -> Shader -> (GLuint -> GLint -> IO ()) -> IO ()
withUniform name (Shader p ls) f =
    case lookup name ls of
        Nothing  -> do P.putStrLn $ "could not find uniform " ++ name
                       exitFailure
        Just loc -> f p loc
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

allLocs :: [AttribLoc]
allLocs = [minBound..maxBound]

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
   mapM_ (glDisableVertexAttribArray . locToGLuint) allLocs
   mapM_ (glEnableVertexAttribArray . locToGLuint) atts
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
--------------------------------------------------------------------------------
-- Shader sources
--------------------------------------------------------------------------------
vertSourceProjPoly :: ByteString
vertSourceProjPoly = $(embedFile "shaders/projected-line.vert")

fragSourceProjPoly :: ByteString
fragSourceProjPoly = $(embedFile "shaders/projected-line.frag")

vertSourceGeom :: ByteString
vertSourceGeom = $(embedFile "shaders/2d.vert")

fragSourceGeom :: ByteString
fragSourceGeom = $(embedFile "shaders/2d.frag")

vertSourceBezier :: ByteString
vertSourceBezier = $(embedFile "shaders/bezier.vert")

fragSourceBezier :: ByteString
fragSourceBezier = $(embedFile "shaders/bezier.frag")

vertSourceMask :: ByteString
vertSourceMask = $(embedFile "shaders/mask.vert")

fragSourceMask :: ByteString
fragSourceMask = $(embedFile "shaders/mask.frag")

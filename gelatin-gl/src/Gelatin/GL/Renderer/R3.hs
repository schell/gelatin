{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Gelatin.GL.Renderer.R3
  (
    -- * R3 Renderers
    colorRenderer
  , textureRenderer
    -- * Uniform updates
  , updateProjection
  , updateModelView
  , updateSampler
  , updateHasUV
  , updateAlpha
  , updateMultiply
  , updateShouldReplaceColor
  , updateReplacementColor
    -- * Load the R3 shader
  , loadSimple3DShader
  ) where

import           Control.Monad.Except       (MonadError)
import           Control.Monad.IO.Class     (MonadIO, liftIO)
import           Data.Proxy                 (Proxy (..))
import           Data.Vector.Unboxed        (Vector)
import qualified Data.Vector.Unboxed        as V
import           Foreign.Marshal.Array
import           Graphics.GL.Core33
import           Graphics.GL.Types
import           System.FilePath            ((</>))
--------------------------------------------------------------------------------
import           Gelatin
import           Gelatin.Shaders.GL
--------------------------------------------------------------------------------
import           Gelatin.GL.Renderer.Common
import           Paths_gelatin_gl

type APosition = Attribute "position" (V3 Float) 0
type AColor    = Attribute "color"    (V4 Float) 1
type AUV       = Attribute "uv"       (V2 Float) 2
type ANormal   = Attribute "normal"   (V3 Float) 3

type Simple3DAttribs = '[APosition, AColor, AUV, ANormal]
type Simple3DAttribToggles = TypeMap AttributeToggling Simple3DAttribs
type Simple3DAttribBuffers = TypeMap AttributeBuffering Simple3DAttribs

type UProjection         = Uniform "projection"         (M44 Float)
type UModelView          = Uniform "modelview"          (M44 Float)
type USampler            = Uniform "sampler"            Int
type UHasUV              = Uniform "hasUV"              Bool
type UAlpha              = Uniform "alpha"              Float
type UMult               = Uniform "mult"               (V4 Float)
type UShouldReplaceColor = Uniform "shouldColorReplace" Bool
type UReplaceColor       = Uniform "replaceColor"       (V4 Float)

type Simple3DUniforms = '[ UProjection
                         , UModelView
                         , USampler
                         , UHasUV
                         , UAlpha
                         , UMult
                         , UShouldReplaceColor
                         , UReplaceColor
                         ]

type Simple3DShaders = '[VertexShader, FragmentShader]

type Simple3DShader = GLuint

inShaderDir :: FilePath -> IO FilePath
inShaderDir = getDataFileName . ("shaders" </>)

simple3dVertFilePath :: IO FilePath
simple3dVertFilePath = inShaderDir "simple3d.vert"

simple3dFragFilePath :: IO FilePath
simple3dFragFilePath = inShaderDir "simple3d.frag"

-- | Compile all 3D shader programs and return a 3D renderer.
loadSimple3DShader :: (MonadIO m, MonadError String m) => m Simple3DShader
loadSimple3DShader = do
  names <- liftIO $ sequence [simple3dVertFilePath, simple3dFragFilePath]
  let paths :: ShaderSteps '[VertexShader, FragmentShader] FilePath
      paths = ShaderSteps names
  loadProgram paths (Proxy :: Proxy Simple3DAttribs)
--------------------------------------------------------------------------------
-- Uniform update functions
--------------------------------------------------------------------------------
updateProjection         :: GLuint -> M44 Float -> IO ()
updateModelView          :: GLuint -> M44 Float -> IO ()
updateSampler            :: GLuint -> Int -> IO ()
updateHasUV              :: GLuint -> Bool -> IO ()
updateAlpha              :: GLuint -> Float -> IO ()
updateMultiply           :: GLuint -> V4 Float -> IO ()
updateShouldReplaceColor :: GLuint -> Bool -> IO ()
updateReplacementColor   :: GLuint -> V4 Float -> IO ()
updateProjection :& updateModelView :& updateSampler :& updateHasUV
  :& updateAlpha :& updateMultiply :& updateShouldReplaceColor
  :& updateReplacementColor :& () = genFunction (Proxy :: Proxy Simple3DUniforms)
--------------------------------------------------------------------------------
-- Attribute toggle and buffering functions
--------------------------------------------------------------------------------
enablePosition  :: IO ()
--disablePosition :: IO ()
enableColor     :: IO ()
disableColor    :: IO ()
enableUV        :: IO ()
disableUV       :: IO ()
--enableNormal    :: IO ()
disableNormal   :: IO ()
(enablePosition, _)
  :& (enableColor, disableColor)
  :& (enableUV, disableUV)
  :& (_, disableNormal)
  :& () = genFunction (Proxy :: Proxy Simple3DAttribToggles)

bufferPosition :: GLint -> GLuint -> Vector (V3 Float) -> IO GLuint
bufferColor    :: GLint -> GLuint -> Vector (V4 Float) -> IO GLuint
bufferUV       :: GLint -> GLuint -> Vector (V2 Float) -> IO GLuint
--bufferNormal   :: (GLint -> GLuint -> Vector (V3 Float) -> IO ())
bufferPosition :& bufferColor :& bufferUV :& _ :& ()
  = genFunction (Proxy :: Proxy Simple3DAttribBuffers)

renderFunctionWith :: Simple3DShader -> Bool -> [RenderTransform3] -> IO ()
renderFunctionWith sh hasUV t = do
  glUseProgram sh
  let (mv, a, m, mr) = unwrapTransforms3 t
  updateModelView sh mv
  updateHasUV sh hasUV
  updateSampler sh 0
  updateAlpha sh a
  updateMultiply sh m
  flip (maybe (updateShouldReplaceColor sh False)) mr $ \c -> do
    updateShouldReplaceColor sh True
    updateReplacementColor sh c

colorRenderer
  :: Context -> Simple3DShader -> GLuint -> Vector (V3 Float) -> Vector (V4 Float)
  -> IO Renderer3
colorRenderer _ sh mode vs cs =
  withVAO $ \vao -> withBuffers 2 $ \[pbuf, cbuf] -> do
    sequence_ [enablePosition, enableColor, disableUV, disableNormal]
    clearErrors "r3.colorRenderer enable attribs"
    bufferPosition 3 pbuf vs
    bufferColor 4 cbuf $ V.take (V.length vs) cs
    clearErrors "r3.colorRenderer buffer attribs"
    let num = fromIntegral $ V.length vs
        renderFunction t = do
          renderFunctionWith sh False t
          drawBuffer sh vao mode num
        cleanup = do
          withArray [pbuf, cbuf] $ glDeleteBuffers 2
          withArray [vao] $ glDeleteVertexArrays 1
    return (cleanup, renderFunction)

textureRenderer
  :: Context -> Simple3DShader -> GLuint -> Vector (V3 Float) -> Vector (V2 Float)
  -> IO Renderer3
textureRenderer _ sh mode vs cs =
  withVAO $ \vao -> withBuffers 2 $ \[pbuf, cbuf] -> do
    sequence_ [enablePosition, disableColor, enableUV, disableNormal]
    clearErrors "r3.textureRenderer enable attribs"
    bufferPosition 3 pbuf vs
    bufferUV 2 cbuf $ V.take (V.length vs) cs
    clearErrors "r3.textureRenderer buffer attribs"
    let num = fromIntegral $ V.length vs
        renderFunction t = do
          renderFunctionWith sh True t
          drawBuffer sh vao mode num
        cleanup = do
          withArray [pbuf, cbuf] $ glDeleteBuffers 2
          withArray [vao] $ glDeleteVertexArrays 1
    return (cleanup, renderFunction)

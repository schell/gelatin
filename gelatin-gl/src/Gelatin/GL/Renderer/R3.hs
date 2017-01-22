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
  ) where

import           Data.Proxy                 (Proxy (..))
import           Data.Vector.Unboxed        (Vector)
import qualified Data.Vector.Unboxed        as V
import           Foreign.Marshal.Array
import           Graphics.GL.Core33
import           Graphics.GL.Types
--------------------------------------------------------------------------------
import           Gelatin
import           Gelatin.Shaders
import           Gelatin.Shaders.Simple3D
--------------------------------------------------------------------------------
import           Gelatin.GL.Renderer.Common
import           Gelatin.GL.Shader
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

bufferPosition :: GLint -> GLuint -> Vector (V3 Float) -> IO ()
bufferColor    :: GLint -> GLuint -> Vector (V4 Float) -> IO ()
bufferUV       :: GLint -> GLuint -> Vector (V2 Float) -> IO ()
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

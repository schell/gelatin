{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE JavaScriptFFI         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE PartialTypeSignatures #-}
module Gelatin.WebGL where

import           Control.Monad.Trans.Either
import           Control.Monad.Trans.Reader
import           Control.Monad.Trans.Class
import           Control.Monad.IO.Class
import           Control.Monad (forM, when)
import           Data.Maybe (catMaybes)
import qualified Data.Vector.Generic as G
import qualified Data.Vector.Storable as S
import qualified Data.Vector.Unboxed as V
import           Data.Vector.Unboxed (Vector,Unbox)
import           Foreign.Storable
import           GHCJS.Types
import           GHCJS.Marshal
import           GHCJS.DOM (currentWindow, currentDocument)
import           GHCJS.DOM.Types
import           GHCJS.DOM.JSFFI.Generated.Document (createElement)
import qualified GHCJS.DOM.JSFFI.Generated.Document as DOM (getBody)
import           GHCJS.DOM.JSFFI.Generated.Element (setAttribute)
import           GHCJS.DOM.JSFFI.Generated.HTMLCanvasElement
  ( getContext
  , castToHTMLCanvasElement
  , setWidth
  , setHeight
  )
import           GHCJS.DOM.JSFFI.Generated.WebGLRenderingContextBase
import           GHCJS.DOM.JSFFI.Generated.OESVertexArrayObject
import           Gelatin
import           Gelatin.Shaders
import           Gelatin.WebGL.Common
import           Gelatin.WebGL.Shaders
--------------------------------------------------------------------------------
-- Setup and underlying machinery
--------------------------------------------------------------------------------
type V2V4 = (V2 Float, V4 Float)
type ColorPictureData = PictureData WebGLTexture (V2 Float, V4 Float)
type ColorPictureT = PictureT WebGLTexture (V2 Float, V4 Float)

type V2V2 = (V2 Float, V2 Float)
type TexturePictureData = PictureData WebGLTexture (V2 Float, V2 Float)
type TexturePictureT = PictureT WebGLTexture (V2 Float, V2 Float)

data WebGLBackends = WebGLBackends
  { backendV2V4 :: Backend WebGLTexture () V2V4 (V2 Float) Float Raster
  , backendV2V2 :: Backend WebGLTexture () V2V2 (V2 Float) Float Raster
  }

runIOMaybe :: MonadIO m => String -> IO (Maybe a) -> EitherT String m a
runIOMaybe str f = liftIO f >>= \case
  Nothing -> fail str
  Just a  -> return a

runWebGLT :: Monad m
          => WebGLT m a -> WebGLRenderingContextBase -> m (Either String a)
runWebGLT f = runReaderT (runEitherT f)

webDocument :: MonadIO m => EitherT String m Document
webDocument = runIOMaybe "Could not access the document." currentDocument

webBody :: MonadIO m => EitherT String m HTMLElement
webBody = do
  doc <- webDocument
  runIOMaybe "Could not return the body element." $ DOM.getBody doc

webCreateElement :: MonadIO m => String -> EitherT String m Element
webCreateElement str = do
  doc <- webDocument
  runIOMaybe ("Could not create a " ++ show str ++ " element.") $
    createElement doc (Just str)

webCanvasAndContext :: MonadIO m
                    => EitherT String m (HTMLCanvasElement, WebGLRenderingContextBase)
webCanvasAndContext = do
  doc    <- webDocument
  canvas <- do
    el <-webCreateElement "canvas"
    liftIO $ castToHTMLCanvasElement el
  ctx  <- getContext canvas "webgl"
  ctx2 <- if isNull ctx
             then do ctx2 <- getContext canvas "experimental-webgl"
                     if isNull ctx2
                        then fail "Could not create the WebGL context."
                        else return ctx2
             else return ctx

  return (canvas, WebGLRenderingContextBase ctx2)
--------------------------------------------------------------------------------
-- WebGL helpers
--------------------------------------------------------------------------------
clearErrors :: MonadIO m => String -> WebGLT m ()
clearErrors str = do
  gl  <- lift ask
  err <- getError gl
  when (err /= 0) $ error $ unwords [str, show err]

foreign import javascript unsafe "new Float32Array($1)"
  allocFloat32Array_js :: JSVal -> IO JSVal

allocFloat32Array :: (ToJSVal a, Floating a) => [a] -> IO Float32Array
allocFloat32Array xs =
  Float32Array <$> (toJSValListOf xs >>= allocFloat32Array_js)

getVAOExt :: MonadIO m => WebGLT m OESVertexArrayObject
getVAOExt = do
  gl <- lift ask
  liftIO . fromJSValUnchecked =<< getExtension gl "OES_vertex_array_object"

-- | Alloc and use a vertex array object.
withVAO :: MonadIO m => (WebGLVertexArrayObjectOES -> WebGLT m b) -> WebGLT m b
withVAO f = do
  gl  <- lift ask
  ext <- getVAOExt
  Just vao <- createVertexArrayOES ext
  r <- f vao
  clearErrors "withVAO"
  return r

-- | Alloc and use some number of buffers in an operation.
withBuffers :: MonadIO m => Int -> ([WebGLBuffer] -> WebGLT m b) -> WebGLT m b
withBuffers n f = do
  buffers <- catMaybes <$>
    forM [0..n] (\_ -> lift ask >>= (liftIO . createBuffer))
  f buffers

-- | Marshal an attributes data to the GPU.
bufferAttrib :: (MonadIO m, ToJSVal a, Unbox a, Floating a)
             => Simple2DAttrib -> GLint -> WebGLBuffer -> Vector a
             -> WebGLT m ()
bufferAttrib attr n buf as = do
  gl <- lift ask
  bindBuffer gl ARRAY_BUFFER $ Just buf
  let loc = attribToGLuint attr

  dat <- liftIO $ allocFloat32Array (V.toList as) >>= castToArrayBuffer
  bufferData gl ARRAY_BUFFER (Just dat) STATIC_DRAW
  enableVertexAttribArray gl loc
  vertexAttribPointer gl loc n FLOAT False 0 0

drawBuffer :: MonadIO m
           => WebGLProgram -> WebGLVertexArrayObjectOES -> GLenum -> GLsizei
           -> WebGLT m ()
drawBuffer program vao mode num = do
  gl  <- lift ask
  ext <- getVAOExt
  useProgram gl (Just program)
  bindVertexArrayOES ext (Just vao)
  clearErrors "drawBuffer:bindVertexArrayOES"
  drawArrays gl mode 0 num
  clearErrors "drawBuffer:drawArrays"

bufferImageData :: MonadIO m
                => HTMLImageElement -> GLenum -> GLenum -> WebGLT m ()
bufferImageData img imgfmt pxfmt = do
  gl <- lift ask
  texImage2D gl
    TEXTURE_2D
    0
    RGBA
    imgfmt
    pxfmt
    (Just img)
  clearErrors "bufferImageData"

-- | Creates an IO () drawing computation that masks an IO () drawing
-- computation with another using a stencil test.
renderStencilMask :: MonadIO m => WebGLT m () -> WebGLT m () -> WebGLT m ()
renderStencilMask r1 r2 = do
  gl <- lift ask
  clear gl DEPTH_BUFFER_BIT
  enable gl STENCIL_TEST
  colorMask gl False False False False
  depthMask gl False
  stencilMask gl 0xFF
  clear gl STENCIL_BUFFER_BIT
  stencilFunc gl NEVER 0 1
  stencilOp gl INVERT INVERT INVERT
  r1

  colorMask gl True True True True
  depthMask gl True
  stencilFunc gl EQUAL 1 1
  stencilOp gl ZERO ZERO ZERO
  r2
  disable gl STENCIL_TEST
--------------------------------------------------------------------------------
-- Renderers
--------------------------------------------------------------------------------
--
--------------------------------------------------------------------------------
-- WebGL Backend
--------------------------------------------------------------------------------
startup :: MonadIO m
        => Int -> Int -> EitherT String m (HTMLCanvasElement, WebGLRenderingContextBase)
startup w h = do
  (canvas, ctx) <- webCanvasAndContext
  setWidth canvas w
  setHeight canvas h
  liftIO $ putStrLn "Got canvas and context."
  return (canvas, ctx)

startupWebGLBackends :: Int -> Int -> IO (Either String WebGLBackends)
startupWebGLBackends w h = runEitherT (undefined w h) >>= \case
  Left str -> do putStrLn str
                 return $ Left str
  Right be -> return $ Right be

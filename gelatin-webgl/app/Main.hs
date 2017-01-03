{-# LANGUAGE LambdaCase #-}
module Main where

import           Gelatin.WebGL
import           Gelatin.WebGL.Common
import           Gelatin.WebGL.Shaders

import           Gelatin

import           Control.Concurrent.Async
import           Control.Concurrent.STM
import           Control.Concurrent.STM.TVar
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Either
import           Control.Monad.Trans.Reader
import           Data.Bits                                           ((.|.))
import           Data.Function                                       (fix)
import           GHCJS.DOM                                           (currentDocument,
                                                                      currentWindow)
import           GHCJS.DOM.EventM                                    (on)
import           GHCJS.DOM.JSFFI.Generated.Element
import           GHCJS.DOM.JSFFI.Generated.HTMLCanvasElement         as Canvas
import           GHCJS.DOM.JSFFI.Generated.HTMLImageElement          as Image
import           GHCJS.DOM.JSFFI.Generated.Node
import           GHCJS.DOM.JSFFI.Generated.WebGLRenderingContextBase
import           GHCJS.DOM.Types
import           GHCJS.Types

type WebGLV2V2 =
  Backend WebGLTexture () (V2 Float, V2 Float) (V2 Float) Float Raster

loadImage :: MonadIO m => FilePath -> EitherT String m HTMLImageElement
loadImage file = do
  img <- liftIO . castToHTMLImageElement =<< webCreateElement "img"
  setSrc img file
  t <- liftIO $ newTVarIO False
  cleanup <- liftIO $ on img load $ liftIO $ atomically $ writeTVar t True
  fix $ \loop -> liftIO (readTVarIO t) >>= \case
    True -> liftIO cleanup >> return img
    False -> loop

loadTexture :: MonadIO m => FilePath -> WebGLT m (WebGLTexture, V2 Int)
loadTexture file = do
  gl  <- lift ask
  tex <- runIOMaybe "Could not create texture." $ createTexture gl
  activeTexture gl TEXTURE0
  bindTexture gl TEXTURE_2D $ Just tex
  img <- loadImage file
  sz  <- V2 <$> Image.getWidth img <*> Image.getHeight img
  -- | TODO: If size has a power of 2 width and height, generate mipmaps
  --generateMipmap gl TEXTURE_2D
  --texParameteri  gl TEXTURE_2D TEXTURE_MIN_FILTER NEAREST_MIPMAP_NEAREST
  texParameteri  gl TEXTURE_2D TEXTURE_WRAP_S CLAMP_TO_EDGE
  texParameteri  gl TEXTURE_2D TEXTURE_WRAP_T CLAMP_TO_EDGE
  texParameteri  gl TEXTURE_2D TEXTURE_MAG_FILTER NEAREST
  texParameteri  gl TEXTURE_2D TEXTURE_MIN_FILTER NEAREST
  bindTexture    gl TEXTURE_2D Nothing
  return (tex, sz)

bindTexsAround :: MonadIO m => [WebGLTexture] -> m a -> WebGLT m a
bindTexsAround texs f = do
  gl <- lift ask
  mapM_ (uncurry $ bindTex gl) (zip texs [TEXTURE0 ..])
  a  <- lift $ lift f
  bindTexture gl TEXTURE_2D Nothing
  return a
    where bindTex gl tex u = do
            activeTexture gl u
            bindTexture gl TEXTURE_2D $ Just tex

-- | Creates an IO () drawing computation that masks an IO () drawing
-- computation with another using a stencil test.
gelStencilMask :: MonadIO m => IO () -> IO () -> WebGLT m ()
gelStencilMask r2 r1  = do
  gl <- lift ask
  clear gl DEPTH_BUFFER_BIT
  -- Enable stencil testing
  enable gl STENCIL_TEST
  -- Disable writing frame buffer color components
  colorMask gl False False False False
  -- Disable writing into the depth buffer
  depthMask gl False
  -- Enable writing to all bits of the stencil mask
  stencilMask gl 0xFF
  -- Clear the stencil buffer
  clear gl STENCIL_BUFFER_BIT
  stencilFunc gl NEVER 0 1
  stencilOp gl INVERT INVERT INVERT
  liftIO r1

  colorMask gl True True True True
  depthMask gl True
  stencilFunc gl EQUAL 1 1
  stencilOp gl ZERO ZERO ZERO
  liftIO r2
  disable gl STENCIL_TEST

applyOption :: WebGLRenderingContextBase
            -> (c, rs -> IO ())
            -> RenderingOption
            -> (c, rs -> IO ())
applyOption ctx (c, r) StencilMaskOption =
  (c, \rs -> runWebGLT (gelStencilMask (r rs) (r rs)) ctx >>= \case
               Left err -> fail err
               Right () -> return ())

webglV2V2 :: MonadIO m
          => HTMLCanvasElement
          -> WebGLRenderingContextBase
          -> EitherT String m WebGLV2V2
webglV2V2 canvas ctx = do
  void $ getExtension ctx "OES_standard_derivatives"
  let webCanvasSize = V2 <$> Canvas.getWidth canvas <*> Canvas.getHeight canvas
      gelClearWindow = do
        V2 w h <- webCanvasSize
        viewport ctx 0 0 (fromIntegral w) (fromIntegral h)
        clear ctx (COLOR_BUFFER_BIT .|. DEPTH_BUFFER_BIT)
      gelSetClearColor (V4 r g b a) = clearColor ctx r g b a
      gelAllocTexture file = runWebGLT (loadTexture file) ctx >>= \case
        Left err -> putStrLn err >> return Nothing
        Right t  -> return $ Just t
      ops = BackendOps{ backendOpGetFramebufferSize = webCanvasSize
                      , backendOpGetWindowSize      = webCanvasSize
                      , backendOpClearWindow        = gelClearWindow
                      , backendOpUpdateWindow       = return () -- non op!
                      , backendOpSetClearColor      = gelSetClearColor . fmap realToFrac
                      , backendOpAllocTexture       = gelAllocTexture
                      , backendOpBindTextures       = \ts f ->
                          runWebGLT (bindTexsAround ts f) ctx >>= \case
                            Left err -> fail err
                            Right a  -> return a
                      , backendOpGetEvents          = return []
                      }
      comp = BackendComp{ backendCompApplyOption = applyOption ctx
                        , backendCompCompiler    = undefined
                        }

  return Backend{ backendOps = ops
                , backendCompiler = comp
                }

app :: MonadIO m => EitherT String m ()
app = do
  body          <- webBody
  (canvas, ctx) <- startup 600 400
  void $ liftIO $ appendChild body $ Just canvas
  v2v2 <- webglV2V2 canvas ctx

  liftIO $ (backendOpGetWindowSize $ backendOps v2v2) >>= print

  liftIO $ do
    enable ctx BLEND
    blendFunc ctx SRC_ALPHA ONE_MINUS_SRC_ALPHA
    clearColor ctx 0.5 0.5 0.5 1
    clear ctx (COLOR_BUFFER_BIT .|. DEPTH_BUFFER_BIT)

  let imagePath =
        "https://upload.wikimedia.org/wikipedia/en/1/18/Mega_Man_Series.jpg"

  img <- loadImage imagePath

  void $ liftIO $ appendChild body $ Just img

  liftIO (allocTexture v2v2 imagePath) >>= liftIO . \case
    Nothing -> putStrLn "Could not alloc texture."
    Just (_, V2 w h) -> putStrLn $ "Texture is " ++ show (w, h) ++ "px"

  let frag = "https://gist.githubusercontent.com/schell/a0e129e01458c8570540a782142857d0/raw/263b1549a527fa9582fd51798f21b1b983cbba6f/simple2dwebgl.frag"
      vert = "https://gist.githubusercontent.com/schell/8ef693f134c68892a0e6d4fd1705f032/raw/4823cc85e8a587e04c543f9aabd06dc4dc2f10d8/simple2dwebgl.vert"

  runWebGLT (loadSumShaderRemote vert frag) ctx >>= \case
    Left err -> liftIO $ print err
    Right sh -> liftIO $ putStrLn "Got sum shader"

main :: IO ()
main = runEitherT app >>= \case
  Left str -> putStrLn str
  Right _ -> putStrLn "Done."

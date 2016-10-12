{-# LANGUAGE LambdaCase #-}
module Main where

import Gelatin.WebGL
import Gelatin.WebGL.Common
import Gelatin.WebGL.Shaders

import Data.Bits ((.|.))
import Data.Function (fix)
import Control.Monad
import Control.Monad.Trans.Either
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Class
import Control.Monad.IO.Class
import Control.Concurrent.Async
import Control.Concurrent.STM.TVar
import Control.Concurrent.STM
import GHCJS.Types
import GHCJS.DOM (currentWindow, currentDocument)
import GHCJS.DOM.EventM (on)
import GHCJS.DOM.Types
import GHCJS.DOM.JSFFI.Generated.Node
import GHCJS.DOM.JSFFI.Generated.WebGLRenderingContextBase
import GHCJS.DOM.JSFFI.Generated.Element
import GHCJS.DOM.JSFFI.Generated.HTMLCanvasElement as Canvas
import GHCJS.DOM.JSFFI.Generated.HTMLImageElement as Image
import Gelatin

loadImage :: MonadIO m => FilePath -> EitherT String m HTMLImageElement
loadImage file = do
  el  <- webCreateElement "img"
  img <- liftIO $ castToHTMLImageElement el
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
  generateMipmap gl TEXTURE_2D
  texParameteri  gl TEXTURE_2D TEXTURE_WRAP_S REPEAT
  texParameteri  gl TEXTURE_2D TEXTURE_WRAP_T REPEAT
  texParameteri  gl TEXTURE_2D TEXTURE_MAG_FILTER NEAREST
  texParameteri  gl TEXTURE_2D TEXTURE_MIN_FILTER NEAREST_MIPMAP_NEAREST
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

app :: MonadIO m => EitherT String m ()
app = do
  body          <- webBody
  (canvas, ctx) <- startup 600 400
  void $ liftIO $ appendChild body $ Just canvas

  let webCanvasSize = V2 <$> Canvas.getWidth canvas <*> Canvas.getHeight canvas
      clearWindow = do
        V2 w h <- webCanvasSize
        viewport ctx 0 0 (fromIntegral w) (fromIntegral h)
        clear ctx (COLOR_BUFFER_BIT .|. DEPTH_BUFFER_BIT)
      --updateWindow = return ()
      setClearColor (V4 r g b a) = clearColor ctx r g b a
      allocTexture file = runWebGLT (loadTexture file) ctx >>= \case
        Left err -> putStrLn err >> return Nothing
        Right t  -> return $ Just t
  webCanvasSize >>= (liftIO . print)

  liftIO $ do
    enable ctx BLEND
    blendFunc ctx SRC_ALPHA ONE_MINUS_SRC_ALPHA
    clearColor ctx 0 0 0 1
    clear ctx (COLOR_BUFFER_BIT .|. DEPTH_BUFFER_BIT)

  img <- loadImage
    "https://upload.wikimedia.org/wikipedia/en/1/18/Mega_Man_Series.jpg"

  void $ liftIO $ appendChild body $ Just img

main :: IO ()
main = runEitherT app >>= \case
  Left str -> putStrLn str
  Right _ -> putStrLn "Done."

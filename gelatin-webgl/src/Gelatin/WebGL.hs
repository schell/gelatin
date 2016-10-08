{-# LANGUAGE LambdaCase    #-}
{-# LANGUAGE TupleSections #-}
module Gelatin.WebGL where

import Control.Monad.Trans.Either
import Control.Monad.Trans.Reader
import Control.Monad.IO.Class
import GHCJS.Types
import GHCJS.DOM (currentWindow, currentDocument)
import GHCJS.DOM.Types
import GHCJS.DOM.JSFFI.Generated.Document (createElement)
import qualified GHCJS.DOM.JSFFI.Generated.Document as DOM (getBody)
import GHCJS.DOM.JSFFI.Generated.Element (setAttribute)
import GHCJS.DOM.JSFFI.Generated.HTMLCanvasElement
  ( getContext
  , castToHTMLCanvasElement
  , setWidth
  , setHeight
  )
import GHCJS.DOM.JSFFI.Generated.WebGLRenderingContextBase
import Gelatin

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

type WebGLT m = EitherT String (ReaderT WebGLRenderingContextBase m)

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

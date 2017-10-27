{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
-- | This module provides an entry point for your gelatin
-- apps that run on sdl2.
module Gelatin.SDL2
  ( -- * Backend definitions
    SDL2Backends(..)
    -- * Obtaining the backends
  , startupSDL2Backends
  , startupSDL2BackendsWithConfig
    -- * Re-exports
  , module Gelatin.GL
  ) where

import           Control.Monad.Except   (MonadError)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Data.String            (fromString)
import           Gelatin.GL
import           SDL                    hiding (Rectangle, Renderer,
                                         glBindTexture, glUnbindTexture)


-- | A record containing both V2V4 and V2V2 backends.
data SDL2Backends = SDL2Backends
  { backendV2V4 :: Backend GLuint Event V2V4 (V2 Float) Float Raster
  , backendV2V2 :: Backend GLuint Event V2V2 (V2 Float) Float Raster
  }


-- | Start up and return the sdl2 backends according to the given
-- sdl2 'WindowConfig'.
startupSDL2BackendsWithConfig
  :: (MonadIO m, MonadError String m)
  => WindowConfig
  -- ^ The configuration used to set up the window.
  -> String
  -- ^ The window title
  -> m SDL2Backends
startupSDL2BackendsWithConfig cfg str = do
  w <- liftIO $ do
    initializeAll
    w     <- createWindow (fromString str) cfg
    _     <- glCreateContext w
    return w
  sh <- loadSimple2DShader

  liftIO $ do
    glEnable GL_BLEND
    glBlendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA

  let wsize =  do V2 x y <- get $ windowSize w
                  return (fromIntegral x, fromIntegral y)
      fsize = do V2 x y <- glGetDrawableSize w
                 return (fromIntegral x, fromIntegral y)

      ctx = Context { ctxFramebufferSize = fsize
                    , ctxWindowSize = wsize
                    }
      rz   = Rez sh ctx
      ops  = glOps rz (updateWindowSDL2 w) pollEvents
      v2v4 = Backend ops $ glV2V4Compiler rz
      v2v2 = Backend ops $ glV2V2Compiler rz
  return $ SDL2Backends v2v4 v2v2


-- | Start up and return the default backends.
-- Uses OpenGL 3.3 with debugging turned on.
startupSDL2Backends
  :: (MonadIO m, MonadError String m)
  => Int
  -- ^ Window width
  -> Int
  -- ^ Window height
  -> String
  -- ^ Window title
  -> Bool
  -- ^ Whether or not to request a high DPI window.
  -- Passing 'True' typically results in a framebuffer with 2x
  -- the window size.
  -> m SDL2Backends
startupSDL2Backends ww wh ws highDPI = do
    let openGL = defaultOpenGL{ glProfile = Core Debug 3 3
                              }
        window = defaultWindow{ windowInitialSize = V2 (fromIntegral ww)
                                                       (fromIntegral wh)
                              , windowOpenGL = Just openGL
                              , windowResizable = True
                              , windowHighDPI = highDPI
                              }
    startupSDL2BackendsWithConfig window ws


updateWindowSDL2 :: Window -> IO ()
updateWindowSDL2 = glSwapWindow

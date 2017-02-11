{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
module Gelatin.SDL2
  ( SDL2Backends(..)
  , startupSDL2Backends
  , startupSDL2BackendsWithConfig
  , module G
  ) where

import           Control.Monad.Except   (MonadError)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Data.String            (fromString)
import           Gelatin.GL             as G
import           Gelatin.GL.Renderer.R2 as R2
import           Gelatin.GL.Renderer.R3 as R3
import           SDL                    hiding (Rectangle, Renderer,
                                         glBindTexture, glUnbindTexture)

data SDL2Backends = SDL2Backends
  { backendV2V4 :: Backend GLuint Event V2V4 (V2 Float) Float Raster
  , backendV2V2 :: Backend GLuint Event V2V2 (V2 Float) Float Raster
  , backendV3V4 :: Backend GLuint Event V3V4 (V3 Float) (Quaternion Float) Raster
  , backendV3V2 :: Backend GLuint Event V3V2 (V3 Float) (Quaternion Float) Raster
  }

startupSDL2Backends :: (MonadIO m, MonadError String m)
                    => Int -> Int -> String -> Bool -> m SDL2Backends
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

startupSDL2BackendsWithConfig :: (MonadIO m, MonadError String m)
                              => WindowConfig -> String -> m SDL2Backends
startupSDL2BackendsWithConfig cfg str = do
  w <- liftIO $ do
    initializeAll
    w     <- createWindow (fromString str) cfg
    _     <- glCreateContext w
    return w

  -- Load the R2 and R3 shaders and set some default uniform values
  sh2 <- R2.loadSimple2DShader
  sh3 <- R3.loadSimple3DShader
  liftIO $ do
    glUseProgram sh2
    R2.updateAlpha sh2 1
    R2.updateMultiply sh2 1

    glUseProgram sh3
    R3.updateAlpha sh3 1
    R3.updateMultiply sh3 1


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
      rz2  = Rez sh2 ctx
      rz3  = Rez sh3 ctx
      ops2 = glOps rz2 (updateWindowSDL2 w) pollEvents $ \m44 -> do
        glUseProgram sh2
        R2.updateProjection sh2 m44
      ops3 = glOps rz3 (updateWindowSDL2 w) pollEvents $ \m44 -> do
        glUseProgram sh3
        R3.updateProjection sh3 m44
      v2v4 = Backend ops2 (glV2V4Compiler rz2)
      v2v2 = Backend ops2 (glV2V2Compiler rz2)
      v3v4 = Backend ops3 (glV3V4Compiler rz3)
      v3v2 = Backend ops3 (glV3V2Compiler rz3)
  return $ SDL2Backends v2v4 v2v2 v3v4 v3v2

updateWindowSDL2 :: Window -> IO ()
updateWindowSDL2 = glSwapWindow

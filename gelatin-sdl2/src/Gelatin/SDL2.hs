{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Gelatin.SDL2
  ( SDL2Backends(..)
  , startupSDL2Backends
  , startupSDL2BackendsWithConfig
  , module G
  ) where

import           Gelatin.GL as G
import           Data.String (fromString)
import           SDL hiding (glBindTexture,glUnbindTexture,Rectangle,Renderer)

data SDL2Backends = SDL2Backends
  { backendV2V4 :: Backend GLuint Event V2V4 (V2 Float) Float Raster
  , backendV2V2 :: Backend GLuint Event V2V2 (V2 Float) Float Raster
  }

startupSDL2Backends :: Int -> Int -> String -> Bool -> IO SDL2Backends
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

startupSDL2BackendsWithConfig :: WindowConfig -> String -> IO SDL2Backends
startupSDL2BackendsWithConfig cfg str = do
  initializeAll

  w     <- createWindow (fromString str) cfg
  _     <- glCreateContext w
  sh    <- loadSumShader

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

updateWindowSDL2 :: Window -> IO ()
updateWindowSDL2 = glSwapWindow

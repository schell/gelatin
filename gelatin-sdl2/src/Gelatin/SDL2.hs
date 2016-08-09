{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Gelatin.SDL2 (
    -- * Startup
    startupSDL2Backend,
    startupSDL2BackendWithConfig,
    -- * Rendering
    renderWithSDL2,
    updateWindowSDL2,
    -- * Re-exports
    module GL
) where

import           Gelatin.GL as GL
import           Control.Monad
import           Control.Arrow (second)
import           Data.Hashable
import qualified Data.Text as T
import           Data.Bits
import           SDL hiding (glBindTexture,glUnbindTexture,Rectangle,Renderer)
import           Linear hiding (rotate)
import           System.Exit
import           System.IO
import           GHC.Generics

startupSDL2Backend :: Int -> Int -> String -> Bool -> IO (Rez, Window)
startupSDL2Backend ww wh ws highDPI = do
    let openGL = defaultOpenGL{ glProfile = Core Debug 3 3
                              }
        window = defaultWindow{ windowInitialSize = V2 (fromIntegral ww)
                                                       (fromIntegral wh)
                              , windowOpenGL = Just openGL
                              , windowResizable = True
                              , windowHighDPI = highDPI
                              }
    startupSDL2BackendWithConfig window ws

startupSDL2BackendWithConfig :: WindowConfig -> String -> IO (Rez, Window)
startupSDL2BackendWithConfig cfg str = do
    initializeAll

    w     <- createWindow (T.pack str) cfg
    glctx <- glCreateContext w
    sh    <- loadSumShader

    glEnable GL_BLEND
    glBlendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA

    let wsize =  do V2 x y <- get $ windowSize w
                    return (fromIntegral x, fromIntegral y)
        fsize = do V2 x y <- glGetDrawableSize w
                   return (fromIntegral x, fromIntegral y)
        dpi = do ww <- fst <$> wsize
                 fw <- fst <$> fsize
                 return $ floor $ 72 * fromIntegral fw / fromIntegral ww

        ctx = Context { ctxFramebufferSize = fsize
                      , ctxWindowSize = wsize
                      , ctxScreenDpi = dpi
                      }
    return (Rez sh ctx, w)

updateWindowSDL2 :: Window -> IO ()
updateWindowSDL2 = glSwapWindow

renderWithSDL2 :: Window -> Rez -> IO () -> IO ()
renderWithSDL2 window rez f = do
  clearFrame rez
  f
  updateWindowSDL2 window

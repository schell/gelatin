{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Gelatin.SDL2 (
    -- * Re-exports
    module GL,
    module SDL,
    startupSDL2Backend
) where

import Gelatin.GL as GL
import Control.Monad
import Control.Arrow (second)
import Data.Hashable
import Data.Text as T
import SDL hiding (glBindTexture,glUnbindTexture,Rectangle,Renderer)
import Linear hiding (rotate)
import System.Exit
import System.IO
import GHC.Generics

startupSDL2Backend :: Int -> Int -> String -> IO (Rez, Window)
startupSDL2Backend ww wh ws = do
    initializeAll

    let openGL = defaultOpenGL{ glProfile = Core Debug 3 3 
                              }
        window = defaultWindow{ windowInitialSize = V2 (fromIntegral ww) 
                                                       (fromIntegral wh)
                              , windowOpenGL = Just openGL
                              , windowResizable = True
                              }
    w <- createWindow (T.pack ws) window

    glctx <- glCreateContext w

    sh <- loadShaders

    glEnable GL_BLEND
    glBlendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA

    let size =  do V2 x y <- get $ windowSize w 
                   return (fromIntegral x, fromIntegral y)

        ctx = Context { ctxFramebufferSize = size
                      , ctxScreenDpi = return 128
                      , ctxWindowSize = size                      
                      }
    return (Rez sh ctx, w)

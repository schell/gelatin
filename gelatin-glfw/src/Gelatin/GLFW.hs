{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Gelatin.GLFW (
    Rez(..),
    startupGLFWBackend,
    newWindow,
    -- * Re-exports
    module GL,
    module GLFW,
) where

import Gelatin.GL as GL
import Control.Monad
import Control.Arrow (second)
import Data.Hashable
import Graphics.UI.GLFW as GLFW
import Linear hiding (rotate)
import System.Exit
import System.IO
import GHC.Generics

-- | Creates a window. This can only be called after initializing with
-- `initGelatin`.
newWindow :: Int -- ^ Width
          -> Int -- ^ Height
          -> String -- ^ Title
          -> Maybe Monitor -- ^ The monitor to fullscreen into.
          -> Maybe Window -- ^ A window to share OpenGL contexts with.
          -> IO Window
newWindow ww wh ws mmon mwin = do
    defaultWindowHints
    windowHint $ WindowHint'OpenGLDebugContext True
    windowHint $ WindowHint'OpenGLProfile OpenGLProfile'Core
    windowHint $ WindowHint'OpenGLForwardCompat True
    windowHint $ WindowHint'ContextVersionMajor 3
    windowHint $ WindowHint'ContextVersionMinor 3
    windowHint $ WindowHint'DepthBits 16
    mwin' <- createWindow ww wh ws mmon mwin
    makeContextCurrent mwin'
    case mwin' of
        Nothing  -> do putStrLn "could not create window"
                       exitFailure
        Just win -> return win

calculateDpi :: Window -> IO Int
calculateDpi win = do
    mMonitor <- getPrimaryMonitor
    -- Calculate the dpi of the primary monitor.
    case mMonitor of
        -- I've choosen 128 as the default DPI because of my macbook 15"
        --   -Schell
        Nothing -> return 128
        Just m  -> do (w, h) <- getMonitorPhysicalSize m
                      mvmode <- getVideoMode m
                      case mvmode of
                          Nothing -> return 128
                          Just (VideoMode vw vh _ _ _ _) -> do
                              let mm2 = fromIntegral $ w*h :: Double
                                  px  = sqrt $ (fromIntegral vw :: Double) 
                                               * fromIntegral vh
                                  inches = sqrt $ mm2 / (25.4 * 25.4)
                              let dpi = floor $ px / inches
                              return dpi

-- | Completes all initialization, creates a new window and returns
-- the resource record and the new window. If any part of the process fails the 
-- program will exit with failure.
startupGLFWBackend :: Int -- ^ Window width
                   -> Int -- ^ Window height
                   -> String -- ^ Window title
                   -> Maybe Monitor -- ^ The monitor to fullscreen into
                   -> Maybe Window -- ^ A window to share OpenGL contexts with
                   -> IO (Rez, Window)
startupGLFWBackend ww wh ws mmon mwin = do
    setErrorCallback $ Just $ \_ -> hPutStrLn stderr
    initd <- GLFW.init
    unless initd $ do putStrLn "could not initialize glfw"
                      exitFailure
    w  <- newWindow ww wh ws mmon mwin
    sh <- loadShaders

    glEnable GL_BLEND
    glBlendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA

    let ctx = Context { ctxFramebufferSize = getFramebufferSize w
                      , ctxWindowSize = getWindowSize w
                      , ctxScreenDpi = calculateDpi w
                      }
    return (Rez sh ctx, w)

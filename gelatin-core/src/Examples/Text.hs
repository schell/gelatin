{-# LANGUAGE OverloadedStrings #-}
module Examples.Text where

import System.Exit
import Gelatin.Core.Rendering
import Gelatin.Core.Color
import Graphics.UI.GLFW
import Graphics.GL.Core33
import Graphics.Text.TrueType
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async
import Control.Monad
import Data.Bits

text :: Window -> GeomRenderSource -> BezRenderSource -> IO ()
text win grs brs = do
    afc  <- compileFontCache
    fc   <- wait afc
    Just arial <- withFont fc (FontDescriptor "Arial" $ FontStyle False False)
                              return
    Just bold  <- withFont fc (FontDescriptor "Arial" $ FontStyle True False)
                              return
    Just italic  <- withFont fc (FontDescriptor "Arial" $ FontStyle False True)
                                return

    glEnable GL_BLEND
    glBlendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA

    r <- do rs <- forM (zip [arial,bold,italic] [1..]) $ \(font,i) ->
                    let fstr = FontString font 32 (0,i * 32) "This is some text."
                    in colorFontRendering win grs brs fstr $ const white
            Rendering f _ <- colorFontRendering win grs brs (FontString arial 128 (0, 3*32 + 128) "S")
                                                            $ const white
            return $ do mapM_ (\(Rendering f _) -> f mempty) rs
                        f mempty

    let loop = do (fbw,fbh) <- getFramebufferSize win
                  glViewport 0 0 (fromIntegral fbw) (fromIntegral fbh)
                  glClear $ GL_COLOR_BUFFER_BIT .|. GL_DEPTH_BUFFER_BIT

                  r

                  pollEvents
                  swapBuffers win
                  shouldClose <- windowShouldClose win
                  if shouldClose
                  then exitSuccess
                  else threadDelay 100
                  loop
    loop

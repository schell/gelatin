{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
module Main where

import           Gelatin
import           Gelatin.GL
import           Gelatin.SDL2 hiding (ek)
import           Gelatin.FreeType2.Internal
import           SDL hiding (glBindTexture)
import           Control.Monad
import           Halive.Utils
import           System.Exit (exitSuccess)

isQuit :: Event -> Bool
isQuit (Event _ payload) = isKeyQ payload || payload == QuitEvent
  where
    isKeyQ (KeyboardEvent (KeyboardEventData _ _ _ (Keysym _ KeycodeQ _))) = True
    isKeyQ _ = False

main :: IO ()
main = do
  let fnt = "/Library/Fonts/Arial.ttf"
  (rez,window) <- reacquire 0 $ startupSDL2Backend 800 600 "gelatin-freetype2" True
  Just glrAtlas <- allocAtlas fnt (PixelSize 64 64) asciiChars
  (glr,_,_) <- freetypeGLRenderer rez glrAtlas white "Straight to gl\n(for retina - 2x)"

  Just (atlasPic,stringPic) <- withAtlas fnt (PixelSize 32 32) asciiChars $ \atlas -> do
    let V2 w h = fromIntegral <$> atlasTextureSize atlas

    (_,atlasPic) <- compileTexturePicture rez $ do
      setTextures [atlasTexture atlas]
      setGeometry $ triangles $ do
        tri (0, 0) (V2 w 0, V2 1 0) (V2 w h, V2 1 1)
        tri (0, 0) (V2 w h, V2 1 1) (V2 0 h, V2 0 1)
    (_,stringPic) <- compileTexturePictureT rez $ do
      let lineHeight = 32
      move $ V2 0 (h + lineHeight)
      embed $ freetypePicture atlas white "Here is a string written"
      embed $ do
        move $ V2 0 lineHeight
        freetypePicture atlas white "with gelatin-freetype2! :)"
      embed $ do
        move $ V2 0 $ lineHeight * 2
        freetypePicture atlas green "Yo, we have good kerning!"
      embed $ do
        move $ V2 0 $ lineHeight * 3
        freetypePicture atlas blue "ToYoTa"
    return (atlasPic,stringPic)

  forever $ do
    events <- pollEvents
    renderWithSDL2 window rez $ do
      atlasPic []
      stringPic []
      snd glr [Spatial $ Translate $ V2 100 100]
    when (any isQuit events) exitSuccess

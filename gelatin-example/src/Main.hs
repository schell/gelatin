{-# LANGUAGE TemplateHaskell #-}

import Control.Concurrent     (threadDelay)
import Control.Monad          (when, unless)
import Data.ByteString        (ByteString)
import Data.ByteString.Lazy   (fromStrict)
import Data.FileEmbed         (embedFile)
import Graphics.Text.TrueType (decodeFont)
import SDL                    (EventPayload(QuitEvent))
import System.Directory       (getCurrentDirectory)
import System.FilePath        ((</>))
import System.Exit            (exitSuccess, exitFailure)

import Data.Renderable
import Gelatin.SDL2

ttfFont, jpgTex :: Data.ByteString.ByteString
ttfFont = $(embedFile $ "assets" </> "Neuton-Regular.ttf")
jpgTex  = $(embedFile $ "assets" </> "tex.jpg")

picture :: FontData -> GLuint -> Picture Transform GLuint ()
picture fd tex = do
  withTransform (Transform 10 1 0) $ draw $ do
    textured tex $ do
      tri (V2 0 0, V2 0 0) (V2 0 100, V2 0 1) (V2 100 100, V2 1 1)
      bez (V2 20 20, V2 0 0) (V2 120 0, V2 1 0) (V2 50 50, V2 1 1)
    colored $ tri (V2 200 200, red) (V2 220 200, green) (V2 220 240, blue)
    polylines [ StrokeWidth 8
              , StrokeFeather 1
              , StrokeCaps (LineCapNone,LineCapNone)
              ] $ do
      lineStart (V2 200 200, red) $ do lineTo (V2 200 300, orange)
                                       lineTo (V2 300 300, purple)
      lineStart (V2 250 100, cyan) $ do curveTo (V2 300 120, yellow)
                                                (V2 250 140, white)
                                        lineTo (V2 300 280, red)
                                        lineTo (V2 400 140, magenta)
                                        curveTo (V2 420 160, purple)
                                                (V2 400 200, blue)

  withTransform (Transform  (V2 0 300) 1 0) $ draw $ letters $
    stroked (Name 0) [StrokeWidth 3, StrokeFeather 1] fd 128 64 "Hi there." $
      const white
  withTransform (Transform (V2 0 364) 1 0) $ draw $ letters $
    filled (Name 0) fd 128 64 "Hey there!" $
      FillTexture tex $ \(V2 x y) -> V2 (x/100) (y/64)
  withTransform (Transform (V2 0 428) 1 0) $ draw $ letters $
    filled (Name 0) fd 128 64 "o/ Hi all." $
      solid grey

isQuit :: Event -> Bool
isQuit (Event _ payload) = isKeyQ payload || payload == QuitEvent
  where
    isKeyQ (KeyboardEvent (KeyboardEventData _ _ _ (Keysym _ KeycodeQ _))) = True
    isKeyQ _ = False

main :: IO ()
main = do
    font <- case fontyData <$> decodeFont (fromStrict ttfFont) of
                    Left err -> putStrLn err >> exitFailure
                    Right f  -> return f
    cwd <- getCurrentDirectory
    (rez, window) <- startupSDL2Backend 800 600 "gelatin-example" True
    Just (_, tex) <- loadImage $ cwd </> "assets" </> "tex.jpg"
    let pic = picture font tex
        loop cache = do threadDelay 100
                        events <- pollEvents
                        unless (any isQuit events) $
                          renderWithSDL2 window rez cache pic >>= loop
    loop mempty

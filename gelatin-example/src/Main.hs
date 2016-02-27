{-# LANGUAGE TemplateHaskell #-}

import Control.Concurrent     (threadDelay)
import Control.Monad          (when, forever)
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

makePicture :: FontData -> Picture ()
makePicture font = move 200 $ do
    let text = withFont font $ withFill (solid white) $
                   letters 128 64 "Hello world!"
        textSize = pictureSize text
        textCenter = pictureCenter text
    move textCenter $ withStroke [ StrokeWidth 4
                                 , StrokeFeather 1
                                 , StrokeFill $ FillColor $ \(V2 x y) ->
                                       V4 (abs x/100) (abs y/100) 1 1
                                 ] $ rectangle textSize
    withFill (FillTexture jpgTex $ \v -> (100 + v) / 200) $ circle 100
    text

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
    (rez, window) <- startupSDL2Backend 800 600 "gelatin-example" True
    (cleanup, render) <- compileRenderer rez (makePicture font)
    forever $ do events <- pollEvents
                 if any isQuit events
                   then do cleanup
                           exitSuccess
                   else do clearFrame rez
                           render mempty
                           updateWindowSDL2 window
                           threadDelay 100

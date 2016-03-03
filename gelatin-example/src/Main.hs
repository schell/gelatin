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

picture :: FilePath -> Picture ()
picture tex = move 10 $ do
  draw $ textured tex $ shapes $ do
    triangle (V2 0 0, V2 0 0) (V2 0 100, V2 0 1) (V2 100 100, V2 1 1)
    curve (V2 20 20, V2 0 0) (V2 120 0, V2 1 0) (V2 50 50, V2 1 1)
  draw $ colored $ shapes $
    triangle (V2 200 200, red) (V2 220 200, green) (V2 220 240, blue)
    --polylines $ do
    --  lineStart (V2 200 200, red) $ do lineTo (V2 200 300, orange)
    --                                   lineTo (V2 300 300, purple)
    --  lineStart (V2 250 100, cyan) $ curveTo (V2 300 150, yellow)
    --                                         (V2 300 300, white)

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
    (cleanup, render) <- compileRenderer rez $ picture $ cwd </> "assets" </> "tex.jpg"
    forever $ do events <- pollEvents
                 if any isQuit events
                   then do cleanup
                           exitSuccess
                   else do clearFrame rez
                           render mempty
                           updateWindowSDL2 window
                           threadDelay 100

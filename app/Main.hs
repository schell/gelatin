module Main where

import Gelatin.SDL2
import Data.Renderable
import Data.Bits
import Control.Concurrent
import Control.Monad
import Graphics.Text.TrueType
import System.Directory
import System.FilePath
import System.Exit

import Debug.Trace

picture :: FontData -> Picture ()
picture font = move 100 $ do
    let text = withFont font $ withFill (solid white) $
                   letters 128 64 "128 dpi, 64 point text"
        textSize = pictureSize text
        textCenter = pictureCenter text
    move textCenter $ withStroke [StrokeWidth 4, StrokeFeather 1
                                 ,StrokeFill $ FillColor $ \(V2 x y) ->
                                    V4 (abs x/100) (abs y/100) 1 1
                                 ] $ rectangle textSize
    withFill (FillTexture "img/tex.jpg" $ \v -> (100 + v) / 200) $ circle 100
    text

isQuit :: Keysym -> Bool
isQuit (Keysym (Scancode 20) (Keycode 113) m) = any ($ m)
    [ keyModifierLeftCtrl
    , keyModifierRightCtrl
    , keyModifierLeftGUI
    , keyModifierRightGUI
    ]
isQuit _ = False

handleEvent :: Event -> IO ()
handleEvent (Event _ (KeyboardEvent (KeyboardEventData _ m r k))) =
    when (isQuit k) exitSuccess
handleEvent _ = return ()

loop :: Picture () -> Rez -> Window -> Cache IO Transform -> IO ()
loop pic rez window cache = do
    pollEvents >>= mapM_ handleEvent
    newCache <- renderWithSDL2 window rez cache pic
    threadDelay 100
    loop pic rez window newCache

main :: IO ()
main = do
    let fontFile = "font" </> "Neuton-Regular.ttf"
    eneuton <- getCurrentDirectory >>= loadFont . (</> fontFile)
    neuton <- case eneuton of
        Left err -> do putStrLn err
                       exitFailure
        Right f -> return f
    (rez,window) <- startupSDL2Backend 800 600 "gelatin" True
    loop (picture neuton) rez window mempty

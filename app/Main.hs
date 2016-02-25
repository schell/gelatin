module Main where

import           Control.Concurrent (threadDelay)
import           Control.Monad      (when)
import           SDL                (EventPayload(QuitEvent))
import           System.Directory   (getCurrentDirectory)
import           System.FilePath    ((</>))
import           System.Exit        (exitSuccess, exitFailure)

import           Data.Renderable
import           Gelatin.SDL2


picture :: FontData -> Picture ()
picture font = move 200 $ do
    let text = withFont font $ withFill (solid white) $
                   letters 128 64 "Hello world!"
        textSize = pictureSize text
        textCenter = pictureCenter text
    move textCenter $ withStroke [ StrokeWidth 4
                                 , StrokeFeather 1
                                 , StrokeFill $ FillColor $ \(V2 x y) ->
                                       V4 (abs x/100) (abs y/100) 1 1
                                 ] $ rectangle textSize
    withFill (FillTexture "img/tex.jpg" $ \v -> (100 + v) / 200) $ circle 100
    text

handleEvent :: Event -> IO ()
handleEvent (Event _ payload) =
    when (isKeyQ payload || payload == QuitEvent) exitSuccess
  where
    isKeyQ (KeyboardEvent (KeyboardEventData _ _ _ (Keysym _ KeycodeQ _))) = True
    isKeyQ _ = False

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
    (rez, window) <- startupSDL2Backend 800 600 "gelatin" True
    loop (picture neuton) rez window mempty

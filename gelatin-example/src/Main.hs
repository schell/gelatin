{-# LANGUAGE LambdaCase #-}
import Control.Concurrent     (threadDelay)
import Control.Monad          (forM_, when, unless)
import qualified Data.Vector.Unboxed as V
import Data.ByteString        (ByteString)
import Data.ByteString.Lazy   (fromStrict)
import Graphics.Text.TrueType (loadFontFile)
import System.Directory       (getCurrentDirectory)
import System.FilePath        ((</>))
import System.Exit            (exitSuccess, exitFailure)
import SDL
import Data.Renderable
import Gelatin.SDL2
import Paths_gelatin_example

picture :: FontData -> GLuint -> Picture GLuint ()
picture fd tex = do
  let example = draw $ textured tex $ do
        tri (V2 0 0, V2 0 0) (V2 0 100, V2 0 1) (V2 100 100, V2 1 1)
        bez (V2 20 20, V2 0 0) (V2 120 0, V2 1 0) (V2 50 50, V2 1 1)

  move 10 $ do
    forM_ [0..10] $ \i -> move (V2 (20*i) 0) $ withAlpha (i/10) example

    move 50  $ withMult red   example
    move 100 $ withMult green example
    move 150 $ withMult blue  example

    draw $ do
      colored $ triList [((V2 200 200, red), (V2 220 200, green), (V2 220 240, blue))]

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

  move (V2 0 300) $ draw $ letters $
    stroked [StrokeWidth 3, StrokeFeather 1] fd 128 64 "Hi there." (Uid 0) $
      const white
  move (V2 0 364) $ draw $ letters $
    filled fd 128 64 "Hey there!" $
      FillTexture (Uid 0) tex $ \(V2 x y) -> V2 (x/100) (y/64)
  move (V2 0 428) $ draw $ letters $
    filled fd 128 64 "o/ Hi all." $
      solid grey

isQuit :: Event -> Bool
isQuit (Event _ payload) = isKeyQ payload || payload == QuitEvent
  where
    isKeyQ (KeyboardEvent (KeyboardEventData _ _ _ (Keysym _ KeycodeQ _))) = True
    isKeyQ _ = False

main :: IO ()
main = do
    ttfName <- getDataFileName $ "assets" </> "Neuton-Regular.ttf"
    font <- loadFontFile ttfName >>= \case
              Left err -> putStrLn err >> exitFailure
              Right f  -> return $ fontyData f
    let (bs,ts) = fontStringGeom font 72 64 "FontData"
    imgName <- getDataFileName $ "assets" </> "tex.jpg"
    (rez, window) <- startupSDL2Backend 800 600 "gelatin-example" True
    Just (_, tex) <- loadImage imgName
    let pic = picture font tex
        loop cache = do threadDelay 100
                        events <- pollEvents
                        unless (any isQuit events) $
                          renderWithSDL2 window rez cache pic >>= loop
    loop mempty

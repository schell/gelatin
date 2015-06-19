module Main where

import Linear
import System.Exit
import Gelatin.Core.Render
import Gelatin.Core.Color
import Graphics.UI.GLFW
import Graphics.GL.Core33
import Control.Concurrent (threadDelay)
import Data.IORef
import Data.Bits

main :: IO ()
main = do
    True <- initGelatin
    win  <- newWindow 300 300 "Syndeca Mapper" Nothing Nothing

    grs <- loadGeomRenderSource
    brs <- loadBezRenderSource

    glEnable GL_BLEND
    glBlendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA

    ref <- newIORef []
    let input i = modifyIORef ref (++ [i])

    --setCharCallback win $ Just $ \_ c -> input $
    --    CharEvent c

    --setWindowSizeCallback win $ Just $ \_ w' h' -> input $
    --    WindowSizeEvent w' h'

    --setKeyCallback win $ Just $ \_ k i ks modi -> input $
    --    KeyEvent k i ks modi

    --setMouseButtonCallback win $ Just $ \_ mb mbs modi -> input $
    --    MouseButtonEvent mb mbs modi

    --setCursorPosCallback win $ Just $ \_ x y -> input $
    --    CursorMoveEvent x y

    --setCursorEnterCallback win $ Just $ \_ cs -> input $
    --    CursorEnterEvent cs

    --setScrollCallback win $ Just $ \_ x y -> input $
    --    ScrollEvent x y

    --setDropCallback win $ Just $ \_ fs -> do
    --    putStrLn $ "Got files:\n" ++ unlines fs
    --    input $ FileDropEvent fs

    --cursor <- createStandardCursor StandardCursorShape'Hand
    --setCursor win cursor

    afc  <- compileFontCache
    box  <- colorRenderer win grs GL_TRIANGLES [V2 0 0, V2 100 0, V2 100 50] $ replicate 3 red
    let t = Triangle (V2 0 0) (V2 100 0) (V2 100 50)
        -- A solid green fill
        greenFill = FillColor $ const green
        -- A yellow to white gradient fill
        gradFill  = FillColor $ \(V2 _ y) -> V4 1 1 (y/200) 1
    tris <- filledTriangleRenderer win grs [t
                                           ,(V2 0 50 +)  <$> t
                                           ,(V2 0 100 +) <$> t
                                           ,(V2 0 150 +) <$> t
                                           ,(V2 0 200 +) <$> t
                                           ] gradFill

    testTris <- filledTriangleRenderer win grs test greenFill

    let loop = do -- Update input events.
                  --es <- lift $ readIORef ref
                  --lift $ writeIORef ref []
                  (fbw,fbh) <- getFramebufferSize win
                  glViewport 0 0 (fromIntegral fbw) (fromIntegral fbh)
                  glClear $ GL_COLOR_BUFFER_BIT .|. GL_DEPTH_BUFFER_BIT

                  mapM_ (uncurry rRender) [ (box, translate 25 25 mempty)
                                          , (tris, mempty)
                                          , (testTris, mempty)]

                  pollEvents
                  swapBuffers win
                  shouldClose <- windowShouldClose win
                  if shouldClose
                  then exitSuccess
                  else threadDelay 100
                  loop
    loop

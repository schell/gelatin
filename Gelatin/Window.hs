{-# LANGUAGE TemplateHaskell #-}
module Gelatin.Window (
    -- * Creating a window
    initWindow,
    -- * Input
    InputEvent(..),
    InputEnv(..),
    ienvEventsLens,
    ienvCursorOnScreenLens,
    ienvLastCursorPosLens,
    ienvKeysDownLens,
    ienvMouseButtonsDownLens,
    ienvWindowSizeLens,
    module GLFW,
    -- * Processing input events
    foldInput
) where

import Graphics.UI.GLFW as GLFW
import Control.Concurrent
import Linear
import Control.Lens
import System.IO
import qualified Data.Set as S

-- TODO: Use SDL2 as another backend and abstract this stuff out.

--------------------------------------------------------------------------------
-- Types
--------------------------------------------------------------------------------

data InputEvent = NoInputEvent
                | CharEvent Char
                | WindowSizeEvent Int Int
                | KeyEvent Key Int KeyState ModifierKeys
                -- ^ Key, scancode, pressed/released, mods
                | MouseButtonEvent MouseButton MouseButtonState ModifierKeys
                | CursorMoveEvent Double Double
                | CursorEnterEvent CursorState
                | ScrollEvent Double Double
                deriving (Show, Eq, Ord)

data InputEnv = InputEnv { ienvEvents           :: [InputEvent]
                         , ienvCursorOnScreen   :: Bool
                         , ienvLastCursorPos    :: (Double, Double)
                         , ienvKeysDown         :: S.Set Key
                         , ienvMouseButtonsDown :: S.Set MouseButton
                         , ienvWindowSize       :: V2 Int
                         } deriving (Show)
makeLensesFor [("ienvEvents", "ienvEventsLens")
              ,("ienvCursorOnScreen", "ienvCursorOnScreenLens")
              ,("ienvLastCursorPos", "ienvLastCursorPosLens")
              ,("ienvKeysDown", "ienvKeysDownLens")
              ,("ienvMouseButtonsDown", "ienvMouseButtonsDownLens")
              ,("ienvWindowSize", "ienvWindowSizeLens")
              ] ''InputEnv

type WindowVar = MVar ([InputEvent], Window)

--------------------------------------------------------------------------------
-- Creating a window.
--------------------------------------------------------------------------------

-- | @initWindow pos size title@ creates and opens a new window stored in an
-- mvar. The window will be positioned at `pos` with size `size`.
-- The mvar is necessary for accumulating certain events that occur in
-- callbacks.
initWindow :: V2 Int -> V2 Int -> String -> IO WindowVar
initWindow (V2 x y) (V2 w h) title = do
    setErrorCallback $ Just $ \_ -> hPutStrLn stderr
    True <- GLFW.init
    defaultWindowHints
    windowHint $ GLFW.WindowHint'OpenGLDebugContext True
    windowHint $ GLFW.WindowHint'DepthBits 16
    makeNewWindow (x,y) (w,h) title

-- | Creates a new window. Fails and crashes if no window can be created.
makeNewWindow :: (Int,Int) -> (Int,Int) -> String -> IO WindowVar
makeNewWindow pos size title = do
    Just win <- uncurry createWindow size title Nothing Nothing
    makeContextCurrent $ Just win
    (uncurry $ setWindowPos win) pos

    let (w, h) = over both fromIntegral size
    mvar <- newMVar ([WindowSizeEvent w h, WindowSizeEvent w h], win)

    setCharCallback win $ Just $ \_ c ->
        input mvar $ CharEvent c

    setWindowSizeCallback win $ Just $ \_ w' h' -> do
        input mvar $ WindowSizeEvent w' h'

    setKeyCallback win $ Just $ \_ k i ks modi ->
        input mvar $ KeyEvent k i ks modi

    setMouseButtonCallback win $ Just $ \_ mb mbs modi ->
        input mvar $ MouseButtonEvent mb mbs modi

    setCursorPosCallback win $ Just $ \_ x y ->
        input mvar $ CursorMoveEvent x y

    setCursorEnterCallback win $ Just $ \_ cs ->
        input mvar $ CursorEnterEvent cs

    setScrollCallback win $ Just $ \_ x y ->
        input mvar $ ScrollEvent x y

    return mvar

-- | Inject some input into a WindowVar.
input :: WindowVar -> InputEvent -> IO ()
input mvar e@(WindowSizeEvent _ _) = do
    (es, w) <- takeMVar mvar
    let es' = filter noWindowEvs es
        noWindowEvs (WindowSizeEvent _ _) = False
        noWindowEvs _                     = True
    putMVar mvar (es' ++ [e], w)
input mvar e = do
    (es, w) <- takeMVar mvar
    putMVar mvar (es ++ [e], w)

--------------------------------------------------------------------------------
-- Processing input events.
--------------------------------------------------------------------------------

-- | Take an input event and fold it into an input environment variable.
foldInput :: InputEnv -> InputEvent -> InputEnv
foldInput ienv e@(CursorMoveEvent x y) =
    ienv & ienvLastCursorPosLens .~ (x,y)
         & ienvEventsLens %~ (++ [e])
foldInput ienv e@(CursorEnterEvent cs) =
    ienv & ienvCursorOnScreenLens .~ (cs == CursorState'InWindow)
         & ienvEventsLens %~ (++ [e])
foldInput ienv e@(MouseButtonEvent mb MouseButtonState'Pressed _) =
    ienv & (ienvMouseButtonsDownLens %~ S.insert mb)
         & ienvEventsLens %~ (++ [e])
foldInput ienv e@(MouseButtonEvent mb MouseButtonState'Released _) =
    ienv & (ienvMouseButtonsDownLens %~ S.delete mb)
         & ienvEventsLens %~ (++ [e])
foldInput ienv e@(WindowSizeEvent w h) =
    ienv & (ienvWindowSizeLens .~ V2 w h)
         & ienvEventsLens %~ (++ [e])
foldInput ienv e = ienv & ienvEventsLens %~ (++ [e])


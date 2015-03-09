{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TupleSections #-}
module Gelatin.Window (
    -- * Creating a window
    WindowRef,
    getWindow,
    getNewEvents,
    unWindowRef,
    initWindow,
    -- * Input
    emptyInputEnv,
    InputEvent(..),
    InputEnv(..),
    ienvEventsLens,
    ienvCursorOnScreenLens,
    ienvLastCursorPosLens,
    ienvKeysDownLens,
    ienvMouseButtonsDownLens,
    ienvWindowSizeLens,
    JoystickInput(..),
    getJoysticks,
    getJoystickInput,
    module GLFW,
    -- * Processing input events
    foldInput,
    clearEvents,
    -- * Querying input events
    isLeftMouseDown,
    isLeftMouseUp
) where

import Graphics.UI.GLFW as GLFW
import Data.IORef
import Data.Typeable
import Data.Maybe
import Linear
import Control.Monad
import Control.Lens
import Control.Applicative
import System.IO
import qualified Data.Set as S
import qualified Data.Map.Strict as M

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

data JoystickInput = JoystickInput { jiJoystick :: Joystick
                                   , jiName     :: String
                                   , jiButtons  :: [JoystickButtonState]
                                   , jiAxes     :: [Double]
                                   } deriving (Show, Typeable)

data InputEnv = InputEnv { ienvEvents           :: [InputEvent]
                         , ienvCursorOnScreen   :: Bool
                         , ienvLastCursorPos    :: (Double, Double)
                         , ienvKeysDown         :: S.Set Key
                         , ienvMouseButtonsDown :: S.Set MouseButton
                         , ienvWindowSize       :: V2 Int
                         } deriving (Show, Typeable)
makeLensesFor [("ienvEvents", "ienvEventsLens")
              ,("ienvCursorOnScreen", "ienvCursorOnScreenLens")
              ,("ienvLastCursorPos", "ienvLastCursorPosLens")
              ,("ienvKeysDown", "ienvKeysDownLens")
              ,("ienvMouseButtonsDown", "ienvMouseButtonsDownLens")
              ,("ienvWindowSize", "ienvWindowSizeLens")
              ] ''InputEnv

newtype WindowRef = WindowRef (IORef ([InputEvent], Window)) deriving (Typeable)
--------------------------------------------------------------------------------
-- Getting things from the window.
--------------------------------------------------------------------------------
unWindowRef :: WindowRef -> IORef ([InputEvent], Window)
unWindowRef (WindowRef ioref) = ioref

getWindow :: WindowRef -> IO Window
getWindow wref = fmap snd $ readIORef $ unWindowRef wref

getNewEvents :: WindowRef -> IO [InputEvent]
getNewEvents (WindowRef ioref) = do
    (events, window) <- readIORef ioref
    writeIORef ioref ([], window)
    return events

getJoysticks :: IO (M.Map Joystick JoystickInput)
getJoysticks = M.fromList . map toTuple . catMaybes <$> forM allJoysticks getJoystickInput
    where toTuple ji = (jiJoystick ji, ji)

getJoystickInput :: Joystick -> IO (Maybe JoystickInput)
getJoystickInput js = do
    mname    <- getJoystickName' js
    mbuttons <- getJoystickButtons js
    maxes    <- getJoystickAxes js
    return $ do name    <- mname
                buttons <- mbuttons
                axes    <- maxes
                return $ JoystickInput js name buttons axes

allJoysticks :: [Joystick]
allJoysticks = [Joystick'1 .. Joystick'16]

getJoystickName' :: Joystick -> IO (Maybe String)
getJoystickName' js = f <$> getJoystickName js
    where f mn = case mn of
                     Just "" -> Nothing
                     _       -> mn


getAllJoystickNames :: IO [(Joystick, String)]
getAllJoystickNames = do
    let
    mNames <- forM allJoysticks $ \js -> ((js,) <$>) <$> getJoystickName' js
    return $ catMaybes mNames

--------------------------------------------------------------------------------
-- Query input events.
--------------------------------------------------------------------------------
isLeftMouseDown :: InputEnv -> Bool
isLeftMouseDown = S.member MouseButton'1 . ienvMouseButtonsDown

isLeftMouseUp :: InputEnv -> Bool
isLeftMouseUp = not . null . filter hasMouseUp . ienvEvents
    where hasMouseUp (MouseButtonEvent MouseButton'1 MouseButtonState'Released _) = True
          hasMouseUp _ = False
--------------------------------------------------------------------------------
-- Processing input events.
--------------------------------------------------------------------------------
emptyInputEnv :: InputEnv
emptyInputEnv = InputEnv [] False (0,0) S.empty S.empty (V2 0 0)

-- | Clear out all input events.
clearEvents :: InputEnv -> InputEnv
clearEvents = (& ienvEventsLens .~ [])

-- | Take an input event and fold it into an input environment variable.
foldInput :: InputEnv -> InputEvent -> InputEnv
foldInput ienv e@(KeyEvent k _ KeyState'Pressed _) =
    ienv & ienvKeysDownLens %~ S.insert k
         & ienvEventsLens %~ (e:)
foldInput ienv e@(KeyEvent k _ KeyState'Released _) =
    ienv & ienvKeysDownLens %~ S.delete k
         & ienvEventsLens %~ (e:)
foldInput ienv e@(CursorMoveEvent x y) =
    ienv & ienvLastCursorPosLens .~ (x,y)
         & ienvEventsLens %~ (e:)
foldInput ienv e@(CursorEnterEvent cs) =
    ienv & ienvCursorOnScreenLens .~ (cs == CursorState'InWindow)
         & ienvEventsLens %~ (e:)
foldInput ienv e@(MouseButtonEvent mb MouseButtonState'Pressed _) =
    ienv & (ienvMouseButtonsDownLens %~ S.insert mb)
         & ienvEventsLens %~ (e:)
foldInput ienv e@(MouseButtonEvent mb MouseButtonState'Released _) =
    ienv & (ienvMouseButtonsDownLens %~ S.delete mb)
         & ienvEventsLens %~ (e:)
foldInput ienv e@(WindowSizeEvent w h) =
    ienv & (ienvWindowSizeLens .~ V2 w h)
         & ienvEventsLens %~ (e:)
foldInput ienv e = ienv & ienvEventsLens %~ (e:)
--------------------------------------------------------------------------------
-- Creating a window.
--------------------------------------------------------------------------------
-- | @initWindow pos size title@ creates and opens a new window stored in an
-- ref. The window will be positioned at `pos` with size `size`.
-- The ref is necessary for accumulating certain events that occur in
-- callbacks.
initWindow :: V2 Int -> V2 Int -> String -> IO WindowRef
initWindow (V2 x y) (V2 w h) title = do
    setErrorCallback $ Just $ \_ -> hPutStrLn stderr
    True <- GLFW.init
    defaultWindowHints
    windowHint $ GLFW.WindowHint'OpenGLDebugContext True
    windowHint $ GLFW.WindowHint'DepthBits 16
    makeNewWindow (x,y) (w,h) title

-- | Creates a new window. Fails and crashes if no window can be created.
makeNewWindow :: (Int,Int) -> (Int,Int) -> String -> IO WindowRef
makeNewWindow pos size title = do
    Just win <- uncurry createWindow size title Nothing Nothing
    makeContextCurrent $ Just win
    (uncurry $ setWindowPos win) pos

    let (w, h) = over both fromIntegral size
    ref <- newIORef $ ([WindowSizeEvent w h, WindowSizeEvent w h], win)

    setCharCallback win $ Just $ \_ c ->
        input ref $ CharEvent c

    setWindowSizeCallback win $ Just $ \_ w' h' -> do
        input ref $ WindowSizeEvent w' h'

    setKeyCallback win $ Just $ \_ k i ks modi ->
        input ref $ KeyEvent k i ks modi

    setMouseButtonCallback win $ Just $ \_ mb mbs modi ->
        input ref $ MouseButtonEvent mb mbs modi

    setCursorPosCallback win $ Just $ \_ x y ->
        input ref $ CursorMoveEvent x y

    setCursorEnterCallback win $ Just $ \_ cs ->
        input ref $ CursorEnterEvent cs

    setScrollCallback win $ Just $ \_ x y ->
        input ref $ ScrollEvent x y

    return $ WindowRef ref

-- | Inject some input into a WindowRef.
input :: IORef ([InputEvent], Window) -> InputEvent -> IO ()
input ref e = modifyIORef' ref (\(es,w) -> (e:es,w))

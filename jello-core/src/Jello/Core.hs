{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
module Jello.Core (
    ReadData(..),
    ModKeys(..),
    InputEvent(..),
    keyIsPressed,
    deleteKey,
    backspaceKey,
    tabKey,
    enterKey,
    Workspace(..),
    runWorkspace,
    -- * Re-exports
    module Picture,
    module Renderable,
    module Varying,
    module RWS
) where

import Gelatin.Picture as Picture
import Data.Renderable as Renderable
import Control.Varying as Varying
import Control.Monad.Trans.RWS.Strict as RWS

-- | ReadData is a type that stores resources and certain values pertaining to
-- user input. An event stream generally has to wait for an event to occur
-- before subsequent streams can take on a real value, which means
-- placeholder values must be used until such an event. Many times using a
-- placeholder value results in a signal that is less than ideal. By storing
-- certain important user input events in a read-only structure, downstream
-- signals can simply query the value instead of folding an event stream.
--
-- For example, using this method we can run a signal that relies on the cursor
-- position and query the cursor position value the first time the signal
-- is evaluated, without having to wait for the user to move the cursor.
data ReadData a = ReadData { readCursorPos :: V2 Float
                           , readWindowSize :: V2 Float
                           , readUserData :: a
                           }
--------------------------------------------------------------------------------
-- User Input
--------------------------------------------------------------------------------
-- | Modifier keys
data ModKeys = ModKeys { modShift :: Bool
                       , modCtrl  :: Bool
                       , modAlt   :: Bool
                       , modSuper :: Bool
                       } deriving (Show, Eq, Ord)

-- | User input events.
data InputEvent where
    NoInputEvent :: InputEvent
    CharEvent    :: Char -> InputEvent
    WindowSizeEvent :: Int -- ^ window width
                    -> Int -- ^ window height
                    -> InputEvent
    KeyEvent :: Int -- ^ key
             -> Int -- ^ scancode
             -> Int -- ^ pressed,released,repeating
             -> ModKeys -- ^ modifiers applied
             -> InputEvent
    MouseButtonEvent :: Int -- ^ mouse button number
                     -> Bool -- ^ is mouse button pressed
                     -> ModKeys -- ^ modifiers applied
                     -> InputEvent
    CursorMoveEvent :: Double -- ^ cursor x
                    -> Double -- ^ cursor y
                    -> InputEvent
    CursorEnterEvent :: Bool -- ^ is cursor in window
                     -> InputEvent
    ScrollEvent :: Double -- ^ scroll x
                -> Double -- ^ scroll y
                -> InputEvent
    FileDropEvent :: [String] -- ^ filepaths
                  -> InputEvent
                deriving (Show, Eq, Ord)

instance Monoid InputEvent where
    mempty = NoInputEvent
    mappend NoInputEvent e = e
    mappend e _ = e

-- | Returns true if the key state is considered "pressed".
keyIsPressed :: Int -> Bool
keyIsPressed 0 = True
keyIsPressed 2 = True
keyIsPressed _ = False

deleteKey :: Int
deleteKey = 56

backspaceKey :: Int
backspaceKey = 54

tabKey :: Int
tabKey = 53

enterKey :: Int
enterKey = 52
--------------------------------------------------------------------------------
-- Rendering pictures and iterating pictures
--------------------------------------------------------------------------------
-- | A workspace bundles up everything we need to run an application
-- that consists of a 'Picture' changing over time.
data Workspace m f z t s w u =
    Workspace { wsRenderPicture :: z -> Cache m t -> Picture f () -> m (z, Cache m t)
              -- ^ Rendering a Picture using a resource and rendering cache
              , wsGetInput :: m [InputEvent]
              -- ^ Get the next sequence of input events
              , wsWriteOutput :: w -> m ()
              -- ^ Write output emmitted by our control splines
              , wsUpdateUserData :: u -> m u
              -- ^ Effectfully update a custom user data structure
              , wsCache :: Cache m t
              -- ^ A cache of renderers
              , wsResource :: z
              -- ^ A resource used to generate new renderers
              , wsReadData :: ReadData u
              -- ^ A read-only data structure for user input (and more)
              , wsStateData :: s
              -- ^ The state data emmitted by our control structures
              }

-- | Run a control spline (our program) using a workspace in an infinite loop.
runWorkspace :: (Monad m, Monoid w)
             => Spline InputEvent (Picture f ()) (RWST (ReadData u) w s m) ()
             -> Workspace m f r t s w u
             -> m ()
runWorkspace net ws = do
    -- get our events from the workspace and update our read data
    events <- wsGetInput ws
    let readData = foldl addInputEvent (wsReadData ws) events
        addInputEvent input (CursorMoveEvent x y) =
            let v = realToFrac <$> V2 x y in input{ readCursorPos = v }
        addInputEvent input _ = input
    -- updated our custom user data
    userData <- wsUpdateUserData ws $ readUserData readData
    let newReadData = readData{ readUserData = userData }
    -- extract the state data and get ready to run this frame's graph
    let stateData = wsStateData ws
        rwst = stepMany events $ runSplineT net
    -- run the graph to get the current output, next graph, state and write
    -- data
    ((Step (Event ui) _, nextNet),newState,writeData) <- runRWST rwst
                                                                 newReadData
                                                                 stateData
    -- write some data out
    wsWriteOutput ws writeData
    -- render the frame
    let rez = wsResource ws
        csh = wsCache ws
    (newRez,newCsh) <- wsRenderPicture ws rez csh ui
    -- loop with new values for resource, read, write, state data and
    -- rendering cache
    let newWs = ws{ wsResource = newRez
                  , wsReadData = newReadData
                  , wsStateData = newState
                  , wsCache = newCsh
                  }
    runWorkspace (SplineT nextNet) newWs

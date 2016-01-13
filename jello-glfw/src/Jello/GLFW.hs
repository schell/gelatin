module Jello.GLFW (
    GLFWWorkspace,
    GLCache,
    GLPic,
    renderFrame,
    getGLFWInput,
    glfwWorkspace,
    -- * Re-exports
    module G,
    module J
) where

import Data.Bits
import Data.IORef
import Gelatin.GLFW as G
import Jello.Core as J
import System.Exit
import Control.Concurrent

type GLFWWorkspace = Workspace IO Font Rez Transform () () ()
type GLCache = Cache IO Transform
type GLPic = Picture Font ()

renderFrame :: Rez -> GLCache -> GLPic -> IO (Rez, GLCache)
renderFrame rz old pic = do
    (fbw,fbh) <- getFramebufferSize $ rezWindow rz
    glViewport 0 0 (fromIntegral fbw) (fromIntegral fbh)
    glClear $ GL_COLOR_BUFFER_BIT .|. GL_DEPTH_BUFFER_BIT

    new <- renderPrimitives rz old $ pictureToR2Primitives pic

    pollEvents
    swapBuffers $ rezWindow rz
    shouldClose <- windowShouldClose $ rezWindow rz
    if shouldClose
    then exitSuccess
    else threadDelay 100
    return (rz, new)

getGLFWInput :: IORef [InputEvent] -> IO [InputEvent]
getGLFWInput ioref = do
    -- Read accumulated events this frame
    events <- readIORef ioref
    -- Clear input events.
    writeIORef ioref []
    return events

glfwWorkspace :: IO GLFWWorkspace
glfwWorkspace = do
    rez <- startupGLFWBackend 800 600 "Odin" Nothing Nothing
    let w = rezWindow rez
    setWindowPos w 400 400

    glEnable GL_BLEND
    glBlendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA

    ref <- newIORef []
    let input i = modifyIORef ref (++ [i])

    setCharCallback w $ Just $ \_ c -> input $
        CharEvent c

    setWindowSizeCallback w $ Just $ \_ w' h' -> input $
        WindowSizeEvent w' h'

    let toMod (ModifierKeys sh c a su) = ModKeys sh c a su

    setKeyCallback w $ Just $ \_ k i ks modi -> input $
        KeyEvent (fromEnum k) i (fromEnum ks) $ toMod modi

    setMouseButtonCallback w $ Just $ \_ mb mbs modi -> input $
        MouseButtonEvent (fromEnum mb) (fromEnum mbs == 0) $ toMod modi

    setCursorPosCallback w $ Just $ \_ x y -> input $
        CursorMoveEvent x y

    setCursorEnterCallback w $ Just $ \_ cs -> input $
        CursorEnterEvent $ fromEnum cs == 0

    setScrollCallback w $ Just $ \_ x y -> input $
        ScrollEvent x y

    ---- Not till GLFW-b with 3.1 additions
    --setDropCallback w $ Just $ \_ fs -> do
    --    putStrLn $ "Got files:\n" ++ unlines fs
    --    input $ FileDropEvent fs

    let i = ReadData { readCursorPos = 0
                     , readWindowSize = V2 800 600
                     , readUserData = ()
                     }
    return Workspace { wsRenderPicture = renderFrame
                     , wsGetInput = getGLFWInput ref
                     , wsWriteOutput = const $ return ()
                     , wsUpdateUserData = return
                     , wsCache = mempty
                     , wsResource = rez
                     , wsReadData = i
                     , wsStateData = ()
                     }

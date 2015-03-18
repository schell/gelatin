{-# LANGUAGE FlexibleContexts #-}
module Render (
    module R,
    module GLFW,
    initResources,
    enableBlending,
    drawThing,
    drawClear,
    withNewFrame,
    gets
) where

import Shader
import Render.Types as R
import Render.Font as R
import Triangulation.KET
import Linear
import Graphics.GL.Core33
import Graphics.GL.Types
import Graphics.UI.GLFW as GLFW
import Graphics.Text.TrueType
import Foreign.Marshal.Array
import Foreign.Marshal.Utils
import Foreign.C.String
import Foreign.Storable
import Foreign.Ptr
import Data.Bits
import Data.Maybe
import Data.Monoid
import Data.Time.Clock
import Data.Typeable
import Control.Applicative
import Control.Concurrent
import Control.Concurrent.Async
import Control.Eff
import Control.Eff.Lift
import Control.Eff.State.Strict
import Control.Monad
import System.Directory
import System.IO
import System.Exit
import qualified Data.Foldable as F
import qualified Data.IntMap as IM
import qualified Data.Map as M
import qualified Data.ByteString.Char8 as B

-- | Starts up the renderer by creating a window and returning some default
-- values for resources.
initResources :: Int -> Int -> String -> IO Resources
initResources ww wh ws = do
    setErrorCallback $ Just $ \_ -> hPutStrLn stderr
    True <- GLFW.init
    defaultWindowHints
    windowHint $ WindowHint'OpenGLDebugContext True
    windowHint $ WindowHint'OpenGLProfile OpenGLProfile'Core
    windowHint $ WindowHint'OpenGLForwardCompat True
    windowHint $ WindowHint'ContextVersionMajor 3
    windowHint $ WindowHint'ContextVersionMinor 2
    windowHint $ WindowHint'DepthBits 16
    mwin <- createWindow ww wh ws Nothing Nothing
    makeContextCurrent mwin
    window <- case mwin of
                  Nothing  -> do putStrLn "could not create window"
                                 exitFailure
                  Just win -> return win
    resources window

gets :: (Typeable a, Member (State a) r) => (a -> b) -> Eff r b
gets f = f <$> get

-- | Tries to fetch the Renderer from the resource's render cache or creates
-- the Renderer and stores it in the cache. Draws with the Renderer.
drawWithCache :: (Member (State Resources) r,
                  SetMember Lift (Lift IO) r,
                  Enum i)
              => i
              -- ^ An id for this rendering
              -> Eff r Renderer
              -- ^ A render computation that creates the Renderer if not
              -- cached.
              -> Transform
              -- ^ The transform of this rendering
              -> Eff r ()
              -- ^ Returns the updated resources
drawWithCache uid f tfrm = do
    (Resources fs rs ss w dpi t) <- get
    let i = fromEnum uid
    draw <- case IM.lookup i rs of
                Just r -> return r
                Nothing -> do r <- f
                              put $ Resources fs (IM.insert i r rs) ss w dpi t
                              return r
    lift $ render draw tfrm

textDraw :: (Member (State Resources) r,
             SetMember Lift (Lift IO) r)
         => V4 Float -> Font -> PointSize -> String -> Eff r Renderer
textDraw color font psize txt = do
    dpi <- gets rsrcDpi
    let cs = getStringCurveAtPoint dpi (0,0) [(font, psize, txt)]
        bs = beziers cs
        bs' = concat $ concat bs
        ts = concatMap (concatMap (concaveTriangles . onContourPoints)) bs
    newPolyRenderer color bs' ts

drawThing :: (Member (State Resources) r,
              SetMember Lift (Lift IO) r,
              Enum i)
          => i -> V4 Float -> Display -> Transform -> Eff r ()
drawThing uid color (DisplayTris ts) = drawWithCache uid $
    newTriRenderer color ts
drawThing uid color (DisplayPoly vs) = drawWithCache uid $
    newTriRenderer color $ triangulate vs
drawThing uid color (DisplayLine vs) = drawWithCache uid $
    newLineRenderer color $ toLines vs
drawThing uid color (DisplayArrows vs) = drawWithCache uid $
    newLineRenderer color $ toArrows vs
drawThing uid color (DisplayText desc px txt) = \t -> do
    dpi <- gets rsrcDpi
    let psize = pixelSizeInPointAtDpi px dpi
    withFont desc $ \font ->
        drawWithCache uid (textDraw color font psize txt) t
drawThing uid color (DisplayText' desc px txt) = \t -> do
    dpi <- gets rsrcDpi
    let psize = pixelSizeInPointAtDpi px dpi
    withFont desc $ \font ->
        flip (drawWithCache uid) t $ do
            txtr <- do let cs = getStringCurveAtPoint dpi (0,0) [(font, psize, txt)]
                           bs = beziers cs
                           bs' = concat $ concat bs
                           ts = concatMap (concatMap (concaveTriangles . onContourPoints)) bs
                       pr <- newPolyRenderer (V4 1 1 1 0.5) [] ts
                       br <- newPolyRenderer (V4 1 1 1 1) bs' []
                       return $ pr <> br
            let cs = getStringCurveAtPoint dpi (0,0) [(font, psize, txt)]
                bs = beziers cs
                as = concatMap (concatMap toArrows) $ map (map onContourPoints) bs
            linr <- newLineRenderer color as
            let r = txtr <> linr
            return r

glFloatSize :: Int
glFloatSize = sizeOf (undefined :: GLfloat)

newRenderSource :: (Member (State Resources) r,
                    SetMember Lift (Lift IO) r)
                => RenderDef -> Eff r RenderSource
newRenderSource rd = do
    (Resources fs rs ss w dpi t) <- get
    case M.lookup rd ss of
        Just r -> return r
        Nothing -> do
            r <- lift $ loadRenderSource rd
            put $ Resources fs rs (M.insert rd r ss) w dpi t
            return r

colorRenderSource :: (Member (State Resources) r,
                      SetMember Lift (Lift IO) r)
                  => Eff r (GLuint, GLint, GLint)
colorRenderSource = do
    let def = RenderDefBS [(vertSourceGeom, GL_VERTEX_SHADER)
                                ,(fragSourceGeom, GL_FRAGMENT_SHADER)]
                                ["projection", "modelview"]
    (RenderSource program locs) <- newRenderSource def
    let Just pjloc = lookup "projection" locs
        Just mvloc = lookup "modelview" locs
    return (program, pjloc, mvloc)

newPolyRenderer :: (Member (State Resources) r,
                    SetMember Lift (Lift IO) r)
                => V4 Float
                -> [Bezier Float]
                -> [Triangle Float]
                -> Eff r Renderer
newPolyRenderer color bs ts = do
    bezr <- newBezRenderer color bs
    trir <- newTriRenderer color ts
    let btr = bezr <> trir
        c = cleanup btr
        r t = do glClear GL_DEPTH_BUFFER_BIT
                 glEnable GL_STENCIL_TEST
                 glColorMask GL_FALSE GL_FALSE GL_FALSE GL_FALSE
                 glDepthMask GL_FALSE
                 glStencilMask 0xFF
                 glClear GL_STENCIL_BUFFER_BIT
                 glStencilFunc GL_NEVER 0 1
                 glStencilOp GL_INVERT GL_INVERT GL_INVERT
                 render btr t

                 glColorMask GL_TRUE GL_TRUE GL_TRUE GL_TRUE
                 glDepthMask GL_TRUE
                 glStencilFunc GL_EQUAL 1 1
                 glStencilOp GL_ZERO GL_ZERO GL_ZERO
                 render btr t
                 glDisable GL_STENCIL_TEST
    return $ Renderer r c

newBezRenderer :: (Member (State Resources) r,
                   SetMember Lift (Lift IO) r)
               => V4 Float -> [Bezier Float] -> Eff r Renderer
newBezRenderer color bs = do
    window <- gets rsrcWindow
    -- Create a new rendersource (compiled shader program) or retreive an
    -- existing one out of our RenderSources
    let def = RenderDefBS [(vertSourceBezier, GL_VERTEX_SHADER)
                                ,(fragSourceBezier, GL_FRAGMENT_SHADER)]
                                ["projection", "modelview"]
    (RenderSource program locs) <- newRenderSource def

    let Just pjloc = lookup "projection" locs
        Just mvloc = lookup "modelview" locs

    lift $ withVAO $ \vao -> withBuffers 3 $ \[pbuf, tbuf, cbuf] -> do
        let ps = concatMap F.toList $ concatMap (\(Bezier _ a b c) -> [a,b,c]) bs :: [GLfloat]
            cs = concatMap F.toList $ replicate (length ps) color :: [GLfloat]
            ts = concatMap (\w -> [0, 0, w, 0.5, 0, w, 1, 1, w]) $
                     map (fromBool . bezWoundClockwise) bs :: [GLfloat]
        bufferPositionAttribs pbuf ps
        bufferColorAttribs cbuf cs
        bufferBezAttribs tbuf ts
        glBindVertexArray 0

        let cleanupFunction = do
                withArray [pbuf, tbuf, cbuf] $ glDeleteBuffers 2
                withArray [vao] $ glDeleteVertexArrays 1
            num = fromIntegral $ length bs * 3
            renderFunction = drawBuffer window program pjloc mvloc vao GL_TRIANGLES num
        return $ Renderer renderFunction cleanupFunction


newTriRenderer :: (Member (State Resources) r,
                   SetMember Lift (Lift IO) r)
               => V4 Float -> [Triangle Float] -> Eff r Renderer
newTriRenderer color ts = do
    window <- gets rsrcWindow
    (program, pjloc, mvloc) <- colorRenderSource
    lift $ withVAO $ \vao -> withBuffers 2 $ \[pbuf,cbuf] -> do
        let ps = concatMap F.toList $ concatMap (\(Triangle a b c) -> [a,b,c]) ts :: [GLfloat]
            cs = concatMap F.toList $ replicate (length ps) color :: [GLfloat]
        bufferPositionAttribs pbuf ps
        bufferColorAttribs cbuf cs
        let num = fromIntegral $ length ts * 3
            renderFunction = drawBuffer window program pjloc mvloc vao GL_TRIANGLES num
            cleanupFunction = do
                withArray [pbuf, cbuf] $ glDeleteBuffers 2
                withArray [vao] $ glDeleteVertexArrays 1
        return $ Renderer renderFunction cleanupFunction

newLineRenderer :: (Member (State Resources) r,
                    SetMember Lift (Lift IO) r)
                => V4 Float -> [Line Float] -> Eff r Renderer
newLineRenderer color ls = do
    window <- gets rsrcWindow
    (program, pjloc, mvloc) <- colorRenderSource
    lift $ withVAO $ \vao -> withBuffers 2 $ \[pbuf,cbuf] -> do
        let ps = concatMap F.toList $ concatMap (\(Line a b) -> [a,b]) ls :: [GLfloat]
            cs = concatMap F.toList $ replicate (length ps) color :: [GLfloat]

        bufferPositionAttribs pbuf ps
        bufferColorAttribs cbuf cs

        glBindVertexArray 0

        let num = fromIntegral $ length ls * 2
            renderFunction = drawBuffer window program pjloc mvloc vao GL_LINES num
            cleanupFunction = do
                withArray [pbuf, cbuf] $ glDeleteBuffers 2
                withArray [vao] $ glDeleteVertexArrays 1

        return $ Renderer renderFunction cleanupFunction

enableBlending :: SetMember Lift (Lift IO) r => Eff r ()
enableBlending = lift $ do
    glEnable GL_BLEND
    glBlendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA

drawClear :: (Member (State Resources) r,
              SetMember Lift (Lift IO) r) => Eff r ()
drawClear = do
    window <- gets rsrcWindow
    lift $ do
        pollEvents
        (fbw, fbh) <- getFramebufferSize window
        glViewport 0 0 (fromIntegral fbw) (fromIntegral fbh)
        glClear $ GL_COLOR_BUFFER_BIT .|. GL_DEPTH_BUFFER_BIT

withNewFrame :: (Member (State Resources) r,
                 SetMember Lift (Lift IO) r)
             => Eff r () -> Eff r ()
withNewFrame f = do
    t <- lift $ do pollEvents
                   getCurrentTime
    modify $ \rsrcs -> rsrcs{rsrcUTC = t}
    f
    window <- gets rsrcWindow
    lift $ do
        swapBuffers window
        shouldClose <- windowShouldClose window
        if shouldClose
        then exitSuccess
        else threadDelay 100
--------------------------------------------------------------------------------
-- IO ops
--------------------------------------------------------------------------------
resources :: Window -> IO Resources
resources window = do
    mMonitor <- getPrimaryMonitor

    -- Calculate the dpi of the primary monitor.
    dpi <- case mMonitor of
                -- I've choosen 128 as the default DPI because of my macbook 15"
               Nothing -> return 128
               Just m  -> do (w, h) <- getMonitorPhysicalSize m
                             mvmode <- getVideoMode m
                             case mvmode of
                                 Nothing -> return 128
                                 Just (VideoMode vw vh _ _ _ _) -> do
                                     let mm2 = fromIntegral $ w*h :: Double
                                         px  = sqrt $ (fromIntegral vw :: Double)*(fromIntegral vh)
                                         inches = sqrt $ mm2 / (25.4 * 25.4)
                                     let dpi = floor $ px / inches
                                     putStrLn $ "Dpi: " ++ show dpi
                                     return dpi
    t <- getCurrentTime
    a <- async $ do
        putStrLn "Loading font cache."
        a <- buildCache
        putStrLn "Font cache loaded."
        return a
    return $ Resources a IM.empty M.empty window dpi t

loadRenderSource :: RenderDef -> IO RenderSource
loadRenderSource (RenderDefBS ss uniforms) = do
    shaders <- mapM (uncurry compileShader) ss
    program <- compileProgram shaders
    glUseProgram program
    locs <- forM uniforms $ \attr -> do
        loc <- withCString attr $ glGetUniformLocation program
        return $ if loc == (-1)
                 then Nothing
                 else Just (attr, loc)
    return $ RenderSource program $ catMaybes locs
loadRenderSource (RenderDefFP fps uniforms) = do
    cwd <- getCurrentDirectory
    srcs <- forM fps $ \(fp, shaderType) -> do
        src <- B.readFile $ cwd ++ "/" ++ fp
        return (src, shaderType)
    loadRenderSource $ RenderDefBS srcs uniforms


withVAO :: (GLuint -> IO b) -> IO b
withVAO f = do
    [vao] <- allocaArray 1 $ \ptr -> do
        glGenVertexArrays 1 ptr
        peekArray 1 ptr
    glBindVertexArray vao
    r <- f vao
    glBindVertexArray vao
    return r

withBuffers :: Int -> ([GLuint] -> IO b) -> IO b
withBuffers n f = do
    bufs <- allocaArray n $ \ptr -> do
        glGenBuffers (fromIntegral n) ptr
        peekArray (fromIntegral n) ptr
    f bufs

bufferAttrib :: Storable a => GLuint -> GLint -> GLuint -> [a] -> IO ()
bufferAttrib loc n buf as = do
    let asize = length as * glFloatSize
    glBindBuffer GL_ARRAY_BUFFER buf
    withArray as $ \ptr ->
        glBufferData GL_ARRAY_BUFFER (fromIntegral asize) (castPtr ptr) GL_STATIC_DRAW
    glEnableVertexAttribArray loc
    glVertexAttribPointer loc n GL_FLOAT GL_FALSE 0 nullPtr

bufferPositionAttribs :: Storable a => GLuint -> [a] -> IO ()
bufferPositionAttribs = bufferAttrib positionLoc 2

bufferColorAttribs :: Storable a => GLuint -> [a] -> IO ()
bufferColorAttribs = bufferAttrib colorLoc 4

bufferBezAttribs :: Storable a => GLuint -> [a] -> IO ()
bufferBezAttribs = bufferAttrib uvLoc 3

drawBuffer :: Window -> GLuint -> GLint -> GLint -> GLuint -> GLenum -> GLsizei -> Transform -> IO ()
drawBuffer window program pjloc mvloc vao mode num (Transform txy sxy rot) = do
    (pj,mv) <- getPJMV window txy sxy rot
    glUseProgram program
    with pj $ glUniformMatrix4fv pjloc 1 GL_TRUE . castPtr
    with mv $ glUniformMatrix4fv mvloc 1 GL_TRUE . castPtr
    glBindVertexArray vao
    glDrawArrays mode 0 num
    err <- glGetError
    when (err /= 0) $ putStrLn $ "Error: " ++ show err

getPJMV :: Window -> V2 GLfloat -> V2 GLfloat -> GLfloat -> IO (M44 GLfloat, M44 GLfloat)
getPJMV window (V2 x y) (V2 w h) r = do
    (ww, wh) <- getWindowSize window
    let (hw,hh) = (fromIntegral ww, fromIntegral wh)
        sxy = V3 w h 1
        txy = V3 x y 0
        rxy = V3 0 0 1
        rot = if r /= 0 then mat4Rotate r rxy else eye4
        pj  = ortho 0 hw hh 0 0 1 :: M44 GLfloat
        mv  = mat4Translate txy !*! rot !*! mat4Scale sxy :: M44 GLfloat
    return (pj,mv)

--------------------------------------------------------------------------------
-- Matrix helpers
--------------------------------------------------------------------------------
mat4Translate :: Num a => V3 a -> M44 a
mat4Translate = mkTransformationMat eye3

mat4Rotate :: (Num a, Epsilon a, Floating a) => a -> V3 a -> M44 a
mat4Rotate phi v = mkTransformation (axisAngle v phi) (V3 0 0 0)

mat4Scale :: Num a => V3 a -> M44 a
mat4Scale (V3 x y z) =
    V4 (V4 x 0 0 0)
       (V4 0 y 0 0)
       (V4 0 0 z 0)
       (V4 0 0 0 1)

--------------------------------------------------------------------------------
-- Geometric helpers
--------------------------------------------------------------------------------
toLines :: [V2 a] -> [Line a]
toLines (a:b:cs) = Line a b : toLines (b:cs)
toLines _ = []

toArrows :: Floating a => [V2 a] -> [Line a]
toArrows = concatMap toArrow . toLines
    where toArrow (Line a b) = [ Line a b
                               , Line (b - u*l + n * w) b
                               , Line (b - u*l + n * (-w)) b ]
            where n = signorm $ perp $ b - a
                  u = signorm $ b - a
                  l = 5 -- head length
                  w = 3 -- head width

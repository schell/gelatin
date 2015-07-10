{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Gelatin.Core.Rendering (
    module R,
    initGelatin,
    newWindow,
    loadGeomRenderSource,
    loadBezRenderSource,
    loadMaskRenderSource,
    loadRenderSource,
    loadTexture,
    loadTextureUnit,
    unloadTexture,
    loadImageAsTexture,
    filledTriangleRendering,
    colorRendering,
    colorBezRendering,
    colorFontRendering,
    textureRendering,
    textureUnitRendering,
    maskRendering,
    transformRendering,
    stencilMask,
    alphaMask,
    toTexture,
    toTextureUnit,
    calculateDpi
) where

import Gelatin.Core.Shader
import Gelatin.Core.Rendering.Types as R
import Gelatin.Core.Rendering.Polylines as R
import Gelatin.Core.Rendering.Geometrical as R
import Gelatin.Core.Rendering.Font as R
import Linear
import Graphics.Text.TrueType
import Graphics.GL.Core33
import Graphics.GL.Types
import Graphics.UI.GLFW as GLFW hiding (Image(..))
import Codec.Picture.Types
import Codec.Picture (readImage)
import Foreign.Marshal.Array
import Foreign.Marshal.Utils
import Foreign.C.String
import Foreign.Storable
import Foreign.Ptr
import Data.Monoid
import Data.Maybe
import Data.Vector.Storable (Vector,unsafeWith)
import Control.Monad
import System.Directory
import System.IO
import System.Exit
import qualified Data.ByteString.Char8 as B
import qualified Data.Foldable as F
import GHC.Stack

-- | Initializes the system.
initGelatin :: IO Bool
initGelatin = do
    setErrorCallback $ Just $ \_ -> hPutStrLn stderr
    GLFW.init

-- | Creates a window.
newWindow :: Int -- ^ Width
          -> Int -- ^ Height
          -> String -- ^ Title
          -> Maybe Monitor -- ^ The monitor to fullscreen into.
          -> Maybe Window -- ^ A window to share OpenGL contexts with.
          -> IO Window
newWindow ww wh ws mmon mwin = do
    defaultWindowHints
    windowHint $ WindowHint'OpenGLDebugContext True
    windowHint $ WindowHint'OpenGLProfile OpenGLProfile'Core
    windowHint $ WindowHint'OpenGLForwardCompat True
    windowHint $ WindowHint'ContextVersionMajor 3
    windowHint $ WindowHint'ContextVersionMinor 2
    windowHint $ WindowHint'DepthBits 16
    mwin' <- createWindow ww wh ws mmon mwin
    makeContextCurrent mwin'
    window <- case mwin' of
                  Nothing  -> do putStrLn "could not create window"
                                 exitFailure
                  Just win -> return win
    return window

--------------------------------------------------------------------------------
-- Renderings
--------------------------------------------------------------------------------
-- | Creates and returns a renderer that renders a given string of
-- triangles with the given filling.
filledTriangleRendering :: Window -> GeomRenderSource -> [Triangle (V2 Float)]
                       -> Fill -> IO Rendering
filledTriangleRendering win grs ts fill = do
    let vs = trisToComp ts
    mfr <- getFillResult fill vs
    case mfr of
        Just (FillResultColor cs) -> colorRendering win grs GL_TRIANGLES vs cs
        Just (FillResultTexture _ uvs) -> textureRendering win grs GL_TRIANGLES
                                                                  vs uvs
        _ -> do putStrLn "Could not create a filledTriangleRendering."
                return $ Rendering (const $ putStrLn "Non op renderer.") (return ())

getFillResult :: Fill -> [V2 Float] -> IO (Maybe FillResult)
getFillResult (FillColor f) vs = return $ Just $ FillResultColor $ map f vs
getFillResult (FillTexture fp f) vs = do
    mtex <- loadImageAsTexture fp
    return $ case mtex of
        Nothing  -> Nothing
        Just tex -> Just $ FillResultTexture tex $ map f vs

-- | TODO: textureFontRendering and then fontRendering.

-- | Creates and returns a renderer that renders a given FontString.
colorFontRendering :: Window -> GeomRenderSource -> BezRenderSource
                  -> FontString -> (V2 Float -> V4 Float) -> IO Rendering
colorFontRendering window grs brs fstr clrf = do
    dpi <- calculateDpi
    let (bs,ts) = fontGeom dpi fstr
        vs = concatMap (\(Triangle a b c) -> [a,b,c]) ts
        cs = map clrf vs
    Rendering fg cg <- colorRendering window grs GL_TRIANGLES vs cs

    let bcs = map ((\(Bezier _ a b c) -> Triangle a b c) . fmap clrf) bs
    Rendering fb cb <- colorBezRendering window brs bs bcs

    let fgb t = fg t >> fb t
        r t   = stencilMask (fgb t) (fgb t)
    return $ Rendering r (cg >> cb)

-- | Creates and returns a renderer that renders the given colored
-- geometry.
colorRendering :: Window -> GeomRenderSource -> GLuint -> [V2 Float]
              -> [V4 Float] -> IO Rendering
colorRendering window grs mode vs gs = do
    let (GRS src) = grs
        srcs = [src]

    withVAO $ \vao -> withBuffers 2 $ \[pbuf,cbuf] -> do
        let ps = map realToFrac $ concatMap F.toList vs :: [GLfloat]
            cs = map realToFrac $ concatMap F.toList $ take (length vs) gs :: [GLfloat]

        glDisableVertexAttribArray uvLoc
        glEnableVertexAttribArray colorLoc

        bufferAttrib positionLoc 2 pbuf ps
        bufferAttrib colorLoc 4 cbuf cs
        glBindVertexArray 0

        let num = fromIntegral $ length vs
            renderFunction t = do
                withUniform "hasUV" srcs $ \p huv -> do
                    glUseProgram p
                    glUniform1i huv 0
                withUniform "projection" srcs $ setOrthoWindowProjection window
                withUniform "modelview" srcs $ setModelview t
                drawBuffer (rsProgram src) vao mode num
            cleanupFunction = do
                withArray [pbuf, cbuf] $ glDeleteBuffers 2
                withArray [vao] $ glDeleteVertexArrays 1
        return $ Rendering renderFunction cleanupFunction

-- | Creates and returns a renderer that renders a textured
-- geometry.
textureRendering :: Window -> GeomRenderSource -> GLuint -> [V2 Float]
                -> [V2 Float] -> IO Rendering
textureRendering = textureUnitRendering Nothing

-- | Creates and returns a renderer that renders the given textured
-- geometry.
textureUnitRendering :: (Maybe GLint) -> Window -> GeomRenderSource -> GLuint
                    -> [V2 Float] -> [V2 Float] -> IO Rendering
textureUnitRendering Nothing w gs md vs uvs =
    textureUnitRendering (Just 0) w gs md vs uvs
textureUnitRendering (Just u) win grs mode vs uvs = do
    let (GRS src) = grs
        srcs = [src]

    withVAO $ \vao -> withBuffers 2 $ \[pbuf,cbuf] -> do
        let f xs = map realToFrac $ concatMap F.toList xs :: [GLfloat]
            ps = f vs
            cs = f $ take (length vs) uvs

        glDisableVertexAttribArray colorLoc
        glEnableVertexAttribArray uvLoc

        bufferAttrib positionLoc 2 pbuf ps
        bufferAttrib uvLoc 2 cbuf cs
        glBindVertexArray 0

        let num = fromIntegral $ length vs
            renderFunction tfrm = do
                withUniform "hasUV" srcs $ \p huv -> do
                    glUseProgram p
                    glUniform1i huv 1
                withUniform "sampler" srcs $ \p smp -> do
                    glUseProgram p
                    glUniform1i smp u
                withUniform "projection" srcs $ setOrthoWindowProjection win
                withUniform "modelview" srcs $ setModelview tfrm
                drawBuffer (rsProgram src) vao mode num
            cleanupFunction = do
                withArray [pbuf, cbuf] $ glDeleteBuffers 2
                withArray [vao] $ glDeleteVertexArrays 1
        return $ Rendering renderFunction cleanupFunction

-- | Creates and returns a renderer that renders the given colored beziers.
colorBezRendering :: Window -> BezRenderSource -> [Bezier (V2 Float)]
                 -> [Triangle (V4 Float)] -> IO Rendering
colorBezRendering window (BRS src) bs ts =
    withVAO $ \vao -> withBuffers 3 $ \[pbuf, tbuf, cbuf] -> do
        let vs = concatMap (\(Bezier _ a b c) -> [a,b,c]) bs
            cvs = concatMap (\(Triangle a b c) -> [a,b,c]) $ take (length bs) ts
            ps = map realToFrac $ concatMap F.toList vs :: [GLfloat]
            cs = map realToFrac $ concatMap F.toList cvs :: [GLfloat]
            ws = concatMap (\(Bezier w _ _ _) -> let w' = fromBool $ w == LT
                                                 in [ 0, 0, w'
                                                    , 0.5, 0, w'
                                                    , 1, 1, w'
                                                    ])
                           bs :: [GLfloat]

        glDisableVertexAttribArray uvLoc
        glEnableVertexAttribArray colorLoc
        bufferAttrib positionLoc 2 pbuf ps
        bufferAttrib bezLoc 3 tbuf ws
        bufferAttrib colorLoc 4 cbuf cs
        glBindVertexArray 0

        let cleanupFunction = do
                withArray [pbuf, tbuf, cbuf] $ glDeleteBuffers 3
                withArray [vao] $ glDeleteVertexArrays 1
            num = fromIntegral $ length vs
            srcs = [src]
            renderFunction t = do
                withUniform "hasUV" srcs $ \p huv -> do
                    glUseProgram p
                    glUniform1i huv 0
                withUniform "projection" srcs $ setOrthoWindowProjection window
                withUniform "modelview" srcs $ setModelview t
                drawBuffer (rsProgram src) vao GL_TRIANGLES num
        return $ Rendering renderFunction cleanupFunction

-- | Creates and returns a renderer that masks a textured rectangular area with
-- another texture.
maskRendering :: Window -> MaskRenderSource -> GLuint -> [V2 Float]
             -> [V2 Float] -> IO Rendering
maskRendering win (MRS src) mode vs uvs =
    withVAO $ \vao -> withBuffers 2 $ \[pbuf, uvbuf] -> do
        let vs'  = map realToFrac $ concatMap F.toList vs :: [GLfloat]
            uvs' = map realToFrac $ concatMap F.toList uvs :: [GLfloat]

        glDisableVertexAttribArray colorLoc
        glEnableVertexAttribArray positionLoc
        glEnableVertexAttribArray uvLoc
        bufferAttrib positionLoc 2 pbuf vs'
        bufferAttrib uvLoc 2 uvbuf uvs'
        glBindVertexArray 0

        let cleanup = do withArray [pbuf, uvbuf] $ glDeleteBuffers 2
                         withArray [vao] $ glDeleteVertexArrays 1
            num = fromIntegral $ length vs
            render t = do
                withUniform "projection" [src] $ setOrthoWindowProjection win
                withUniform "modelview" [src] $ setModelview t
                withUniform "mainTex" [src] $ \p smp -> do
                    glUseProgram p
                    glUniform1i smp 0
                withUniform "maskTex" [src] $ \p smp -> do
                    glUseProgram p
                    glUniform1i smp 1
                drawBuffer (rsProgram src) vao mode num
        return $ Rendering render cleanup

alphaMask :: Window -> MaskRenderSource -> IO () -> IO () -> IO Rendering
alphaMask win mrs r2 r1 = do
    mainTex <- toTextureUnit (Just GL_TEXTURE0) win r2
    maskTex <- toTextureUnit (Just GL_TEXTURE1) win r1
    (w,h)   <- getWindowSize win
    let vs = map (fmap fromIntegral) [V2 0 0, V2 w 0, V2 w h, V2 0 h]
        uvs = [V2 0 1, V2 1 1, V2 1 0, V2 0 0]
    Rendering f c <- maskRendering win mrs GL_TRIANGLE_FAN vs uvs
    let f' _ = do glActiveTexture GL_TEXTURE0
                  glBindTexture GL_TEXTURE_2D mainTex
                  glActiveTexture GL_TEXTURE1
                  glBindTexture GL_TEXTURE_2D maskTex
        c'    = withArray [mainTex,maskTex] $ glDeleteTextures 2
        f'' _ = do glActiveTexture GL_TEXTURE0
                   glBindTexture GL_TEXTURE_2D 0
                   glActiveTexture GL_TEXTURE1
                   glBindTexture GL_TEXTURE_2D 0
    return $ Rendering (\t -> f' t >> f t >> f'' t) (c >> c')

-- | Mask one renderer using a stencil test.
stencilMask :: IO () -> IO () -> IO ()
stencilMask r2 r1  = do
    glClear GL_DEPTH_BUFFER_BIT
    -- Enable stencil testing
    glEnable GL_STENCIL_TEST
    -- Disable writing frame buffer color components
    glColorMask GL_FALSE GL_FALSE GL_FALSE GL_FALSE
    -- Disable writing into the depth buffer
    glDepthMask GL_FALSE
    -- Enable writing to all bits of the stencil mask
    glStencilMask 0xFF
    -- Clear the stencil buffer
    glClear GL_STENCIL_BUFFER_BIT
    glStencilFunc GL_NEVER 0 1
    glStencilOp GL_INVERT GL_INVERT GL_INVERT
    r1

    glColorMask GL_TRUE GL_TRUE GL_TRUE GL_TRUE
    glDepthMask GL_TRUE
    glStencilFunc GL_EQUAL 1 1
    glStencilOp GL_ZERO GL_ZERO GL_ZERO
    r2
    glDisable GL_STENCIL_TEST


transformRendering :: Transform -> Rendering -> Rendering
transformRendering t (Rendering r c) = Rendering (r . (t <>)) c
--------------------------------------------------------------------------------
-- Updating uniforms
--------------------------------------------------------------------------------
withUniform :: String -> [RenderSource] -> (GLuint -> GLint -> IO ()) -> IO ()
withUniform name srcs f = mapM_ update srcs
    where update (RenderSource p ls) = case lookup name ls of
                                           Nothing -> return ()
                                           Just u  -> do f p u

setOrthoWindowProjection :: Window -> GLuint -> GLint -> IO ()
setOrthoWindowProjection window program pju = do
    pj <- orthoWindowProjection window
    glUseProgram program
    with pj $ glUniformMatrix4fv pju 1 GL_TRUE . castPtr

setModelview :: Transform -> GLuint -> GLint -> IO ()
setModelview (Transform (V2 x y) (V2 w h) r) program uniform = do
    let mv = mat4Translate txy !*! rot !*! mat4Scale sxy :: M44 GLfloat
        sxy = V3 w h 1
        txy = V3 x y 0
        rxy = V3 0 0 1
        rot = if r /= 0 then mat4Rotate r rxy else identity
    glUseProgram program
    with mv $ glUniformMatrix4fv uniform 1 GL_TRUE . castPtr

orthoWindowProjection :: Window -> IO (M44 GLfloat)
orthoWindowProjection window = do
    (ww, wh) <- getWindowSize window
    let (hw,hh) = (fromIntegral ww, fromIntegral wh)
    return $ ortho 0 hw hh 0 0 1
--------------------------------------------------------------------------------
-- Loading resources and things
--------------------------------------------------------------------------------

-- | Loads a new shader program and attributes for rendering geometry.
loadGeomRenderSource :: IO GeomRenderSource
loadGeomRenderSource = do
    let def = RenderDefBS [(vertSourceGeom, GL_VERTEX_SHADER)
                          ,(fragSourceGeom, GL_FRAGMENT_SHADER)
                          ] ["projection", "modelview", "sampler", "hasUV"]
    GRS <$> loadRenderSource def

-- | Loads a new shader progarm and attributes for rendering beziers.
loadBezRenderSource :: IO BezRenderSource
loadBezRenderSource = do
    let def = RenderDefBS [(vertSourceBezier, GL_VERTEX_SHADER)
                          ,(fragSourceBezier, GL_FRAGMENT_SHADER)
                          ] ["projection", "modelview", "sampler", "hasUV"]
    BRS <$> loadRenderSource def

-- | Loads a new shader program and attributes for masking textures.
loadMaskRenderSource :: IO MaskRenderSource
loadMaskRenderSource = do
    let def = RenderDefBS [(vertSourceMask, GL_VERTEX_SHADER)
                          ,(fragSourceMask, GL_FRAGMENT_SHADER)
                          ] ["projection","modelview","mainTex","maskTex"]
    MRS <$> loadRenderSource def

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
    print locs
    return $ RenderSource program $ catMaybes locs
loadRenderSource (RenderDefFP fps uniforms) = do
    cwd <- getCurrentDirectory
    srcs <- forM fps $ \(fp, shaderType) -> do
        src <- B.readFile $ cwd ++ "/" ++ fp
        return (src, shaderType)
    loadRenderSource $ RenderDefBS srcs uniforms

loadImageAsTexture :: FilePath -> IO (Maybe GLuint)
loadImageAsTexture fp = do
    eStrOrImg <- readImage fp
    case eStrOrImg of
        Left err -> putStrLn err >> return Nothing
        Right i  -> loadTexture i >>= return . Just

loadTexture :: DynamicImage -> IO GLuint
loadTexture = loadTextureUnit Nothing

loadTextureUnit :: Maybe GLuint -> DynamicImage -> IO GLuint
loadTextureUnit Nothing img = loadTextureUnit (Just GL_TEXTURE0) img
loadTextureUnit (Just u) img = do
    [t] <- allocaArray 1 $ \ptr -> do
        glGenTextures 1 ptr
        peekArray 1 ptr
    glActiveTexture u
    glBindTexture GL_TEXTURE_2D t
    loadJuicy img
    glGenerateMipmap GL_TEXTURE_2D  -- Generate mipmaps now!!!
    glTexParameteri GL_TEXTURE_2D GL_TEXTURE_WRAP_S GL_REPEAT
    glTexParameteri GL_TEXTURE_2D GL_TEXTURE_WRAP_T GL_REPEAT
    glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MAG_FILTER GL_NEAREST
    glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MIN_FILTER GL_NEAREST_MIPMAP_NEAREST
    glBindTexture GL_TEXTURE_2D 0
    return t

unloadTexture :: GLuint -> IO ()
unloadTexture t = withArray [t] $ glDeleteTextures 1

loadJuicy :: DynamicImage -> IO ()
loadJuicy (ImageY8 (Image w h d)) = bufferImageData w h d GL_RED GL_UNSIGNED_BYTE
loadJuicy (ImageY16 (Image w h d)) = bufferImageData w h d GL_RED GL_UNSIGNED_SHORT
loadJuicy (ImageYF (Image w h d)) = bufferImageData w h d GL_RED GL_FLOAT
loadJuicy (ImageYA8 i) = loadJuicy $ ImageRGB8 $ promoteImage i
loadJuicy (ImageYA16 i) = loadJuicy $ ImageRGBA16 $ promoteImage i
loadJuicy (ImageRGB8 (Image w h d)) = bufferImageData w h d GL_RGB GL_UNSIGNED_BYTE
loadJuicy (ImageRGB16 (Image w h d)) = bufferImageData w h d GL_RGB GL_UNSIGNED_SHORT
loadJuicy (ImageRGBF (Image w h d)) = bufferImageData w h d GL_RGB GL_FLOAT
loadJuicy (ImageRGBA8 (Image w h d)) = bufferImageData w h d GL_RGBA GL_UNSIGNED_BYTE
loadJuicy (ImageRGBA16 (Image w h d)) = bufferImageData w h d GL_RGBA GL_UNSIGNED_SHORT
loadJuicy (ImageYCbCr8 i) = loadJuicy $ ImageRGB8 $ convertImage i
loadJuicy (ImageCMYK8 i) = loadJuicy $ ImageRGB8 $ convertImage i
loadJuicy (ImageCMYK16 i) = loadJuicy $ ImageRGB16 $ convertImage i


toTexture :: Window -> IO () -> IO GLuint
toTexture = toTextureUnit Nothing

toTextureUnit :: Maybe GLuint -> Window -> IO () -> IO GLuint
toTextureUnit Nothing win r = toTextureUnit (Just GL_TEXTURE0) win r
toTextureUnit (Just u) win r = do
    [fb] <- allocaArray 1 $ \ptr -> do
        glGenFramebuffers 1 ptr
        peekArray 1 ptr
    glBindFramebuffer GL_FRAMEBUFFER fb

    [t] <- allocaArray 1 $ \ptr -> do
        glGenTextures 1 ptr
        peekArray 1 ptr
    glActiveTexture u
    glBindTexture GL_TEXTURE_2D t
    (w,h) <- getWindowSize win
    let [w',h'] = map fromIntegral [w,h]
    glTexImage2D GL_TEXTURE_2D
                 0
                 GL_RGBA
                 w'
                 h'
                 0
                 GL_RGBA
                 GL_UNSIGNED_BYTE
                 nullPtr
    glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MAG_FILTER GL_NEAREST
    glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MIN_FILTER GL_NEAREST

    glFramebufferTexture GL_FRAMEBUFFER GL_COLOR_ATTACHMENT0 t 0
    withArray [GL_COLOR_ATTACHMENT0] $ glDrawBuffers 1

    status <- glCheckFramebufferStatus GL_FRAMEBUFFER
    if status /= GL_FRAMEBUFFER_COMPLETE
    then putStrLn "incomplete framebuffer!"
    else do glClearColor 0 0 0 0
            glClear GL_COLOR_BUFFER_BIT
            --ww <- (fromIntegral . fst) <$> getWindowSize win

            --let s = floor (fbw/ww :: Double)
            --print s
            glViewport 0 0 w' h' --fbw' fbh'
            r
            glBindFramebuffer GL_FRAMEBUFFER 0
            with fb $ glDeleteFramebuffers 1
            (fbw, fbh) <- getFramebufferSize win
            glViewport 0 0 (fromIntegral fbw) (fromIntegral fbh)
    return t

calculateDpi :: IO Dpi
calculateDpi = do
    mMonitor <- getPrimaryMonitor

    -- Calculate the dpi of the primary monitor.
    case mMonitor of
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
                              return dpi
--------------------------------------------------------------------------------
-- Buffering, Vertex Array Objects, Uniforms, etc.
--------------------------------------------------------------------------------
bufferImageData :: forall a a1 a2. (Storable a2, Integral a1, Integral a) => a -> a1 -> Vector a2 -> GLenum -> GLenum -> IO ()
bufferImageData w h dat imgfmt pxfmt = unsafeWith dat $ \ptr -> do
    --glTexStorage2D GL_TEXTURE_2D 1 GL_RGBA8 (fromIntegral w) (fromIntegral h)
    --glTexSubImage2D GL_TEXTURE_2D 0 0 0 (fromIntegral w) (fromIntegral h) GL_RGBA GL_UNSIGNED_BYTE (castPtr ptr)
    glTexImage2D
        GL_TEXTURE_2D
        0
        GL_RGBA
        (fromIntegral w)
        (fromIntegral h)
        0
        imgfmt
        pxfmt
        (castPtr ptr)
    err <- glGetError
    when (err /= 0) $ putStrLn $ "glTexImage2D Error: " ++ show err

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

drawBuffer :: GLuint
           -> GLuint
           -> GLenum
           -> GLsizei
           -> IO ()
drawBuffer program vao mode num = do
    glUseProgram program
    glBindVertexArray vao
    err <- glGetError
    when (err /= 0) $ do
        putStrLn $ "glBindVertex Error: " ++ show err
        cs <- currentCallStack
        mapM_ (putStrLn . ("    " ++)) cs
        exitFailure
    glDrawArrays mode 0 num
    err' <- glGetError
    when (err' /= 0) $ do
        putStrLn $ "glDrawArrays Error: " ++ show err'
        cs <- currentCallStack
        mapM_ (putStrLn . ("    " ++)) cs
        exitFailure

glFloatSize :: Int
glFloatSize = sizeOf (undefined :: GLfloat)

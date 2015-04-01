{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Render.IO where

--import Debug.Trace
import Shader
import Render.Types
import Render.Geometrical
import Graphics.GL.Core33
import Foreign.Marshal.Array
import Foreign.Marshal.Utils
import Foreign.C.String
import Foreign.Storable
import Foreign.Ptr
import Data.Maybe
import Data.Time.Clock
import Data.Vector.Storable (Vector,unsafeWith)
import Control.Applicative
import Control.Concurrent.Async
import Control.Monad
import System.Directory
import qualified Data.IntMap as IM
import qualified Data.Map as M
import qualified Data.ByteString.Char8 as B

toTexture :: (Int, Int) -> (Int, Int) -> IO () -> IO GLuint
toTexture (w, h) (ww, wh) r = do
    [fb] <- allocaArray 1 $ \ptr -> do
        glGenFramebuffers 1 ptr
        peekArray 1 ptr
    glBindFramebuffer GL_FRAMEBUFFER fb

    [t] <- allocaArray 1 $ \ptr -> do
        glGenTextures 1 ptr
        peekArray 1 ptr
    glBindTexture GL_TEXTURE_2D t
    let [w',h',ww',wh'] = map fromIntegral [w,h,ww,wh]
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

{-
    [d] <- allocaArray 1 $ \ptr -> do
        glGenRenderbuffers 1 ptr
        peekArray 1 ptr
    glBindRenderbuffer GL_RENDERBUFFER d
    glRenderbufferStorage GL_RENDERBUFFER GL_DEPTH_COMPONENT w' h'
    glFramebufferRenderbuffer GL_FRAMEBUFFER GL_DEPTH_ATTACHMENT GL_RENDERBUFFER d
-}

    glFramebufferTexture GL_FRAMEBUFFER GL_COLOR_ATTACHMENT0 t 0
    withArray [GL_COLOR_ATTACHMENT0] $ glDrawBuffers 1

    status <- glCheckFramebufferStatus GL_FRAMEBUFFER
    if status /= GL_FRAMEBUFFER_COMPLETE
    then putStrLn "incomplete framebuffer!"
    else do glClearColor 0 0 0 0
            glClear GL_COLOR_BUFFER_BIT
            glViewport 0 0 w' h'
            r
            glBindFramebuffer GL_FRAMEBUFFER 0
            with fb $ glDeleteFramebuffers 1
    return t

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
    return $ Resources { rsrcFonts = a
                       , rsrcRenderers = IM.empty
                       , rsrcSources = M.empty
                       , rsrcWindow = window
                       , rsrcDpi = dpi
                       , rsrcUTC = t
                       }

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

loadTexture :: DynamicImage -> IO GLuint
loadTexture img = do
    putStrLn "Loading texture"
    [t] <- allocaArray 1 $ \ptr -> do
        glGenTextures 1 ptr
        peekArray 1 ptr
    glBindTexture GL_TEXTURE_2D t
    loadJuicy img
    glGenerateMipmap GL_TEXTURE_2D  -- Generate mipmaps now!!!
    glTexParameteri GL_TEXTURE_2D GL_TEXTURE_WRAP_S GL_REPEAT
    glTexParameteri GL_TEXTURE_2D GL_TEXTURE_WRAP_T GL_REPEAT
    glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MAG_FILTER GL_LINEAR
    glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MIN_FILTER GL_LINEAR_MIPMAP_LINEAR
    -- glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MIN_FILTER GL_NEAREST
    -- glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MAG_FILTER GL_NEAREST
    -- glTexParameteri GL_TEXTURE_2D GL_TEXTURE_WRAP_S GL_REPEAT
    -- glTexParameteri GL_TEXTURE_2D GL_TEXTURE_WRAP_T GL_REPEAT
    -- glBindTexture GL_TEXTURE_2D 0
    return t

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

drawBuffer :: Window
           -> GLuint
           -> UniformUpdates
           -> GLuint
           -> GLenum
           -> GLsizei
           -> Transform
           -> IO ()
drawBuffer window program UniformUpdates{..} vao mode num (Transform txy sxy rot) = do
    glUseProgram program
    case (,) <$> uuProjection <*> uuModelview of
        Nothing -> return ()
        Just (pju,mvu) -> do (pj,mv) <- getPJMV window txy sxy rot
                             with pj $ glUniformMatrix4fv pju 1 GL_TRUE . castPtr
                             with mv $ glUniformMatrix4fv mvu 1 GL_TRUE . castPtr
    let (spu,spub) = uuSampler
        (hvu,hvub) = uuHasUV
    glUniform1i spu spub
    glUniform1i hvu hvub
    glBindVertexArray vao
    err <- glGetError
    when (err /= 0) $ putStrLn $ "glBindVertex Error: " ++ show err
    glDrawArrays mode 0 num
    err' <- glGetError
    when (err /= 0) $ putStrLn $ "glDrawArrays Error: " ++ show err'

getPJMV :: Window -> V2 GLfloat -> V2 GLfloat -> GLfloat
        -> IO (M44 GLfloat, M44 GLfloat)
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

glFloatSize :: Int
glFloatSize = sizeOf (undefined :: GLfloat)

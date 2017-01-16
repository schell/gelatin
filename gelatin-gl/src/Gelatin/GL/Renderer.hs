{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ScopedTypeVariables   #-}
module Gelatin.GL.Renderer (
    -- * Renderer
    Renderer2,
    Context(..),
    -- * Loading and using textures
    allocAndActivateTex,
    initializeTexImage2D,
    loadImage,
    maybeLoadTexture,
    loadTexture,
    loadTextureUnit,
    unloadTexture,
    loadImageAsTexture,
    bindTexsAround,
    bindTexAround,
    -- * Line rendering
    colorPolylineRenderer,
    texPolylineRenderer,
    -- * Triangle rendering
    colorRenderer,
    textureRenderer,
    -- * Bezier rendering
    colorBezRenderer,
    textureBezRenderer,
    -- * Masking
    maskRenderer,
    stencilMask,
    alphaMask,
    -- * Transforming a rendering
    transformRenderer,
    -- * Utils
    toTexture,
    toTextureUnit,
    clipTexture
) where

import           Codec.Picture          (readImage)
import           Codec.Picture.Types
import           Control.Exception      (assert)
import           Control.Monad
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Control.Monad.Reader   (ReaderT (..))
import qualified Data.Foldable          as F
import           Data.Proxy             (Proxy (..))
import qualified Data.Vector.Generic    as G
import qualified Data.Vector.Storable   as S
import           Data.Vector.Unboxed    (Unbox, Vector)
import qualified Data.Vector.Unboxed    as V
import           Foreign.Marshal.Array
import           Foreign.Marshal.Utils
import           Foreign.Ptr
import           Foreign.Storable
import           Gelatin
import           Gelatin.GL.Common
import           Gelatin.GL.Shader
import           Gelatin.Shaders
import           Graphics.GL.Core33
import           Graphics.GL.Types
import           System.Exit

--------------------------------------------------------------------------------
-- Uniform updates for the Simple2DShader
--------------------------------------------------------------------------------
updatePrimitive :& updateProjection :& updateModelView :& updateThickness :&
  updateFeather :& updateSumLength :& updateCap :& updateHasUV :& updateSampler :&
  updateMainTex :& updateMaskTex :& updateAlpha :& updateMultiply :&
  updateShouldReplaceColor :& updateReplacementColor :& ()
  = genFunction (Proxy :: Proxy Simple2DUniforms)
--------------------------------------------------------------------------------
-- Attribute toggling
--------------------------------------------------------------------------------
(enablePosition, disablePosition) :& (enableColor, disableColor) :&
  (enableUV, disableUV) :& (enableBez, disableBez) :&
  (enableBezUV, disableBezUV) :& (enablePrev, disablePrev) :&
  (enableNext, disableNext) :& ()
  = genFunction (Proxy :: Proxy Simple2DAttribToggles)

disableAll :: IO ()
disableAll =
  sequence_ [ disablePosition, disableColor, disableUV, disableBez, disableBezUV
            , disablePrev, disableNext
            ]

--enableAll :: IO ()
--enableAll =
--  sequence_ [ enablePosition, enableColor, enableUV, enableBez, enableBezUV
--            , enablePrev, enableNext
--            ]

enableAttribsForLines :: Bool -> IO ()
enableAttribsForLines hasUV = do
  disableAll
  enablePosition
  enableBezUV
  enablePrev
  enableNext
  if hasUV
    then enableUV
    else enableColor

enableAttribsForTris :: Bool -> IO ()
enableAttribsForTris hasUV =
  disableAll >> enablePosition >> if hasUV then enableUV
                                           else enableColor

enableAttribsForBezs :: Bool -> IO ()
enableAttribsForBezs hasUV =
  disableAll >> enablePosition >> enableBez >> if hasUV then enableUV
                                                        else enableColor

enableAttribsForMask :: IO ()
enableAttribsForMask = disableAll >> enablePosition >> enableUV
--------------------------------------------------------------------------------
-- Attribute buffering
--------------------------------------------------------------------------------
bufferPosition :& bufferColor :& bufferUV :& bufferBez :& bufferBezUV :&
  bufferPrev :& bufferNext :& ()
  = genFunction (Proxy :: Proxy Simple2DAttribBuffers)
--------------------------------------------------------------------------------
-- Rendering
--------------------------------------------------------------------------------
-- | Creates and returns a renderer that renders a colored, expanded 2d polyline
-- projected in 2d space.
colorPolylineRenderer :: Context -> Simple2DShader -> Float -> Float
                      -> (LineCap,LineCap) -> Vector (V2 Float)
                      -> Vector (V4 Float) -> IO Renderer2
colorPolylineRenderer win sh thickness feather caps verts colors = do
  let empty = putStrLn "could not expand polyline" >> return mempty
      mpoly = expandPolyline verts colors thickness feather
  flip (maybe empty) mpoly $ \(vs_,cs_,us_,ns_,ps_,totalLen) -> do
    let toFrac :: Float -> GLfloat
        toFrac = realToFrac
        vs = V.map (fmap toFrac) vs_
        cs = V.map (fmap toFrac) cs_
        uvs = V.map (fmap toFrac) cs_
        us = V.map (fmap toFrac) us_
        ns = V.map (fmap toFrac) ns_
        ps = V.map (fmap toFrac) ps_

    withVAO $ \vao -> withBuffers 5 $ \bufs@[vbuf, cbuf, buvbuf, nbuf, pbuf] -> do
      enableAttribsForLines False
      bufferPosition 2 vbuf vs
      bufferColor 4 cbuf cs
      bufferBezUV 2 buvbuf us
      bufferNext 2 nbuf ns
      bufferPrev 2 pbuf ps
      glBindVertexArray 0

      let num = fromIntegral $ V.length vs_
          r t = do
            glUseProgram sh
            let (mv, a, m, mr) = unwrapTransforms t
            pj <- orthoContextProjection win
            updatePrimitive sh PrimLine
            updateModelView sh mv
            updateHasUV sh False
            updateThickness sh thickness
            updateFeather sh feather
            updateSumLength sh totalLen
            updateCap sh caps
            updateAlpha sh a
            updateMultiply sh m
            case mr of
              Just c -> do updateShouldReplaceColor sh True
                           updateReplacementColor sh c
              _      -> updateShouldReplaceColor sh False
            drawBuffer sh vao GL_TRIANGLE_STRIP num
          c = do withArray bufs $ glDeleteBuffers 5
                 withArray [vao] $ glDeleteVertexArrays 1
      return (c,r)

-- | Creates and returns a renderer that renders a textured, expanded 2d
-- polyline projected in 2d space.
texPolylineRenderer :: Context -> Simple2DShader -> Float
                    -> Float -> (LineCap,LineCap) -> Vector (V2 Float)
                    -> Vector (V2 Float) -> IO Renderer2
texPolylineRenderer win sh thickness feather caps verts uvs = do
  let empty = putStrLn "could not expand polyline" >> return mempty
      mpoly = expandPolyline verts uvs thickness feather
  flip (maybe empty) mpoly $ \(vs_,cs_,us_,ns_,ps_,totalLen) -> do
    let toFrac :: Float -> GLfloat
        toFrac = realToFrac
        vs = V.map (fmap toFrac) vs_
        cs = V.map (fmap toFrac) cs_
        uvs = V.map (fmap toFrac) cs_
        us = V.map (fmap toFrac) us_
        ns = V.map (fmap toFrac) ns_
        ps = V.map (fmap toFrac) ps_

    withVAO $ \vao -> withBuffers 5 $ \bufs@[vbuf, cbuf, buvbuf, nbuf, pbuf] -> do
      enableAttribsForLines True
      bufferPosition 2 vbuf vs
      bufferUV 2 cbuf cs
      bufferBezUV 2 buvbuf us
      bufferNext 2 nbuf ns
      bufferPrev 2 pbuf ps
      glBindVertexArray 0

      let num = fromIntegral $ V.length vs_
          r t = do
            glUseProgram sh
            let (mv, a, m, mr) = unwrapTransforms t
            pj <- orthoContextProjection win
            updatePrimitive sh PrimLine
            updateProjection sh pj
            updateModelView sh mv
            updateHasUV sh True
            updateThickness sh thickness
            updateFeather sh feather
            updateSumLength sh totalLen
            updateCap sh caps
            updateAlpha sh a
            updateMultiply sh m
            case mr of
              Just c -> do updateShouldReplaceColor sh True
                           updateReplacementColor sh c
              _      -> updateShouldReplaceColor sh False
            drawBuffer sh vao GL_TRIANGLE_STRIP num
          c = do withArray bufs $ glDeleteBuffers 5
                 withArray [vao] $ glDeleteVertexArrays 1
      return (c,r)

-- | Binds the given textures to GL_TEXTURE0, GL_TEXTURE1, ... in ascending
-- order of the texture unit, runs the IO action and then unbinds the textures.
bindTexsAround :: MonadIO m => [GLuint] -> m a -> m a
bindTexsAround ts f = do
  liftIO $ mapM_ (uncurry bindTex) (zip ts [GL_TEXTURE0 ..])
  a <- f
  liftIO $ glBindTexture GL_TEXTURE_2D 0
  return a
  where bindTex tex u = glActiveTexture u >> glBindTexture GL_TEXTURE_2D tex

bindTexAround :: MonadIO m => GLuint -> m a -> m a
bindTexAround tx = bindTexsAround [tx]

-- | Creates and returns a renderer that renders the given colored
-- geometry.
colorRenderer :: Context -> Simple2DShader -> GLuint -> Vector (V2 Float)
              -> Vector (V4 Float) -> IO Renderer2
colorRenderer window sh mode vs gs =
  withVAO $ \vao -> withBuffers 2 $ \[pbuf,cbuf] -> do
    --let ps = V.map realToFrac $ V.concatMap (V.fromList . F.toList) vs :: Vector GLfloat
    --    cs = V.map realToFrac $ V.concatMap (V.fromList . F.toList) $ V.take (V.length vs) gs :: Vector GLfloat

    enableAttribsForTris False
    clearErrors "colorRenderer: enable attribs"
    bufferPosition 2 pbuf vs
    clearErrors "colorRenderer: buffer position"
    bufferColor 4 cbuf $ V.take (V.length vs) gs
    clearErrors "colorRenderer: buffer color"
    let num = fromIntegral $ V.length vs
        renderFunction t = do
          glUseProgram sh
          let (mv,a,m,mr) = unwrapTransforms t
          pj <- orthoContextProjection window
          updatePrimitive sh PrimTri
          updateProjection sh pj
          updateModelView sh mv
          updateHasUV sh False
          updateAlpha sh a
          updateMultiply sh m
          case mr of
            Just c -> do updateShouldReplaceColor sh True
                         updateReplacementColor sh c
            _      -> updateShouldReplaceColor sh False
          drawBuffer sh vao mode num
        cleanupFunction = do
          withArray [pbuf, cbuf] $ glDeleteBuffers 2
          withArray [vao] $ glDeleteVertexArrays 1
    return (cleanupFunction,renderFunction)

-- | Creates and returns a renderer that renders a textured
-- geometry.
textureRenderer :: Context -> Simple2DShader -> GLuint -> Vector (V2 Float)
                -> Vector (V2 Float) -> IO Renderer2
textureRenderer win sh mode vs uvs =
  withVAO $ \vao -> withBuffers 2 $ \[pbuf,cbuf] -> do
  --let f xs = V.map realToFrac $ V.concatMap (V.fromList . F.toList) xs :: Vector GLfloat
  --    ps = f vs
  --    cs = f $ V.take (V.length vs) uvs

  enableAttribsForTris True
  bufferPosition 2 pbuf vs
  bufferUV 2 cbuf uvs
  glBindVertexArray 0

  let num = fromIntegral $ V.length vs
      renderFunction t = do
        glUseProgram sh
        let (mv,a,m,mr) = unwrapTransforms t
        pj <- orthoContextProjection win
        updatePrimitive sh PrimTri
        updateProjection sh pj
        updateModelView sh mv
        updateHasUV sh True
        updateSampler sh 0
        updateAlpha sh a
        updateMultiply sh m
        case mr of
          Just c -> do updateShouldReplaceColor sh True
                       updateReplacementColor sh c
          _      -> updateShouldReplaceColor sh False
        drawBuffer sh vao mode num
      cleanupFunction = do
        withArray [pbuf, cbuf] $ glDeleteBuffers 2
        withArray [vao] $ glDeleteVertexArrays 1
  return (cleanupFunction,renderFunction)

--bezAttributes :: (Foldable f, Unbox (f Float))
--              => Vector (V2 Float)
--              -> Vector (f Float)
--              -> (Vector GLfloat, Vector GLfloat, Vector GLfloat)
--bezAttributes vs cvs = (ps, cs, ws)
--  where ps = V.map realToFrac $
--             V.concatMap (V.fromList . F.toList) vs :: Vector GLfloat
--        cs = V.map realToFrac $
--               V.concatMap (V.fromList . F.toList) cvs :: Vector GLfloat
--        getWinding i =
--          let n = i * 3
--              (a,b,c) = (vs V.! n, vs V.! (n + 1), vs V.! (n + 2))
--              w = fromBool $ triangleArea a b c <= 0
--          in V.fromList [ 0, 0, w
--                        , 0.5, 0, w
--                        , 1, 1, w
--                        ]
--        numBezs = floor $ realToFrac (V.length vs) / (3 :: Double)
--        ws :: Vector GLfloat
--        ws = V.concatMap getWinding $ V.generate numBezs id

bezWinding :: Vector (V2 Float) -> Vector (V3 Float)
bezWinding vs = V.concatMap getWinding $ V.generate numBezs id
  where getWinding i =
          let n = i * 3
              (a,b,c) = (vs V.! n, vs V.! (n + 1), vs V.! (n + 2))
              w = fromBool $ triangleArea a b c <= 0
          in V.fromList [ V3 0 0 w
                        , V3 0.5 0 w
                        , V3 1 1 w
                        ]
        numBezs = floor $ realToFrac (V.length vs) / (3 :: Double)

-- | Creates and returns a renderer that renders the given colored beziers.
colorBezRenderer :: Context -> Simple2DShader
                 -> Vector (V2 Float) -> Vector (V4 Float) -> IO Renderer2
colorBezRenderer win sh vs cs = do
  let ws = bezWinding vs
  withVAO $ \vao -> withBuffers 3 $ \[pbuf, tbuf, cbuf] -> do
    enableAttribsForBezs False
    bufferPosition 2 pbuf vs
    bufferBez 3 tbuf ws
    bufferColor 4 cbuf $ V.take (V.length vs) cs
    glBindVertexArray 0

    let cleanupFunction = do
          withArray [pbuf, tbuf, cbuf] $ glDeleteBuffers 3
          withArray [vao] $ glDeleteVertexArrays 1
        num = fromIntegral $ V.length vs
        renderFunction t = do
          glUseProgram sh
          pj <- orthoContextProjection win
          let (mv,a,m,mr) = unwrapTransforms t
          updatePrimitive sh PrimBez
          updateProjection sh pj
          updateModelView sh mv
          updateHasUV sh False
          updateAlpha sh a
          updateMultiply sh m
          case mr of
            Just c -> do updateShouldReplaceColor sh True
                         updateReplacementColor sh c
            _      -> updateShouldReplaceColor sh False
          drawBuffer sh vao GL_TRIANGLES num
    return (cleanupFunction,renderFunction)

-- | Creates and returns a renderer that renders the given textured beziers.
textureBezRenderer :: Context -> Simple2DShader
                   -> Vector (V2 Float) -> Vector (V2 Float) -> IO Renderer2
textureBezRenderer win sh vs cs = do
  let ws = bezWinding vs
  withVAO $ \vao -> withBuffers 3 $ \[pbuf, tbuf, cbuf] -> do
    enableAttribsForBezs True
    bufferPosition 2 pbuf vs
    bufferBez 3 tbuf ws
    bufferUV 2 cbuf cs
    glBindVertexArray 0

    let cleanupFunction = do
            withArray [pbuf, tbuf, cbuf] $ glDeleteBuffers 3
            withArray [vao] $ glDeleteVertexArrays 1
        num = fromIntegral $ V.length vs
        renderFunction t = do
          glUseProgram sh
          pj <- orthoContextProjection win
          let (mv,a,m,mr) = unwrapTransforms t
          updatePrimitive sh PrimBez
          updateProjection sh pj
          updateModelView sh mv
          updateHasUV sh True
          updateSampler sh 0
          updateAlpha sh a
          updateMultiply sh m
          case mr of
            Just c -> do updateShouldReplaceColor sh True
                         updateReplacementColor sh c
            _      -> updateShouldReplaceColor sh False
          drawBuffer sh vao GL_TRIANGLES num
    return (cleanupFunction,renderFunction)

-- | Creates and returns a renderer that masks a textured rectangular area with
-- another texture.
maskRenderer :: Context -> Simple2DShader -> GLuint -> Vector (V2 Float)
             -> Vector (V2 Float) -> IO Renderer2
maskRenderer win sh mode vs uvs =
    withVAO $ \vao -> withBuffers 2 $ \[pbuf, uvbuf] -> do
        --let vs'  = V.map realToFrac $
        --             V.concatMap (V.fromList . F.toList) vs :: Vector GLfloat
        --    uvs' = V.map realToFrac $
        --             V.concatMap (V.fromList . F.toList) uvs :: Vector GLfloat

        enableAttribsForMask
        bufferPosition 2 pbuf vs
        bufferUV 2 uvbuf uvs
        glBindVertexArray 0

        let cleanup = do withArray [pbuf, uvbuf] $ glDeleteBuffers 2
                         withArray [vao] $ glDeleteVertexArrays 1
            num = fromIntegral $ V.length vs
            render t = do
                let (mv,a,m,_) = unwrapTransforms t
                pj <- orthoContextProjection win
                --updateUniformsForMask (unShader sh) pj mv a m 0 1
                updateProjection sh pj
                updateModelView sh mv
                updateAlpha sh a
                updateMultiply sh m
                updateMainTex sh 0
                updateMaskTex sh 1
                drawBuffer sh vao mode num
        return (cleanup,render)

-- | Creates a rendering that masks an IO () drawing computation with the alpha
-- value of another.
alphaMask :: Context -> Simple2DShader -> IO () -> IO () -> IO Renderer2
alphaMask win mrs r2 r1 = do
    mainTex <- toTextureUnit (Just GL_TEXTURE0) win r2
    maskTex <- toTextureUnit (Just GL_TEXTURE1) win r1
    (w,h)   <- ctxWindowSize win
    let vs = V.fromList $ map (fmap fromIntegral) [V2 0 0, V2 w 0, V2 w h, V2 0 h]
        uvs = V.fromList [V2 0 1, V2 1 1, V2 1 0, V2 0 0]
    (c,f) <- maskRenderer win mrs GL_TRIANGLE_FAN vs uvs
    let f' _ = do glActiveTexture GL_TEXTURE0
                  glBindTexture GL_TEXTURE_2D mainTex
                  glActiveTexture GL_TEXTURE1
                  glBindTexture GL_TEXTURE_2D maskTex
        c'    = withArray [mainTex,maskTex] $ glDeleteTextures 2
        f'' _ = do glActiveTexture GL_TEXTURE0
                   glBindTexture GL_TEXTURE_2D 0
                   glActiveTexture GL_TEXTURE1
                   glBindTexture GL_TEXTURE_2D 0
    return (c >> c', \t -> f' t >> f t >> f'' t)

-- | Creates an IO () drawing computation that masks an IO () drawing
-- computation with another using a stencil test.
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

transformRenderer :: [RenderTransform2] -> Renderer2 -> Renderer2
transformRenderer ts (c, r) = (c, r . (ts ++))
--------------------------------------------------------------------------------
-- Working with textures.
--------------------------------------------------------------------------------
loadImage :: FilePath -> IO (Maybe (V2 Int, GLuint))
loadImage fp = readImage fp >>= maybeLoadTexture

loadImageAsTexture :: FilePath -> IO (Maybe GLuint)
loadImageAsTexture fp = do
  edyn <- readImage fp
  fmap snd <$> maybeLoadTexture edyn

maybeLoadTexture :: Either String DynamicImage -> IO (Maybe (V2 Int, GLuint))
maybeLoadTexture strOrImg = case strOrImg of
    Left err -> putStrLn err >> return Nothing
    Right i  -> Just <$> loadTexture i

loadTexture :: DynamicImage -> IO (V2 Int, GLuint)
loadTexture = loadTextureUnit Nothing

allocAndActivateTex :: GLenum -> IO GLuint
allocAndActivateTex u = do
    [t] <- allocaArray 1 $ \ptr -> do
        glGenTextures 1 ptr
        peekArray 1 ptr
    glActiveTexture u
    glBindTexture GL_TEXTURE_2D t
    return t

loadTextureUnit :: Maybe GLuint -> DynamicImage -> IO (V2 Int, GLuint)
loadTextureUnit Nothing img = loadTextureUnit (Just GL_TEXTURE0) img
loadTextureUnit (Just u) img = do
    t <- allocAndActivateTex u
    (w,h) <- loadJuicy img
    glGenerateMipmap GL_TEXTURE_2D  -- Generate mipmaps now!!!
    glTexParameteri GL_TEXTURE_2D GL_TEXTURE_WRAP_S GL_REPEAT
    glTexParameteri GL_TEXTURE_2D GL_TEXTURE_WRAP_T GL_REPEAT
    glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MAG_FILTER GL_NEAREST
    glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MIN_FILTER GL_NEAREST_MIPMAP_NEAREST
    glBindTexture GL_TEXTURE_2D 0
    return (V2 w h, t)

unloadTexture :: GLuint -> IO ()
unloadTexture t = withArray [t] $ glDeleteTextures 1

loadJuicy :: DynamicImage -> IO (Int,Int)
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

toTexture :: Context -> IO () -> IO GLuint
toTexture = toTextureUnit Nothing

toTextureUnit :: Maybe GLuint -> Context -> IO () -> IO GLuint
toTextureUnit Nothing win r = toTextureUnit (Just GL_TEXTURE0) win r
toTextureUnit (Just u) win r = do
    [fb] <- allocaArray 1 $ \ptr -> do
        glGenFramebuffers 1 ptr
        peekArray 1 ptr
    glBindFramebuffer GL_FRAMEBUFFER fb

    t <- allocAndActivateTex u

    (w,h) <- ctxWindowSize win
    let [w',h'] = map fromIntegral [w,h]

    initializeTexImage2D w' h'

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
            (fbw, fbh) <- ctxFramebufferSize win
            glViewport 0 0 (fromIntegral fbw) (fromIntegral fbh)
    return t

initializeTexImage2D :: GLsizei -> GLsizei -> IO ()
initializeTexImage2D w h = do
  glTexImage2D GL_TEXTURE_2D 0 GL_RGBA w h 0 GL_RGBA GL_UNSIGNED_BYTE nullPtr
  glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MAG_FILTER GL_NEAREST
  glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MIN_FILTER GL_NEAREST

type ClippingArea = (V2 Int, V2 Int)

-- | Sub-samples a texture using the given coordinate box and creates a new
-- texture. Keep in mind that OpenGL texture coordinates are flipped from
-- 'normal' graphics coordinates (y = 0 is the bottom of the texture). That
-- fact has bitten the author a number of times while clipping a texture
-- created with `toTexture` and `toUnitTexture`.
clipTexture :: GLuint -> ClippingArea -> IO GLuint
clipTexture rtex (V2 x1 y1, V2 x2 y2) = do
    -- Create our framebuffers
    [fbread,fbwrite] <- allocaArray 2 $ \ptr -> do
        glGenFramebuffers 2 ptr
        peekArray 2 ptr
    -- Bind our read frame buffer and attach the input texture to it
    glBindFramebuffer GL_READ_FRAMEBUFFER fbread
    glFramebufferTexture2D GL_READ_FRAMEBUFFER GL_COLOR_ATTACHMENT0 GL_TEXTURE_2D rtex 0
    clearErrors "clipTexture bind read framebuffer"
    -- Generate a new texture and bind our write framebuffer to it
    [wtex] <- allocaArray 1 $ \ptr -> do
        glGenTextures 1 ptr
        peekArray 1 ptr
    glActiveTexture GL_TEXTURE0
    glBindTexture GL_TEXTURE_2D wtex
    let [x1',y1',x2',y2',w',h'] = map fromIntegral
                                      [x1,y1,x2,y2,abs $ x2 - x1
                                                  ,abs $ y2 - y1]
    initializeTexImage2D w' h'
    glBindFramebuffer GL_DRAW_FRAMEBUFFER fbwrite
    glFramebufferTexture2D GL_DRAW_FRAMEBUFFER GL_COLOR_ATTACHMENT0 GL_TEXTURE_2D wtex 0
    clearErrors "clipTexture bind write framebuffer"
    -- Check our frame buffer stati
    forM_ [GL_READ_FRAMEBUFFER,GL_DRAW_FRAMEBUFFER] $ \fb -> do
        status <- glCheckFramebufferStatus fb
        when (status /= GL_FRAMEBUFFER_COMPLETE) $ do
            putStrLn "incomplete framebuffer!"
            exitFailure
    -- Blit the read framebuffer into the write framebuffer
    glBlitFramebuffer x1' y1' x2' y2' 0 0 w' h' GL_COLOR_BUFFER_BIT GL_NEAREST
    clearErrors "clipTexture blit framebuffers"
    -- Cleanup
    glBindFramebuffer GL_FRAMEBUFFER 0
    withArray [fbread,fbwrite] $ glDeleteFramebuffers 2
    glBindTexture GL_TEXTURE_2D 0
    return wtex
--------------------------------------------------------------------------------
-- Buffering, Vertex Array Objects, Uniforms, etc.
--------------------------------------------------------------------------------
bufferImageData :: forall a a1 a2. (Storable a2, Integral a1, Integral a) => a -> a1 -> S.Vector a2 -> GLenum -> GLenum -> IO (a,a1)
bufferImageData w h dat imgfmt pxfmt = S.unsafeWith dat $ \ptr -> do
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
    return (w,h)

withVAO :: (GLuint -> IO b) -> IO b
withVAO f = do
    [vao] <- allocaArray 1 $ \ptr -> do
        glGenVertexArrays 1 ptr
        peekArray 1 ptr
    glBindVertexArray vao
    r <- f vao
    clearErrors "withVAO"
    glBindVertexArray 0
    return r

withBuffers :: Int -> ([GLuint] -> IO b) -> IO b
withBuffers n f = do
    bufs <- allocaArray n $ \ptr -> do
        glGenBuffers (fromIntegral n) ptr
        peekArray (fromIntegral n) ptr
    f bufs

--bufferAttrib :: (Storable a, Unbox a)
--             => Simple2DAttrib -> GLint -> GLuint -> Vector a -> IO ()
--bufferAttrib attr n buf as = do
--    let loc = locToGLuint attr
--        asize = V.length as * sizeOf (V.head as)
--        f = S.convert :: (G.Vector Vector a, Storable a)
--                      => Vector a -> S.Vector a
--    glBindBuffer GL_ARRAY_BUFFER buf
--
--    S.unsafeWith (f as) $ \ptr ->
--        glBufferData GL_ARRAY_BUFFER (fromIntegral asize) (castPtr ptr) GL_STATIC_DRAW
--    glEnableVertexAttribArray loc
--    glVertexAttribPointer loc n GL_FLOAT GL_FALSE 0 nullPtr

drawBuffer :: GLuint
           -> GLuint
           -> GLenum
           -> GLsizei
           -> IO ()
drawBuffer program vao mode num = do
    glUseProgram program
    glBindVertexArray vao
    clearErrors "drawBuffer:glBindVertex"
    glDrawArrays mode 0 num
    clearErrors "drawBuffer:glDrawArrays"

clearErrors :: String -> IO ()
clearErrors str = do
    err' <- glGetError
    when (err' /= 0) $ do
      putStrLn $ unwords [str, show err']
      assert False $ return ()

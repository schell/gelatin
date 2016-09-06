{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PartialTypeSignatures #-}
module Gelatin.GL.Renderer (
    -- * Renderer
    GLRenderer,
    Context(..),
    -- * Loading and using textures
    allocAndActivateTex,
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

import           Gelatin.GL.Shader
import           Gelatin.GL.Common
import           Gelatin
import           Graphics.GL.Core33
import           Graphics.GL.Types
import           Codec.Picture.Types
import           Codec.Picture (readImage)
import           Foreign.Marshal.Array
import           Foreign.Marshal.Utils
import           Foreign.Storable
import           Foreign.Ptr
import           Data.Monoid
import qualified Data.Vector.Generic as G
import qualified Data.Vector.Storable as S
import qualified Data.Vector.Unboxed as V
import           Data.Vector.Unboxed (Vector,Unbox)
import           Data.List (unzip5)
import           Control.Monad
import           System.Exit
import qualified Data.Foldable as F
import           GHC.Stack
import           Linear hiding (trace)

--------------------------------------------------------------------------------
-- Quick Helpers
--------------------------------------------------------------------------------
unwrapTransform :: PictureTransform -> (M44 Float, Float, V4 Float, Maybe (V4 Float))
unwrapTransform t = (ptfrmMV t, ptfrmAlpha t, ptfrmMultiply t, ptfrmReplace t)
--------------------------------------------------------------------------------
-- GLRenderers
--------------------------------------------------------------------------------
type PolylineData f =
  ( Vector (V2 Float)
  , Vector (f Float)
  , Vector (V2 Float)
  , Vector (V2 Float)
  , Vector (V2 Float)
  , Float
  )

expandPolyline :: Unbox (f Float)
               => Vector (V2 Float) -> Vector (f Float) -> Float -> Float
               -> Maybe (PolylineData f)
expandPolyline verts colors thickness feather
    | Just (v1,v2) <- (,) <$> (verts V.!? 0) <*> (verts V.!? 1)
    , Just c1  <- colors V.!? 0
    , Just (v3,v3n) <- (,) <$> (verts V.!? (V.length verts -1))
                           <*> (verts V.!? (V.length verts -2))
    , Just c3 <- colors V.!? (V.length verts -1) =
    let -- clamp the lower bound of our thickness to 1
        absthick = max thickness 1
        d = fromIntegral (ceiling $ absthick + 2.5 * feather :: Integer)
        lens = 0 `V.cons` V.zipWith distance verts (V.drop 1 verts)
        totalLen = V.foldl' (+) 0 lens
        totalEnd = totalLen + d
        seqfunc (total,ts) len = (total + len,ts V.++ V.singleton (total + len))
        seqLens  = snd $ V.foldl' seqfunc (0,mempty) lens
        isClosed = distance v1 v3 <= 0.00001
        -- if the polyline is closed return a miter with the last point
        startCap = ( V.fromList [cap,cap]
                   , V.fromList [c1,c1]
                   , uvs
                   , V.fromList [v2,v2]
                   , V.fromList [prev,prev]
                   )
            where (uvs,cap,prev) = if isClosed
                                   -- no cap
                                   then (V.fromList [V2 0 d, V2 0 (-d)],v1,v3n)
                                   -- cap
                                   else let c = d *^ signorm (v2 - v1)
                                        in ( V.fromList [V2 (-d) d, V2 (-d) (-d)]
                                           , v1 - c
                                           , v1 - 2*c)
        endCap = ( V.fromList [cap,cap]
                 , V.fromList [c3,c3]
                 , uvs
                 , V.fromList [next,next]
                 , V.fromList [v3n,v3n]
                 )
            where (uvs,cap,next) = if isClosed
                                   -- no cap
                                   then ( V.fromList [ V2 totalLen d
                                                     , V2 totalLen (-d)
                                                     ]
                                        , v3
                                        , v2
                                        )
                                   -- cap
                                   else let c = d *^ signorm (v3 - v3n)
                                        in (V.fromList [ V2 totalEnd d
                                                       , V2 totalEnd (-d)
                                                       ]
                                           , v3 + c
                                           , v3 + 2*c
                                           )
        vcs  = V.toList $ V.zip3 verts colors seqLens
        zs   = zipWith3 strp vcs (drop 1 vcs) (drop 2 vcs)
        -- Expand the line into a triangle strip
        strp :: Unbox (f Float)
             => (V2 Float, f Float, Float) -> (V2 Float, f Float, Float)
             -> (V2 Float, f Float, Float) -> (Vector (V2 Float)
                                              ,Vector (f Float)
                                              ,Vector (V2 Float)
                                              ,Vector (V2 Float)
                                              ,Vector (V2 Float)
                                              )
        strp (a,_,_) (b,bc,l) (c,_,_) =
          ( V.fromList [b,b]
          , V.fromList [bc,bc]
          , V.fromList [V2 l d,V2 l (-d)]
          , V.fromList [c,c]
          , V.fromList [a,a]
          )
        (vs,cs,us,ns,ps) = unzip5 $ startCap : zs ++ [endCap]
      in Just (V.concat vs, V.concat cs, V.concat us, V.concat ns, V.concat ps, totalLen)
    | otherwise = Nothing


polylineRenderer :: (Foldable f, Unbox (f Float), Storable (f Float), Functor f)
                 => Context -> SumShader
                 -> Float -> Float
                 -> (LineCap,LineCap) -> Bool
                 -> PolylineData f
                 -> IO GLRenderer
polylineRenderer win sh thickness feather caps isTex (vs_,cs_,us_,ns_,ps_,totalLen) = do
  let toFrac :: Float -> GLfloat
      toFrac = realToFrac
      vs = V.map (fmap toFrac) vs_
      cs = V.map (fmap toFrac) cs_
      us = V.map (fmap toFrac) us_
      ns = V.map (fmap toFrac) ns_
      ps = V.map (fmap toFrac) ps_

  withVAO $ \vao -> withBuffers 5 $ \bufs@[vbuf, cbuf, buvbuf, nbuf, pbuf] -> do
    enableAttribsForLines isTex
    bufferAttrib PositionLoc 2 vbuf vs
    if isTex then bufferAttrib UVLoc 2 cbuf cs
             else bufferAttrib ColorLoc 4 cbuf cs
    bufferAttrib BezUVLoc 2 buvbuf us
    bufferAttrib NextLoc 2 nbuf ns
    bufferAttrib PrevLoc 2 pbuf ps
    glBindVertexArray 0

    let num = fromIntegral $ V.length vs_
        r t = do let (mv, a, m, mr) = unwrapTransform t
                 pj <- orthoContextProjection win
                 updateUniformsForLines (unShader sh) pj mv isTex a m mr
                                        thickness feather totalLen caps
                 drawBuffer (shProgram $ unShader sh) vao GL_TRIANGLE_STRIP num
        c = do withArray bufs $ glDeleteBuffers 5
               withArray [vao] $ glDeleteVertexArrays 1
    return (c,r)

-- | Creates and returns a renderer that renders a colored, expanded 2d polyline
-- projected in 2d space.
colorPolylineRenderer :: Context -> SumShader -> Float -> Float
                      -> (LineCap,LineCap) -> Vector (V2 Float)
                      -> Vector (V4 Float) -> IO GLRenderer
colorPolylineRenderer win psh thickness feather caps verts colors = do
  let empty = putStrLn "could not expand polyline" >> return mempty
      mpoly = expandPolyline verts colors thickness feather
  flip (maybe empty) mpoly $
    polylineRenderer win psh thickness feather caps False

-- | Creates and returns a renderer that renders a textured, expanded 2d
-- polyline projected in 2d space.
texPolylineRenderer :: Context -> SumShader -> Float
                    -> Float -> (LineCap,LineCap) -> Vector (V2 Float)
                    -> Vector (V2 Float) -> IO GLRenderer
texPolylineRenderer win psh thickness feather caps verts uvs = do
  let empty = putStrLn "could not expand polyline" >> return mempty
      mpoly = expandPolyline verts uvs thickness feather
  flip (maybe empty) mpoly $
    polylineRenderer win psh thickness feather caps True

-- | Binds the given textures to GL_TEXTURE0, GL_TEXTURE1, ... in ascending
-- order of the texture unit, runs the IO action and then unbinds the textures.
bindTexsAround :: [GLuint] -> IO () -> IO ()
bindTexsAround ts f = do
  mapM_ (uncurry bindTex) (zip ts [GL_TEXTURE0 ..])
  f
  glBindTexture GL_TEXTURE_2D 0
  where bindTex tex u = glActiveTexture u >> glBindTexture GL_TEXTURE_2D tex

bindTexAround :: GLuint -> IO () -> IO ()
bindTexAround tx f = bindTexsAround [tx] f

-- | Creates and returns a renderer that renders the given colored
-- geometry.
colorRenderer :: Context -> SumShader -> GLuint -> Vector (V2 Float)
              -> Vector (V4 Float) -> IO GLRenderer
colorRenderer window sh mode vs gs =
    withVAO $ \vao -> withBuffers 2 $ \[pbuf,cbuf] -> do
    let ps = V.map realToFrac $ V.concatMap (V.fromList . F.toList) vs :: Vector GLfloat
        cs = V.map realToFrac $ V.concatMap (V.fromList . F.toList) $ V.take (V.length vs) gs :: Vector GLfloat

    enableAttribsForTris False
    bufferAttrib PositionLoc 2 pbuf ps
    bufferAttrib ColorLoc 4 cbuf cs
    glBindVertexArray 0
    let num = fromIntegral $ V.length vs
        renderFunction t = do
            let (mv,a,m,mr) = unwrapTransform t
            pj <- orthoContextProjection window
            updateUniformsForTris (unShader sh) pj mv False a m mr
            drawBuffer (shProgram $ unShader sh) vao mode num
        cleanupFunction = do
            withArray [pbuf, cbuf] $ glDeleteBuffers 2
            withArray [vao] $ glDeleteVertexArrays 1
    return (cleanupFunction,renderFunction)

-- | Creates and returns a renderer that renders a textured
-- geometry.
textureRenderer :: Context -> SumShader -> GLuint -> Vector (V2 Float)
                -> Vector (V2 Float) -> IO GLRenderer
textureRenderer win sh mode vs uvs =
  withVAO $ \vao -> withBuffers 2 $ \[pbuf,cbuf] -> do
  let f xs = V.map realToFrac $ V.concatMap (V.fromList . F.toList) xs :: Vector GLfloat
      ps = f vs
      cs = f $ V.take (V.length vs) uvs

  enableAttribsForTris True
  bufferAttrib PositionLoc 2 pbuf ps
  bufferAttrib UVLoc 2 cbuf cs
  glBindVertexArray 0

  let num = fromIntegral $ V.length vs
      renderFunction t = do
        let (mv,a,m,mr) = unwrapTransform t
        print mr
        pj <- orthoContextProjection win
        updateUniformsForTris (unShader sh) pj mv True a m mr
        drawBuffer (shProgram $ unShader sh) vao mode num
      cleanupFunction = do
        withArray [pbuf, cbuf] $ glDeleteBuffers 2
        withArray [vao] $ glDeleteVertexArrays 1
  return (cleanupFunction,renderFunction)

bezAttributes :: (Foldable f, Unbox (f Float))
              => Vector (V2 Float)
              -> Vector (f Float)
              -> (Vector GLfloat, Vector GLfloat, Vector GLfloat)
bezAttributes vs cvs = (ps, cs, ws)
  where ps = V.map realToFrac $
             V.concatMap (V.fromList . F.toList) vs :: Vector GLfloat
        cs = V.map realToFrac $
               V.concatMap (V.fromList . F.toList) cvs :: Vector GLfloat
        getWinding i =
          let n = i * 3
              (a,b,c) = (vs V.! n, vs V.! (n + 1), vs V.! (n + 2))
              w = fromBool $ triangleArea a b c <= 0
          in V.fromList [ 0, 0, w
                        , 0.5, 0, w
                        , 1, 1, w
                        ]
        numBezs = floor $ (realToFrac $ V.length vs) / (3 :: Double)
        ws :: Vector GLfloat
        ws = V.concatMap getWinding $ V.generate numBezs id

bezRenderer :: (Foldable f, Unbox (f Float))
            => Bool -> Context -> SumShader
            -> Vector (V2 Float)
            -> Vector (f Float)
            -> IO GLRenderer
bezRenderer isTex window sh vs cvs = do
  let (ps, cs, ws) = bezAttributes vs cvs
  withVAO $ \vao -> withBuffers 3 $ \[pbuf, tbuf, cbuf] -> do
    enableAttribsForBezs isTex
    bufferAttrib PositionLoc 2 pbuf ps
    bufferAttrib BezLoc 3 tbuf ws
    if isTex
      then bufferAttrib UVLoc 2 cbuf cs
      else bufferAttrib ColorLoc 4 cbuf cs
    glBindVertexArray 0

    let cleanupFunction = do
            withArray [pbuf, tbuf, cbuf] $ glDeleteBuffers 3
            withArray [vao] $ glDeleteVertexArrays 1
        num = fromIntegral $ V.length vs
        renderFunction t = do
            pj <- orthoContextProjection window
            let (mv,a,m,mr) = unwrapTransform t
            updateUniformsForBezs (unShader sh) pj mv isTex a m mr
            drawBuffer (shProgram $ unShader sh) vao GL_TRIANGLES num
    return (cleanupFunction,renderFunction)

-- | Creates and returns a renderer that renders the given colored beziers.
colorBezRenderer :: Context -> SumShader
                 -> Vector (V2 Float) -> Vector (V4 Float) -> IO GLRenderer
colorBezRenderer = bezRenderer False

-- | Creates and returns a renderer that renders the given textured beziers.
textureBezRenderer :: Context -> SumShader
                   -> Vector (V2 Float) -> Vector (V2 Float) -> IO GLRenderer
textureBezRenderer = bezRenderer True

-- | Creates and returns a renderer that masks a textured rectangular area with
-- another texture.
maskRenderer :: Context -> SumShader -> GLuint -> Vector (V2 Float)
             -> Vector (V2 Float) -> IO GLRenderer
maskRenderer win sh mode vs uvs =
    withVAO $ \vao -> withBuffers 2 $ \[pbuf, uvbuf] -> do
        let vs'  = V.map realToFrac $
                     V.concatMap (V.fromList . F.toList) vs :: Vector GLfloat
            uvs' = V.map realToFrac $
                     V.concatMap (V.fromList . F.toList) uvs :: Vector GLfloat

        enableAttribsForMask
        bufferAttrib PositionLoc 2 pbuf vs'
        bufferAttrib UVLoc 2 uvbuf uvs'
        glBindVertexArray 0

        let cleanup = do withArray [pbuf, uvbuf] $ glDeleteBuffers 2
                         withArray [vao] $ glDeleteVertexArrays 1
            num = fromIntegral $ V.length vs
            render t = do
                let (mv,a,m,_) = unwrapTransform t
                pj <- orthoContextProjection win
                updateUniformsForMask (unShader sh) pj mv a m 0 1
                drawBuffer (shProgram $ unShader sh) vao mode num
        return (cleanup,render)

-- | Creates a rendering that masks an IO () drawing computation with the alpha
-- value of another.
alphaMask :: Context -> SumShader -> IO () -> IO () -> IO GLRenderer
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

transformRenderer :: PictureTransform -> GLRenderer -> GLRenderer
transformRenderer t (c, r) = (c, r . (t <>))
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
    return r

withBuffers :: Int -> ([GLuint] -> IO b) -> IO b
withBuffers n f = do
    bufs <- allocaArray n $ \ptr -> do
        glGenBuffers (fromIntegral n) ptr
        peekArray (fromIntegral n) ptr
    f bufs

bufferAttrib :: (Storable a, Unbox a)
             => AttribLoc -> GLint -> GLuint -> Vector a -> IO ()
bufferAttrib attr n buf as = do
    let loc = locToGLuint attr
        asize = V.length as * sizeOf (V.head as)
        f = S.convert :: (G.Vector Vector a, Storable a)
                      => Vector a -> S.Vector a
    glBindBuffer GL_ARRAY_BUFFER buf

    S.unsafeWith (f as) $ \ptr ->
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
    clearErrors "drawBuffer:glBindVertex"
    glDrawArrays mode 0 num
    clearErrors "drawBuffer:glDrawArrays"

clearErrors :: String -> IO ()
clearErrors str = do
    err' <- glGetError
    when (err' /= 0) $ errorWithStackTrace $ unwords [str, show err']

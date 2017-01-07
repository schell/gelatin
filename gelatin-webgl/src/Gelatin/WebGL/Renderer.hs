{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE JavaScriptFFI         #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE RecordWildCards         #-}
module Gelatin.WebGL.Renderer where

import           Control.Concurrent                                  (threadDelay)
import           Control.Concurrent.STM
import System.Exit (exitFailure)
import           Control.Monad                                       (when)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Control.Monad.Reader
import           Control.Monad.Trans.Either
import Data.List (isPrefixOf)
import Data.Foldable (toList)
import           Data.Bits                                           ((.|.))
import           Data.Function                                       (fix)
import           Data.Maybe                                          (catMaybes)
import           Data.Vector.Unboxed                                 (Unbox,
                                                                      Vector)
import qualified Data.Vector.Unboxed                                 as V
import           GHCJS.DOM                                           (currentDocument)
import           GHCJS.DOM.EventM                                    (on)
import           GHCJS.DOM.JSFFI.Generated.Document                  (createElement)
import qualified GHCJS.DOM.JSFFI.Generated.Document                  as DOM (getBody)
import           GHCJS.DOM.JSFFI.Generated.Element                   (load)
import           GHCJS.DOM.JSFFI.Generated.HTMLCanvasElement         (castToHTMLCanvasElement,
                                                                      getContext)
import qualified GHCJS.DOM.JSFFI.Generated.HTMLCanvasElement         as Canvas
import qualified GHCJS.DOM.JSFFI.Generated.HTMLImageElement          as Image
import           GHCJS.DOM.JSFFI.Generated.OESVertexArrayObject
import           GHCJS.DOM.JSFFI.Generated.WebGLRenderingContextBase
import           GHCJS.DOM.Types
import           GHCJS.Marshal
import           GHCJS.Types
import Foreign.Marshal.Utils (fromBool)
--------------------------------------------------------------------------------
import           Gelatin
import           Gelatin.Shaders
import           Gelatin.WebGL.Common
import           Gelatin.WebGL.Shaders
--------------------------------------------------------------------------------
-- Setup and underlying machinery
--------------------------------------------------------------------------------
runIOMaybe :: MonadIO m => String -> IO (Maybe a) -> EitherT String m a
runIOMaybe str f = liftIO f >>= \case
  Nothing -> fail str
  Just a  -> return a

runWebGLT :: WebGLT m a -> WebGLRenderingContextBase -> m (Either String a)
runWebGLT f = runReaderT (runEitherT f)

webDocument :: MonadIO m => EitherT String m Document
webDocument = runIOMaybe "Could not access the document." currentDocument

webBody :: MonadIO m => EitherT String m HTMLElement
webBody = do
  doc <- webDocument
  runIOMaybe "Could not return the body element." $ DOM.getBody doc

webCreateElement :: MonadIO m => String -> EitherT String m Element
webCreateElement str = do
  doc <- webDocument
  runIOMaybe ("Could not create a " ++ show str ++ " element.") $
    createElement doc (Just str)

webCanvasAndContext :: MonadIO m
                    => EitherT String m (HTMLCanvasElement, WebGLRenderingContextBase)
webCanvasAndContext = do
  cnvs <- liftIO . castToHTMLCanvasElement =<< webCreateElement "canvas"
  gl   <- getContext cnvs "webgl"
  gl2  <- if isNull gl
             then do gl2 <- getContext cnvs "experimental-webgl"
                     if isNull gl2
                        then fail "Could not create the WebGL context."
                        else return gl2
             else return gl
  return (cnvs, WebGLRenderingContextBase gl2)

--------------------------------------------------------------------------------
-- WebGL helpers
--------------------------------------------------------------------------------
clearErrors :: WebGLRenderingContextBase -> String -> IO ()
clearErrors gl str = do
  err <- getError gl
  when (err /= 0) $ error $ unwords [str, show err]

foreign import javascript unsafe "new Float32Array($1)"
  allocFloat32Array_js :: JSVal -> IO JSVal

allocFloat32Array :: ToJSVal a => [a] -> IO Float32Array
allocFloat32Array xs =
  Float32Array <$> (toJSValListOf xs >>= allocFloat32Array_js)

getVAOExt :: WebGLRenderingContextBase -> IO OESVertexArrayObject
getVAOExt gl =
  liftIO . fromJSValUnchecked =<< getExtension gl "OES_vertex_array_object"

-- | Alloc and use a vertex array object.
withVAO :: (OESVertexArrayObject -> WebGLVertexArrayObjectOES -> Gelatin b)
        -> Gelatin b
withVAO f = do
  gl       <- asks gelRenderingContext
  ext      <- liftIO $ getVAOExt gl
  Just vao <- createVertexArrayOES ext
  bindVertexArrayOES ext $ Just vao
  r        <- f ext vao
  liftIO $ clearErrors gl "withVAO"
  return r

-- | Alloc and use some number of buffers in an operation.
withBuffers :: Int
            -> ([WebGLBuffer] -> Gelatin b)
            -> Gelatin b
withBuffers n f = do
  gl      <- asks gelRenderingContext
  buffers <- sequenceA $ replicate n (liftIO $ createBuffer gl)
  let goodBuffs = catMaybes buffers
  f goodBuffs

-- | Marshal an attributes data to the GPU.
bufferAttrib :: (ToJSVal a, Unbox a)
             => Simple2DAttrib -> GLint -> WebGLBuffer -> Vector a
             -> Gelatin ()
bufferAttrib attr n buf as = do
  gl <- asks gelRenderingContext
  bindBuffer gl ARRAY_BUFFER $ Just buf
  let loc = attribToGLuint attr

  Float32Array dat <- liftIO $ allocFloat32Array (V.toList as)
  bufferData gl ARRAY_BUFFER (Just $ ArrayBuffer dat) STATIC_DRAW
  enableVertexAttribArray gl loc
  vertexAttribPointer gl loc n FLOAT False 0 0

drawBuffer
  :: WebGLRenderingContextBase -> WebGLProgram -> WebGLVertexArrayObjectOES
  -> GLenum -> GLsizei -> IO ()
drawBuffer gl program vao mode num = do
  ext <- getVAOExt gl
  useProgram gl (Just program)
  bindVertexArrayOES ext (Just vao)
  clearErrors gl "drawBuffer:bindVertexArrayOES"
  drawArrays gl mode 0 num
  clearErrors gl "drawBuffer:drawArrays"

bufferImageData :: MonadIO m => WebGLRenderingContextBase -> HTMLImageElement -> GLenum -> GLenum -> m ()
bufferImageData gl img imgfmt pxfmt = do
  texImage2D gl
    TEXTURE_2D
    0
    RGBA
    imgfmt
    pxfmt
    (Just img)
  liftIO $ clearErrors gl "bufferImageData"

-- | Creates an IO () drawing computation that masks an IO () drawing
-- computation with another using a stencil test.
gelStencilMask :: WebGLRenderingContextBase -> IO () -> IO () -> IO ()
gelStencilMask gl r2 r1  = do
  clear gl DEPTH_BUFFER_BIT
  -- Enable stencil testing
  enable gl STENCIL_TEST
  -- Disable writing frame buffer color components
  colorMask gl False False False False
  -- Disable writing into the depth buffer
  depthMask gl False
  -- Enable writing to all bits of the stencil mask
  stencilMask gl 0xFF
  -- Clear the stencil buffer
  clear gl STENCIL_BUFFER_BIT
  stencilFunc gl NEVER 0 1
  stencilOp gl INVERT INVERT INVERT
  liftIO r1

  colorMask gl True True True True
  depthMask gl True
  stencilFunc gl EQUAL 1 1
  stencilOp gl ZERO ZERO ZERO
  liftIO r2
  disable gl STENCIL_TEST

applyOption :: WebGLRenderingContextBase
            -> (c, rs -> IO ())
            -> RenderingOption
            -> (c, rs -> IO ())
applyOption gl (c, r) StencilMaskOption =
  (c, \rs -> gelStencilMask gl (r rs) (r rs))
--------------------------------------------------------------------------------
-- Texture Support
--------------------------------------------------------------------------------
loadImage :: MonadIO m => FilePath -> EitherT String m HTMLImageElement
loadImage file = do
  img <- liftIO . castToHTMLImageElement =<< webCreateElement "img"
  Image.setSrc img file
  t <- liftIO $ newTVarIO False
  cleanup <- liftIO $ on img load $
    liftIO $ atomically $ writeTVar t True
  fix $ \loop -> liftIO (readTVarIO t) >>= \case
    True  -> liftIO cleanup          >> return img
    False -> liftIO (threadDelay 1)  >> loop

loadTexture :: (MonadIO m, MonadReader WebGLRenderingContextBase m)
            => FilePath
            -> EitherT String m (WebGLTexture, V2 Int)
loadTexture file = do
  img <- loadImage file
  gl  <- ask
  tex <- runIOMaybe "Could not create texture." $ createTexture gl
  activeTexture gl TEXTURE0
  bindTexture gl TEXTURE_2D $ Just tex
  bufferImageData gl img RGBA UNSIGNED_BYTE
  sz  <- V2 <$> Image.getWidth img <*> Image.getHeight img
  -- | TODO: If size has a power of 2 width and height, generate mipmaps
  --generateMipmap gl TEXTURE_2D
  --texParameteri  gl TEXTURE_2D TEXTURE_MIN_FILTER NEAREST_MIPMAP_NEAREST
  texParameteri  gl TEXTURE_2D TEXTURE_MAG_FILTER LINEAR
  texParameteri  gl TEXTURE_2D TEXTURE_MIN_FILTER LINEAR
  texParameteri  gl TEXTURE_2D TEXTURE_WRAP_S CLAMP_TO_EDGE
  texParameteri  gl TEXTURE_2D TEXTURE_WRAP_T CLAMP_TO_EDGE
  bindTexture    gl TEXTURE_2D Nothing
  return (tex, sz)

bindTexsAround :: MonadIO m
               => WebGLRenderingContextBase
               -> [WebGLTexture]
               -> m a
               -> m a
bindTexsAround gl texs f = do
  mapM_ (uncurry bindTex) (zip texs [TEXTURE0 ..])
  a  <- f
  bindTexture gl TEXTURE_2D Nothing
  return a
    where bindTex tex u = do
            liftIO $ putStrLn "Binding tex"
            activeTexture gl u
            bindTexture gl TEXTURE_2D $ Just tex
--------------------------------------------------------------------------------
-- Projection Support
--------------------------------------------------------------------------------
webCanvasSize :: MonadIO m => HTMLCanvasElement -> m (V2 Int)
webCanvasSize canvas = V2 <$> Canvas.getWidth canvas <*> Canvas.getHeight canvas

orthoContextProjection :: HTMLCanvasElement -> IO (M44 Float)
orthoContextProjection canvas = do
  V2 w h <- fmap fromIntegral <$> webCanvasSize canvas
  return $ ortho 0 w h 0 0 1
--------------------------------------------------------------------------------
-- Updating Shader Uniforms
--------------------------------------------------------------------------------
-- | Updates uniforms for rendering triangles.
updateUniformsForTris
  :: WebGLRenderingContextBase -> WGLShader -> M44 Float -> M44 Float -> Bool
  -> Float -> V4 Float -> Maybe (V4 Float) -> IO ()
updateUniformsForTris gl sh pj mv hasUV a m mr =
  updateUniforms gl (uniformsForTris pj mv hasUV a m mr) sh
{-# INLINE updateUniformsForTris #-}

-- | Updates uniforms for rendering loop-blinn beziers.
updateUniformsForBezs
  :: WebGLRenderingContextBase -> WGLShader -> M44 Float -> M44 Float -> Bool
  -> Float -> V4 Float -> Maybe (V4 Float) -> IO ()
updateUniformsForBezs gl sh pj mv hasUV a m mr =
  updateUniforms gl (uniformsForBezs pj mv hasUV a m mr) sh
{-# INLINE updateUniformsForBezs #-}

-- | Updates uniforms for rendering projected polylines.
updateUniformsForLines
  :: WebGLRenderingContextBase -> WGLShader -> M44 Float -> M44 Float -> Bool
  -> Float -> V4 Float -> Maybe (V4 Float) -> Float -> Float -> Float
  -> (LineCap,LineCap) -> IO ()
updateUniformsForLines gl sh pj mv hasUV a m mr thickness feather sumlength caps =
  let us = uniformsForLines pj mv hasUV a m mr thickness feather sumlength caps
  in updateUniforms gl us sh
{-# INLINE updateUniformsForLines #-}

-- | Updates uniforms for rendering alpha masking.
updateUniformsForMask
  :: WebGLRenderingContextBase -> WGLShader -> M44 Float -> M44 Float -> Float
  -> V4 Float -> GLuint -> GLuint -> IO ()
updateUniformsForMask gl sh pj mv a m main mask =
  updateUniforms gl (uniformsForMask pj mv a m main mask) sh
{-# INLINE updateUniformsForMask #-}

updateUniforms
  :: WebGLRenderingContextBase -> [Simple2DUniform] -> WGLShader -> IO ()
updateUniforms gl us s = mapM_ (\u -> updateUniform gl u s) us
{-# INLINE updateUniforms #-}

updateUniform
  :: WebGLRenderingContextBase -> Simple2DUniform -> WGLShader -> IO ()
updateUniform gl u s = withUniform (simple2DUniformIdentifier u) s $ \p loc -> do
  useProgram gl $ Just p
  uniformUpdateFunc gl u loc
{-# INLINE updateUniform #-}

v4ToList :: V4 a -> [a]
v4ToList (V4 a b c d) = [a,b,c,d]

m44ToList :: M44 a -> [a]
m44ToList = concatMap v4ToList . v4ToList

uniformUpdateFunc :: WebGLRenderingContextBase
                  -> Simple2DUniform -> WebGLUniformLocation -> IO ()
uniformUpdateFunc gl (UniformPrimType p) u =
  uniform1i gl (Just u) $ fromIntegral $ fromEnum p
uniformUpdateFunc gl (UniformProjection m44) u = do
  array <- allocFloat32Array $ m44ToList $ transpose m44
  uniformMatrix4fv gl (Just u) False $ Just array
uniformUpdateFunc gl (UniformModelView m44) u = do
  array <- allocFloat32Array $ m44ToList $ transpose m44
  uniformMatrix4fv gl (Just u) False $ Just array
uniformUpdateFunc gl (UniformThickness t) u =
  uniform1f gl (Just u) $ realToFrac t
uniformUpdateFunc gl (UniformFeather f) u =
  uniform1f gl (Just u) $ realToFrac f
uniformUpdateFunc gl (UniformSumLength l) u =
  uniform1f gl (Just u) $ realToFrac l
uniformUpdateFunc gl (UniformLineCaps (capx, capy)) u =
  let [x,y] = map (fromIntegral . fromEnum) [capx,capy] in uniform2f gl (Just u) x y
uniformUpdateFunc gl (UniformHasUV has) u = uniform1i gl (Just u) $ if has then 1 else 0
uniformUpdateFunc gl (UniformSampler s) u = uniform1i gl (Just u) $ fromIntegral s
uniformUpdateFunc gl (UniformMainTex t) u = uniform1i gl (Just u) $ fromIntegral t
uniformUpdateFunc gl (UniformMaskTex t) u = uniform1i gl (Just u) $ fromIntegral t
uniformUpdateFunc gl (UniformAlpha a) u = uniform1f gl (Just u) $ realToFrac a
uniformUpdateFunc gl (UniformMult v) u =
  let (V4 r g b a) = realToFrac <$> v in uniform4f gl (Just u) r g b a
uniformUpdateFunc gl (UniformShouldReplaceColor s) u =
  uniform1i gl (Just u) $ if s then 1 else 0
uniformUpdateFunc gl (UniformReplaceColor c) u =
  let (V4 r g b a) = realToFrac <$> c in uniform4f gl (Just u) r g b a
{-# INLINE uniformUpdateFunc #-}

withUniform
  :: String -> WGLShader -> (WebGLProgram -> WebGLUniformLocation -> IO ())
  -> IO ()
withUniform name (Shader p ls) f =
    case lookup name ls of
        Nothing  -> do putStrLn $ "could not find uniform " ++ name
                       exitFailure
        Just loc -> f p loc
{-# INLINE withUniform #-}
--------------------------------------------------------------------------------
-- Attribute Toggling
--------------------------------------------------------------------------------
locToGLuint :: Simple2DAttrib -> GLuint
locToGLuint = fromIntegral . fromEnum

-- | Enables the provided attributes and disables all others.
onlyEnableAttribs :: WebGLRenderingContextBase -> [Simple2DAttrib] -> IO ()
onlyEnableAttribs gl atts = do
  mapM_ (disableVertexAttribArray gl . locToGLuint) allAttribs
  mapM_ (enableVertexAttribArray gl . locToGLuint) atts

enableAttribsForTris :: WebGLRenderingContextBase -> Bool -> IO ()
enableAttribsForTris gl True = onlyEnableAttribs gl [PositionLoc,UVLoc]
enableAttribsForTris gl False = onlyEnableAttribs gl [PositionLoc,ColorLoc]

enableAttribsForBezs :: WebGLRenderingContextBase -> Bool -> IO ()
enableAttribsForBezs gl True = onlyEnableAttribs gl [PositionLoc,UVLoc,BezLoc]
enableAttribsForBezs gl False = onlyEnableAttribs gl [PositionLoc,ColorLoc,BezLoc]

enableAttribsForLines :: WebGLRenderingContextBase -> Bool -> IO ()
enableAttribsForLines gl True =
  onlyEnableAttribs gl [PositionLoc,UVLoc,BezUVLoc,NextLoc,PrevLoc]
enableAttribsForLines gl False =
  onlyEnableAttribs gl [PositionLoc,ColorLoc,BezUVLoc,NextLoc,PrevLoc]

enableAttribsForMask :: WebGLRenderingContextBase -> IO ()
enableAttribsForMask gl = onlyEnableAttribs gl [PositionLoc,UVLoc]
--------------------------------------------------------------------------------
-- Polyline Renderers
--------------------------------------------------------------------------------
polylineRenderer
  :: (Unbox (f Float), Foldable f, Functor f)
  => WGLShader
  -> Float -> Float
  -> (LineCap,LineCap) -> Bool
  -> PolylineData f
  -> Gelatin Renderer2
polylineRenderer sh thickness feather caps isTex (vs_,cs_,us_,ns_,ps_,totalLen) = do
  gl <- asks gelRenderingContext
  let toFrac :: Float -> GLfloat
      toFrac = realToFrac
      vs = V.concatMap (V.fromList . toList . fmap toFrac) vs_
      cs = V.concatMap (V.fromList . toList . fmap toFrac) cs_
      us = V.concatMap (V.fromList . toList . fmap toFrac) us_
      ns = V.concatMap (V.fromList . toList . fmap toFrac) ns_
      ps = V.concatMap (V.fromList . toList . fmap toFrac) ps_

  withVAO $ \ext vao -> withBuffers 5 $ \bufs@[vbuf, cbuf, buvbuf, nbuf, pbuf] -> do
    liftIO $ enableAttribsForLines gl isTex
    bufferAttrib PositionLoc 2 vbuf vs
    if isTex then bufferAttrib UVLoc 2 cbuf cs
             else bufferAttrib ColorLoc 4 cbuf cs
    bufferAttrib BezUVLoc 2 buvbuf us
    bufferAttrib NextLoc 2 nbuf ns
    bufferAttrib PrevLoc 2 pbuf ps
    bindVertexArrayOES ext Nothing

    canvas <- asks gelCanvas

    let num = fromIntegral $ V.length vs_
        r t = do let (mv, a, m, mr) = unwrapTransforms t
                 pj <- orthoContextProjection canvas
                 updateUniformsForLines gl sh pj mv isTex a m mr
                                        thickness feather totalLen caps
                 drawBuffer gl (shProgram sh) vao TRIANGLE_STRIP num
        c = do mapM_ (deleteBuffer gl . Just) bufs
               deleteVertexArrayOES ext $ Just vao
    return (c,r)

-- | Creates and returns a renderer that renders a colored, expanded 2d polyline
-- projected in 2d space.
colorPolylineRenderer :: WGLShader
                      -> Float -> Float
                      -> (LineCap,LineCap) -> Vector (V2 Float)
                      -> Vector (V4 Float)
                      -> Gelatin Renderer2
colorPolylineRenderer sh thickness feather caps verts colors
  | Just poly <- expandPolyline verts colors thickness feather =
      polylineRenderer sh thickness feather caps False poly
  | otherwise = liftIO $ putStrLn "could not expand polyline" >> return mempty

-- | Creates and returns a renderer that renders a textured, expanded 2d
-- polyline projected in 2d space.
texPolylineRenderer :: WGLShader
                    -> Float -> Float
                    -> (LineCap,LineCap)
                    -> Vector (V2 Float) -> Vector (V2 Float)
                    -> Gelatin Renderer2
texPolylineRenderer sh thickness feather caps verts uvs
  | Just poly <- expandPolyline verts uvs thickness feather =
      polylineRenderer sh thickness feather caps True poly
  | otherwise = liftIO $ putStrLn "could not expand polyline" >> return mempty

-- | Creates and returns a renderer that renders the given colored
-- geometry.
colorRenderer :: WGLShader -> GLuint -> Vector (V2 Float) -> Vector (V4 Float)
              -> Gelatin Renderer2
colorRenderer sh mode vs gs =
  withVAO $ \ext vao -> withBuffers 2 $ \[pbuf,cbuf] -> do
    gl     <- asks gelRenderingContext
    canvas <- asks gelCanvas
    let ps = V.map realToFrac $ V.concatMap (V.fromList . toList) vs :: Vector GLfloat
        cs = V.map realToFrac $ V.concatMap (V.fromList . toList) $ V.take (V.length vs) gs :: Vector GLfloat

    liftIO $ enableAttribsForTris gl False
    bufferAttrib PositionLoc 2 pbuf ps
    bufferAttrib ColorLoc 4 cbuf cs
    bindVertexArrayOES ext Nothing
    let num = fromIntegral $ V.length vs
        renderFunction t = do
          bindTexture gl TEXTURE_2D Nothing
          let (mv,a,m,mr) = unwrapTransforms t
          pj <- orthoContextProjection canvas
          updateUniformsForTris gl sh pj mv False a m mr
          drawBuffer gl (shProgram sh) vao mode num
        cleanupFunction = do
          mapM_ (deleteBuffer gl . Just) [pbuf, cbuf]
          deleteVertexArrayOES ext $ Just vao
    return (cleanupFunction, renderFunction)

-- | Creates and returns a renderer that renders a textured
-- geometry.
textureRenderer :: WGLShader -> GLuint -> Vector (V2 Float) -> Vector (V2 Float)
                -> Gelatin Renderer2
textureRenderer sh mode vs uvs =
  withVAO $ \ext vao -> withBuffers 2 $ \[pbuf,cbuf] -> do
  gl     <- asks gelRenderingContext
  canvas <- asks gelCanvas
  let f xs = V.map realToFrac $ V.concatMap (V.fromList . toList) xs :: Vector GLfloat
      ps = f vs
      cs = f $ V.take (V.length vs) uvs

  liftIO $ enableAttribsForTris gl True
  bufferAttrib PositionLoc 2 pbuf ps
  bufferAttrib UVLoc 2 cbuf cs
  bindVertexArrayOES ext Nothing

  let num = fromIntegral $ V.length vs
      renderFunction t = do
        let (mv,a,m,mr) = unwrapTransforms t
        pj <- orthoContextProjection canvas
        updateUniformsForTris gl sh pj mv True a m mr
        drawBuffer gl (shProgram sh) vao mode num
      cleanupFunction = do
        mapM_ (deleteBuffer gl . Just) [pbuf, cbuf]
        deleteVertexArrayOES ext $ Just vao
  return (cleanupFunction,renderFunction)

-- Package up some attributes for buffering
bezAttributes
  :: (Foldable f, Unbox (f Float))
  => Vector (V2 Float) -> Vector (f Float)
  -> (Vector GLfloat, Vector GLfloat, Vector GLfloat)
bezAttributes vs cvs = (ps, cs, ws)
  where ps = V.map realToFrac $
             V.concatMap (V.fromList . toList) vs :: Vector GLfloat
        cs = V.map realToFrac $
               V.concatMap (V.fromList . toList) cvs :: Vector GLfloat
        getWinding i =
          let n = i * 3
              (a,b,c) = (vs V.! n, vs V.! (n + 1), vs V.! (n + 2))
              w = fromBool $ triangleArea a b c <= 0
          in V.fromList [ 0, 0, w
                        , 0.5, 0, w
                        , 1, 1, w
                        ]
        numBezs = floor $ realToFrac (V.length vs) / (3 :: Double)
        ws :: Vector GLfloat
        ws = V.concatMap getWinding $ V.generate numBezs id

bezRenderer
  :: (Foldable f, Unbox (f Float))
  => Bool -> WGLShader -> Vector (V2 Float) -> Vector (f Float)
  -> Gelatin Renderer2
bezRenderer isTex sh vs cvs = do
  let (ps, cs, ws) = bezAttributes vs cvs
  withVAO $ \ext vao -> withBuffers 3 $ \[pbuf, tbuf, cbuf] -> do
    gl     <- asks gelRenderingContext
    canvas <- asks gelCanvas
    liftIO $ enableAttribsForBezs gl isTex
    bufferAttrib PositionLoc 2 pbuf ps
    bufferAttrib BezLoc 3 tbuf ws
    if isTex
      then bufferAttrib UVLoc 2 cbuf cs
      else bufferAttrib ColorLoc 4 cbuf cs
    bindVertexArrayOES ext Nothing

    let cleanupFunction = do
          mapM_ (deleteBuffer gl . Just) [pbuf, tbuf, cbuf]
          deleteVertexArrayOES ext $ Just vao
        num = fromIntegral $ V.length vs
        renderFunction t = do
          unless isTex $ bindTexture gl TEXTURE_2D Nothing
          pj <- orthoContextProjection canvas
          let (mv,a,m,mr) = unwrapTransforms t
          updateUniformsForBezs gl sh pj mv isTex a m mr
          drawBuffer gl (shProgram sh) vao TRIANGLES num
    return (cleanupFunction,renderFunction)

-- | Creates and returns a renderer that renders the given colored beziers.
colorBezRenderer :: WGLShader -> Vector (V2 Float) -> Vector (V4 Float)
                 -> Gelatin Renderer2
colorBezRenderer = bezRenderer False

-- | Creates and returns a renderer that renders the given textured beziers.
textureBezRenderer :: WGLShader -> Vector (V2 Float) -> Vector (V2 Float)
                   -> Gelatin Renderer2
textureBezRenderer = bezRenderer True


--------------------------------------------------------------------------------
-- Backends
--------------------------------------------------------------------------------
rgbaCompiler :: GelatinContext -> WGLShader
             -> GeometryCompiler V2V4 (V2 Float) Float Raster
rgbaCompiler gl sh = GeometryCompiler runShape runLine
  where runShape vt vx = runReaderT (s vt vx) gl
        runLine  st vx = runReaderT (l st vx) gl
        s VertexTriangles = uncurry (colorRenderer sh TRIANGLES) . V.unzip
        s VertexStrip     = uncurry (colorRenderer sh TRIANGLE_STRIP) . V.unzip
        s VertexFan       = uncurry (colorRenderer sh TRIANGLE_FAN) . V.unzip
        s VertexBeziers   = uncurry (colorBezRenderer sh) . V.unzip
        l Stroke{..}      =
          uncurry (colorPolylineRenderer sh strokeWidth strokeFeather strokeLineCaps) . V.unzip

uvCompiler :: GelatinContext -> WGLShader
           -> GeometryCompiler V2V2 (V2 Float) Float Raster
uvCompiler ctx sh = GeometryCompiler runShape runLine
  where runShape vt vx = runReaderT (s vt vx) ctx
        runLine  st vx = runReaderT (l st vx) ctx
        s VertexTriangles = uncurry (textureRenderer sh TRIANGLES) . V.unzip
        s VertexStrip     = uncurry (textureRenderer sh TRIANGLE_STRIP) . V.unzip
        s VertexFan       = uncurry (textureRenderer sh TRIANGLE_FAN) . V.unzip
        s VertexBeziers   = uncurry (textureBezRenderer sh) . V.unzip
        l Stroke{..}      =
          uncurry (texPolylineRenderer sh strokeWidth strokeFeather strokeLineCaps) . V.unzip

webglOps :: GelatinContext -> BackendOps WebGLTexture a
webglOps (GelatinContext canvas gl) =
  BackendOps{ backendOpGetFramebufferSize = webCanvasSize canvas
            , backendOpGetWindowSize      = webCanvasSize canvas
            , backendOpClearWindow        = gelClearWindow
            , backendOpUpdateWindow       = return () -- non op!
            , backendOpSetClearColor      = gelSetClearColor . fmap realToFrac
            , backendOpAllocTexture       = gelAllocTexture
            , backendOpBindTextures       = bindTexsAround gl
            , backendOpGetEvents          = return []
            }
  where gelClearWindow = do
          V2 w h <- webCanvasSize canvas
          viewport gl 0 0 (fromIntegral w) (fromIntegral h)
          clear gl (COLOR_BUFFER_BIT .|. DEPTH_BUFFER_BIT)
        gelSetClearColor (V4 r g b a) = clearColor gl r g b a
        gelAllocTexture file = runWebGLT (loadTexture file) gl >>= \case
              Left err -> putStrLn err >> return Nothing
              Right t  -> return $ Just t

webglV2V4 :: WGLShader -> Gelatin WebGLV2V4
webglV2V4 sh = do
  ctx@(GelatinContext _ gl) <- ask
  let comp = BackendComp{ backendCompApplyOption = applyOption gl
                        , backendCompCompiler    = rgbaCompiler ctx sh
                        }
  return Backend{ backendOps = webglOps ctx
                , backendCompiler = comp
                }

webglV2V2 :: WGLShader -> Gelatin WebGLV2V2
webglV2V2 sh = do
  ctx@(GelatinContext _ gl) <- ask
  let comp = BackendComp{ backendCompApplyOption = applyOption gl
                        , backendCompCompiler    = uvCompiler ctx sh
                        }
  return Backend{ backendOps = webglOps ctx
                , backendCompiler = comp
                }
--------------------------------------------------------------------------------
-- Getting a WebGL Backend(s)
--------------------------------------------------------------------------------
startup :: MonadIO m => Int -> Int
        -> EitherT String m (HTMLCanvasElement, WebGLRenderingContextBase)
startup w h = do
  (canvas, gl) <- webCanvasAndContext
  Canvas.setWidth canvas w
  Canvas.setHeight canvas h
  viewport gl 0 0 (fromIntegral w) (fromIntegral h)
  enable gl BLEND
  blendFunc gl SRC_ALPHA ONE_MINUS_SRC_ALPHA
  clearColor gl 0 0 0 1
  clear gl (COLOR_BUFFER_BIT .|. DEPTH_BUFFER_BIT)
  liftIO $ putStrLn "Got canvas and context."
  return (canvas, gl)

startupWebGLBackends :: MonadIO m => Int -> Int -> FilePath -> FilePath
                     -> EitherT String m WebGLBackends
startupWebGLBackends w h vpath fpath = do
  ctx <- uncurry GelatinContext <$> startup w h
  liftIO $ flip runReaderT ctx $ do
    sh <- loadShaderRemote vpath fpath
    WebGLBackends <$> webglV2V4 sh
                  <*> webglV2V2 sh
                  <*> pure (gelRenderingContext ctx)
                  <*> pure (gelCanvas ctx)

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PartialTypeSignatures #-}
module Gelatin.GL.Renderer (
    -- * Renderer
    GLRenderer,
    Context(..),
    -- * Loading textures
    loadTexture,
    loadTextureUnit,
    unloadTexture,
    loadImageAsTexture,
    -- * Line rendering
    projectedPolylineRenderer,
    -- * Triangle rendering
    colorRenderer,
    textureRenderer,
    textureUnitRenderer,
    filledTriangleRenderer,
    -- * Bezier rendering
    colorBezRenderer,
    textureBezRenderer,
    textureBezUnitRenderer,
    filledBezierRenderer,
    -- * Font rendering
    colorFontRenderer,
    fontCurves,
    fontGeom,
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

import Gelatin.GL.Shader
import Gelatin.GL.Common
import Gelatin.Picture
import Linear
import Graphics.Text.TrueType
import Graphics.GL.Core33
import Graphics.GL.Types
import Codec.Picture.Types
import Codec.Picture (readImage)
import Foreign.Marshal.Array
import Foreign.Marshal.Utils
import Foreign.C.String
import Foreign.Storable
import Foreign.Ptr
import Data.Renderable
import Data.Monoid
import Data.Maybe
import Data.Vector.Storable (Vector,unsafeWith)
import qualified Data.Vector.Unboxed as UV
import Control.Monad
import Control.Applicative
import System.Directory
import System.IO
import System.Exit
import qualified Data.ByteString.Char8 as B
import qualified Data.Foldable as F
import GHC.Stack
--------------------------------------------------------------------------------
-- GLRenderers
--------------------------------------------------------------------------------
-- | Creates and returns a renderer that renders an expanded 2d polyline
-- projected in 3d space.
projectedPolylineRenderer :: Context -> ProjectedPolylineShader -> Float
                           -> Float -> (LineCap,LineCap) -> [V2 Float]
                           -> [V4 Float] -> IO GLRenderer
projectedPolylineRenderer win psh thickness feather (capx,capy) verts colors
    | (v1:v2:_) <- verts
    , (c1:_:_) <- colors = do
    let v3:v3n:_ = reverse verts
        c3:_:_ = reverse $ take (length verts) colors
        -- clamp the lower bound of our thickness to 1
        absthick = max thickness 1
        d = fromIntegral (ceiling $ absthick + 2.5 * feather :: Integer)
        lens = 0 : zipWith distance verts (drop 1 verts)
        totalLen = sum lens
        totalEnd = totalLen + d
        seqfunc (total,ts) len = (total + len,ts ++ [total + len])
        seqLens  = snd $ foldl seqfunc (0,[]) lens
        isClosed = distance v1 v3 <= 0.00001
        -- if the polyline is closed return a miter with the last point
        startCap = ([cap,cap], [c1,c1], uvs, [v2,v2],[prev,prev])
            where (uvs,cap,prev) = if isClosed
                                   -- no cap
                                   then ([V2 0 d, V2 0 (-d)],v1,v3n)
                                   -- cap
                                   else let c = d *^ signorm (v2 - v1)
                                        in ([V2 (-d) d, V2 (-d) (-d)],v1 - c, v1 - 2*c)

        endCap = ([cap,cap], [c3,c3], uvs,[next,next],[v3n,v3n])
            where (uvs,cap,next) = if isClosed
                                   -- no cap
                                   then ([V2 totalLen d, V2 totalLen (-d)], v3, v2)
                                   -- cap
                                   else let c = d *^ signorm (v3 - v3n)
                                        in ([V2 totalEnd d, V2 totalEnd (-d)], v3 + c, v3 + 2*c)

        vcs  = zip3 verts colors seqLens :: [(V2 Float, V4 Float, Float)]
        tris = startCap : zipWith3 strip vcs (drop 1 vcs) (drop 2 vcs)
                        ++ [endCap]
        -- Expand the line into a triangle strip
        strip (a,_,_) (b,bc,l) (c,_,_) = ([b,b],[bc,bc],[V2 l d,V2 l (-d)],[c,c],[a,a])
        vToGL :: Foldable f => [f Float] -> [GLfloat]
        vToGL = map realToFrac . concatMap F.toList
        vs_ = concatMap (\(a,_,_,_,_) -> a) tris
        cs_ = concatMap (\(_,a,_,_,_) -> a) tris
        us_ = concatMap (\(_,_,a,_,_) -> a) tris
        ns_ = concatMap (\(_,_,_,a,_) -> a) tris
        ps_ = concatMap (\(_,_,_,_,a) -> a) tris
        vs  = vToGL vs_
        cs  = vToGL cs_
        us  = vToGL us_
        ns  = vToGL ns_
        ps  = vToGL ps_
        PPRS src = psh
        srcs = [src]

    withVAO $ \vao -> withBuffers 5 $ \bufs@[vbuf, cbuf, uvbuf, nbuf, pbuf] -> do
        onlyEnableAttribs [positionLoc, colorLoc, uvLoc, nextLoc, prevLoc]
        bufferAttrib positionLoc 2 vbuf vs
        bufferAttrib colorLoc 4 cbuf cs
        bufferAttrib uvLoc 2 uvbuf us
        bufferAttrib nextLoc 2 nbuf ns
        bufferAttrib prevLoc 2 pbuf ps
        glBindVertexArray 0
        let num = fromIntegral $ length vs_
            r t = do withUniform "projection" srcs $ setOrthoContextProjection win
                     withUniform "modelview" srcs $ setModelview t
                     withUniform "thickness" srcs $ \p u -> do
                         glUseProgram p
                         glUniform1f u thickness
                     withUniform "feather" srcs $ \p u -> do
                         glUseProgram p
                         glUniform1f u feather
                     withUniform "sumlength" srcs $ \p u -> do
                         glUseProgram p
                         glUniform1f u totalLen
                     withUniform "cap" srcs $ \p u -> do
                         glUseProgram p
                         let [x,y] = map (fromIntegral . fromEnum) [capx,capy]
                         glUniform2f u x y
                     -- set the thickness uniform with the actual thickness
                     -- so the shader knows how to anti-alias fragments
                     drawBuffer (shProgram src) vao GL_TRIANGLE_STRIP num
            c = do withArray bufs $ glDeleteBuffers 5
                   withArray [vao] $ glDeleteVertexArrays 1
        return (c, r)
    | otherwise = return emptyRenderer

-- | Creates and returns a renderer that renders a given string of
-- triangles with the given filling.
filledTriangleRenderer :: Context -> GeomShader -> [Triangle (V2 Float)]
                       -> Fill -> IO GLRenderer
filledTriangleRenderer win gsh ts (FillColor cm) = do
    let vs = trisToComp ts
        -- If we can't find a color in the color map we'll just use
        -- transparent black.
        cs = map (fromMaybe 0 . lookupColor (unColorMap cm)) vs
    colorRenderer win gsh GL_TRIANGLES vs cs 
filledTriangleRenderer win gsh ts (FillTexture fp tm) = do
    mtex <- loadImageAsTexture fp
    case mtex of
        Just tx -> do
            let vs = trisToComp ts
                -- If we can't find a uv in the uv map we'll just use
                -- 0,0 
                uvs = map (fromMaybe 0 . lookupColor (unTextureMap tm)) vs
            (c, r) <- textureRenderer win gsh GL_TRIANGLES vs uvs
            let r' t = bindTexAround tx $ r t
            return (c, r')
        _ -> do putStrLn "Could not create a filledTriangleRenderer."
                return (return (), const $ putStrLn "Non op renderer.")

-- | Binds the given texture to the zeroeth texture unit, runs the IO
-- action and then unbinds the texture.
bindTexAround :: GLuint -> IO () -> IO ()
bindTexAround tx f = do
    glActiveTexture GL_TEXTURE0
    glBindTexture GL_TEXTURE_2D tx
    f
    glBindTexture GL_TEXTURE_2D 0
--------------------------------------------------------------------------------
-- Font decomposition into triangles and beziers
--------------------------------------------------------------------------------
-- | Ephemeral types for creating polygons from font outlines.
-- Fonty gives us a [[Vector (Float, Float)]] for an entire string, which
-- breaks down to
type Contour = [Bezier (V2 Float)] -- Beziers
type CharacterOutline = [Contour]
type StringOutline = [CharacterOutline]

fontGeom :: Font -> Int -> Float -> String -> ([Bezier (V2 Float)], [Triangle (V2 Float)])
fontGeom font dpi px str =
    let sz  = pixelSizeInPointAtDpi px dpi
        cs  = getStringCurveAtPoint dpi (0,0) [(font, sz, str)]
        bs  = beziers cs
        ts  = concatMap (concatMap (concaveTriangles . onContourPoints)) bs
    in (concat $ concat bs,ts)

fontCurves :: Font -> Int -> Float -> String -> [[[QuadraticBezier (V2 Float)]]]
fontCurves font dpi px str =
    let sz = pixelSizeInPointAtDpi px dpi
        cs = getStringCurveAtPoint dpi (0,0) [(font, sz, str)]
        bs = beziers cs
    in fmap (fmap (fmap (\(Bezier _ a b c) -> bez3 a b c))) bs

fromFonty :: (UV.Unbox b1, Functor f1, Functor f) => ([V2 b1] -> b) -> f (f1 (UV.Vector (b1, b1))) -> f (f1 b)
fromFonty f = fmap $ fmap $ f . UV.toList . UV.map (uncurry V2)

beziers :: [[UV.Vector (Float, Float)]] -> StringOutline
beziers = fromFonty (toBeziers . fmap (fmap realToFrac))

-- | Turns a polygon into a list of triangles that can be rendered using the
-- Concave Polygon Stencil Test
-- @see http://www.glprogramming.com/red/chapter14.html#name13
concaveTriangles :: [a] -> [Triangle a]
concaveTriangles [] = []
concaveTriangles (a:as) = tris a as
    where tris p (p':p'':ps) = Triangle p p' p'' : tris p (p'':ps)
          tris _ _ = []

-- | Collects the points that lie directly on the contour of the font
-- outline.
onContourPoints :: [Bezier a] -> [a]
onContourPoints [] = []
onContourPoints (Bezier LT a b c :bs) = [a,b,c] ++ onContourPoints bs
onContourPoints (Bezier _ a _ c :bs) = [a,c] ++ onContourPoints bs
-- | TODO: textureFontRenderer and then fontRenderer.

-- | Creates and returns a renderer that renders some text with a font. 
colorFontRenderer :: Context -> GeomShader -> BezShader
                  -> FontData -> Int -> Float -> String 
                  -> V4 Float -> IO GLRenderer
colorFontRenderer window gsh brs fd dpi px str clr = do
    let (bs,ts) = fontStringGeom fd dpi px str
        vs = concatMap (\(Triangle a b c) -> [a,b,c]) ts
        clrf = const clr
        cs = map clrf vs
    (cg,fg) <- colorRenderer window gsh GL_TRIANGLES vs cs

    let bcs = map ((\(Bezier _ a b c) -> Triangle a b c) . fmap clrf) bs
    (cb,fb) <- colorBezRenderer window brs bs bcs

    let s t  = stencilMask (fg t) (fg t)
        gs t = s t >> fb t
    return (cg >> cb,gs)

-- | Creates and returns a renderer that renders the given colored
-- geometry.
colorRenderer :: Context -> GeomShader -> GLuint -> [V2 Float]
              -> [V4 Float] -> IO GLRenderer
colorRenderer window gsh mode vs gs = do
    let (GRS src) = gsh
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
                withUniform "projection" srcs $ setOrthoContextProjection window
                withUniform "modelview" srcs $ setModelview t
                drawBuffer (shProgram src) vao mode num
            cleanupFunction = do
                withArray [pbuf, cbuf] $ glDeleteBuffers 2
                withArray [vao] $ glDeleteVertexArrays 1
        return (cleanupFunction,renderFunction)

-- | Creates and returns a renderer that renders a textured
-- geometry using the texture bound to GL_TEXTURE0.
textureRenderer :: Context -> GeomShader -> GLuint -> [V2 Float]
                -> [V2 Float] -> IO GLRenderer
textureRenderer = textureUnitRenderer Nothing

-- | Creates and returns a renderer that renders the given textured
-- geometry using the specified texture binding.
textureUnitRenderer :: Maybe GLint -> Context -> GeomShader -> GLuint
                    -> [V2 Float] -> [V2 Float] -> IO GLRenderer
textureUnitRenderer Nothing w gs md vs uvs =
    textureUnitRenderer (Just 0) w gs md vs uvs
textureUnitRenderer (Just u) win gsh mode vs uvs = do
    let (GRS src) = gsh
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
                withUniform "projection" srcs $ setOrthoContextProjection win
                withUniform "modelview" srcs $ setModelview tfrm
                drawBuffer (shProgram src) vao mode num
            cleanupFunction = do
                withArray [pbuf, cbuf] $ glDeleteBuffers 2
                withArray [vao] $ glDeleteVertexArrays 1
        return (cleanupFunction,renderFunction)

-- | Creates and returns a renderer that renders the given colored beziers.
colorBezRenderer :: Context -> BezShader -> [Bezier (V2 Float)]
                 -> [Triangle (V4 Float)] -> IO GLRenderer
colorBezRenderer window (BRS src) bs ts =
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
                withUniform "projection" srcs $ setOrthoContextProjection window
                withUniform "modelview" srcs $ setModelview t
                drawBuffer (shProgram src) vao GL_TRIANGLES num
        return (cleanupFunction,renderFunction)

-- | Creates and returns a renderer that renders the given textured beziers.
textureBezUnitRenderer :: Maybe GLint -> Context -> BezShader
                        -> [Bezier (V2 Float)] -> [Triangle (V2 Float)] -> IO GLRenderer
textureBezUnitRenderer Nothing window sh bs ts =
    textureBezUnitRenderer (Just 0) window sh bs ts
textureBezUnitRenderer (Just u) window (BRS src) bs ts =
    withVAO $ \vao -> withBuffers 3 $ \[pbuf, uvbuf, tbuf] -> do
        let vs = concatMap (\(Bezier _ a b c) -> [a,b,c]) bs
            uvs = concatMap (\(Triangle a b c) -> [a,b,c]) $ take (length bs) ts
            f = map realToFrac . concatMap F.toList
            uvs' = f uvs :: [GLfloat]
            ps = f vs    :: [GLfloat]
            ws = concatMap (\(Bezier w _ _ _) -> let w' = fromBool $ w == LT
                                                 in [ 0, 0, w'
                                                    , 0.5, 0, w'
                                                    , 1, 1, w'
                                                    ])
                           bs :: [GLfloat]

        onlyEnableAttribs [positionLoc, uvLoc, bezLoc]
        bufferAttrib positionLoc 2 pbuf ps
        bufferAttrib uvLoc 2 uvbuf uvs'
        bufferAttrib bezLoc 3 tbuf ws
        glBindVertexArray 0

        let cleanupFunction = do
                withArray [pbuf, tbuf, uvbuf] $ glDeleteBuffers 3
                withArray [vao] $ glDeleteVertexArrays 1
            num = fromIntegral $ length vs
            srcs = [src]
            renderFunction t = do
                withUniform "hasUV" srcs $ \p huv -> do
                    glUseProgram p
                    glUniform1i huv 1
                withUniform "sampler" srcs $ \p smp -> do
                    glUseProgram p
                    glUniform1i smp u
                withUniform "projection" srcs $ setOrthoContextProjection window
                withUniform "modelview" srcs $ setModelview t
                drawBuffer (shProgram src) vao GL_TRIANGLES num
        return (cleanupFunction,renderFunction)

-- | Creates and returns a renderer that renders textured beziers using the
-- texture bound to GL_TEXTURE0.
textureBezRenderer :: Context -> BezShader -> [Bezier (V2 Float)]
                    -> [Triangle (V2 Float)] -> IO GLRenderer
textureBezRenderer = textureBezUnitRenderer Nothing

-- | Creates and returns a renderer that renders a given string of
-- triangles with the given filling.
filledBezierRenderer :: Context -> BezShader -> [Bezier (V2 Float)] -> Fill
                      -> IO GLRenderer
filledBezierRenderer win sh bs (FillColor cm) = do
    let ts = map (\(Bezier _ a b c) -> clr <$> Triangle a b c) bs
        clr v = fromMaybe 0 $ lookupColor (unColorMap cm) v
    colorBezRenderer win sh bs ts
filledBezierRenderer win sh bs (FillTexture fp tm) = do
    let ts = map (\(Bezier _ a b c) -> uv <$> Triangle a b c) bs
        uv v = fromMaybe 0 $ lookupColor (unTextureMap tm) v 
    mtex <- loadImageAsTexture fp
    case mtex of
        Just tx -> do (c,r) <- textureBezRenderer win sh bs ts
                      let r' t = bindTexAround tx $ r t
                      return (c, r')
        Nothing -> do putStrLn "Could not create a filledBezRenderer."
                      return (return (), const $ putStrLn "Non op renderer.")

-- | Creates and returns a renderer that masks a textured rectangular area with
-- another texture.
maskRenderer :: Context -> MaskShader -> GLuint -> [V2 Float]
             -> [V2 Float] -> IO GLRenderer
maskRenderer win (MRS src) mode vs uvs =
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
                withUniform "projection" [src] $ setOrthoContextProjection win
                withUniform "modelview" [src] $ setModelview t
                withUniform "mainTex" [src] $ \p smp -> do
                    glUseProgram p
                    glUniform1i smp 0
                withUniform "maskTex" [src] $ \p smp -> do
                    glUseProgram p
                    glUniform1i smp 1
                drawBuffer (shProgram src) vao mode num
        return (cleanup,render)

-- | Creates a rendering that masks an IO () drawing computation with the alpha
-- value of another.
alphaMask :: Context -> MaskShader -> IO () -> IO () -> IO GLRenderer
alphaMask win mrs r2 r1 = do
    mainTex <- toTextureUnit (Just GL_TEXTURE0) win r2
    maskTex <- toTextureUnit (Just GL_TEXTURE1) win r1
    (w,h)   <- ctxWindowSize win
    let vs = map (fmap fromIntegral) [V2 0 0, V2 w 0, V2 w h, V2 0 h]
        uvs = [V2 0 1, V2 1 1, V2 1 0, V2 0 0]
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


transformRenderer :: Transform -> GLRenderer -> GLRenderer
transformRenderer t (c, r) = (c, r . (t <>))
--------------------------------------------------------------------------------
-- Updating uniforms
--------------------------------------------------------------------------------
withUniform :: String -> [Shader] -> (GLuint -> GLint -> IO ()) -> IO ()
withUniform name srcs f = mapM_ update srcs
    where update (Shader p ls) = forM_ (lookup name ls) (f p)

setOrthoContextProjection :: Context -> GLuint -> GLint -> IO ()
setOrthoContextProjection window program pju = do
    pj <- orthoContextProjection window
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

orthoContextProjection :: Context -> IO (M44 GLfloat)
orthoContextProjection window = do
    (ww, wh) <- ctxWindowSize window
    let (hw,hh) = (fromIntegral ww, fromIntegral wh)
    return $ ortho 0 hw hh 0 0 1
--------------------------------------------------------------------------------
-- Working with textures.
--------------------------------------------------------------------------------
loadImageAsTexture :: FilePath -> IO (Maybe GLuint)
loadImageAsTexture fp = do
    eStrOrImg <- readImage fp
    case eStrOrImg of
        Left err -> putStrLn err >> return Nothing
        Right i  -> liftM Just (loadTexture i)

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


toTexture :: Context -> IO () -> IO GLuint
toTexture = toTextureUnit Nothing

toTextureUnit :: Maybe GLuint -> Context -> IO () -> IO GLuint
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
    (w,h) <- ctxWindowSize win
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
            --ww <- (fromIntegral . fst) <$> ctxWindowSize win

            --let s = floor (fbw/ww :: Double)
            --print s
            glViewport 0 0 w' h' --fbw' fbh'
            r
            glBindFramebuffer GL_FRAMEBUFFER 0
            with fb $ glDeleteFramebuffers 1
            (fbw, fbh) <- ctxFramebufferSize win
            glViewport 0 0 (fromIntegral fbw) (fromIntegral fbh)
    return t

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
    clearErrors "glBindVertex"
    glDrawArrays mode 0 num
    clearErrors "glDrawArrays"

clearErrors :: String -> IO ()
clearErrors str = do
    err' <- glGetError
    when (err' /= 0) $ errorWithStackTrace $ unwords [str, show err']

glFloatSize :: Int
glFloatSize = sizeOf (undefined :: GLfloat)

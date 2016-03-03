{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveGeneric #-}
module Gelatin.GL.Renderer (
    -- * Renderer
    GLRenderer,
    Context(..),
    -- * Loading and using textures
    loadImage,
    maybeLoadTexture,
    loadTexture,
    loadTextureUnit,
    unloadTexture,
    loadImageAsTexture,
    bindTexAround,
    -- * Rendering pictures
    compiledPicRenderer,
    -- * Rendering geometry
    colorPrimsRenderer,
    texturePrimsRenderer,
    -- * Line rendering
    --filledPolylineRenderer,
    colorPolylineRenderer,
    texPolylineRenderer,
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
    fontyData,
    loadFont,
    filledFontRenderer,
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

import           Gelatin.GL.Shader
import           Gelatin.GL.Common
import           Gelatin.Core
import           Gelatin.Picture
import           Graphics.Text.TrueType
import           Graphics.GL.Core33
import           Graphics.GL.Types
import           Codec.Picture.Types
import           Codec.Picture (decodeImage, readImage)
import           Foreign.Marshal.Array
import           Foreign.Marshal.Utils
import           Foreign.Storable
import           Foreign.Ptr
import           Data.ByteString (ByteString)
import           Data.Hashable
import           Data.Renderable
import           Data.Monoid
import           Data.Foldable (foldl')
import qualified Data.Vector.Generic as G
import qualified Data.Vector as B
import qualified Data.Vector.Storable as S
import qualified Data.Vector.Unboxed as V
import           Data.Vector.Unboxed (Vector,Unbox)
import           Control.Monad
import           System.Exit
import qualified Data.Foldable as F
import           GHC.Stack
import           GHC.Generics
import Debug.Trace
import           Linear hiding (trace)

--------------------------------------------------------------------------------
-- Rendering compiled pictures
--------------------------------------------------------------------------------
compiledPicRenderer :: Rez -> CompiledPicture GLuint
                    -> IO GLRenderer
compiledPicRenderer rez (CompiledTex op ps tx) = do
  rs <- mapM (texturePrimsRenderer rez) ps
  let (c,f) = foldl' appendRenderer emptyRenderer rs
      r t = bindTexAround tx $ f t
  return $ applySpecialOp op (c,r)
compiledPicRenderer rez (CompiledColor op ps) = do
  rs <- mapM (colorPrimsRenderer rez) ps
  return $ applySpecialOp op $ foldl' appendRenderer emptyRenderer rs
compiledPicRenderer rez (CompiledLine op s ps) = do
  let f (GPLine _ vs) = GPLine s vs
      f p = p
  rs <- mapM (colorPrimsRenderer rez . f) ps
  return $ applySpecialOp op $ foldl' appendRenderer emptyRenderer rs

applySpecialOp :: SpecialOp -> GLRenderer -> GLRenderer
applySpecialOp SOpNone r = r
applySpecialOp SOpStencilMask (c,r) =
  let s t  = stencilMask (r t) (r t)
  in (c,s)
--------------------------------------------------------------------------------
-- Renderers for GPrims with specific attributes
-- @(V2 Float, V4 Float)@
-- @(V2 Float, V2 Float)@
--------------------------------------------------------------------------------
fromTris :: Unbox a
         => Vector ((V2 Float, a), (V2 Float, a), (V2 Float, a))
         -> (Vector (V2 Float), Vector a)
fromTris = uncurry mycat . V.unzip . V.map f
  where f ((va,ca),(vb,cb),(vc,cc)) = ((va,vb,vc), (ca,cb,cc))
        mycat vs cs = (V.concatMap g vs, V.concatMap g cs)
        g (a,b,c) = V.fromList [a,b,c]

--fromTris ts = atts
--  where atts = V.foldr' fatt (mempty,mempty) ts
--        fatt (xs,ys) =
--          (V.fromList [va,vb,vc] V.++ xs, V.fromList [ca,cb,cc] V.++ ys)

toBezAndTris :: Unbox a
             => Vector ((V2 Float, a), (V2 Float, a), (V2 Float, a))
             -> (Vector (Bezier (V2 Float)), Vector (Triangle a))
toBezAndTris = V.unzip . V.map f
  where f ((va,ca),(vb,cb),(vc,cc)) = ( bezier va vb vc
                                      , (ca,cb,cc)
                                      )

type BezRenderer a t =
  Context -> SumShader -> Vector (Bezier (V2 Float)) -> Vector (Triangle a)
          -> t

renderBezsWith :: Unbox a
               => BezRenderer a t -> Rez
               -> Vector ((V2 Float, a), (V2 Float, a), (V2 Float, a)) -> t
renderBezsWith f rez bs = do
  let sh = rezShader rez
      ctx = rezContext rez
      (vs,cs) = toBezAndTris bs
  f ctx sh vs cs

type TriSeqRenderer a b c t =
  Context -> SumShader -> a -> Vector b -> Vector c -> t

renderTriSeqWith :: (Eq a, Num a, Unbox b, Unbox a1)
                 => TriSeqRenderer a a1 b c
                 -> Rez -> SeqType -> (a1, b) -> (a1, b) -> (a1, b)
                 -> Vector (a1, b) -> c
renderTriSeqWith f rez stype a b c ds = do
  let sh = rezShader rez
      ctx = rezContext rez
      mode = if stype == SeqStrip
             then GL_TRIANGLE_STRIP else GL_TRIANGLE_FAN
  uncurry (f ctx sh mode) $ V.unzip $ V.fromList [a,b,c] V.++ ds

renderTrisWith :: (Eq a, Num a, Unbox a1)
               => (Context -> SumShader -> a -> Vector (V2 Float) -> Vector a1 -> c)
               -> Rez -> Vector ((V2 Float, a1), (V2 Float, a1), (V2 Float, a1))
               -> c
renderTrisWith f rez ts = do
  let sh = rezShader rez
      ctx = rezContext rez
  uncurry (f ctx sh GL_TRIANGLES) $ fromTris ts

renderLineWith :: (Show (f Float), Additive f, Unbox (f Float))
               => (Context -> SumShader -> Float -> Float -> (LineCap, LineCap) -> Vector (V2 Float) -> Vector (f Float) -> c)
               -> Rez -> Stroke -> Vector (V2 Float, f Float) -> c
renderLineWith f rez (Stroke w fth cs) = uncurry (f ctx sh w fth cs) . V.unzip
  where sh = rezShader rez
        ctx = rezContext rez

-- | Create a renderer for some colored geometry.
colorPrimsRenderer :: Rez -> GPrims (V2 Float, V4 Float) -> IO GLRenderer
colorPrimsRenderer rez (GPTris ts) = renderTrisWith colorRenderer rez ts
colorPrimsRenderer rez (GPBezs bs) = renderBezsWith colorBezRenderer rez bs
colorPrimsRenderer rez (GPTriSeq stype a b c ds) =
  renderTriSeqWith colorRenderer rez stype a b c ds
colorPrimsRenderer rez (GPLine s ls) =
  renderLineWith colorPolylineRenderer rez s ls

-- | Create a renderer for some textured geometry.
texturePrimsRenderer :: Rez -> GPrims (V2 Float, V2 Float) -> IO GLRenderer
texturePrimsRenderer rez (GPTris ts) = renderTrisWith textureRenderer rez ts
texturePrimsRenderer rez (GPBezs bs) = renderBezsWith textureBezRenderer rez bs
texturePrimsRenderer rez (GPTriSeq stype a b c ds) =
  renderTriSeqWith textureRenderer rez stype a b c ds
texturePrimsRenderer rez (GPLine s ls) =
  renderLineWith texPolylineRenderer rez s ls
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
        vcs  = B.convert $ V.zip3 verts colors seqLens
        zs   = B.zipWith3 strp vcs (B.drop 1 vcs) (B.drop 2 vcs)
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
        tris = startCap `B.cons` zs B.++ B.singleton endCap
        cat5 (vs',cs',us',ns',ps') (v,c,u,n,p) = (vs' V.++ v
                                                 ,cs' V.++ c
                                                 ,us' V.++ u
                                                 ,ns' V.++ n
                                                 ,ps' V.++ p
                                                 )
        (vs,cs,us,ns,ps) = B.foldl' cat5 (mempty,mempty,mempty,mempty,mempty) tris
      in Just (vs, cs, us, ns, ps, totalLen)
    | otherwise = Nothing

unwrapTransform :: PictureTransform -> (M44 Float, Float, V4 Float)
unwrapTransform t =
  let mv = modelviewProjection $ ptAffine t
      a  = ptAlpha t
      m  = ptMult t
  in (mv, a, m)

polylineRenderer :: (Foldable f, Unbox (f Float))
                 => Context -> SumShader -> Float -> Float
                 -> (LineCap,LineCap) -> Bool
                 -> PolylineData f
                 -> IO GLRenderer
polylineRenderer win sh thickness feather caps isTex (vs_,cs_,us_,ns_,ps_,totalLen) = do
  let vToGL :: (Foldable f, Unbox (f Float)) => Vector (f Float) -> Vector GLfloat
      vToGL = V.map realToFrac . V.concatMap (V.fromList . F.toList)
      vs = vToGL vs_
      cs = vToGL cs_
      us = vToGL us_
      ns = vToGL ns_
      ps = vToGL ps_

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
        r t = do let (mv, a, m) = unwrapTransform t
                 pj <- orthoContextProjection win
                 updateUniformsForLines (unShader sh) pj mv isTex a m thickness
                                        feather totalLen caps
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
  let empty = putStrLn "could not expand polyline" >> return emptyRenderer
      mpoly = expandPolyline verts colors thickness feather
  flip (maybe empty) mpoly $
    polylineRenderer win psh thickness feather caps False

-- | Creates and returns a renderer that renders a textured, expanded 2d
-- polyline projected in 2d space.
texPolylineRenderer :: Context -> SumShader -> Float
                    -> Float -> (LineCap,LineCap) -> Vector (V2 Float)
                    -> Vector (V2 Float) -> IO GLRenderer
texPolylineRenderer win psh thickness feather caps verts uvs = do
  let empty = putStrLn "could not expand polyline" >> return emptyRenderer
      mpoly = expandPolyline verts uvs thickness feather
  flip (maybe empty) mpoly $
    polylineRenderer win psh thickness feather caps True

-- | Creates and returns a renderer that renders a filled, expanded 2d polyline
-- projected in 2d space.
--filledPolylineRenderer :: Context -> SumShader -> Fill -> Float
--                       -> Float -> (LineCap,LineCap) -> [V2 Float]
--                       -> IO GLRenderer
--filledPolylineRenderer win psh fill thickness feather caps verts =
--  case fill of
--    FillColor f ->
--      colorPolylineRenderer win psh thickness feather caps verts $ f verts
--    FillTexture _ f -> do
--      texPolylineRenderer win psh thickness feather caps verts $ f verts

-- | Creates and returns a renderer that renders a given string of
-- triangles with the given filling.
filledTriangleRenderer :: Context -> SumShader -> Vector (Triangle (V2 Float))
                       -> Fill GLuint -> IO GLRenderer
filledTriangleRenderer win gsh ts (FillColor f) = do
  let vs = trisToComp ts
      cs = V.map f vs
  colorRenderer win gsh GL_TRIANGLES vs cs
filledTriangleRenderer win gsh ts (FillTexture tx f) = do
  let vs = trisToComp ts
      uvs = V.map f vs
  (c, r) <- textureRenderer win gsh GL_TRIANGLES vs uvs
  let r' t = bindTexAround tx $ r t
  return (c, r')

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
type Contour = Vector (Bezier (V2 Float)) -- Beziers
type CharacterOutline = [Contour]
type StringOutline = [CharacterOutline]

type ContourInnards = Vector (V2 Float)
type CharacterInnards = [ContourInnards]
type StringInnards = [CharacterInnards]

deriving instance Generic FontStyle
instance Hashable FontStyle
deriving instance Generic FontDescriptor
instance Hashable FontDescriptor

-- | Provide a FontData for a given FontyFruity TrueType Font.
fontyData :: Font -> FontData
fontyData font = FontData { fontStringCurves = fontCurves font
                          , fontStringGeom = fontGeom font
                          , fontHash = \s -> hashWithSalt s $ descriptorOf font
                          , fontShow = show $ descriptorOf font
                          }

loadFont :: FilePath -> IO (Either String FontData)
loadFont fp = fmap fontyData <$> loadFontFile fp

stringOutline :: Font -> Int -> Float -> String -> StringOutline
stringOutline font dpi px str = beziers cs
    where sz = pixelSizeInPointAtDpi px dpi
          cs = getStringCurveAtPoint dpi (0,0) [(font, sz, str)]

fontGeom :: Font -> Int -> Float -> String -> ( Vector (Bezier (V2 Float))
                                              , [Vector (V2 Float)]
                                              )
fontGeom font dpi px str =
    let bs  = stringOutline font dpi px str -- list of list of vector of beziers
        ts  = concatMap (fmap innerTriFan) bs
    in ( V.concat $ concat bs
       , ts
       )

fontCurves :: Font -> Int -> Float -> String -> [[Vector (QuadraticBezier (V2 Float))]]
fontCurves font dpi px str =
    let bs = stringOutline font dpi px str
    in fmap (fmap (V.map (\(_,a,b,c) -> bez3 a b c))) bs

fromFonty :: (Unbox b1, Functor f1, Functor f)
          => (Vector (V2 b1) -> b) -> f (f1 (Vector (b1, b1))) -> f (f1 b)
fromFonty f = fmap $ fmap $ f . V.map (uncurry V2)

-- | Turn a polyline into a list of bezier primitives.
toBeziers :: (Fractional a, Ord a, Unbox a)
          => Vector (V2 a) -> Vector (Bezier (V2 a))
toBeziers vs =
  V.fromList $ map (\(a,b,c) -> bezier (vs V.! a) (vs V.! b) (vs V.! c)) ndxs
  where ndxs = map f [0 .. nt $ V.length vs -1]
        nt n = max 0 $ ceiling $ (fromIntegral n - 3) / 2
        f i = let a = i * 2
                  b = a + 1
                  c = a + 2
               in (a,b,c)

beziers :: [[Vector (Float, Float)]] -> StringOutline
beziers = fromFonty (toBeziers . V.map (fmap realToFrac))

-- |
innerTriFan :: Unbox a => Vector (Bezier a) -> Vector a
innerTriFan bs
  | V.null bs = V.empty
  | (True,x,_,c) <- V.head bs  =
    x `V.cons` (c `V.cons` V.map (\(_,_,_,z) -> z) (V.drop 1 bs))
  | (False,a,b,c) <- V.head bs =
    a `V.cons` (b `V.cons` (c `V.cons` V.concatMap (\(_,_,y,z) ->
      V.fromList [y,z]) (V.drop 1 bs)))

-- | Turns a polygon into a list of triangles that can be rendered using the
-- Concave Polygon Stencil Test
-- @see http://www.glprogramming.com/red/chapter14.html#name13
concaveTriangles :: Unbox a => Vector a -> Vector (Triangle a)
concaveTriangles ts
  | Just a <- ts V.!? 0 = tris a $ V.drop 1 ts
  | otherwise = V.empty
    where tris v vs
            | Just (b,c) <- (,) <$> (vs V.!? 0) <*> (vs V.!? 1)  =
              (v,b,c) `V.cons` tris v (V.drop 1 vs)
            | otherwise = V.empty

-- | Collects the points that lie directly on the contour of the font
-- outline.
onContourPoints :: Unbox a => Vector (Bezier a) -> Vector a
onContourPoints = V.foldl' f mempty
  where f bs (False,a,b,c) = bs V.++ V.fromList [a,b,c]
        f bs (_,a,_,c) = bs V.++ V.fromList [a,c]

-- | Creates and returns a renderer that renders some text with a font.
filledFontRenderer :: Context -> SumShader
                  -> FontData -> Int -> Float -> String
                  -> Fill GLuint -> IO GLRenderer
filledFontRenderer window sh fd dpi px str fill = do
    let (bs,ts) = fontStringGeom fd dpi px str
    (cg,fg) <- case fill of
                 (FillColor f) -> do
                   rs <- forM ts $ \vs ->
                     let cs = V.map f vs
                     in colorRenderer window sh GL_TRIANGLE_FAN vs cs
                   return $ foldl' appendRenderer emptyRenderer rs
                 (FillTexture tx f) -> do
                   rs <- forM ts $ \vs ->
                     let uvs = V.map f vs
                     in textureRenderer window sh GL_TRIANGLE_FAN vs uvs
                   let (c,r) = foldl' appendRenderer emptyRenderer rs
                       r' t = bindTexAround tx $ r t
                   return (c,r')
    (cb,fb) <- filledBezierRenderer window sh bs fill
    let s t  = stencilMask (fg t) (fg t)
        gs t = s t >> fb t
    return (cg >> cb,gs)

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
            let (mv,a,m) = unwrapTransform t
            pj <- orthoContextProjection window
            updateUniformsForTris (unShader sh) pj mv False a m
            drawBuffer (shProgram $ unShader sh) vao mode num
        cleanupFunction = do
            withArray [pbuf, cbuf] $ glDeleteBuffers 2
            withArray [vao] $ glDeleteVertexArrays 1
    return (cleanupFunction,renderFunction)

-- | Creates and returns a renderer that renders a textured
-- geometry using the texture bound to GL_TEXTURE0.
textureRenderer :: Context -> SumShader -> GLuint -> Vector (V2 Float)
                -> Vector (V2 Float) -> IO GLRenderer
textureRenderer = textureUnitRenderer Nothing

-- | Creates and returns a renderer that renders the given textured
-- geometry using the specified texture binding.
textureUnitRenderer :: Maybe GLint -> Context -> SumShader -> GLuint
                    -> Vector (V2 Float) -> Vector (V2 Float) -> IO GLRenderer
textureUnitRenderer Nothing w sh md vs uvs =
    textureUnitRenderer (Just 0) w sh md vs uvs
textureUnitRenderer (Just u) win sh mode vs uvs =
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
        let (mv,a,m) = unwrapTransform t
        pj <- orthoContextProjection win
        updateUniformsForTris (unShader sh) pj mv True a m
        drawBuffer (shProgram $ unShader sh) vao mode num
      cleanupFunction = do
        withArray [pbuf, cbuf] $ glDeleteBuffers 2
        withArray [vao] $ glDeleteVertexArrays 1
  return (cleanupFunction,renderFunction)

-- | Creates and returns a renderer that renders the given colored beziers.
colorBezRenderer :: Context -> SumShader -> Vector (Bezier (V2 Float))
                 -> Vector (Triangle (V4 Float)) -> IO GLRenderer
colorBezRenderer window sh bs ts =
    withVAO $ \vao -> withBuffers 3 $ \[pbuf, tbuf, cbuf] -> do
        let vs = V.concatMap (\(_,a,b,c) -> V.fromList [a,b,c]) bs
            cvs = V.concatMap (\(a,b,c) -> V.fromList [a,b,c]) $
                    V.take (V.length bs) ts
            ps = V.map realToFrac $
                   V.concatMap (V.fromList . F.toList) vs :: Vector GLfloat
            cs = V.map realToFrac $
                   V.concatMap (V.fromList . F.toList) cvs :: Vector GLfloat
            ws = V.concatMap (\(w,_,_,_) -> let w' = fromBool $ not w
                                            in V.fromList [ 0, 0, w'
                                                          , 0.5, 0, w'
                                                          , 1, 1, w'
                                                          ])
                           bs :: Vector GLfloat

        enableAttribsForBezs False
        bufferAttrib PositionLoc 2 pbuf ps
        bufferAttrib BezLoc 3 tbuf ws
        bufferAttrib ColorLoc 4 cbuf cs
        glBindVertexArray 0

        let cleanupFunction = do
                withArray [pbuf, tbuf, cbuf] $ glDeleteBuffers 3
                withArray [vao] $ glDeleteVertexArrays 1
            num = fromIntegral $ V.length vs
            renderFunction t = do
                pj <- orthoContextProjection window
                let (mv,a,m) = unwrapTransform t
                updateUniformsForBezs (unShader sh) pj mv False a m
                drawBuffer (shProgram $ unShader sh) vao GL_TRIANGLES num
        return (cleanupFunction,renderFunction)

-- | Creates and returns a renderer that renders the given textured beziers.
textureBezUnitRenderer :: Maybe GLint -> Context -> SumShader
                        -> Vector (Bezier (V2 Float))
                        -> Vector (Triangle (V2 Float)) -> IO GLRenderer
textureBezUnitRenderer Nothing window sh bs ts =
    textureBezUnitRenderer (Just 0) window sh bs ts
textureBezUnitRenderer (Just u) window sh bs ts =
    withVAO $ \vao -> withBuffers 3 $ \[pbuf, uvbuf, tbuf] -> do
        let vs = V.concatMap (\(_,a,b,c) -> V.fromList [a,b,c]) bs
            uvs = V.concatMap (\(a,b,c) -> V.fromList [a,b,c]) $
                    V.take (V.length bs) ts
            f = V.map realToFrac . V.concatMap (V.fromList . F.toList)
            uvs' = f uvs :: Vector GLfloat
            ps = f vs    :: Vector GLfloat
            ws = V.concatMap (\(w,_,_,_) -> let w' = fromBool $ not w
                                            in V.fromList [ 0, 0, w'
                                                          , 0.5, 0, w'
                                                          , 1, 1, w'
                                                          ])
                           bs :: Vector GLfloat

        enableAttribsForBezs True
        bufferAttrib PositionLoc 2 pbuf ps
        bufferAttrib UVLoc 2 uvbuf uvs'
        bufferAttrib BezLoc 3 tbuf ws
        glBindVertexArray 0

        let cleanupFunction = do
                withArray [pbuf, tbuf, uvbuf] $ glDeleteBuffers 3
                withArray [vao] $ glDeleteVertexArrays 1
            num = fromIntegral $ V.length vs
            renderFunction t = do
                let (mv,a,m) = unwrapTransform t
                pj <- orthoContextProjection window
                updateUniformsForBezs (unShader sh) pj mv True a m
                drawBuffer (shProgram $ unShader sh) vao GL_TRIANGLES num
        return (cleanupFunction,renderFunction)

-- | Creates and returns a renderer that renders textured beziers using the
-- texture bound to GL_TEXTURE0.
textureBezRenderer :: Context -> SumShader -> Vector (Bezier (V2 Float))
                    -> Vector (Triangle (V2 Float)) -> IO GLRenderer
textureBezRenderer = textureBezUnitRenderer Nothing

-- | Creates and returns a renderer that renders a given string of
-- triangles with the given filling.
filledBezierRenderer :: Context -> SumShader -> Vector (Bezier (V2 Float))
                     -> Fill GLuint -> IO GLRenderer
filledBezierRenderer win sh bs (FillColor f) = do
    let ts = V.map (\(_,a,b,c) -> fmapTriangle f (a,b,c)) bs
    colorBezRenderer win sh bs ts
filledBezierRenderer win sh bs (FillTexture tx f) = do
    let ts = V.map (\(_,a,b,c) -> fmapTriangle f (a,b,c)) bs
    (c,r) <- textureBezRenderer win sh bs ts
    let r' t = bindTexAround tx $ r t
    return (c, r')

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
                let (mv,a,m) = unwrapTransform t
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
-- Updating uniforms
--------------------------------------------------------------------------------
modelviewProjection :: Transform -> M44 Float
modelviewProjection (Transform (V2 x y) (V2 w h) r) =
    let sxy = V3 w h 1
        txy = V3 x y 0
        rxy = V3 0 0 1
        rot = if r /= 0 then mat4Rotate r rxy else identity
    in mat4Translate txy !*! rot !*! mat4Scale sxy

orthoContextProjection :: Context -> IO (M44 Float)
orthoContextProjection window = do
    (ww, wh) <- ctxWindowSize window
    let (hw,hh) = (fromIntegral ww, fromIntegral wh)
    return $ ortho 0 hw hh 0 0 1
--------------------------------------------------------------------------------
-- Working with textures.
--------------------------------------------------------------------------------
loadImage :: FilePath -> IO (Maybe (V2 Int, GLuint))
loadImage fp = readImage fp >>= maybeLoadTexture

decodeImageAsTexture :: ByteString -> IO (Maybe GLuint)
decodeImageAsTexture bstr = fmap snd <$> maybeLoadTexture (decodeImage bstr)

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
    glBindVertexArray vao
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
        asize = V.length as * glFloatSize
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
    clearErrors "glBindVertex"
    glDrawArrays mode 0 num
    clearErrors "glDrawArrays"

clearErrors :: String -> IO ()
clearErrors str = do
    err' <- glGetError
    when (err' /= 0) $ errorWithStackTrace $ unwords [str, show err']

glFloatSize :: Int
glFloatSize = sizeOf (undefined :: GLfloat)

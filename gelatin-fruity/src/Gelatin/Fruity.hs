{-# LANGUAGE FlexibleContexts #-}
-- |
-- Module:     Gelatin.Fruity
-- Copyright:  (c) 2017 Schell Scivally
-- License:    MIT
-- Maintainer: Schell Scivally <schell@takt.com>
--
-- Provides two high-level functions that create gelatin renderers:
--
-- ['coloredString']: font strings filled with color
--
-- ['texturedString']: font strings filled with a texture mapping
--
--
-- Provides one mid-level function for extracting a font outline:
--
-- ['stringOutline']: raw geometry of a font string
--
--
-- For help obtaining a 'Font' within your program, check out
-- 'loadFontFile'.
module Gelatin.Fruity (
  module TT,
  coloredString,
  texturedString,
  stringOutline
) where

import           Gelatin
import           Graphics.Text.TrueType as TT
import           Data.Vector.Unboxed (Vector, Unbox)
import qualified Data.Vector.Unboxed as V
import qualified Data.Vector as B
import           Control.Arrow (first,second)
--------------------------------------------------------------------------------
-- Font decomposition into triangles and beziers
--------------------------------------------------------------------------------
-- | Ephemeral types for creating polygons from font outlines.
-- Fonty gives us a [[Vector (Float, Float)]] for an entire string, which
-- breaks down to
type Contour = Vector (V2 Float) -- Beziers
type CharacterOutline = [Contour]
type StringOutline = [CharacterOutline]

fromFonty :: (Unbox b1, Functor f1, Functor f)
          => (Vector (V2 b1) -> b) -> f (f1 (Vector (b1, b1))) -> f (f1 b)
fromFonty f = fmap $ fmap $ f . V.map (uncurry V2)

-- | Turn a polyline into a list of bezier points.
toBeziers :: (Fractional a, Ord a, Unbox a)
          => Vector (V2 a) -> Vector (Bezier (V2 a))
toBeziers vs =
  V.fromList $ map (\(a,b,c) -> bezier (vs V.! a) (vs V.! b) (vs V.! c)) ndxs
  where ndxs = map f [0 .. nt $ V.length vs -1]
        nt n = max 0 $ ceiling $ (fromIntegral n - 3) / (2 :: Double)
        f i = let a = i * 2
                  b = a + 1
                  c = a + 2
               in (a,b,c)

unBeziers :: (Fractional a, Ord a, Unbox a)
          => Vector (Bezier a)
          -> Vector a
unBeziers = V.concatMap (\(_,a,b,c) -> V.fromList [a,b,c])

fruityBeziers :: [[Vector (Float, Float)]] -> StringOutline
fruityBeziers = fromFonty (unBeziers . toBeziers . V.map (fmap realToFrac))

-- | Collects the points that lie directly on the contour of the font
-- outline.
onContourPoints :: Unbox a => Vector (Bezier a) -> Vector a
onContourPoints = V.concatMap f
  where f (False,a,b,c) = V.fromList [a,b,c]
        f (_,a,_,c) = V.fromList [a,c]
--onContourPoints = V.foldl' f mempty
--  where f bs (False,a,b,c) = bs V.++ V.fromList [a,b,c]
--        f bs (_,a,_,c) = bs V.++ V.fromList [a,c]

stringCurve :: Font -> Int -> Float -> String -> [[Vector (Float, Float)]]
stringCurve font dpi px str = getStringCurveAtPoint dpi (0,0) [(font, sz, str)]
    where --sz = pixelSizeInPointAtDpi px dpi
          sz = PointSize px

-- | Extract the outlines of a given string using a font. Returns a
-- vector of 'RawBeziers' and 'RawTriangles'.
stringOutline
  :: Font
  -- ^ The font to extract geometry from.
  -> Int
  -- ^ The dpi to read the font at.
  -> Float
  -- ^ The target pixel width of the resulting geometry.
  -> String
  -- ^ The string to construct and extract the geometry with.
  -> B.Vector (RawGeometry (V2 Float))
stringOutline font dpi px str =
  B.fromList $ map RawLine $ concat $
    fromFonty (cleanSeqDupes . V.concatMap divide . toBeziers . V.map (fmap realToFrac)) $
      stringCurve font dpi px str
  where divide (_,a,b,c) = subdivideAdaptive 100 0 $ bez3 a b c

fontBezAndTris :: Font -> Int -> Float -> String
               -> (RawGeometry (V2 Float), B.Vector (RawGeometry (V2 Float)))
fontBezAndTris font dpi px str =
    let cs  = stringCurve font dpi px str
        bs  = fruityBeziers cs
        ts  = concatMap (fmap onContourPoints) $ fromFonty (toBeziers . V.map (fmap realToFrac)) cs
    in ( RawBeziers $ V.concat $ concat bs
       , B.map RawTriangleFan $ B.fromList ts
       )

-- | Creates a gelatin Renderer that renders the given string in 2d space.
coloredString
  :: Backend t e (V2 Float, V4 Float) (V2 Float) Float s
  -- ^ A backend for rendering geometry with 'V2V4' vertices.
  -> Font
  -- ^ The font to use.
  -> Int
  -- ^ The dpi to use for reading the font geometry.
  -> Float
  -- ^ Your target pixel width.
  -> String
  -- ^ The string to render.
  -> (V2 Float -> V4 Float)
  -- ^ A function from font geometry/space to color.
  -> IO (Renderer (V2 Float) Float s)
coloredString b font dpi px str fill = do
  let g        = mapRawGeometry h
      h v      = (v, fill v)
      (bs, ts) = second (B.map g) $ first g $ fontBezAndTris font dpi px str

  (_, r1) <- compilePicture b $ do
    setRawGeometry ts
    setRenderingOptions [StencilMaskOption]

  (_, r2) <- compilePicture b $ setRawGeometry $ B.singleton bs

  return $ r1 `mappend` r2

-- | Creates a gelatin Renderer that renders the given string in 2d space,
-- using a given texture.
texturedString
  :: Backend t e (V2 Float, V2 Float) (V2 Float) Float s
  -- ^ A backend for rendering geometry with 'V2V2' vertices.
  -> Font
  -- ^ The font to use.
  -> Int
  -- ^ The dpi to use for reading the font geometry.
  -> Float
  -- ^ Your target pixel width.
  -> String
  -- ^ The string to render.
  -> t
  -- ^ The texture.
  -> (V2 Float -> V2 Float)
  -- ^ A function from font geometry/space to texture mapping (uv coords).
  -> IO (Renderer (V2 Float) Float s)
texturedString b font dpi px str t fill = do
  let g   = mapRawGeometry h
      h v = (v, fill v)
      (bs, ts) = second (B.map g) $ first g $ fontBezAndTris font dpi px str

  (_, r1) <- compilePicture b $ do
    setRawGeometry ts
    setRenderingOptions [StencilMaskOption]
    setTextures [t]

  (_, r2) <- compilePicture b $ do
    setRawGeometry $ B.singleton bs
    setTextures [t]

  return $ r1 `mappend` r2

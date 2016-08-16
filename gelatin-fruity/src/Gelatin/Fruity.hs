{-# LANGUAGE FlexibleContexts #-}
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
import           Linear
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

stringOutline :: Font -> Int -> Float -> String
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

coloredString :: Monoid (PictureData t (V2 Float) Float (V2 Float, V4 Float))
              => Font -> Int -> Float -> String -> (V2 Float -> V4 Float)
              -> Picture t (V2 Float) Float (V2 Float, V4 Float) ()
coloredString font dpi px str fill = do
  let g        = mapRawGeometry h
      h v      = (v, fill v)
      (bs, ts) = second (B.map g) $ first g $ fontBezAndTris font dpi px str
  embed $ do
    setRawGeometry ts
    setRenderingOptions [StencilMaskOption]
  embed $ setRawGeometry $ B.singleton bs

texturedString :: Monoid (PictureData t (V2 Float) Float (V2 Float, V2 Float))
               => Font -> Int -> Float -> String -> t -> (V2 Float -> V2 Float)
               -> Picture t (V2 Float) Float (V2 Float, V2 Float) ()
texturedString font dpi px str t fill = do
  let g   = mapRawGeometry h
      h v = (v, fill v)
      (bs, ts) = second (B.map g) $ first g $ fontBezAndTris font dpi px str
  embed $ do
    setRawGeometry ts
    setRenderingOptions [StencilMaskOption]
    setTextures [t]
  embed $ do
    setRawGeometry $ B.singleton bs
    setTextures [t]

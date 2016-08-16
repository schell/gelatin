{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Gelatin.GL.Picture where

import           Data.Monoid
import qualified Data.Vector as B
import qualified Data.Vector.Unboxed as V
import           Data.Vector.Unboxed (Unbox)
import           Data.Foldable (foldr')
import           Linear as L
import           Control.Lens
import           Control.Arrow
import           Graphics.GL.Types
import           Graphics.GL.Core33

import           Gelatin
import           Gelatin.Picture.Internal
import           Gelatin.GL.Common
import           Gelatin.GL.Renderer
--------------------------------------------------------------------------------
-- Helpers for Common Picture Types
--------------------------------------------------------------------------------
embedPictureData :: Monoid (PictureData t s r v)
                 => [PictureData t s r v] -> PictureData t s r v
embedPictureData ps = mempty { _picDataChildren = B.fromList ps }

bothToFrac :: (Real a, Fractional b) => (V2 a, V2 a) -> (V2 b, V2 b)
bothToFrac= second (fmap realToFrac) . first (fmap realToFrac)

calcV2VX_applyTfrm :: Num a => V4 (V4 a) -> V2 a -> V2 a
calcV2VX_applyTfrm mv = demoteV3 . m41ToV3 . (mv !*!) . v3ToM41 . promoteV2
{-# INLINE calcV2VX_applyTfrm #-}

affineToModelView :: (Num a, Real a) => Affine (V2 a) a -> M44 Float
affineToModelView (Translate v) = mat4Translate $ realToFrac <$> promoteV2 v
affineToModelView (Scale v) = mat4Scale $ realToFrac <$> promoteV2 v
affineToModelView (Rotate r) = mat4Rotate (realToFrac r) (V3 0 0 1)

calcV2VX_mv :: Real a => PictureData t (V2 a) a v -> M44 Float
calcV2VX_mv dat =
  foldr' f identity $ _picDataAffine dat
    where f a mv = (!*! mv) $ affineToModelView a

calcV2VX_extractSpatial :: (Unbox a, Unbox b, Real a)
                        => PictureData t (V2 a) a (V2 a,b) -> V.Vector (V2 Float)
calcV2VX_extractSpatial dat =
  let gs = _picDataGeometry dat
      mv = calcV2VX_mv dat
      extractAndTfrm = calcV2VX_applyTfrm mv . fmap realToFrac . fst
      f = V.map extractAndTfrm . vertexData . (gs B.!)
  in V.concatMap f $ V.enumFromTo 0 (B.length gs - 1)

calcV2VX_kids :: (Fractional a, Real a, Unbox a, Unbox b)
              => PictureData t (V2 a) a (V2 a, b)
              -> (B.Vector (PictureData t (V2 a) a (V2 a, b)), V.Vector (V2 Float, V2 Float))
calcV2VX_kids dat =
  let (ks, kbs) = B.unzip $ B.map calculateBoundsV2VX $ _picDataChildren dat
      kidBounds = V.map bothToFrac $ B.convert kbs
  in (ks, kidBounds)

calculateBoundsV2VX :: (Unbox a, Unbox b, Real a, Fractional a)
                    => PictureData t (V2 a) a (V2 a, b)
                    -> (PictureData t (V2 a) a (V2 a, b), (V2 a, V2 a))
calculateBoundsV2VX dat =
  let vs :: V.Vector (V2 Float)
      vs = calcV2VX_extractSpatial dat
      (ks, kidBounds) = calcV2VX_kids dat
      bounds    = boundsBounds (polyBounds vs `V.cons` kidBounds)
      boundsFin :: Fractional a => (V2 a, V2 a)
      boundsFin = bothToFrac bounds
  in (dat{ _picDataBounds = Just boundsFin, _picDataChildren = ks }, boundsFin)

emptyPictureDataV2VX :: (Unbox a, Unbox b, Real a, Floating a, Fractional a, Epsilon a)
                     => PictureData t (V2 a) a (V2 a, b)
emptyPictureDataV2VX =
    PictureData { _picDataGeometry  = B.empty
                , _picDataAffine    = []
                , _picDataCalcBounds = calculateBoundsV2VX
                , _picDataAlpha     = 1
                , _picDataMultiply  = 1
                , _picDataStroke    = []
                , _picDataToSpatial = fst
                , _picDataBounds    = Nothing
                , _picDataTextures  = []
                , _picDataOptions   = []
                , _picDataChildren  = B.empty
                }
--------------------------------------------------------------------------------
-- Concrete Picture Types
--------------------------------------------------------------------------------
type V2V4 = (V2 Float, V4 Float)
type ColorPictureData = PictureData () (V2 Float) Float (V2 Float, V4 Float)
type ColorPicture = Picture () (V2 Float) Float (V2 Float, V4 Float)

instance Monoid ColorPictureData where
  mempty = emptyPictureDataV2VX
  mappend a b = embedPictureData [a,b]
  mconcat = embedPictureData

type V2V2 = (V2 Float, V2 Float)
type TexturePictureData = PictureData GLuint (V2 Float) Float (V2 Float, V2 Float)
type TexturePicture = Picture GLuint (V2 Float) Float (V2 Float, V2 Float)

instance Monoid TexturePictureData where
  mempty = emptyPictureDataV2VX
  mappend a b = embedPictureData [a,b]
  mconcat = embedPictureData
--------------------------------------------------------------------------------
-- Compiling Concrete Picture Types
--------------------------------------------------------------------------------
compileColorGeometry :: Rez -> Stroke -> RawGeometry V2V4 -> IO GLRenderer
compileColorGeometry Rez{..} _ (RawTriangles v) =
  let (vs, cs) = V.unzip v
  in colorRenderer rezContext rezShader GL_TRIANGLES vs cs
compileColorGeometry Rez{..} _ (RawBeziers v) =
  let (vs, cs) = V.unzip v
  in colorBezRenderer rezContext rezShader vs cs
compileColorGeometry Rez{..} _ (RawTriangleStrip v) =
  let (vs, cs) = V.unzip v
  in colorRenderer rezContext rezShader GL_TRIANGLE_STRIP vs cs
compileColorGeometry Rez{..} _ (RawTriangleFan v) =
  let (vs, cs) = V.unzip v
  in colorRenderer rezContext rezShader GL_TRIANGLE_FAN vs cs
compileColorGeometry Rez{..} Stroke{..} (RawLine v) =
  let (vs, cs) = V.unzip v
  in colorPolylineRenderer rezContext rezShader strokeWidth strokeFeather
                           strokeLineCaps vs cs

compileTextureGeometry :: Rez -> Stroke -> RawGeometry V2V2 -> IO GLRenderer
compileTextureGeometry Rez{..} _ (RawTriangles v) =
  let (vs, cs) = V.unzip v
  in textureRenderer rezContext rezShader GL_TRIANGLES vs cs
compileTextureGeometry Rez{..} _ (RawBeziers v) =
  let (vs, cs) = V.unzip v
  in textureBezRenderer rezContext rezShader vs cs
compileTextureGeometry Rez{..} _ (RawTriangleStrip v) =
  let (vs, cs) = V.unzip v
  in textureRenderer rezContext rezShader GL_TRIANGLE_STRIP vs cs
compileTextureGeometry Rez{..} _ (RawTriangleFan v) =
  let (vs, cs) = V.unzip v
  in textureRenderer rezContext rezShader GL_TRIANGLE_FAN vs cs
compileTextureGeometry Rez{..} Stroke{..} (RawLine v) =
  let (vs, cs) = V.unzip v
  in texPolylineRenderer rezContext rezShader strokeWidth strokeFeather
                         strokeLineCaps vs cs

picDataToPicTransform :: PictureData t (V2 Float) Float v -> PictureTransform
picDataToPicTransform p@PictureData{..} =
  PictureTransform (calcV2VX_mv p) _picDataAlpha _picDataMultiply

compileColorPictureData :: Rez -> ColorPictureData -> IO GLRenderer
compileColorPictureData rz p = compileColorPictureData' rz p mempty

compileColorPictureData' :: Rez -> ColorPictureData -> PictureTransform -> IO GLRenderer
compileColorPictureData' rz pic@PictureData{..} parentTfrm
  | StencilMaskOption:ops <- _picDataOptions = do
    (c,r) <- compileColorPictureData' rz pic{ _picDataOptions = ops } parentTfrm
    return (c, \t -> stencilMask (r t) (r t))
  | otherwise = do
    r <- transformRenderer parentTfrm <$> B.foldM fc mempty _picDataGeometry
    let t = picDataToPicTransform pic
    r1 <- B.foldM (fg t) mempty _picDataChildren
    return $ r <> r1
  where fc r0 g = (r0 <>) <$> compileColorGeometry rz (strokeWith _picDataStroke) g
        fg t r0 p = (r0 <>) <$> compileColorPictureData' rz p (parentTfrm <> t)

compileTexturePictureData :: Rez -> TexturePictureData -> IO GLRenderer
compileTexturePictureData rz pic@PictureData{..}
  | StencilMaskOption:ops <- _picDataOptions = do
    (c,r) <- compileTexturePictureData rz pic{ _picDataOptions = ops }
    return (c, \t -> stencilMask (r t) (r t))
  | otherwise = do
    (c0,r0) <- B.foldM fc mempty _picDataGeometry
    (c1,r1) <- B.foldM fg mempty _picDataChildren
    let t = picDataToPicTransform pic
        rfin tfrm = bindTexsAround _picDataTextures (r0 tfrm) >> r1 tfrm
    return (c0 >> c1, rfin . (t<>))
  where fc r0 g = (r0 <>) <$> compileTextureGeometry rz (strokeWith _picDataStroke) g
        fg r0 p = (r0 <>) <$> compileTexturePictureData rz p

compileColorPicture :: Rez -> ColorPicture a -> IO GLRenderer
compileColorPicture rz = compileColorPictureData rz . snd . runPicture

compileTexturePicture :: Rez -> TexturePicture a -> IO GLRenderer
compileTexturePicture rz = compileTexturePictureData rz . snd . runPicture

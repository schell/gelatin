{-# LANGUAGE RecordWildCards #-}
module Gelatin.GL.Picture where

import Data.Monoid
import qualified Data.Vector as B
import qualified Data.Vector.Unboxed as V
import Gelatin
import Gelatin.GL.Common
import Gelatin.GL.Renderer
import Graphics.GL.Types
import Graphics.GL.Core33
import Linear

--conthree :: (V.Unbox a, V.Unbox (f a))
--         => (a -> a -> a -> f a) -> V.Vector a -> V.Vector (f a)
--conthree f vs
--  | V.length vs < 3 = V.empty
--  | otherwise =
--    let (ps,vss) = V.splitAt 3 vs
--        mabc     = do a <- ps V.!? 0
--                      b <- ps V.!? 1
--                      c <- ps V.!? 2
--                      return (a,b,c)
--        in case mabc of
--         Nothing -> V.empty
--         Just (a,b,c) ->
--           let next = conthree f vss
--           in f a b c `V.cons` next

--toBezAndTris :: (V.Unbox a, V.Unbox b, Fractional a, Ord a)
--             => V.Vector (V2 a,b) -> (V.Vector (Bezier (V2 a)), V.Vector (Triangle b))
--toBezAndTris vs =
--  let (bs,cs) = V.unzip vs
--  in (conthree bezier bs, conthree (\a b c -> (a,b,c)) cs)

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

compileDrawing :: Rez -> DrawingData GLuint -> IO GLRenderer
compileDrawing rz DrawingData{..}
  | Just tex <- _drawTexture = do
    (c,r) <- compileDrawing rz DrawingData{ _drawGeometry = _drawGeometry
                                          , _drawTexture  = Nothing
                                          , _drawStroke   = _drawStroke
                                          , _drawOptions  = _drawOptions
                                          }
    return (c, bindTexAround tex . r)
  | StencilMaskOption:ops <- _drawOptions = do
    (c,r) <- compileDrawing rz DrawingData { _drawGeometry = _drawGeometry
                                           , _drawTexture  = _drawTexture
                                           , _drawStroke   = _drawStroke
                                           , _drawOptions  = ops
                                           }
    return (c, \t -> stencilMask (r t) (r t))
  | ColorGeometry gs <- _drawGeometry = B.foldM fc mempty gs
  | TextureGeometry gs <- _drawGeometry = B.foldM ft mempty gs
  | otherwise = return mempty
  where fc r0 g = do r1 <- compileColorGeometry rz (strokeWith _drawStroke) g
                     return $ r0 <> r1
        ft r0 g = do r1 <- compileTextureGeometry rz (strokeWith _drawStroke) g
                     return $ r0 <> r1

compilePicture :: Rez
               -> Picture GLuint
               -> IO GLRenderer
compilePicture rz = B.foldM f mempty . runPicture
  where f r d = mappend r <$> compileDrawing rz d

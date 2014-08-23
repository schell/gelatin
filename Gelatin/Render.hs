{-# LANGUAGE DataKinds #-}
module Gelatin.Render (
    renderDrawing,
    showDrawing
) where

import Graphics.Rendering.OpenGL hiding (Fill, Color, color, vertex)
import Graphics.GLUtil.ShaderProgram
import Graphics.VinylGL
import Gelatin.Core
import Gelatin.Shaders
import Data.Vinyl
import Linear
import Control.Monad.Free
import Control.Monad.Free.Church

transform2Mat :: Num a => Transform a -> M44 a
transform2Mat (Transform p (V3 sx sy sz) q) = t !*! s !*! q'
    where t  = mkTransformationMat eye3 p
          q' = mkTransformation q $ V3 0 0 0
          s  = V4 (V4 sx 0 0  0)
                  (V4 0 sy 0  0)
                  (V4 0  0 sz 0)
                  (V4 0  0 0  1)

color2V4 :: Color4 a -> V4 a
color2V4 (Color4 r b g a) = V4 r g b a

renderVertsAndUVs :: ShaderProgram -> M44 GLfloat -> [PlainFieldRec [V3gl, V4gl]] -> GLint -> IO ()
renderVertsAndUVs s m vs len = do
    vbo <- bufferVertices vs
    setUniforms s (modelview =: m)
    enableVertices' s vbo
    drawArrays Triangles 0 (fromIntegral len)
    deleteObjectNames [getVertexBuffer vbo]

renderCommand :: Real a => M44 a -> ShaderProgram -> Free (DrawCommand a) () -> IO ()
renderCommand _ _ (Pure ()) = return ()
renderCommand m s (Free (Fill (Color4 r g b a) vs n)) = do
    let vs'' = zipWith (<+>) (map (vertex =:) vs') (map (color =:) cs)
        len  = length vs
        vs'  = map (fmap realToFrac) vs
        cs   = replicate len $ fmap realToFrac $ V4 r g b a
        m'   = fmap (fmap realToFrac) m
    renderVertsAndUVs s m' vs'' $ fromIntegral len
    renderCommand m s n
renderCommand m s (Free (Gradient cs vs n)) = do
    let vs'' = zipWith (<+>) (map (vertex =:) vs') (map (color =:) cs')
        len = fromIntegral $ length vs
        vs'  = map (fmap realToFrac) vs
        cs'  = map (fmap realToFrac . color2V4) cs
        m'   = fmap (fmap realToFrac) m
    renderVertsAndUVs s m' vs'' len
    renderCommand m s n
renderCommand m rndr (Free (WithTransform t d n)) = do
    renderCommand (m !*! transform2Mat t) rndr (fromF d)
    renderCommand m rndr n

renderDrawing :: (Num a, Real a) => ShaderProgram -> Drawing a () -> IO ()
renderDrawing r = renderCommand eye4 r . fromF

pad :: Int -> String
pad i = replicate (2*i) ' '

showCommand :: Show a => Int -> Free (DrawCommand a) () -> String
showCommand _ (Pure ()) = ""
showCommand i (Free (Fill c vs n)) =
    unlines [ pad i ++ "Fill:" ++ show i
            , pad i ++ show c
            , pad i ++ show vs
            ] ++ pad i ++ showCommand i n
showCommand i (Free (Gradient cs vs n)) =
    unlines [ pad i ++ "Gradient:" ++ show i
            , pad i ++ show cs
            , pad i ++ show vs
            ] ++ pad i ++ showCommand i n
showCommand i (Free (WithTransform t d n)) =
    unlines [ pad i ++ "WithTransform:" ++ show i
            , pad i ++ show t
            , pad i ++ (showCommand (i+1) $ fromF d)
            ] ++ pad i ++ showCommand i n

showDrawing' :: Show a => Int -> Drawing a () -> String
showDrawing' i = showCommand i . fromF

showDrawing :: Show a => Drawing a () -> String
showDrawing = showDrawing' 0

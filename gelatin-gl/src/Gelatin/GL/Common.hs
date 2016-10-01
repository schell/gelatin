{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module Gelatin.GL.Common where

import Gelatin
import Gelatin.GL.Shader
import Linear

orthoContextProjection :: Context -> IO (M44 Float)
orthoContextProjection window = do
    (ww, wh) <- ctxWindowSize window
    let (hw,hh) = (fromIntegral ww, fromIntegral wh)
    return $ ortho 0 hw hh 0 0 1
--------------------------------------------------------------------------------
-- Renderings
--------------------------------------------------------------------------------
data Raster = Alpha Float
            | Multiply (V4 Float)
            | ColorReplacement (V4 Float)
            deriving (Show, Eq)

type RenderTransform2 = RenderTransform (V2 Float) Float Raster
type Renderer2        = Renderer        (V2 Float) Float Raster

move :: Float -> Float -> RenderTransform2
move x y = Spatial $ Translate $ V2 x y

moveV2 :: V2 Float -> RenderTransform2
moveV2 (V2 x y) = move x y

scale :: Float -> Float -> RenderTransform2
scale x y = Spatial $ Scale $ V2 x y

scaleV2 :: V2 Float -> RenderTransform2
scaleV2 (V2 x y) = scale x y

rotate :: Float -> RenderTransform2
rotate = Spatial . Rotate

alpha :: Float -> RenderTransform2
alpha = Special . Alpha

multiply :: Float -> Float -> Float -> Float -> RenderTransform2
multiply r g b a = Special $ Multiply $ V4 r g b a

multiplyV4 :: V4 Float -> RenderTransform2
multiplyV4 (V4 r g b a) = multiply r g b a

redChannelReplacement :: Float -> Float -> Float -> Float -> RenderTransform2
redChannelReplacement r g b a = Special $ ColorReplacement $ V4 r g b a

redChannelReplacementV4 :: V4 Float -> RenderTransform2
redChannelReplacementV4 (V4 r g b a) = redChannelReplacement r g b a
--------------------------------------------------------------------------------
-- GL helper types
--------------------------------------------------------------------------------
data Context = Context { ctxFramebufferSize :: IO (Int,Int)
                       , ctxWindowSize :: IO (Int,Int)
                       --, ctxScreenDpi :: IO Int
                       }

data Rez = Rez { rezShader  :: SumShader
               , rezContext :: Context
               }

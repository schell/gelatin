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
data RenderTransform = Spatial (Affine2 Float)
                     | Alpha Float
                     | Multiply (V4 Float)
                     | ColorReplacement (V4 Float)

extractSpatial :: [RenderTransform] -> [Affine2 Float]
extractSpatial = concatMap f
  where f (Spatial x) = [x]
        f _ = []

type GLRenderer = (IO (), [RenderTransform] -> IO ())

data Context = Context { ctxFramebufferSize :: IO (Int,Int)
                       , ctxWindowSize :: IO (Int,Int)
                       , ctxScreenDpi :: IO Int
                       }

data Rez = Rez { rezShader  :: SumShader
               , rezContext :: Context
               }

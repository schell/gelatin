{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module Gelatin.GL.Common where

import Gelatin
import Gelatin.GL.Shader
import Linear
--------------------------------------------------------------------------------
-- Transforming Renderings
--------------------------------------------------------------------------------
data PictureTransform = PictureTransform { ptfrmMV       :: M44 Float
                                         , ptfrmAlpha    :: !Float
                                         , ptfrmMultiply :: !(V4 Float)
                                         } deriving (Show, Eq)

instance Monoid PictureTransform where
  mempty = PictureTransform identity 1 1
  mappend (PictureTransform amv aa am)
          (PictureTransform bmv ba bm) =
    PictureTransform (amv !*! bmv) (aa * ba) (am * bm)

--modelviewProjection :: V2 Float -> V2 Float -> Float -> M44 Float
--modelviewProjection (V2 x y) (V2 w h) r =
--    let sxy = V3 w h 1
--        txy = V3 x y 0
--        rxy = V3 0 0 1
--        rot = if r /= 0 then mat4Rotate r rxy else identity
--    in mat4Translate txy !*! rot !*! mat4Scale sxy

orthoContextProjection :: Context -> IO (M44 Float)
orthoContextProjection window = do
    (ww, wh) <- ctxWindowSize window
    let (hw,hh) = (fromIntegral ww, fromIntegral wh)
    return $ ortho 0 hw hh 0 0 1

--applyPicTfrmV2 :: PictureTransform -> V2 Float -> V2 Float
--applyPicTfrmV2 p v =
--  let mv  = ptfrmMV p
--      m41 = mv !*! v3ToM41 (promoteV2 v)
--  in demoteV3 $ m41ToV3 m41
--------------------------------------------------------------------------------
-- Renderings
--------------------------------------------------------------------------------
type GLRenderer = (IO (), PictureTransform -> IO ())

data Context = Context { ctxFramebufferSize :: IO (Int,Int)
                       , ctxWindowSize :: IO (Int,Int)
                       , ctxScreenDpi :: IO Int
                       }

data Rez = Rez { rezShader  :: SumShader
               , rezContext :: Context
               }

{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module Gelatin.GL.Common where

import Gelatin.GL.Shader
import Linear
import Control.Monad (msum)
--------------------------------------------------------------------------------
-- Transforming Renderings
--------------------------------------------------------------------------------
data PictureTransform = PictureTransform { ptfrmMV       :: M44 Float
                                         , ptfrmAlpha    :: !Float
                                         , ptfrmMultiply :: !(V4 Float)
                                         , ptfrmReplace  :: !(Maybe (V4 Float))
                                         } deriving (Show, Eq)

instance Monoid PictureTransform where
  mempty = PictureTransform identity 1 1 Nothing
  mappend (PictureTransform amv aa am ar)
          (PictureTransform bmv ba bm br) =
    PictureTransform (amv !*! bmv) (aa * ba) (am * bm) (appendReplace ar br)
      where appendReplace (Just a) (Just b) = Just $ a * b
            appendReplace a b = msum [a,b]

orthoContextProjection :: Context -> IO (M44 Float)
orthoContextProjection window = do
    (ww, wh) <- ctxWindowSize window
    let (hw,hh) = (fromIntegral ww, fromIntegral wh)
    return $ ortho 0 hw hh 0 0 1
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

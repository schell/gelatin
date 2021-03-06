{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Gelatin.GL.Common where

import           Gelatin
import           Gelatin.GL.Shader

orthoContextProjection :: Context -> IO (M44 Float)
orthoContextProjection window = do
    (ww, wh) <- ctxWindowSize window
    let (hw,hh) = (fromIntegral ww, fromIntegral wh)
    return $ ortho 0 hw hh 0 0 1
--------------------------------------------------------------------------------
-- GL helper types
--------------------------------------------------------------------------------
data Context = Context { ctxFramebufferSize :: IO (Int,Int)
                       , ctxWindowSize      :: IO (Int,Int)
                       --, ctxScreenDpi :: IO Int
                       }

data Rez = Rez { rezShader  :: Simple2DShader
               , rezContext :: Context
               }

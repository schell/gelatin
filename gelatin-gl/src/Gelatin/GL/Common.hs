module Gelatin.GL.Common where

import Gelatin.Picture
import Gelatin.GL.Shader
import Data.Renderable

type GLRenderer = Renderer IO Transform

data Context = Context { ctxFramebufferSize :: IO (Int,Int) 
                       , ctxWindowSize :: IO (Int,Int)
                       , ctxScreenDpi :: IO Int
                       }

data Rez = Rez { rezShader  :: SumShader
               , rezContext :: Context
               }

{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
module Gelatin.Rendering.Core where

import Linear
import Gelatin.Shaders.Core
import Graphics.Rendering.OpenGL
import Graphics.VinylGL
import Graphics.GLUtil hiding (Elem, setUniform)
import Control.Monad.Free
import Control.Monad.Free.Church
import Data.Vinyl
import Data.Vinyl.Universe
import Data.Vinyl.Reflect

data ShaderUniform where
    ShaderUniform :: ( HasFieldNames (PlainFieldRec '[n ::: t])
                     , HasFieldGLTypes (PlainFieldRec '[n ::: t])
                     , SetUniformFields (PlainFieldRec '[n ::: t])
                     ) => SField (n:::t) -> t -> ShaderUniform

data SetUniform next = SetUniform ShaderUniform next

type SetUniformCommand = F SetUniform

instance Functor SetUniform where
    fmap f (SetUniform su next) = SetUniform su $ f next

setUniform :: ( HasFieldNames (PlainFieldRec '[n ::: t])
              , HasFieldGLTypes (PlainFieldRec '[n ::: t])
              , SetUniformFields (PlainFieldRec '[n ::: t])
              ) => SField (n:::t) -> t -> SetUniformCommand ()
setUniform u d = liftF $ SetUniform (ShaderUniform u d) ()

performUniformCommand :: ShaderProgram -> Free SetUniform () -> IO ()
performUniformCommand _ (Pure ()) = return ()
performUniformCommand s (Free (SetUniform (ShaderUniform u m) next)) = do
    setUniforms s (u =: m)
    performUniformCommand s next

updatePJMV :: SetUniformCommand ()
updatePJMV = do
    setUniform modelview (eye4 :: M44 GLfloat)
    setUniform projection (eye4 :: M44 GLfloat)


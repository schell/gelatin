{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
module Gelatin.Rendering.Core where

import Linear
import Gelatin.Shaders.Core
import Graphics.Rendering.OpenGL
import Graphics.VinylGL
import Graphics.GLUtil hiding (setUniform)
import Control.Monad.Free
import Control.Monad.Free.Church
import Data.Vinyl.Universe
import Data.Vinyl.Derived
import Data.Vinyl
import Data.Vinyl.TyFun
import Data.Vinyl.Reflect

--------------------------------------------------------------------------------
-- Types
--------------------------------------------------------------------------------

data SetUniform a b next = SetUniform a b next

type SetUniformCommand a b = F (SetUniform a b)

instance Functor (SetUniform a b) where
    fmap f (SetUniform a b next) = SetUniform a b $ f next

setUniform :: a -> b -> SetUniformCommand a b ()
setUniform loc u = liftF $ SetUniform loc u ()

performUniformCommand :: (HasFieldNames (PlainFieldRec '[k1]),
                          HasFieldGLTypes (PlainFieldRec '[k1]),
                          SetUniformFields (PlainFieldRec '[k1]))
                      => ShaderProgram
                      -> Free (SetUniform (sing k1) (ElField $ k1)) ()
                      -> IO ()
performUniformCommand _ (Pure ()) = return ()
performUniformCommand s (Free (SetUniform loc u next)) = do
    setUniforms s (loc =: u)
    performUniformCommand s next

updatePJMV = do
    setUniform projection $ eye4
    setUniform modelview $ eye4

--type VertexBufferDef u v = F (VertexCompCommand u v ())

data OpenGLCommand next = UsingDepthFunc ComparisonFunction (OpenGL ()) next
                        | UsingShader ShaderProgram (OpenGL ()) next
--                        | UsingVertexBuffer (VertexBufferDef u v) (Rendering ()) next
--                        -- TODO: Add a bunch more...

type OpenGL = F OpenGLCommand

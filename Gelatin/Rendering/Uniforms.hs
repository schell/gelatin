{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
module Gelatin.Rendering.Core where

import Linear
import Gelatin.Shaders.Core
import Graphics.Rendering.OpenGL
import Control.Monad.Free
import Control.Monad.Free.Church
import Graphics.VinylGL
import Data.Vinyl
import Data.Vinyl.Universe


data SetUniform next where
    SetUniform :: SField (n:::t) -> t -> next -> SetUniform next

type SetUniformCommand = F SetUniform
--
instance Functor SetUniform where
    fmap f (SetUniform u d next) = SetUniform u d $ f next

setUniform :: SField (n:::t) -> t -> SetUniformCommand ()
setUniform n t = liftF $ SetUniform n t ()

performUniformCommand _ (Pure ()) = return ()
performUniformCommand s (Free (SetUniform loc u next)) = do
    setUniforms s (loc =: u)
    performUniformCommand s next

updatePJMV :: SetUniformCommand ()
updatePJMV = do
    setUniform projection (eye4 :: M44 GLfloat)
    setUniform modelview (eye4 :: M44 GLfloat)


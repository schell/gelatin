module Gelatin.Rendering.Attributes where

import Control.Monad.Free
import Control.Monad.Free.Church

--------------------------------------------------------------------------------
-- Types
--------------------------------------------------------------------------------

data SetAttribute a next = SetAttribute a next

type SetUniformCommand a = F (SetUniform a)

--------------------------------------------------------------------------------
-- Instances
--------------------------------------------------------------------------------

instance Functor (SetUniform a) where
    fmap f (SetUniform a next) = SetUniform a $ f next

--------------------------------------------------------------------------------
-- Creating a uniform update.
--------------------------------------------------------------------------------

setUniform :: u -> SetUniformCommand u ()
setUniform u = liftF $ SetUniform u ()

--------------------------------------------------------------------------------
-- Compiling a uniform update.
--------------------------------------------------------------------------------

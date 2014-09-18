{-# LANGUAGE GADTs #-}
module Gelatin.TextureCommands where

import Control.Monad.Free.Church
import Graphics.Rendering.OpenGL
import Graphics.GLUtil
import System.FilePath
import System.Directory

-- | TODO: Add a bunch more texture commands.

--------------------------------------------------------------------------------
-- Types
--------------------------------------------------------------------------------
data TextureOp next where
    SetFilter :: MinificationFilter -> MagnificationFilter -> next -> TextureOp next
    SetWrapMode :: TextureCoordName -> Repetition -> Clamping -> next -> TextureOp next

type TextureCommand = F TextureOp

data TextureSrc = Local FilePath
                | Relative FilePath
                deriving (Show, Eq, Ord)
--------------------------------------------------------------------------------
-- Instances
--------------------------------------------------------------------------------
instance Functor TextureOp where
    fmap f (SetFilter mn mg n) = SetFilter mn mg $ f n
    fmap f (SetWrapMode c rep clamp n) = SetWrapMode c rep clamp $ f n
--------------------------------------------------------------------------------
-- User API
--------------------------------------------------------------------------------
setFilter :: MinificationFilter -> MagnificationFilter -> TextureCommand ()
setFilter mn mag = liftF $ SetFilter mn mag ()

setWrapMode :: TextureCoordName -> Repetition -> Clamping -> TextureCommand ()
setWrapMode c rep clamp = liftF $ SetWrapMode c rep clamp ()
--------------------------------------------------------------------------------
-- Loading textures.
--------------------------------------------------------------------------------
loadTextureSrc :: TextureSrc -> IO (Either String TextureObject)
loadTextureSrc (Local fp) = readTexture fp
loadTextureSrc (Relative fp) = do
    fp' <- fmap (</> fp) getCurrentDirectory
    readTexture fp'

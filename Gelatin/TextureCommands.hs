{-# LANGUAGE GADTs #-}
module Gelatin.TextureCommands where

import Control.Monad.Free.Church
import Graphics.Rendering.OpenGL
import Graphics.GLUtil
import System.FilePath
import System.Directory

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
loadTextureSrc (Loaded obj) = return $ Right obj
--------------------------------------------------------------------------------
-- Instances
--------------------------------------------------------------------------------
instance Functor TextureOp where
    fmap f (SetFilter mn mg n) = SetFilter mn mg $ f n
    fmap f (SetWrapMode c rep clamp n) = SetWrapMode c rep clamp $ f n
--------------------------------------------------------------------------------
-- Types
--------------------------------------------------------------------------------
-- | TODO: Add a bunch more texture commands.
data TextureOp next where
    SetFilter :: MinificationFilter -> MagnificationFilter -> next -> TextureOp next
    SetWrapMode :: TextureCoordName -> Repetition -> Clamping -> next -> TextureOp next

type TextureCommand = F TextureOp

-- | TODO: Enable loading from the network?
data TextureSrc = Local FilePath
                | Relative FilePath
                | Loaded TextureObject
                deriving (Show, Eq, Ord)

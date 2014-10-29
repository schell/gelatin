{-# LANGUAGE GADTs #-}
module Gelatin.Core.TextureCommands where

import Control.Monad.Free.Church
import Graphics.Rendering.OpenGL
import Graphics.GLUtil
import System.FilePath
import System.Directory
import qualified Data.Map.Strict as M

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
loadTextureSrc :: TextureSrc -> IO (M.Map FilePath TextureObject)
loadTextureSrc (Loaded str obj) = return $ M.insert str obj M.empty
loadTextureSrc (Local fp) = do
    putStrLn $ "Loading texture " ++ fp
    wrapTexture fp
loadTextureSrc (Relative fp) = do
    fp' <- fmap (</> fp) getCurrentDirectory
    putStrLn $ "Loading texture " ++ fp'
    wrapTexture fp

wrapTexture :: FilePath -> IO (M.Map FilePath TextureObject)
wrapTexture k = do
    eT <- readTexture k
    case eT of
        Left s  -> putStrLn s >> return M.empty
        Right v -> return $ M.insert k v M.empty

textureKey :: TextureSrc -> String
textureKey (Loaded k _) = k
textureKey (Local k) = k
textureKey (Relative k) = k
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
                | Loaded String TextureObject
                deriving (Show, Eq, Ord)

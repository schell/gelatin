{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
import           Gelatin
import           Gelatin.GL.Picture
import           Linear as L
import qualified Data.Vector as B
import qualified Data.Vector.Unboxed as V
import           Control.Lens

--------------------------------------------------------------------------------
-- Example
--------------------------------------------------------------------------------
main :: IO ()
main = putStrLn "Hello"

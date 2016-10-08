module Gelatin.Shaders
  ( module S
  , simple2dVertFilePath
  , simple2dFragFilePath
  ) where

import Gelatin.Shaders.Common as S
import Gelatin.Shaders.Simple2D as S
import Paths_gelatin_shaders as S
import System.FilePath

simple2dVertFilePath :: IO FilePath
simple2dVertFilePath = getDataFileName $ "shaders" </> "simple2d.vert"

simple2dFragFilePath :: IO FilePath
simple2dFragFilePath = getDataFileName $ "shaders" </> "simple2d.frag"

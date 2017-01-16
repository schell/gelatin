module Gelatin.Shaders
  ( module S
  , simple2dVertFilePath
  , simple2dFragFilePath
  , simple2dVertWebGLFilePath
  , simple2dFragWebGLFilePath
  ) where

import           Gelatin.Shaders.Common    as S
import           Gelatin.Shaders.Simple2D  as S
import           Gelatin.Shaders.TypeLevel as S
import           Paths_gelatin_shaders     as S
import           System.FilePath

simple2dVertFilePath :: IO FilePath
simple2dVertFilePath = getDataFileName $ "shaders" </> "simple2d.vert"

simple2dFragFilePath :: IO FilePath
simple2dFragFilePath = getDataFileName $ "shaders" </> "simple2d.frag"

simple2dVertWebGLFilePath :: IO FilePath
simple2dVertWebGLFilePath = getDataFileName $ "shaders" </> "simple2dwebgl.vert"

simple2dFragWebGLFilePath :: IO FilePath
simple2dFragWebGLFilePath = getDataFileName $ "shaders" </> "simple2dwebgl.frag"

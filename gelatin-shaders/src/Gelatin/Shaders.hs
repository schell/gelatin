module Gelatin.Shaders
  ( module Gelatin.Shaders
  , module S
  ) where

import           Gelatin.Shaders.Common    as S
import           Gelatin.Shaders.TypeLevel as S
import           Paths_gelatin_shaders     as S
import           System.FilePath

inShaderDir :: FilePath -> IO FilePath
inShaderDir = getDataFileName . ("shaders" </>)

simple2dVertFilePath :: IO FilePath
simple2dVertFilePath = inShaderDir "simple2d.vert"

simple2dFragFilePath :: IO FilePath
simple2dFragFilePath = inShaderDir "simple2d.frag"

simple3dVertFilePath :: IO FilePath
simple3dVertFilePath = inShaderDir "simple3d.vert"

simple3dFragFilePath :: IO FilePath
simple3dFragFilePath = inShaderDir "simple3d.frag"

simple2dVertWebGLFilePath :: IO FilePath
simple2dVertWebGLFilePath = inShaderDir "simple2dwebgl.vert"

simple2dFragWebGLFilePath :: IO FilePath
simple2dFragWebGLFilePath = inShaderDir "simple2dwebgl.frag"

simple3dVertWebGLFilePath :: IO FilePath
simple3dVertWebGLFilePath = inShaderDir "simple3dwebgl.vert"

simple3dFragWebGLFilePath :: IO FilePath
simple3dFragWebGLFilePath = inShaderDir "simple3dwebgl.frag"

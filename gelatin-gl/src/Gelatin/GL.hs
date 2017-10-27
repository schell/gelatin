{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TupleSections #-}
-- |
-- Module:     Gelatin.GL
-- Copyright:  (c) 2017 Schell Scivally
-- License:    MIT
-- Maintainer: Schell Scivally <schell@takt.com>
--
-- ["Gelatin.GL.Renderer"]
-- Rendering specific geometries in IO.
--
-- ["Gelatin.GL.Compiler"]
-- Compiling and marshaling general geometries to the renderer.
--
-- ["Gelatin.GL.Shader"]
-- Loading and compiling the OpenGL shaders needed to run the renderer.
--
-- ["Gelatin.GL.Common"]
-- Some shared stuff.
--


module Gelatin.GL
  ( module Gelatin.GL.Renderer
  , module Gelatin.GL.Shader
  , module Gelatin.GL.Common
  , module Gelatin.GL.Compiler
    -- * Re-exports
  , module Gelatin
  , module Graphics.GL.Types
  , module Graphics.GL.Core33
  ) where

import Gelatin.GL.Renderer
import Gelatin.GL.Shader
import Gelatin.GL.Common
import Gelatin.GL.Compiler
import Gelatin
import Graphics.GL.Types
import Graphics.GL.Core33

{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}

{-# LANGUAGE TupleSections         #-}
module Gelatin.GL
  ( module G
  , module GL
  ) where

import           Gelatin                    as G
import           Gelatin.GL.Picture         as G
import           Gelatin.GL.Renderer.Common as G
import           Graphics.GL.Core33         as GL
import           Graphics.GL.Types          as GL

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}

{-# LANGUAGE TupleSections #-}
module Gelatin.GL (
    -- * Re-exports
    module G,
    module GL,
    module Linear,
) where

import Gelatin.GL.Renderer as G
import Gelatin.GL.Shader as G
import Gelatin.GL.Common as G
import Gelatin.GL.Picture as G
import Gelatin as G
import Data.Bits ((.|.))
import Graphics.GL.Types as GL
import Graphics.GL.Core33 as GL

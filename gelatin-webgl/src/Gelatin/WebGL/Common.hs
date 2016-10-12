module Gelatin.WebGL.Common where

import           Control.Monad.Trans.Either
import           Control.Monad.Trans.Reader
import           GHCJS.Types
import           GHCJS.DOM.Types

type WebGLT m = EitherT String (ReaderT WebGLRenderingContextBase m)


-- |
-- Module:     Gelatin
-- Copyright:  (c) 2017 Schell Scivally
-- License:    MIT
-- Maintainer: Schell Scivally <schell@takt.com>
--
-- [@Core@]
-- Core types and pure functions.
--
-- [@Picture@]
-- Creating pictures.
--
-- [@Compiler@]
-- Shared types for writing backends and compiling pictures.
--
module Gelatin
  ( -- * Re-exports
    module Gelatin.Core
  , module Gelatin.Picture
  , module Gelatin.Compiler
  , module Linear
  ) where

import Gelatin.Core
import Gelatin.Picture
import Gelatin.Compiler
import Linear hiding (rotate)

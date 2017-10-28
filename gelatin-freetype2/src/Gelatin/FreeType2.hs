-- |
-- Module:     Gelatin.GL
-- Copyright:  (c) 2017 Schell Scivally
-- License:    MIT
-- Maintainer: Schell Scivally <schell@takt.com>
--
-- This module provides font string rendering through the legendary freetype2.
-- It automatically manages a texture atlas and word atlas to speed up rendering.
module Gelatin.FreeType2
  (-- * Getting straight to rendering
    freetypeRenderer2
    -- * Creating a gelatin picture
  , freetypePicture
    -- * Creating an Atlas
  , allocAtlas
  , asciiChars
  , freeAtlas
  , loadWords
  , unloadMissingWords
  , Atlas(..)
    -- * Going deeper
    -- ** Glyphs
  , GlyphSize(..)
  , glyphWidth
  , glyphHeight
    -- ** Measuring glyphs
  , GlyphMetrics(..)
  ) where

import Gelatin.FreeType2.Internal

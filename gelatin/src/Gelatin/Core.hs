-- | In this module you'll find the types and functions used throughout gelatin.
--
-- [@Bezier@]
-- Inner and outer beziers.
--
-- [@Bounds@]
-- Working with bounding boxes.
--
-- [@Color@]
-- All the nifty named css colors.
--
-- [@Polyline@]
-- Creating smooth, anti-aliased lines with end caps.
--
-- [@Stroke@]
-- Helpers for stroking polylines.
--
-- [@Transform@]
-- Affine transformations (and more).
--
-- [@Triangle@]
-- Most likely not used - contains triangles.
--
-- [@Utils@]
-- Various utilities.
module Gelatin.Core (
    module Gelatin.Core.Bezier
  , module Gelatin.Core.Bounds
  , module Gelatin.Core.Color
--  , module Gelatin.Core.Font
  , module Gelatin.Core.Polyline
  , module Gelatin.Core.Utils
  , module Gelatin.Core.Stroke
  , module Gelatin.Core.Transform
  , module Gelatin.Core.Triangle
) where

import Gelatin.Core.Bezier
import Gelatin.Core.Bounds
import Gelatin.Core.Color
--import Gelatin.Core.Font
import Gelatin.Core.Polyline
import Gelatin.Core.Utils
import Gelatin.Core.Stroke
import Gelatin.Core.Transform
import Gelatin.Core.Triangle

module Gelatin.Geometry.Types where

import Linear

type Triangle a = (V2 a, V2 a, V2 a)
type Line a = (V2 a, V2 a)
type Polygon a = [V2 a]

module Gelatin.Geometry.Types where

data Primitive f a = Line (f a) (f a)
                   -- ^ A straight line from one point to another.
                   | Curve a [f a]
                   -- ^ A n-bezier curve with a maximum segment distance.
                   -- This distance is used during rasterization as the
                   -- moximum tolerable distance between resolved points on
                   -- the curve. A smaller number will result in a higher
                   -- curve resolution. (So if your curve doesn't look
                   -- smooth, make this number smaller.)
                   | Triangle (f a) (f a) (f a)
                   -- ^ A flat triangle of three points.
                   | Path [f a]
                   -- ^ A list of points defining a path .
                   deriving (Show)

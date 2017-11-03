-- | A picture in gelatin's context is a collection of vertices, organized into
-- geometries of triangles, beziers, triangle strips, triangle fans and polylines.
-- The vertices of these pictures can be anything, but the currently available
-- backends already support these vertices:
--
-- * @(V2 Float, V4 Float)@, ie. colored points in 2d space
-- * @(V2 Float, V2 Float)@, ie. textured points in 2d space
--
module Gelatin.Picture (
  -- * Defining Vertex Data
    VerticesT(..)
  , runVerticesT
  , Vertices
  , runVertices
  , tri
  , bez
  , to
  , addVertexList
  , segment
  , mapVertices
  -- * Defining Geometry (Vertex Data + Drawing Operation)
  , RawGeometry(..)
  , mapRawGeometry
  , GeometryT(..)
  , runGeometryT
  , Geometry
  , runGeometry
  , triangles
  , beziers
  , strip
  , fan
  , line
  , mapGeometry
  -- * The Picture API
  , PictureT
  , runPictureT
  , Picture
  , runPicture
  , setRawGeometry
  , getRawGeometry
  , setGeometry
  , setStroke
  , getStroke
  , setTextures
  , getTextures
  , setRenderingOptions
  , getRenderingOptions
  -- * An example of creating a Picture
  -- $creating
  -- * Making shapes
  , module S
  -- * Measuring Pictures (2d)
  , mapPictureVertices
  , pictureBounds2
  , pictureSize2
  , pictureOrigin2
  , pictureCenter2
  -- * Measuring Pictures (3d)
  , pictureBounds3
  , pictureSize3
  , pictureOrigin3
  , pictureCenter3
  -- * Underlying PictureData Exported for renderers
  , RenderingOption(..)
  , PictureData(..)
) where

import           Gelatin.Picture.Internal
import           Gelatin.Picture.Shapes   as S

-- $creating
-- Here is an example of drawing two colored beziers into a 2d picture using
-- colors from the 'Gelatin.Core.Color' module:
--
-- > bezierPicture :: Picture tex (V2 Float, V4 Float) ()
-- > bezierPicture = setGeometry $ beziers $ do
-- >   bez (V2 0   0,   white) (V2 200 0, blue) (V2 200 200, green)
-- >   bez (V2 400 200, white) (V2 400 0, blue) (V2 200 0,   green)
--
-- Here is the rendering of that picture after being compiled by a backend:
--
-- <<docimages/twoBeziers.png>>
--
-- As you can see the two beziers have different fill directions, the first is
-- fill inner while the second is fill outer. This is determined by the bezier's
-- winding.

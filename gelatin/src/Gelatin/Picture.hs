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
  -- * Making shapes
  , module S
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
  -- * Measuring Pictures
  , pictureBounds2
  , pictureSize2
  , pictureOrigin2
  , pictureCenter2
--  -- * Measuring Pictures (Outside of their definition)
--  , runPictureBoundsT
--  , runPictureSizeT
--  , runPictureOriginT
--  , runPictureCenterT
--  , runPictureBounds
--  , runPictureSize
--  , runPictureOrigin
--  , runPictureCenter
  -- * Underlying PictureData Exported for renderers
  , RenderingOption(..)
  , PictureData(..)
) where

import Gelatin.Picture.Internal
import Gelatin.Picture.Shapes as S

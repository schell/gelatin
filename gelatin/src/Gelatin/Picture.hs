module Gelatin.Picture (
  -- * Defining Vertex Data
    VerticesT(..)
  , runVerticesT
  , Vertices
  , runVertices
  , tri
  , bez
  , to
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
  , embed
  , overlay
  , setRawGeometry
  , getRawGeometry
  , setGeometry
  , move
  , scale
  , rotate
  , setAlpha
  , getAlpha
  , alpha
  , setMultiply
  , getMultiply
  , multiply
  , setReplacementColor
  , clearReplacementColor
  , setStroke
  , getStroke
  , setTextures
  , getTextures
  , setRenderingOptions
  , getRenderingOptions
  -- * Measuring Pictures
  , pictureBounds
  , pictureSize
  , pictureOrigin
  , pictureCenter
  -- * Measuring Pictures (Outside of their definition)
  , runPictureBoundsT
  , runPictureSizeT
  , runPictureOriginT
  , runPictureCenterT
  , runPictureBounds
  , runPictureSize
  , runPictureOrigin
  , runPictureCenter
  -- * Underlying PictureData Exported for renderers
  , RenderingOption(..)
  , PictureData(..)
) where

import Gelatin.Picture.Internal
import Gelatin.Picture.Shapes as S

module Gelatin.Picture (
  -- * Defining Vertex Data
    Vertices(..)
  , tri
  , bez
  , to
  , segment
  , vertices
  , mapVertices
  , runVertices
  -- * Making shapes
  , module S
  -- * Defining Geometry (Vertex Data + Drawing Operation)
  , RawGeometry(..)
  , mapRawGeometry
  , Geometry(..)
  , add
  , triangles
  , beziers
  , strip
  , fan
  , line
  , geometry
  , mapGeometry
  , runGeometry
  -- * The Picture API
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
  , pictureBounds'
  , pictureSize'
  , pictureOrigin'
  , pictureCenter'
  -- * Underlying PictureData Exported for renderers
  , RenderingOption(..)
  , PictureData(..)

) where

import Gelatin.Picture.Internal
import Gelatin.Picture.Shapes as S

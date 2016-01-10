{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
module Gelatin.Core.Rendering.Types (
    runRendering,
    cleanRendering,
    Rendering(..),
    ShaderDef(..),
    Shader(..),
    PolylineShader(..),
    ProjectedPolylineShader(..),
    GeomShader(..),
    BezShader(..),
    MaskShader(..),
    SumShader(..),
    shProjectedPolyline,
    shPolyline,
    shBezier,
    shMask,
    shGeometry,
    ClippingArea,
    Point(..),
    Line(..),
    Triangle(..),
    triPoints,
    trisToComp,
    FontString(..),
    LineCap(..),
    LineJoin(..),
    Joint(..),
    Winding(..),
    Fill(..),
    FillResult(..),
    module T
) where

import Linear as J hiding (rotate)
import Prelude hiding (init)
import Graphics.GL.Types
import Graphics.Text.TrueType hiding (CompositeScaling(..))
import Data.ByteString.Char8 (ByteString)
import Control.Lens hiding (transform)
import Gelatin.Core.Rendering.Transform as T
--------------------------------------------------------------------------------
-- Text
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
-- Special Rendering
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
-- Shaders
--------------------------------------------------------------------------------
type ShaderProgram = GLuint

data Shader = Shader { shProgram :: ShaderProgram
                     , shAttributes :: [(String, GLint)]
                     } deriving (Show)

data ShaderDef = ShaderDefFP { shShaderPaths :: [(String, GLuint)]
                             -- ^ [("path/to/shader.vert", GL_VERTEX_SHADER)]
                             , shUniforms :: [String]
                             -- ^ ["projection", "modelview", ..]
                             }
               | ShaderDefBS { shShaderSrcs :: [(ByteString, GLuint)]
                             , shUniforms :: [String]
                             } deriving (Show, Eq, Ord)

newtype PolylineShader = PRS Shader
newtype ProjectedPolylineShader = PPRS Shader
newtype GeomShader = GRS Shader
newtype BezShader = BRS Shader
newtype MaskShader = MRS Shader
data SumShader = SRS { _shProjectedPolyline :: ProjectedPolylineShader
                     , _shPolyline :: PolylineShader
                     , _shGeometry :: GeomShader
                     , _shBezier   :: BezShader
                     , _shMask     :: MaskShader
                     }
makeLenses ''SumShader

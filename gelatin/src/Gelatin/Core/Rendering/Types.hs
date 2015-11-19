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
    Bezier(..),
    QuadraticBezier(..),
    CubicBezier(..),
    Triangle(..),
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
data FontString = FontString Font Float (Float,Float) String
--------------------------------------------------------------------------------
-- Coloring
--------------------------------------------------------------------------------
data Fill = FillColor (V2 Float -> V4 Float)
          | FillTexture FilePath (V2 Float -> V2 Float)

data FillResult = FillResultColor [V4 Float]
                | FillResultTexture GLuint [V2 Float]
--------------------------------------------------------------------------------
-- Polylines
--------------------------------------------------------------------------------
data LineJoin = LineJoinMiter
              | LineJoinBevel
              deriving (Show, Eq)
data LineCap = LineCapNone
             | LineCapButt
             | LineCapSquare
             | LineCapRound
             | LineCapTriOut
             | LineCapTriIn
             deriving (Show, Eq, Enum)
data Winding = Clockwise
             | CounterCW
             deriving (Show, Eq)
data Joint = Cap (V2 Float) [V2 Float]
           | Elbow Winding (V2 Float, V2 Float) [V2 Float]
           deriving (Show, Eq)
--------------------------------------------------------------------------------
-- Drawing Primitives
--------------------------------------------------------------------------------
data Bezier a = Bezier Ordering a a a deriving (Show, Eq)
data QuadraticBezier a = QuadraticBezier a a a deriving (Show, Eq)
data CubicBezier a = CubicBezier a a a a deriving (Show, Eq)
newtype NBezier a = NBezier [a] deriving (Show, Eq)
data Triangle a = Triangle a a a deriving (Show, Eq)
data Line a = Line a a deriving (Show, Eq)
data Point a = Point a

instance Functor Triangle where
    fmap f (Triangle a b c) = Triangle (f a ) (f b) (f c)

instance Functor Bezier where
    fmap f (Bezier o a b c) = Bezier o (f a) (f b) (f c)

instance Functor QuadraticBezier where
    fmap f (QuadraticBezier a b c) = QuadraticBezier (f a) (f b) (f c)

instance Functor CubicBezier where
    fmap f (CubicBezier a b c d) = CubicBezier (f a) (f b) (f c) (f d)

instance Functor Line where
    fmap f (Line a b) = Line (f a) (f b)

instance Functor Point where
    fmap f (Point v) = Point $ f v

instance Transformable Transform a => Transformable Transform (Line a) where
    transform = fmap . transform

instance Transformable Transform a => Transformable Transform (Triangle a) where
    transform = fmap . transform

instance Transformable Transform a => Transformable Transform (Bezier a) where
    transform = fmap . transform

instance Transformable Transform a => Transformable Transform (QuadraticBezier a) where
    transform = fmap . transform

instance Transformable Transform a => Transformable Transform (CubicBezier a) where
    transform = fmap . transform
--------------------------------------------------------------------------------
-- Special Rendering
--------------------------------------------------------------------------------
type ClippingArea = (V2 Int, V2 Int)
--------------------------------------------------------------------------------
-- General Rendering
--------------------------------------------------------------------------------
runRendering :: Transform -> Rendering -> IO ()
runRendering t (Rendering f _) = f t

cleanRendering :: Rendering -> IO ()
cleanRendering (Rendering _ c) = c

instance Monoid Rendering where
    mempty = Rendering (const $ return ()) (return ())
    (Rendering ar ac) `mappend` (Rendering br bc) =
        Rendering (\t -> ar t >> br t) (ac >> bc)

data Rendering = Rendering RenderFunction CleanupFunction
type RenderFunction = Transform -> IO ()

type CleanupFunction = IO ()
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

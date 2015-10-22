{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell #-}
module Gelatin.Core.Rendering.Types (
    runRendering,
    cleanRendering,
    Rendering(..),
    ShaderDef(..),
    Shader(..),
    PolylineShader(..),
    GeomShader(..),
    BezShader(..),
    MaskShader(..),
    SumShader(..),
    shBezier,
    shPolyline,
    shMask,
    shGeometry,
    Transform(..),
    ClippingArea,
    Point(..),
    Line(..),
    Bezier(..),
    Triangle(..),
    FontString(..),
    EndCap(..),
    LineJoin(..),
    Joint(..),
    Winding(..),
    Fill(..),
    FillResult(..)
) where

import Linear as J hiding (rotate)
import Prelude hiding (init)
import Graphics.GL.Types
import Graphics.Text.TrueType hiding (CompositeScaling(..))
import Data.Typeable
import Data.ByteString.Char8 (ByteString)
import Control.Lens
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
data EndCap = EndCapButt
            | EndCapBevel
            | EndCapSquare
            | EndCapRound
            deriving (Show, Eq)
data Winding = Clockwise
             | CounterCW
             deriving (Show, Eq)
data Joint = Cap (V2 Float) [V2 Float]
           | Elbow Winding (V2 Float, V2 Float) [V2 Float]
           deriving (Show, Eq)
--------------------------------------------------------------------------------
-- Drawing Primitives
--------------------------------------------------------------------------------
data Primitive a = PrimitiveBez (Bezier a)
                 | PrimitiveTri (Triangle a)
                 deriving (Show, Eq)

instance Functor Triangle where
    fmap f (Triangle a b c) = Triangle (f a ) (f b) (f c)

instance Functor Bezier where
    fmap f (Bezier o a b c) = Bezier o (f a) (f b) (f c)

instance Functor Line where
    fmap f (Line a b) = Line (f a) (f b)

instance Functor Point where
    fmap f (Point v) = Point $ f v

data Bezier a = Bezier Ordering a a a deriving (Show, Eq)
data Triangle a = Triangle a a a deriving (Show, Eq)
data Line a = Line a a deriving (Show, Eq)
data Point a = Point a
--------------------------------------------------------------------------------
-- Special Rendering
--------------------------------------------------------------------------------
type ClippingArea = (V2 Int, V2 Int)
--------------------------------------------------------------------------------
-- Affine Transformation
--------------------------------------------------------------------------------
data Transform = Transform { tfrmTranslation :: Position
                           , tfrmScale       :: Scale
                           , tfrmRotation    :: Rotation
                           } deriving (Show, Typeable)

instance Monoid Transform where
    mempty = Transform zero (V2 1 1) 0
    (Transform t1 s1 r1) `mappend` (Transform t2 s2 r2) = Transform (t1 + t2) (s1 * s2) (r1 + r2)

type Position = V2 Float
type Scale = V2 Float
type Rotation = Float
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
newtype GeomShader = GRS Shader
newtype BezShader = BRS Shader
newtype MaskShader = MRS Shader
data SumShader = SRS { _shPolyline :: PolylineShader
                     , _shGeometry :: GeomShader
                     , _shBezier   :: BezShader
                     , _shMask     :: MaskShader
                     }
makeLenses ''SumShader

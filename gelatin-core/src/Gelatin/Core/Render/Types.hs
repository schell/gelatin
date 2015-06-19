{-# LANGUAGE DeriveDataTypeable #-}
module Gelatin.Core.Render.Types (
    Resources(..),
    Renderer(..),
    RenderDef(..),
    RenderSource(..),
    GeomRenderSource(..),
    BezRenderSource(..),
    Transform(..),
    UniformUpdates(..),
    Point(..),
    Line(..),
    Bezier(..),
    Triangle(..),
    FontString(..),
    EndCap(..),
    LineJoin(..),
    Fill(..),
    FillResult(..)
) where

import Linear as J hiding (rotate)
import Prelude hiding (init)
import Graphics.UI.GLFW
import Graphics.GL.Types
import Graphics.Text.TrueType hiding (CompositeScaling(..))
import Data.Time.Clock
import Data.Typeable
import Data.ByteString.Char8 (ByteString)
import Control.Concurrent.Async
import Data.IntMap (IntMap)
import Data.Map (Map)

type ShaderProgram = GLuint

data Resources = Resources { rsrcFonts     :: Async FontCache
                           , rsrcRenderers :: RenderCache
                           , rsrcSources   :: RenderSources
                           , rsrcWindow    :: Window
                           , rsrcDpi       :: Dpi
                           , rsrcUTC       :: UTCTime
                           } deriving (Typeable)

type RenderCache = IntMap Renderer

data RenderDef = RenderDefFP { rdShaderPaths :: [(String, GLuint)]
                             -- ^ ie [("path/to/shader.vert", GL_VERTEX_SHADER), ..]
                             , rdUniforms :: [String]
                             -- ^ ie ["projection", "modelview", ..]
                             }
               | RenderDefBS { rdShaderSrcs :: [(ByteString, GLuint)]
                             , rdUniforms :: [String]
                             } deriving (Show, Eq, Ord)

data GeomRenderSource = GRS RenderSource
data BezRenderSource = BRS RenderSource

data RenderSource = RenderSource { rsProgram    :: ShaderProgram
                                 , rsAttributes :: [(String, GLint)]
                                 } deriving (Show)

type RenderSources = Map RenderDef RenderSource

type RenderFunction = Transform -> IO ()

type CleanupFunction = IO ()

-- | TODO: Do we really need rSrc?
data Renderer = Renderer { rRender  :: RenderFunction
                         , rSrc     :: [RenderSource]
                         , rCleanup :: CleanupFunction
                         }

instance Monoid Renderer where
    mempty = Renderer (const $ return ()) [] (return ())
    (Renderer ar as ac) `mappend` (Renderer br bs bc) = Renderer (\t -> ar t >> br t) (as ++ bs) (ac >> bc)

data Primitive a = PrimitiveBez (Bezier a)
                 | PrimitiveTri (Triangle a)
                 deriving (Show, Eq)
data Point a = Point a
data Line a = Line a a deriving (Show, Eq)
data Bezier a = Bezier Ordering a a a deriving (Show, Eq)
data Triangle a = Triangle a a a deriving (Show, Eq)
data FontString = FontString Font Float (Float,Float) String

data LineJoin = LineJoinMiter
              | LineJoinBevel
              | LineJoinRound
              deriving (Show, Eq)
data EndCap = EndCapButt
            | EndCapRound
            | EndCapSquare
            deriving (Show, Eq)

instance Functor Point where
    fmap f (Point v) = Point $ f v

instance Functor Line where
    fmap f (Line a b) = Line (f a) (f b)

instance Functor Bezier where
    fmap f (Bezier o a b c) = Bezier o (f a) (f b) (f c)

instance Functor Triangle where
    fmap f (Triangle a b c) = Triangle (f a ) (f b) (f c)

data UniformUpdates = UniformUpdates { uuProjection :: Maybe GLint
                                     , uuModelview  :: Maybe GLint
                                     , uuSampler    :: (GLint, GLint)
                                     , uuHasUV      :: (GLint, GLint)
                                     }

type Position = V2 Float
type Scale = V2 Float
type Rotation = Float

data Transform = Transform { tfrmTranslation :: Position
                           , tfrmScale       :: Scale
                           , tfrmRotation    :: Rotation
                           } deriving (Show, Typeable)

instance Monoid Transform where
    mempty = Transform zero (V2 1 1) 0
    (Transform t1 s1 r1) `mappend` (Transform t2 s2 r2) = Transform (t1 + t2) (s1 * s2) (r1 + r2)

data Fill = FillColor (V2 Float -> V4 Float)
          | FillTexture FilePath (V2 Float -> V2 Float)

data FillResult = FillResultColor [V4 Float]
                | FillResultTexture GLuint [V2 Float]

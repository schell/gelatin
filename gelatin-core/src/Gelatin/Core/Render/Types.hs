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
    module J
) where

import Linear as J hiding (rotate)
import Prelude hiding (init)
import Graphics.UI.GLFW as J
import Graphics.GL.Types as J
import Graphics.Text.TrueType as J hiding (CompositeScaling(..))
import Codec.Picture as J
import Codec.Picture.Types as J
import Data.Time.Clock
import Data.Monoid
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

data Point a = Point a
data Line a = Line a a
data Bezier a = Bezier Ordering a a a
data Triangle a = Triangle a a a
data FontString = FontString Font Float String

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

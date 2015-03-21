{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GADTs #-}
module Render.Types where

import Linear
import Prelude hiding (init)
import Network.HTTP.Client
import Graphics.UI.GLFW
import Graphics.GL.Types
import Graphics.Text.TrueType
import Data.Time.Clock
import Data.Monoid
import Data.Typeable
import Data.ByteString.Char8 (ByteString)
import Control.Concurrent.Async
import Data.IntMap (IntMap)
import Data.Map (Map)

type ShaderProgram = GLuint
type UniformLocation = GLint

data Display = DisplayTris [Triangle Float]
             -- ^ Display as a list of triangles.
             | DisplayPoly [V2 Float]
             -- ^ Display as a filled polygon.
             | DisplayText FontDescriptor PixelSize String
             -- ^ Display as text of a given font and size.
             | DisplayText' FontDescriptor PixelSize String
             -- ^ With testing visuals
             | DisplayLine [V2 Float]
             -- ^ Display as an outline.
             | DisplayArrows [V2 Float]
             -- ^ Display as an outline with arrow heads at each point,
             -- showing the winding/direction of the outline.
             deriving (Show, Typeable)

data Resources = Resources { rsrcFonts     :: Async FontCache
                           , rsrcRenderers :: RenderCache
                           , rsrcSources   :: RenderSources
                           , rsrcWindow    :: Window
                           , rsrcManager   :: Manager
                           , rsrcDpi       :: Dpi
                           , rsrcUTC       :: UTCTime
                           } deriving (Typeable)

type ImageCache = Map Image (Async ByteString)

type RenderCache = IntMap Renderer

data RenderDef = RenderDefFP { rdShaderPaths :: [(String, GLuint)]
                             -- ^ ie [("path/to/shader.vert", GL_VERTEX_SHADER), ..]
                             , rdUniforms :: [String]
                             -- ^ ie ["projection", "modelview", ..]
                             }
               | RenderDefBS { rdShaderSrcs :: [(ByteString, GLuint)]
                             , rdUniforms :: [String]
                             } deriving (Show, Eq, Ord)


data RenderSource = RenderSource { rsProgram    :: ShaderProgram
                                 , rsAttributes :: [(String, GLint)]
                                 } deriving (Show)

type RenderSources = Map RenderDef RenderSource

type RenderFunction = Transform -> IO ()

type CleanupFunction = IO ()

data Renderer = Renderer { render  :: RenderFunction
                         , cleanup :: CleanupFunction
                         }

instance Monoid Renderer where
    mempty = Renderer (const $ return ()) (return ())
    (Renderer ar ac) `mappend` (Renderer br bc) = Renderer (\t -> ar t >> br t) (ac >> bc)

data Bezier a = Bezier { bezTriArea :: a
                       , bezA :: V2 a
                       , bezB :: V2 a
                       , bezC :: V2 a
                       } deriving (Show)

bezWoundClockwise :: (Ord a, Num a) => Bezier a -> Bool
bezWoundClockwise = (< 0) . bezTriArea

data Triangle a = Triangle (V2 a) (V2 a) (V2 a) deriving (Show)
data Line a = Line (V2 a) (V2 a) deriving (Show)

data Image = LocalImage FilePath
           -- | HttpImage String
           deriving (Typeable, Show)

data Color = SolidColor (V4 Float)
           | GradientColor [V4 Float]
           | TextureColor Image [V2 Float]
           deriving (Typeable)

data UniformUpdates = UniformUpdates { uuProjection :: Maybe GLint
                                     , uuModelview  :: Maybe GLint
                                     , uuSampler    :: (GLint, GLint)
                                     , uuHasUV      :: (GLint, GLint)
                                     }

newtype UniqueId = UniqueId { unId :: Int } deriving (Enum, Ord, Eq, Num, Show)
newtype Name = Name { unName :: String } deriving (Ord, Eq)
newtype ParentEntity = Parent { unParent :: Int } deriving (Show)
type Translation = V2 Float
type Position = V2 Float
type Scale = V2 Float
type Rotation = Float
type Size = V2 Float
type PixelSize = Float
type TimeDelta = Float
data Transform = Transform { tfrmTranslation :: Position
                           , tfrmScale       :: Scale
                           , tfrmRotation    :: Rotation
                           } deriving (Show, Typeable)
--makeLensesFor [("tfrmTranslation", "tfrmTranslation_")
--              ,("tfrmScale", "tfrmScale_")
--              ,("tfrmRotation", "tfrmRotation_")
--              ] ''Transform

idTransform :: Transform
idTransform = mempty

instance Monoid Transform where
    mempty = Transform zero (V2 1 1) 0
    (Transform t1 s1 r1) `mappend` (Transform t2 s2 r2) = Transform (t1 + t2) (s1 * s2) (r1 + r2)

data AABB = AABB { aabbCenter     :: Position
                 , aabbHalfVector :: Size
                 } deriving (Show, Eq, Ord)
--makeLensesFor [("aabbCenter", "aabbCenter_")
--              ,("aabbHalfVector", "aabbHalfVector_")
--              ] ''AABB

--makeLensesFor [("gUTC", "gUTC_")
--              ,("gBox", "gBox_")
--              ,("gBez", "gBez_")
--              ,("gWindow", "gWindow_")
--              ] ''Globals

data Input = Input { inputLeftAxis    :: (Float, Float)
                   , inputRightAxis   :: (Float, Float)
                   , inputWindowSizef :: (Float, Float)
                   } deriving (Show, Eq)


type SeparatingAxis = V2 Float

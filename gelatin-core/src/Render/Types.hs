{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE GADTs #-}
module Render.Types (
    Resources(..),
    Renderer(..),
    RenderDef(..),
    RenderSource(..),
    Transform(..),
    UniformUpdates(..),
    Geometrical(..),
    isLikeGeom,
    Gradient(..),
    AtLeast2D
) where

import Linear
import Prelude hiding (init)
import Network.HTTP.Client
import Graphics.UI.GLFW
import Graphics.GL.Types
import Graphics.Text.TrueType
import Codec.Picture
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
                           , rsrcManager   :: Manager
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


data RenderSource = RenderSource { rsProgram    :: ShaderProgram
                                 , rsAttributes :: [(String, GLint)]
                                 } deriving (Show)

type RenderSources = Map RenderDef RenderSource

type RenderFunction = Transform -> IO ()

type CleanupFunction = IO ()

data Renderer = Renderer { rRender  :: RenderFunction
                         , rCleanup :: CleanupFunction
                         }

instance Monoid Renderer where
    mempty = Renderer (const $ return ()) (return ())
    (Renderer ar ac) `mappend` (Renderer br bc) = Renderer (\t -> ar t >> br t) (ac >> bc)

type AtLeast2D f a = (R1 f, R2 f, RealFrac a, Ord a)

data Geometrical a where
    Point      :: V2 a                             -> Geometrical a
    --Circle     :: V2 a -> a                        -> Geometrical a
    Line       :: V2 a -> V2 a                     -> Geometrical a
    --AABB       :: V2 a -> a -> a                   -> Geometrical a
    Triangle   :: V2 a -> V2 a -> V2 a             -> Geometrical a
    Bezier     :: Ordering -> V2 a -> V2 a -> V2 a -> Geometrical a
    --Polygon  :: [V2 a]                           -> Geometrical a
    FontString :: Font -> a -> String              -> Geometrical a

instance Eq (Geometrical a) where
    (==) = isLikeGeom


instance Ord (Geometrical a) where
    compare a b
        | a `isLikeGeom` b = EQ
        | (Point _) <- a = LT
        | (Point _) <- b = GT
        | (Line _ _) <- a = LT
        | (Line _ _) <- b = GT
        | (Triangle _ _ _) <- a = LT
        | (Triangle _ _ _) <- b = GT
        | (Bezier _ _ _ _) <- a = LT
        | (Bezier _ _ _ _) <- b = GT
        | (FontString _ _ _) <- a = LT
        | (FontString _ _ _) <- a = GT
        | otherwise = EQ

instance Show a => Show (Geometrical a) where
    show (Point a) = concat ["Point (",show a,")"]
    show (Line a b) = concat ["Line (",show a,") (",show b,")"]
    show (Triangle a b c) =
        concat ["Triangle (",show a,") (",show b,") (", show c,")"]
    show (Bezier o a b c) =
        concat ["Bezier",show o," (",show a,") (",show b,") (",show c,")"]
    show (FontString _ px s) = concat ["FontString _ ",show px," ",show s]

isLikeGeom :: Geometrical a -> Geometrical a -> Bool
isLikeGeom (Point _) (Point _) = True
--isLikeGeom (Circle _ _) (Circle _ _) = True
isLikeGeom (Line _ _) (Line _ _) = True
--isLikeGeom (AABB _ _ _) (AABB _ _ _) = True
isLikeGeom (Triangle _ _ _) (Triangle _ _ _) = True
isLikeGeom (Bezier _ _ _ _) (Bezier _ _ _ _) = True
--isLikeGeom (Polygon _) (Polygon _) = True
isLikeGeom (FontString _ _ _) (FontString _ _ _) = True
isLikeGeom _ _ = False

data Gradient a = SolidGradient  (V4 a)
                | RadialGradient (V4 a) (V4 a) a (V2 a)
    --LinearGradient :: V4 a -> V4 a -> V2 a -> a    -> Gradient a
    --BoxGradient    :: V4 a -> V4 a -> V4 a -> V4 a -> Gradient a

data Texture a = Texture DynamicImage [Geometrical a]

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

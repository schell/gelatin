{-# LANGUAGE TemplateHaskell #-}
module Gelatin.Core where

import Linear
import Graphics.Rendering.OpenGL hiding (Color, Fill, Texture)
import Graphics.GLUtil
import Control.Monad.Free.Church
import Control.Monad.Reader
import qualified Data.Map as M

--------------------------------------------------------------------------------
-- Types
--------------------------------------------------------------------------------

data REnv a = REnv { reWindowSize      :: V2 Int
                   , reFrameBufferSize :: V2 Int
                   , reProjection      :: M44 Double
                   , reModelview       :: M44 Double
                   , reUserData        :: a
                   }

type Render a = ReaderT (REnv a) IO

type RenderPair a = (Render a (), IO ())

data Transform = Transform (V3 Double) (V3 Double) (Quaternion Double) deriving (Show, Eq, Ord)

type Color = V4 Double

--data VertexDefinition next = AddColor [V4 Double] next
--                           | AddPosition [V3 Double] next
--                           | AddUVMap [V2 Double] next
--                           | AddElements [Int] next
--
--type Vertices = F VertexDefinition
--
--data DrawCommand a next = WithTransform Transform (Drawing a ()) next
--                        | WithProjection (M44 Double) (Drawing a ()) next
--                        | Fill [V3 Double] (Color) next
--                        | Gradient [V3 Double] [Color] next
--                        | WithTexture TextureSrc (Vertices ()) next
--                        | WithVertices (Vertices ()) next
--                        | TexTris [V3 Double] [V2 Double] next
--                        | RawRendering (Renderer -> IO (RenderPair a)) next
--
--type Drawing a = F (DrawCommand a)

data TextureSrc = Local FilePath
                | Relative FilePath
                deriving (Show, Eq, Ord)

type TextureAtlas = M.Map TextureSrc TextureObject

data Renderer = Renderer { colorShader :: ShaderProgram
                         , textureShader :: ShaderProgram
                         , textureAtlas :: TextureAtlas
                         }

--------------------------------------------------------------------------------
-- Classes
--------------------------------------------------------------------------------

class Embedable a where
    embed  :: Fractional b => a -> V3 b

--------------------------------------------------------------------------------
-- Instances
--------------------------------------------------------------------------------

--instance Functor (DrawCommand a) where
--    fmap f (WithTransform t d n) = WithTransform t d $ f n
--    fmap f (WithProjection pj d n) = WithProjection pj d $ f n
--    fmap f (Fill c vs n) = Fill c vs $ f n
--    fmap f (Gradient cs vs n) = Gradient cs vs $ f n
--    fmap f (WithTexture src d n) = WithTexture src d $ f n
--    fmap f (TexTris vs ts n) = TexTris vs ts $ f n
--    fmap f (RawRendering g n) = RawRendering g $ f n

instance (Real a, Fractional a) => Embedable (V1 a) where
    embed (V1 x) = V3 (realToFrac x) 0 0

instance Real a => Embedable (V2 a) where
    embed (V2 x y) = V3 (realToFrac x) (realToFrac y) 0

instance Real a => Embedable (V3 a) where
    embed = fmap realToFrac

--------------------------------------------------------------------------------
-- Building geometry
--------------------------------------------------------------------------------

-- | Given two points, creates a pair of triangles representing
-- a rectangle.
rectangle :: V2 Double -- ^ The top left of the rectangle
          -> V2 Double -- ^ The width and height of the rectangle
          -> [V2 Double]
rectangle (V2 x y) (V2 w h) = [ tl, bl, br, tl, tr, br ]
    where tl = V2 x y
          tr = V2 (x+w) y
          bl = V2 x (y+h)
          br = V2 (x+w) (y+h)

--------------------------------------------------------------------------------
-- Building transforms
--------------------------------------------------------------------------------

idTransform :: Transform
idTransform = Transform (V3 0 0 0) (V3 1 1 1) $ axisAngle (V3 0 0 1) 0

tfrm :: Embedable v => v -> v -> Quaternion Double -> Transform
tfrm p s q = Transform (embed p) (embed s) q

rotateX :: Double -> Quaternion Double
rotateX = axisAngle (V3 1 0 0)

rotateY :: Double -> Quaternion Double
rotateY = axisAngle (V3 0 1 0)

rotateZ :: Double -> Quaternion Double
rotateZ = axisAngle (V3 0 0 1)

setPosition :: Embedable v => Transform -> v -> Transform
setPosition (Transform _ s q) p = Transform (embed p) s q

setScale :: Embedable v => Transform -> v -> Transform
setScale (Transform p _ q) s = Transform p (embed s) q

setRotation :: Transform -> Quaternion Double -> Transform
setRotation (Transform p s _) q = Transform p s q

--------------------------------------------------------------------------------
-- Building a drawing
--------------------------------------------------------------------------------

--fill :: Embedable v => [v] -> Color -> Drawing a ()
--fill vs c = liftF $ Fill (map embed vs) c ()
--
--gradient :: Embedable v => [v] -> [Color] -> Drawing a ()
--gradient vs cs = liftF $ Gradient (map embed vs) cs ()
--
--textris :: Embedable v => [v] -> [V2 Double] -> Drawing a ()
--textris vs ts = liftF $ TexTris (map embed vs) ts ()
--
--renderWith :: (Renderer -> IO (RenderPair a))
--           -> Drawing a ()
--renderWith f = liftF $ RawRendering f ()
--
--withTexture :: TextureSrc -> Vertices () -> Drawing a ()
--withTexture src vs = liftF $ WithTexture src vs ()
--
--withProjection :: M44 Double -> Drawing a () -> Drawing a ()
--withProjection pj d = liftF $ WithProjection pj d ()
--
--withTransform :: Transform -> Drawing a () -> Drawing a ()
--withTransform t d = liftF $ WithTransform t d ()
--
--withPosition :: Embedable v => v -> Drawing a () -> Drawing a ()
--withPosition = withTransform . setPosition idTransform
--
--withScale :: Embedable v => v -> Drawing a () -> Drawing a ()
--withScale = withTransform . setScale idTransform
--
--withRotation :: Quaternion Double -> Drawing a () -> Drawing a ()
--withRotation = withTransform . setRotation idTransform

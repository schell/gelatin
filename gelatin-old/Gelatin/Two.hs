module Gelatin.Two (
    -- * Compiling
    --module T,
    --Rendering2d,
    --renderOnce2d,
    --runRendering2d,
    --mk2dRendering,
    ---- * Creating
    --lift2d,
    --clear,
    --fill,
    --stroke,
    --outline,
    --solid,
    --gradient,
    --texture,
    --withSize,
    --withTransform,
    --withPosition,
    --withScale,
    --withRotation,
) where

import Gelatin.Two.Compiling as T
import Gelatin.Two.Types
import Gelatin.Core
import Gelatin.Transform
import Gelatin.Geometry
import Gelatin.Core.TextureCommands
import Control.Monad.Free
import Control.Monad.Free.Church
import Linear hiding (rotate)
import qualified Graphics.Rendering.OpenGL as GL

-- | Compile and disploy the Rendering2d immediately and clean up after.
--renderOnce2d :: Rendering2d () -> IO ()
--renderOnce2d r = do
--    r' <- runRendering2d r
--    render r'
--    cleanup r'
--
---- | Compile the rendering.
--runRendering2d :: Rendering2d () -> IO CompiledRendering
--runRendering2d two = do
--    r <- mkRenderer2d
--    runRendering $ mk2dRendering r two
--
---- | Compile a Two (2d) rendering into a Core rendering.
--mk2dRendering :: Renderer2d -> Rendering2d () -> Rendering ()
--mk2dRendering r t = render2 r $ fromF $ do
--    withPosition (V2 0 0 :: V2 Float) t
--    lift2d $ do GL.blend GL.$= GL.Enabled
--                GL.blendEquationSeparate GL.$= (GL.FuncAdd, GL.FuncAdd)
--                GL.blendFuncSeparate GL.$= ((GL.SrcAlpha, GL.OneMinusSrcAlpha),
--                                            (GL.One, GL.Zero))
----------------------------------------------------------------------------------
---- Building a Rendering2d
----------------------------------------------------------------------------------
---- | TODO: I'm pretty sure we'd like to be able to do things like
----    withTexture (Relative "img/quantum-foam.jpg") $ fill (rectangle 0 0 100 100)
---- What that does I'm not sure of yet.
---- http://hackage.haskell.org/package/Rasterific-0.3/docs/Graphics-Rasterific-Texture.html
--
--lift2d :: IO () -> Rendering2d ()
--lift2d io = liftF $ Render2d io ()
--
---- | Clear the screen.
--clear :: Rendering2d ()
--clear = liftF $ Clear ()
--
---- | Fill some geometry with some color.
--fill :: RealFloat a => ColorMapping a -> [Primitive V2 a] -> Rendering2d ()
--fill c ts = liftF $ Fill c ts ()
--
---- | Stroke some geometry with some color using width.
--stroke :: RealFloat a
--       => a -> ColorMapping a -> [Primitive V2 a] -> Rendering2d ()
--stroke width color geom = liftF $ Fill color (strokePrimitives width geom) ()
--
---- | Outline some geometry with some color.
--outline :: RealFloat a => ColorMapping a -> [Primitive V2 a] -> Rendering2d ()
--outline c ps = liftF $ Outline c ps ()
--
---- | Create a solid color.
--solid :: V4 a -> ColorMapping a
--solid = Color
--
---- | Create a gradient of color using the given function.
--gradient :: (V2 a -> V4 a) -> ColorMapping a
--gradient = ColorMapping
--
---- | Create a color sampler from a texture using the given function.
--texture :: TextureSrc -> (V2 a -> V2 a) -> ColorMapping a
--texture = TextureMapping
--
---- | Specify the size of the current rendering.
--withSize :: Int -> Int -> Rendering2d () -> Rendering2d ()
--withSize w h r = liftF $ WithSize w h r ()
--
---- | Specify the transform of the inner rendering.
--withTransform :: Transformation () -> Rendering2d () -> Rendering2d ()
--withTransform t d = liftF $ WithTransform t d ()
--
---- | Specify the position of the inner rendering.
--withPosition :: (Real a, Fractional a) => V2 a -> Rendering2d () -> Rendering2d ()
--withPosition = withTransform . translate . (\(V2 x y) -> V3 x y 0)
--
---- | Specify the scale of the inner rendering.
--withScale :: (Real a, Fractional a) => V2 a -> Rendering2d () -> Rendering2d ()
--withScale = withTransform . scale . (\(V2 x y) -> V3 x y 1)
--
---- | Specify the rotation of the inner rendering.
--withRotation :: Double -> Rendering2d () -> Rendering2d ()
--withRotation r t = withTransform (rotate r $ V3 0 0 1) t

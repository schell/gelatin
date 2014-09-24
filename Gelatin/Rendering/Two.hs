{-# LANGUAGE GADTs #-}
module Gelatin.Rendering.Two (
    Rendering2d,
    fill,
    gradient,
    fillTex,
    withTransform,
    withPosition,
    withScale,
    withRotation,
    compileTwo,
    mk2dRendering,
    mkRenderer2d,
    Renderer2d(..)
) where

import Gelatin.Transform
import Gelatin.TextureCommands
import Gelatin.Geometry
import Gelatin.Rendering
import Gelatin.Shaders
import Gelatin.ShaderCommands
import Gelatin.Compiling
import Control.Monad
import Control.Monad.Free
import Control.Monad.Free.Church
import Graphics.GLUtil hiding (setUniform)
import Graphics.Rendering.OpenGL hiding (Fill, translate, scale, rotate, ortho, position, color, drawArrays)
import Linear hiding (rotate)

--------------------------------------------------------------------------------
-- Types
--------------------------------------------------------------------------------
data TwoCommand next where
    WithTransform :: Transformation () -> Rendering2d () -> next -> TwoCommand next
    Fill :: (Embedable v, Real a) => [v] -> V4 a -> next -> TwoCommand next
    Gradient :: (Embedable v, Real a) => [v] -> [V4 a] -> next -> TwoCommand next
    TexTris :: (Embedable v, Real a) => TextureSrc -> [v] -> [V2 a] -> next -> TwoCommand next

type Rendering2d = F TwoCommand
--------------------------------------------------------------------------------
-- Instances
--------------------------------------------------------------------------------
instance Functor TwoCommand where
    fmap f (WithTransform t d n) = WithTransform t d $ f n
    fmap f (Fill c vs n) = Fill c vs $ f n
    fmap f (Gradient cs vs n) = Gradient cs vs $ f n
    fmap f (TexTris t vs ts n) = TexTris t vs ts $ f n
--------------------------------------------------------------------------------
-- Building a Rendering2d
--------------------------------------------------------------------------------
-- | TODO: I'm pretty sure we'd like to be able to do things like
--    withTexture (Relative "img/quantum-foam.jpg") $ fill (rectangle 0 0 100 100)
-- What that does I'm not sure of yet.
-- http://hackage.haskell.org/package/Rasterific-0.3/docs/Graphics-Rasterific-Texture.html
fill :: (Embedable v, Real a) => [v] -> V4 a -> Rendering2d ()
fill vs c = liftF $ Fill vs c ()

gradient :: (Embedable v, Real a) => [v] -> [V4 a] -> Rendering2d ()
gradient vs cs = liftF $ Gradient vs cs ()

fillTex :: (Embedable v, Real a) => TextureSrc -> [v] -> [V2 a] -> Rendering2d ()
fillTex t vs ts = liftF $ TexTris t vs ts ()

withTransform :: Transformation () -> Rendering2d () -> Rendering2d ()
withTransform t d = liftF $ WithTransform t d ()

withPosition :: Embedable v => v -> Rendering2d () -> Rendering2d ()
withPosition = withTransform . translate . embed

withScale :: Embedable v => v -> Rendering2d () -> Rendering2d ()
withScale = withTransform . scale . embedWith 1

withRotation :: Double -> Rendering2d () -> Rendering2d ()
withRotation r t = withTransform (rotate r $ V3 0 0 1) t
--------------------------------------------------------------------------------
-- Compiling a Rendering2d
--------------------------------------------------------------------------------
data Renderer2d = Renderer2d { twoColorShader :: ShaderProgram
                               , twoTextureShader :: ShaderProgram
                               , twoProjection :: M44 GLfloat
                               , twoModelview :: M44 GLfloat
                               }

compileTwo :: Integral i => V2 i -> Rendering2d () -> IO CompiledRendering
compileTwo v two = do
    r <- mkRenderer2d v
    compileRendering $ mk2dRendering r two

mk2dRendering :: Renderer2d -> Rendering2d () -> Rendering ()
mk2dRendering r t = do
    -- Start out by setting the projection on both shaders.
    forM_ [twoColorShader r, twoTextureShader r] $ \shader ->
        usingShader shader $ setProjection $ twoProjection r
    render2 r $ fromF t

mkRenderer2d :: Integral i => V2 i -> IO Renderer2d
mkRenderer2d (V2 w h) = do
    clr <- simpleColorShader
    tex <- simpleTextureShader
    return $ Renderer2d clr tex pj eye4
        where pj = ortho 0 (fromIntegral w) 0 (fromIntegral h) 0 1

render2 :: Renderer2d -> Free TwoCommand () -> Rendering ()
render2 _ (Pure ()) = return ()
render2 r (Free (WithTransform t d n)) = do
    let mv' = (twoModelview r) !*! (fmap (fmap realToFrac) $ mkM44 t)
    render2 (r{twoModelview = mv'}) $ fromF d
    render2 r n
render2 r (Free (Fill vs c n)) =
    let cs  = replicate (length vs) c
    in render2 r (Free (Gradient vs cs n))
render2 r (Free (Gradient vs cs n)) = do
    usingShader (twoColorShader r) $ do
        let vs' = position $ map embed vs
            cs' = color cs
        setModelview $ twoModelview r
        withVertices (addComponent vs' >> addComponent cs') $
            drawArrays Triangles (length vs)
    render2 r n
render2 r (Free (TexTris src vs uvs n)) = do
    let params = do setFilter (Nearest, Nothing) Nearest
                    setWrapMode S Repeated Clamp
                    setWrapMode T Repeated Clamp
    usingTexture Texture2D src params $ do
        usingShader (twoTextureShader r) $ do
            setModelview $ twoModelview r
            let vs'  = position $ map embed vs
                uvs' = texcoord uvs
            withVertices (addComponent vs' >> addComponent uvs') $
                drawArrays Triangles $ length vs
    render2 r n

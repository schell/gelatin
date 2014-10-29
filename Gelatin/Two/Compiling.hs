module Gelatin.Two.Compiling where

import Gelatin.Two.Types
import Gelatin.Transform
import Gelatin.Core
import Gelatin.Core.Compiling
import Gelatin.Core.ShaderCommands
import Gelatin.Core.TextureCommands
import Gelatin.Geometry
import Gelatin.Shaders
import Gelatin.Color
import Control.Monad
import Control.Monad.Free
import Control.Monad.Free.Church
import Graphics.GLUtil hiding (setUniform)
import Graphics.Rendering.OpenGL hiding (Color, Line, Polygon, Triangle, Primitive, Fill, translate, scale, rotate, ortho, position, color, drawArrays, Clear, clear)
import Linear hiding (rotate, trace)
import Data.Monoid

renderOnce2d :: Rendering2d () -> IO ()
renderOnce2d r = do
    r' <- runRendering2d r
    render r'
    cleanup r'

runRendering2d :: Rendering2d () -> IO CompiledRendering
runRendering2d two = do
    r <- mkRenderer2d
    runRendering $ mk2dRendering r two

mk2dRendering :: Renderer2d -> Rendering2d () -> Rendering ()
mk2dRendering r t = render2 r $ fromF $ withPosition (V2 0 0 :: V2 Float) t

mkRenderer2d :: IO Renderer2d
mkRenderer2d = do
    clr <- simple2dColorShader
    tex <- simple2dTextureShader
    return $ Renderer2d clr tex eye4 eye4 mempty

render2 :: Renderer2d -> Free TwoCommand () -> Rendering ()
render2 _ (Pure ()) = return ()
render2 r (Free (Clear n)) = do
    clearColorWith (black :: V4 Float)
    render2 r n
render2 r (Free (Size2d w h cmd n)) = do
    let pj = ortho 0 (fromIntegral w) 0 (fromIntegral h) 0 1
        r' = r{ twoProjection = pj }
    forM_ [twoColorShader r, twoTextureShader r] $ \shader ->
        usingShader shader $ setProjection $ twoProjection r'
    render2 r' $ fromF cmd
    render2 r n
render2 r (Free (WithTransform t d n)) = do
    let mv' = (twoModelview r) !*! mkM44 t
    forM_ [twoColorShader r, twoTextureShader r] $ \shader ->
        usingShader shader $ setModelview mv'
    render2 (r{twoModelview = mv'}) $ fromF d
    render2 r n
--------------------------------------------------------------------------------
-- Filling
--------------------------------------------------------------------------------
render2 r (Free (Fill (Color c) ps n)) = do
    let vs  = concatMap primitiveToList $ concatMap clipEars ps
        cs  = replicate (length vs) c
        vs' = addComponent (position2 vs) >> addComponent (color cs)
    gradientRender r vs' Triangles $ length vs
    render2 r n
render2 r (Free (Fill (ColorMapping g) ps n)) = do
    let vs  = concatMap primitiveToList $ concatMap clipEars ps
        cs  = map g vs
        vs' = addComponent (position2 vs) >> addComponent (color cs)
    gradientRender r vs' Triangles $ length vs
    render2 r n
render2 r (Free (Fill (TextureMapping s g) ps n)) = do
    let vs  = concatMap primitiveToList $ concatMap clipEars ps
        cs  = map g vs
        vs' = addComponent (position2 vs) >> addComponent (texcoord cs)
    textureRender r vs' s Triangles $ length vs
    render2 r n
--------------------------------------------------------------------------------
-- Outlining
--------------------------------------------------------------------------------
render2 r (Free (Outline (Color c) ps n)) = do
    let vs  = concatMap primitiveToList $ concatMap primitiveToLines ps
        cs  = replicate (length vs) c
        vs' = addComponent (position2 vs) >> addComponent (color cs)
    gradientRender r vs' Lines $ length vs
    render2 r n

render2 r (Free (Outline (ColorMapping g) ps n)) = do
    let vs  = concatMap primitiveToList $ concatMap primitiveToLines ps
        cs  = map g vs
        vs' = addComponent (position2 vs) >> addComponent (color cs)

    gradientRender r vs' Lines $ length vs
    render2 r n

render2 r (Free (Outline (TextureMapping s g) ps n)) = do
    let vs  = concatMap primitiveToList $ concatMap primitiveToLines ps
        cs  = map g vs
        vs' = addComponent (position2 vs) >> addComponent (texcoord cs)
    textureRender r vs' s Lines $ length vs
    render2 r n
--------------------------------------------------------------------------------
-- Helpers
--------------------------------------------------------------------------------
gradientRender :: Renderer2d -> VertexBufferCommand ()
               -> PrimitiveMode -> Int -> Rendering ()
gradientRender r vs mode n = do
    usingShader (twoColorShader r) $ do
        withVertices vs $ drawArrays mode n

textureRender :: Renderer2d -> VertexBufferCommand ()
              -> TextureSrc -> PrimitiveMode -> Int -> Rendering ()
textureRender r vs src mode n = do
    let prams = do setFilter (Nearest, Nothing) Nearest
                   setWrapMode S Repeated Clamp
                   setWrapMode T Repeated Clamp
    usingTexture Texture2D src prams $ usingShader (twoTextureShader r) $ do
        setSampler (0 :: Int)
        withVertices vs $ drawArrays mode n

data Renderer2d = Renderer2d { twoColorShader   :: ShaderProgram
                             , twoTextureShader :: ShaderProgram
                             , twoProjection    :: M44 GLfloat
                             , twoModelview     :: M44 GLfloat
                             , twoTextureAtlas  :: TextureAtlas
                             }

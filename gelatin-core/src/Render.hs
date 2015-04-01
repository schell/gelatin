{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Render (
    module R,
    module GLFW,
    initResources,
    enableBlending,
    getRenderer,
    render,
    rendering,
    renderToTexture,
    cache,
    colorRenderer,
    colorBezRenderer,
    colorFontRenderer,
    textureRenderer,
    stencilXOR,
    drawClear,
    withNewFrame,
    gets
) where

--import Debug.Trace
import Shader
import Render.Types as R
import Render.Geometrical as R
import Render.IO
import Render.Font as R
import Graphics.GL.Core33
import Graphics.UI.GLFW as GLFW
import Foreign.Marshal.Array
import Foreign.Marshal.Utils
import Data.Bits
import Data.Monoid
import Data.Time.Clock
import Data.Typeable
import Control.Applicative
import Control.Concurrent
import Control.Eff
import Control.Eff.Lift
import Control.Eff.Fresh
import Control.Eff.State.Strict
import System.IO
import System.Exit
import qualified Data.Foldable as F
import qualified Data.IntMap as IM
import qualified Data.Map as M

type CanCacheRendering r i = ( Member (State Resources) r
                             , SetMember Lift (Lift IO) r
                             , Member (Fresh i) r
                             , Enum i
                             , Typeable i
                             )

-- | Starts up the app by creating a window and returning some default
-- values for resources.
initResources :: Int -> Int -> String -> IO Resources
initResources ww wh ws = do
    setErrorCallback $ Just $ \_ -> hPutStrLn stderr
    True <- GLFW.init
    defaultWindowHints
    windowHint $ WindowHint'OpenGLDebugContext True
    windowHint $ WindowHint'OpenGLProfile OpenGLProfile'Core
    windowHint $ WindowHint'OpenGLForwardCompat True
    windowHint $ WindowHint'ContextVersionMajor 3
    windowHint $ WindowHint'ContextVersionMinor 2
    windowHint $ WindowHint'DepthBits 16
    mwin <- createWindow ww wh ws Nothing Nothing
    makeContextCurrent mwin
    window <- case mwin of
                  Nothing  -> do putStrLn "could not create window"
                                 exitFailure
                  Just win -> return win
    resources window

gets :: (Typeable a, Member (State a) r) => (a -> b) -> Eff r b
gets f = f <$> get

renderToTexture :: (Member (State Resources) r,
                    SetMember Lift (Lift IO) r)
                => (Int, Int) -> IO () -> Eff r GLuint
renderToTexture sz r = do
    fbsz <- framebufferSize
    lift $ toTexture sz fbsz r

render :: (Member (State Resources) r,
           SetMember Lift (Lift IO) r,
           Enum i, Show i)
       => i -> Transform -> Eff r ()
render uid t = do
    mR <- rendering uid
    case mR of
        Nothing -> lift $ putStrLn $ "Could not render with uid value: " ++ show uid
        Just r  -> lift $ r t

rendering :: (Member (State Resources) r, Enum i, Show i)
          => i -> Eff r (Maybe (Transform -> IO ()))
rendering uid = do
    mR <- getRenderer uid
    return $ rRender <$> mR

getRenderer :: (Member (State Resources) r,
              Enum i)
          => i -> Eff r (Maybe Renderer)
getRenderer uid = do
    rs <- gets rsrcRenderers
    let i = fromEnum uid
        mR = IM.lookup i rs
    return mR

-- | Creates a renderer for the given geometry and coloring and caches it.
-- Returns the uid of the cached renderer.
cache :: CanCacheRendering r i
      => Eff r Renderer -> Eff r i
cache eff = do
    uid <- fresh
    r   <- eff
    insertIntoCache uid r
    lift $ putStrLn "inserted into cache"
    return uid

-- | Inserts a renderer into the cache of resources.
insertIntoCache :: (Member (State Resources) r,
                 SetMember Lift (Lift IO) r,
                 Enum i)
             => i -> Renderer -> Eff r ()
insertIntoCache uid r = do
    rsrcs@Resources{..} <- get
    let i = fromEnum uid
        rs = IM.insert i r rsrcRenderers
    put $ rsrcs{rsrcRenderers = rs}

colorFontRenderer :: (Member (State Resources) r,
                 SetMember Lift (Lift IO) r)
             => FontString -> (V2 Float -> V4 Float) -> Eff r Renderer
colorFontRenderer fstr clrf = do
    dpi <- gets rsrcDpi

    let (bs,ts) = fontGeom dpi fstr
        vs = concatMap (\(Triangle a b c) -> [a,b,c]) ts
        cs = map clrf vs
    clrr <- colorRenderer GL_TRIANGLES vs cs

    let bcs = map ((\(Bezier _ a b c) -> Triangle a b c) . fmap clrf) bs
    bezr <- colorBezRenderer bs bcs

    return $ stencilXOR $ clrr <> bezr


stencilXOR :: Renderer -> Renderer
stencilXOR (Renderer r c) = Renderer r' c
    where r' t = do glClear GL_DEPTH_BUFFER_BIT
                    glEnable GL_STENCIL_TEST
                    glColorMask GL_FALSE GL_FALSE GL_FALSE GL_FALSE
                    glDepthMask GL_FALSE
                    glStencilMask 0xFF
                    glClear GL_STENCIL_BUFFER_BIT
                    glStencilFunc GL_NEVER 0 1
                    glStencilOp GL_INVERT GL_INVERT GL_INVERT
                    r t

                    glColorMask GL_TRUE GL_TRUE GL_TRUE GL_TRUE
                    glDepthMask GL_TRUE
                    glStencilFunc GL_EQUAL 1 1
                    glStencilOp GL_ZERO GL_ZERO GL_ZERO
                    r t
                    glDisable GL_STENCIL_TEST

colorRenderer :: (Member (State Resources) r,
                  SetMember Lift (Lift IO) r)
              => GLuint -> [V2 Float] -> [V4 Float] -> Eff r Renderer
colorRenderer mode vs gs = do
    window <- gets rsrcWindow
    (RenderSource program locs) <- geomRenderSource

    let Just pjU = lookup "projection" locs
        Just mvU = lookup "modelview" locs
        Just smU = lookup "sampler" locs
        Just uvU = lookup "hasUV" locs

    lift $ withVAO $ \vao -> withBuffers 2 $ \[pbuf,cbuf] -> do
        let ps = map realToFrac $ concatMap F.toList vs :: [GLfloat]
            cs = map realToFrac $ concatMap F.toList $ take (length vs) gs :: [GLfloat]

        glDisableVertexAttribArray uvLoc
        glEnableVertexAttribArray colorLoc

        bufferAttrib positionLoc 2 pbuf ps
        bufferAttrib colorLoc 4 cbuf cs
        glBindVertexArray 0

        let num = fromIntegral $ length vs
            uus = UniformUpdates (Just pjU) (Just mvU) (smU,0) (uvU,0)
            renderFunction = drawBuffer window program uus vao mode num
            cleanupFunction = do
                withArray [pbuf, cbuf] $ glDeleteBuffers 2
                withArray [vao] $ glDeleteVertexArrays 1
        return $ Renderer renderFunction cleanupFunction

textureRenderer :: (Member (State Resources) r,
                  SetMember Lift (Lift IO) r)
                => GLuint -> GLuint -> [V2 Float] -> [V2 Float] -> Eff r Renderer
textureRenderer t mode vs gs = do
    window <- gets rsrcWindow
    (RenderSource program locs) <- geomRenderSource

    let Just pjU = lookup "projection" locs
        Just mvU = lookup "modelview" locs
        Just smU = lookup "sampler" locs
        Just uvU = lookup "hasUV" locs

    let texr = Renderer (const $ glBindTexture GL_TEXTURE_2D t)
                        (withArray [t] $ glDeleteTextures 1)

    lift $ withVAO $ \vao -> withBuffers 2 $ \[pbuf,cbuf] -> do
        let ps = map realToFrac $ concatMap F.toList vs :: [GLfloat]
            cs = map realToFrac $ concatMap F.toList $ take (length vs) gs :: [GLfloat]

        glDisableVertexAttribArray colorLoc
        glEnableVertexAttribArray uvLoc

        bufferAttrib positionLoc 2 pbuf ps
        bufferAttrib uvLoc 2 cbuf cs
        glBindVertexArray 0

        let num = fromIntegral $ length vs
            uus = UniformUpdates (Just pjU) (Just mvU) (smU,0) (uvU,1)
            renderFunction = drawBuffer window program uus vao mode num
            cleanupFunction = do
                withArray [pbuf, cbuf] $ glDeleteBuffers 2
                withArray [vao] $ glDeleteVertexArrays 1
            geomr = Renderer renderFunction cleanupFunction
        return $ texr <> geomr

colorBezRenderer :: (Member (State Resources) r,
                     SetMember Lift (Lift IO) r)
                 => [Bezier (V2 Float)] -> [Triangle (V4 Float)] -> Eff r Renderer
colorBezRenderer bs ts = do
    window <- gets rsrcWindow
    (RenderSource program locs) <- bezRenderSource

    let Just pjU = lookup "projection" locs
        Just mvU = lookup "modelview" locs
        Just smU = lookup "sampler" locs
        Just uvU = lookup "hasUV" locs

    lift $ withVAO $ \vao -> withBuffers 3 $ \[pbuf, tbuf, cbuf] -> do
        let vs = concatMap (\(Bezier _ a b c) -> [a,b,c]) bs
            cvs = concatMap (\(Triangle a b c) -> [a,b,c]) $ take (length bs) ts
            ps = map realToFrac $ concatMap F.toList vs :: [GLfloat]
            cs = map realToFrac $ concatMap F.toList cvs :: [GLfloat]
            ws = concatMap (\(Bezier w _ _ _) -> let w' = fromBool $ w == LT
                                                 in [ 0, 0, w'
                                                    , 0.5, 0, w'
                                                    , 1, 1, w'
                                                    ])
                           bs :: [GLfloat]

        glDisableVertexAttribArray uvLoc
        glEnableVertexAttribArray colorLoc

        bufferAttrib positionLoc 2 pbuf ps
        bufferAttrib bezLoc 3 tbuf ws
        bufferAttrib colorLoc 4 cbuf cs
        glBindVertexArray 0

        let cleanupFunction = do
                withArray [pbuf, tbuf, cbuf] $ glDeleteBuffers 3
                withArray [vao] $ glDeleteVertexArrays 1
            num = fromIntegral $ length vs
            uus = UniformUpdates (Just pjU) (Just mvU) (smU,0) (uvU,0)
            renderFunction = drawBuffer window program uus vao GL_TRIANGLES num
        return $ Renderer renderFunction cleanupFunction

-- | Create a new rendersource (compiled shader program) or retreive an
-- existing one out of our RenderSources
newRenderSource :: (Member (State Resources) r,
                    SetMember Lift (Lift IO) r)
                => RenderDef -> Eff r RenderSource
newRenderSource rd = do
    rsrcs@Resources{..} <- get
    case M.lookup rd rsrcSources of
        Just r -> return r
        Nothing -> do
            r <- lift $ loadRenderSource rd
            let ss = M.insert rd r rsrcSources
            put $ rsrcs{ rsrcSources = ss }
            return r

geomRenderSource :: (Member (State Resources) r,
                      SetMember Lift (Lift IO) r)
                  => Eff r RenderSource
geomRenderSource = do
    let def = RenderDefBS [(vertSourceGeom, GL_VERTEX_SHADER)
                          ,(fragSourceGeom, GL_FRAGMENT_SHADER)]
                          ["projection", "modelview", "sampler", "hasUV"]
    newRenderSource def

bezRenderSource :: (Member (State Resources) r,
                    SetMember Lift (Lift IO) r)
                => Eff r RenderSource
bezRenderSource = do
    let def = RenderDefBS [(vertSourceBezier, GL_VERTEX_SHADER)
                          ,(fragSourceBezier, GL_FRAGMENT_SHADER)
                          ]
                          ["projection", "modelview", "sampler", "hasUV"]
    newRenderSource def

enableBlending :: SetMember Lift (Lift IO) r => Eff r ()
enableBlending = lift $ do
    glEnable GL_BLEND
    glBlendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA

drawClear :: (Member (State Resources) r,
              SetMember Lift (Lift IO) r) => Eff r ()
drawClear = do
    (fbw, fbh) <- framebufferSize
    lift $ do
        pollEvents
        glViewport 0 0 (fromIntegral fbw) (fromIntegral fbh)
        glClear $ GL_COLOR_BUFFER_BIT .|. GL_DEPTH_BUFFER_BIT

framebufferSize :: (Member (State Resources) r,
                    SetMember Lift (Lift IO) r) => Eff r (Int, Int)
framebufferSize = gets rsrcWindow >>= lift . getFramebufferSize

withNewFrame :: (Member (State Resources) r,
                 SetMember Lift (Lift IO) r)
             => Eff r () -> Eff r ()
withNewFrame f = do
    t <- lift $ do pollEvents
                   getCurrentTime
    modify $ \rsrcs -> rsrcs{rsrcUTC = t}
    f
    window <- gets rsrcWindow
    lift $ do
        swapBuffers window
        shouldClose <- windowShouldClose window
        if shouldClose
        then exitSuccess
        else threadDelay 100



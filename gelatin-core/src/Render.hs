{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Render (
    module R,
    module GLFW,
    initResources,
    enableBlending,
    render,
    cache,
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
--import Triangulation.KET
import Linear hiding (trace)
import Graphics.GL.Core33
import Graphics.GL.Types
import Graphics.UI.GLFW as GLFW
import Foreign.Marshal.Array
import Foreign.Marshal.Utils
import Data.Bits
import Data.Monoid
import Data.Time.Clock
import Data.Typeable
--import Data.Vector.Storable (unsafeWith)
--import Codec.Picture as J
--import Codec.Picture.Types
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


render :: (Member (State Resources) r,
           SetMember Lift (Lift IO) r,
           Enum i)
       => i -> Transform -> Eff r ()
render uid t = do
    rs <- fmap rsrcRenderers get
    let i  = fromEnum uid
        mR = IM.lookup i rs
    case mR of
        Nothing -> lift $ putStrLn $ "Could not render with uid value: " ++ show i
        Just r  -> lift $ (rRender r) t

-- | Creates a renderer for the given geometry and coloring and caches it.
-- Returns the uid of the cached renderer.
cache :: (CanCacheRendering r i,
          RealFrac a, Floating a, Ord a, Show i)
     => [Geometrical a] -> [Gradient a] -> Eff r i
cache geom colors = do
    uid <- fresh
    lift $ putStrLn $ "cacheing" ++ show uid
    r   <- newRenderer geom colors
    insertIntoCache uid r
    lift $ putStrLn $ "cached" ++ show uid
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

-- | Creates a new renderer for the given geometry and coloring.
newRenderer :: (SetMember Lift (Lift IO) r,
                Member (State Resources) r,
                RealFrac a, Floating a, Ord a)
            => [Geometrical a] -> [Gradient a] -> Eff r Renderer
newRenderer [] _ = return mempty
newRenderer geom colors = do
    (r, (gs,cs)) <- partialRenderer geom colors
    rest <- newRenderer gs cs
    return $ r <> rest

-- | Folds over the geometry and coloring and creates renderer for
-- like-components. Returns the renderer and the remaining geometry and
-- coloring.
partialRenderer :: forall r a. (SetMember Lift (Lift IO) r,
                    Member (State Resources) r,
                    RealFrac a, Floating a, Ord a)
                => [Geometrical a] -> [Gradient a]
                -> Eff r (Renderer, ([Geometrical a], [Gradient a]))
partialRenderer [] _ = return (mempty, ([], []))
partialRenderer geom@(g:_) colors = do
    dpi <- gets rsrcDpi
    r <- case gs of
             [] -> return mempty
             ((Point _):_)          -> uncurry undefined $ coloredGeom gs cs
             --((Circle _ _):_)       -> uncurry undefined $ coloredGeom gs cs
             ((Line _ _):_)         -> uncurry (newGeomRenderer GL_LINES) $ coloredGeom gs cs
             --((AABB _ _ _):_)       -> uncurry undefined $ coloredGeom gs cs
             ((Triangle _ _ _):_)   -> uncurry (newGeomRenderer GL_TRIANGLES) $ coloredGeom gs cs
             ((Bezier _ _ _ _):_)   -> uncurry (newBezRenderer ws) $ coloredGeom gs cs
             --((Polygon _):_)        -> uncurry undefined $ coloredGeom gs cs
             ((FontString _ _ _):_) -> do
                 lift $ putStrLn "fontstring"
                 let (FontString f px s) = head gs
                     c = head cs
                     fs = fontGeom dpi [(f, px, s)] :: [Geometrical a]
                 text <- stencilXOR <$> (newRenderer fs $ replicate (length fs) c)
                 rest <- newRenderer (drop 1 gs) (drop 1 cs)
                 return $ text <> rest
    return (r, (gs',cs'))

    where gs = takeWhile (isLikeGeom g) geom
          cs = take (length gs) colors
          gs' = drop (length gs) geom
          cs' = drop (length gs) colors
          ws = foldl winding [] gs
          winding xs (Bezier LT _ _ _) = xs ++ [True]
          winding xs (Bezier _ _ _ _) = xs ++ [False]
          winding xs _ = xs

coloredGeom :: (Ord a, Floating a) => [Geometrical a] -> [Gradient a] -> ([V2 a], [V4 a])
coloredGeom gs cs = gcs
    where (gs',cs') = unzip $ zipWith colorWith gs cs
          gcs = (concat gs', concat cs')

colorWith :: (Floating a, Ord a) => Geometrical a -> Gradient a -> ([V2 a], [V4 a])
colorWith (Point a) (SolidGradient g) = ([a], [g])
colorWith (Point a) (RadialGradient c1 c2 r o) =
    ([a], [radialColorAt c1 c2 r o a])

colorWith (Line a b) (SolidGradient g) = ([a,b], [g,g])
colorWith (Line a b) (RadialGradient c1 c2 r o) =
    ([a,b], map (radialColorAt c1 c2 r o) [a,b])

colorWith (Triangle a b c) (SolidGradient g) = ([a,b,c], [g,g,g])
colorWith (Triangle a b c) (RadialGradient c1 c2 r o) =
    ([a,b,c], map (radialColorAt c1 c2 r o) [a,b,c])

colorWith (Bezier _ a b c) (SolidGradient g) = ([a,b,c], [g,g,g])
colorWith (Bezier _ a b c) (RadialGradient c1 c2 r o) =
    ([a,b,c], map (radialColorAt c1 c2 r o) [a,b,c])

colorWith _ _ = ([],[])

radialColorAt :: (Floating a, Ord a) => V4 a -> V4 a -> a -> V2 a -> V2 a -> V4 a
radialColorAt c1 c2 r off pnt =
    let d = distance pnt off
        a = min (d / r) 1
    in lerp a c2 c1

--textRenderer :: (Member (State Resources) r,
--             SetMember Lift (Lift IO) r)
--         => Color -> Font -> PointSize -> String -> Eff r Renderer
--textRenderer color font psize txt = do
--    dpi <- gets rsrcDpi
--    let cs = getStringCurveAtPoint dpi (0,0) [(font, psize, txt)]
--        bs = beziers cs
--        bs' = concat $ concat bs
--        ts = concatMap (concatMap (concaveTriangles . onContourPoints)) bs
--    newPolyRenderer color bs' ts

-- | Combines geometric space coordinates with color space coordinates,
-- sends them to the gpu and returns a cache index to draw with later.

--    newTriRenderer color ts
--draw uid color (DisplayPoly vs) = drawWithCache uid $
--    -- TODO: Triangulating before passing this to newTriRenderer
--    -- is causing the color mapping to bork. The color corresponds directly
--    -- to vs before triangulation.
--    newTriRenderer color $ triangulate vs
--draw uid color (DisplayLine vs) = drawWithCache uid $
--    newLineRenderer color $ toLines vs
--draw uid color (DisplayArrows vs) = drawWithCache uid $
--    newLineRenderer color $ toArrows vs
--draw uid color (DisplayText desc px txt) = \t -> do
--    dpi <- gets rsrcDpi
--    let psize = pixelSizeInPointAtDpi px dpi
--    withFont desc $ \font ->
--        drawWithCache uid (textRenderer color font psize txt) t
--draw uid color (DisplayText' desc px txt) = \t -> do
--    dpi <- gets rsrcDpi
--    let psize = pixelSizeInPointAtDpi px dpi
--    withFont desc $ \font ->
--        flip (drawWithCache uid) t $ do
--            txtr <- do let cs = getStringCurveAtPoint dpi (0,0) [(font, psize, txt)]
--                           bs = beziers cs
--                           bs' = concat $ concat bs
--                           ts = concatMap (concatMap (concaveTriangles . onContourPoints)) bs
--                       pr <- newPolyRenderer (SolidColor $ V4 1 1 1 0.5) [] ts
--                       br <- newPolyRenderer (SolidColor $ V4 1 1 1 1) bs' []
--                       return $ pr <> br
--            let cs = getStringCurveAtPoint dpi (0,0) [(font, psize, txt)]
--                bs = beziers cs
--                as = concatMap (concatMap toArrows) $ map (map onContourPoints) bs
--            linr <- newLineRenderer color as
--            let r = txtr <> linr
--            return r

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



newGeomRenderer :: (Member (State Resources) r,
                   SetMember Lift (Lift IO) r,
                   Real a, Real b)
               => GLuint -> [V2 a] -> [V4 b] -> Eff r Renderer
newGeomRenderer mode vs gs = do
    window <- gets rsrcWindow
    (RenderSource program locs) <- geomRenderSource

    let Just pjU = lookup "projection" locs
        Just mvU = lookup "modelview" locs
        Just smU = lookup "sampler" locs
        Just uvU = lookup "hasUV" locs

    lift $ withVAO $ \vao -> withBuffers 2 $ \[pbuf,cbuf] -> do
        let ps = map realToFrac $ concatMap F.toList vs :: [GLfloat]
            cs = map realToFrac $ concatMap F.toList gs :: [GLfloat]

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

newBezRenderer :: (Member (State Resources) r,
                   SetMember Lift (Lift IO) r,
                   Real a, Real b)
               => [Bool] -> [V2 a] -> [V4 b] -> Eff r Renderer
newBezRenderer ws vs gs = do
    window <- gets rsrcWindow
    (RenderSource program locs) <- bezRenderSource

    let Just pjU = lookup "projection" locs
        Just mvU = lookup "modelview" locs
        Just smU = lookup "sampler" locs
        Just uvU = lookup "hasUV" locs

    lift $ withVAO $ \vao -> withBuffers 3 $ \[pbuf, tbuf, cbuf] -> do
        let ps = map realToFrac $ concatMap F.toList vs :: [GLfloat]
            cs = map realToFrac $ concatMap F.toList gs :: [GLfloat]
            ts = concatMap (\w -> [0, 0, w, 0.5, 0, w, 1, 1, w]) $
                     map fromBool ws :: [GLfloat]

        glDisableVertexAttribArray uvLoc
        glEnableVertexAttribArray colorLoc

        bufferAttrib positionLoc 2 pbuf ps
        bufferAttrib bezLoc 3 tbuf ts
        bufferAttrib colorLoc 4 cbuf cs
        glBindVertexArray 0

        let cleanupFunction = do
                withArray [pbuf, tbuf, cbuf] $ glDeleteBuffers 2
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
    window <- gets rsrcWindow
    lift $ do
        pollEvents
        (fbw, fbh) <- getFramebufferSize window
        glViewport 0 0 (fromIntegral fbw) (fromIntegral fbh)
        glClear $ GL_COLOR_BUFFER_BIT .|. GL_DEPTH_BUFFER_BIT

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

--loadTextureRendererIfNeeded :: (Member (State Resources) r,
--                      SetMember Lift (Lift IO) r)
--                  => Color -> Eff r Renderer
--loadTextureRendererIfNeeded clr
--    | TextureColor (LocalImage fp) _ <- clr = do
--        lift $ putStrLn "Loading image..."
--        esdi <- lift $ readImage fp
--        case esdi of
--            Left s   -> lift $ putStrLn s >> return mempty
--            Right di -> lift $ do
--                t <- loadTexture di
--                let r = const $ glBindTexture GL_TEXTURE_2D t
--                    c = withArray [t] $ glDeleteTextures 1
--                return $ Renderer r c

    -- | TextureColor (HttpImage fp) _ <- clr = do
    --     m <- gets rsrcManager
    --     case parseUrl fp of
    --        Nothing  -> do lift $ putStrLn $ "Error parsing url:" ++ fp
    --                       return Nothing
    --        Just req -> do abs' <- lift $ async $ (LB.toStrict . responseBody) <$> httpLbs req m
    --                       return $ Just abs'
--    | otherwise = return mempty



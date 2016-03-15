{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TupleSections #-}
module Gelatin.GL (
    -- * Re-exports
    module G,
    module Picture,
    module GL,
    module Linear,
    module Renderable,
    -- * Prepare a picture renderer
    compilePictureRenderer,
    -- * Cleaning a picture renderer cache
    cleanPictureRendererCache,
    -- * Clearing the frame
    clearFrame,
    -- * Renderable
    --pictureRenderStrategy
) where

import Gelatin.GL.Renderer as G
import Gelatin.GL.Shader as G
import Gelatin.GL.Common as G
import Gelatin.Core as G
import Gelatin.Picture as Picture
import Control.Monad
import Control.Monad.Trans.State.Strict
import Control.Monad.IO.Class
import Control.Monad.Free
import Control.Monad.Free.Church
import Control.Arrow (first, second)
import Data.Renderable as Renderable
import Data.Hashable
import Data.Bits ((.|.))
import Data.Foldable (foldl')
import Data.Maybe (catMaybes)
import qualified Data.IntMap as IM
import Graphics.Text.TrueType
import Graphics.GL.Types as GL
import Graphics.GL.Core33 as GL
import Linear hiding (rotate, trace)
import System.Exit
import GHC.Generics
--------------------------------------------------------------------------------
-- Compiling a picture
--------------------------------------------------------------------------------
compileLettering :: Lettering a () -> [CompiledPicture a]
compileLettering = compile . freeL
  where compile (Pure ()) = []
        compile (Free (Stroked _ s fd dpi px str f n)) =
          case stringCurvesToPaths fd dpi px str of
            [] -> compile n
            cs -> let prims = compilePrims $ line $ path cs
                      path :: [Path (V2 Float)] -> Lines Color2d ()
                      path [] = return ()
                      path (Path []:pps) = path pps
                      path (Path (p:ps):pps) = do
                        lineStart (p,f p) $ forM_ ps $ \pp ->
                          lineTo (pp, f pp)
                        path pps
                      stk = strokeWith s
                    in CompiledLine SOpNone stk prims : compile n
        compile (Free (Filled _ fd dpi px str (FillColor f) n)) =
          let (bs,ts) = letteringToGeom fd dpi px str f
          in CompiledColor SOpStencilMask (compilePrims ts) :
             CompiledColor SOpNone (compilePrims bs) : compile n
        compile (Free (Filled _ fd dpi px str (FillTexture tx f) n)) =
          let (bs,ts) = letteringToGeom fd dpi px str f
          in CompiledTex SOpStencilMask (compilePrims ts) tx :
             CompiledTex SOpNone (compilePrims bs) tx : compile n


compileDraw :: Draw a () -> [CompiledPicture a]
compileDraw = compile . fromF
  where compile (Pure ()) = []
        compile (Free (Colored g n)) =
          CompiledColor SOpNone (compilePrims g) : compile n
        compile (Free (Textured fp g n)) =
          CompiledTex SOpNone (compilePrims g) fp : compile n
        compile (Free (Line ss ls n)) =
          let prims = compilePrims $ line ls
          in CompiledLine SOpNone (strokeWith ss) prims : compile n
        compile (Free (Letters l n)) =
          compileLettering l ++ compile n

compilePicture :: Picture Transform a () -> [(Transform, CompiledPicture a)]
compilePicture = compile . fromF
  where compile (Pure ()) = []
        compile (Free (Blank n)) = compile n
        compile (Free (Draw d n)) =
          let ps = map (mempty,) $ compileDraw d
          in ps ++ compile n
        compile (Free (WithTransform t p n)) =
          let ps = map (first (mappend t)) (compilePicture p)
          in ps ++ compile n
--------------------------------------------------------------------------------
-- Rendering a picture
--------------------------------------------------------------------------------
--pictureRenderStrategy :: RenderStrategy IO Transform Rez (Picture Transform GLuint)
--pictureRenderStrategy = RenderStrategy
--    { canAllocPrimitive = const $ const $ True
--    , compilePrimitive = \rez pic -> do er <- compiledPicRenderer rez pic
--                                        case er of
--                                          Left err -> do putStrLn err
--                                                         return emptyRenderer
--                                          Right r -> return r
--    }

-- | Creates a renderer for a specific picture without regard for changes
-- over time.
--compileRenderer :: Rez -> Picture () -> IO (Renderer IO Transform)
--compileRenderer rez p = do
--  let s = pictureRenderStrategy
--      ps = compilePicture p
--      makeRenderer (t, p) = do (c,r) <- compilePrimitive s rez p
--                               return (c, r . mappend t)
--  rs <- mapM makeRenderer ps
--  return $ foldl appendRenderer emptyRenderer rs

compilePictureRenderer :: Rez -> Cache IO Transform -> Picture Transform GLuint ()
                       -> IO (GLRenderer, Cache IO Transform)
compilePictureRenderer z cache pic
  | Just r <- IM.lookup (hash pic) cache = return (r, cache)
  | otherwise = do (r, newCache) <- runStateT (compile $ fromF pic) cache
                   return (r, IM.insert (hash pic) r newCache)
  where compile (Pure ()) = return emptyRenderer
        compile (Free (Blank n)) = compile n
        compile (Free (Draw d n)) = do
          rs <- mapM (liftIO . compiledPicRenderer z) $ compileDraw d
          r  <- compile n
          return $ foldl' appendRenderer emptyRenderer $ rs ++ [r]
        compile (Free (WithTransform t p n)) = do
          let k = hash p
          mr <- gets $ IM.lookup k
          (c,f) <- case mr of
            Nothing -> do r <- compile $ fromF p
                          modify' $ IM.insert k r
                          return r
            Just r  -> return r

          r1 <- compile n

          let f1 t1 = f $ t `mappend` t1
          return $ (c,f1) `appendRenderer` r1

cleanPictureRendererCache :: (Monad m, Hashable a)
                          => Cache m t -> Picture t a () -> m (Cache m t)
cleanPictureRendererCache cache pic = do
  let hashes = pictureHashes pic
      rs = map (`IM.lookup` cache) hashes
      found = IM.fromList $ catMaybes $ map f $ zip hashes rs
      f (_, Nothing) = Nothing
      f (x, Just r) = Just $ (x,r)
      stale = cache `IM.difference` found
  sequence_ $ fmap fst stale
  return found

clearFrame :: Rez -> IO ()
clearFrame rez = do
  (fbw,fbh) <- ctxFramebufferSize $ rezContext rez
  glViewport 0 0 (fromIntegral fbw) (fromIntegral fbh)
  glClear $ GL_COLOR_BUFFER_BIT .|. GL_DEPTH_BUFFER_BIT

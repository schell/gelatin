{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}

{-# LANGUAGE TupleSections #-}
module Gelatin.GL (
    -- * Re-exports
    module G,
    module Picture,
    module GL,
    module Linear,
    module Renderable,
    -- * Prepare a picture renderer
    flattenPic,
    compilePictureRenderer,
    -- * Cleaning a picture renderer cache
    cleanPictureRendererCache,
    -- * Clearing the frame
    clearFrame,
    -- * Renderable
    drawRenderStrategy
) where

import Gelatin.GL.Renderer as G
import Gelatin.GL.Shader as G
import Gelatin.GL.Common as G
import Gelatin.Core as G
import Gelatin.Picture as Picture
import Control.Monad.Trans.State.Strict
import Control.Monad.IO.Class
import Control.Monad.Free
import Control.Monad.Free.Church
import Control.Arrow (first)
import Data.Renderable as Renderable
import Data.Hashable
import Data.Bits ((.|.))
import Data.Foldable (foldl')
import Data.Maybe (mapMaybe)
import qualified Data.Vector as B
import qualified Data.Vector.Unboxed as V
import qualified Data.IntMap as IM
import Graphics.GL.Types as GL
import Graphics.GL.Core33 as GL
import Linear hiding (rotate, trace)
--------------------------------------------------------------------------------
-- Compiling a picture
--------------------------------------------------------------------------------
compileLettering :: Lettering a () -> B.Vector (CompiledPicture a)
compileLettering = compile . freeL
  where compile (Pure ()) = B.empty
        compile (Free (Stroked s fd dpi px str _ f n)) =
          case stringCurvesToPaths fd dpi px str of
            [] -> compile n
            cs -> let prims = compilePrims $ line $ path cs
                      path :: [Path (V2 Float)] -> Lines Color2d ()
                      path [] = return ()
                      path (Path ps:pps) =
                        case ps V.!? 0 of
                          Nothing -> path pps
                          Just p -> do lineStart (p, f p) $ V.forM_ (V.drop 1 ps) $
                                         \pp -> lineTo (pp, f pp)
                                       path pps
                      stk = strokeWith s
                    in CompiledLine SOpNone stk prims `B.cons` compile n
        compile (Free (Filled fd dpi px str (FillColor _ f) n)) =
          let (bs,ts) = letteringToGeom fd dpi px str f
          in CompiledColor SOpStencilMask (compilePrims ts) `B.cons`
             (CompiledColor SOpNone (compilePrims bs) `B.cons` compile n)
        compile (Free (Filled fd dpi px str (FillTexture _ tx f) n)) =
          let (bs,ts) = letteringToGeom fd dpi px str f
          in CompiledTex SOpStencilMask (compilePrims ts) tx `B.cons`
             (CompiledTex SOpNone (compilePrims bs) tx `B.cons` compile n)


compileDraw :: Draw a () -> B.Vector (CompiledPicture a)
compileDraw = compile . fromF
  where compile (Pure ()) = B.empty
        compile (Free (Colored g n)) =
          CompiledColor SOpNone (compilePrims g) `B.cons` compile n
        compile (Free (Textured fp g n)) =
          CompiledTex SOpNone (compilePrims g) fp `B.cons` compile n
        compile (Free (Line ss ls n)) =
          let prims = compilePrims $ line ls
          in CompiledLine SOpNone (strokeWith ss) prims `B.cons` compile n
        compile (Free (Letters l n)) =
          compileLettering l B.++ compile n

flattenPic :: Picture a () -> [(PictureTransform, Draw a ())]
flattenPic = compile . fromF
  where compile (Pure ()) = []
        compile (Free (Blank n)) = compile n
        compile (Free (Draw d n)) = (mempty, d) : compile n
        compile (Free (WithTransform t p n)) =
          let ds = map (first (mappend t)) $ flattenPic p
          in ds ++ compile n
--------------------------------------------------------------------------------
-- Rendering a picture
--------------------------------------------------------------------------------
drawRenderStrategy :: RenderStrategy IO PictureTransform Rez (Draw GLuint ())
drawRenderStrategy = RenderStrategy
    { canAllocPrimitive = const $ const True
    , compilePrimitive = \rez d -> do
        rs <- mapM (compiledPicRenderer rez) $ compileDraw d
        return $ foldl appendRenderer emptyRenderer rs
    }

compilePictureRenderer :: Rez -> Cache IO PictureTransform
                       -> Picture GLuint ()
                       -> IO (GLRenderer, Cache IO PictureTransform)
compilePictureRenderer z cache p = runStateT (compile $ flattenPic p) cache
  where compile [] = return emptyRenderer
        compile ((t,d):ds) = do
          let k = hash d
          mr <- gets $ IM.lookup k
          r <- case mr of
            Nothing -> do
              r <- liftIO $ do
                rs <- mapM (compiledPicRenderer z) $ compileDraw d
                return $ foldl' appendRenderer emptyRenderer rs
              modify' $ IM.insert k r
              return r
            Just r -> return r
          r1 <- compile ds
          return $ transformRenderer t r `mappend` r1

cleanPictureRendererCache :: (Monad m, Hashable a)
                          => Cache m PictureTransform -> Picture a ()
                          -> m (Cache m PictureTransform)
cleanPictureRendererCache cache pic = do
  let hashes = map (hash . snd) $ flattenPic pic
      rs = map (`IM.lookup` cache) hashes
      found = IM.fromList $ mapMaybe f (zip hashes rs)
      f (_, Nothing) = Nothing
      f (x, Just r) = Just (x,r)
      stale = cache `IM.difference` found
  sequence_ $ fmap fst stale
  return found

clearFrame :: Rez -> IO ()
clearFrame rez = do
  (fbw,fbh) <- ctxFramebufferSize $ rezContext rez
  glViewport 0 0 (fromIntegral fbw) (fromIntegral fbh)
  glClear $ GL_COLOR_BUFFER_BIT .|. GL_DEPTH_BUFFER_BIT

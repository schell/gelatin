{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveGeneric #-}
module Gelatin.GLFW (
    Rez(..),
    startupGLFWBackend,
    -- * Re-exports
    module Picture,
    module GL,
    module GLFW,
    module Fonty,
    module Linear,
    module Renderable
) where

import Gelatin.Picture as Picture
import Gelatin.PicturePrimitives as Picture
import Gelatin.GL.Renderer
import Gelatin.GL.Shader
import Control.Monad
import Control.Arrow (second)
import Data.Renderable as Renderable
import Data.Hashable
import Graphics.Text.TrueType as Fonty hiding (stringBoundingBox, _e)
import qualified Graphics.Text.TrueType as TT
import Graphics.UI.GLFW as GLFW hiding (init)
import Graphics.GL.Types as GL
import Graphics.GL.Core33 as GL
import Linear hiding (rotate)
import System.Exit
import GHC.Generics

data Rez = Rez { rezShader :: SumShader
               , rezWindow :: Window
               }

-- | Completes all initialization, creates a new window and returns
-- the resource record. If any part of the process fails the program
-- will exit with failure.
startupGLFWBackend :: Int -- ^ Window width
                   -> Int -- ^ Window height
                   -> String -- ^ Window title
                   -> Maybe Monitor -- ^ The monitor to fullscreen into
                   -> Maybe Window -- ^ A window to share OpenGL contexts with
                   -> IO Rez
startupGLFWBackend ww wh ws mmon mwin = do
    initd <- initGelatin
    unless initd $ do putStrLn "could not initialize glfw"
                      exitFailure
    w  <- newWindow ww wh ws mmon mwin
    sh <- loadShaders
    return $ Rez sh w

toPaths :: PathPrimitives Font -> [Path (V2 Float)]
toPaths (Paths ps) = ps
toPaths (PathText f px str) =
    let qs = fontCurves 72 f px str
        sub = subdivideAdaptive 100 0
        mkPath = Path . cleanSeqDupes . concat . fmap sub
        in concat $ fmap (fmap mkPath) qs

instance Primitive (Stroked (PathPrimitives Font)) where
    type PrimM (Stroked (PathPrimitives Font)) = IO
    type PrimR (Stroked (PathPrimitives Font)) = Rez
    type PrimT (Stroked (PathPrimitives Font)) = Transform
    canAllocPrimitive _ (Stroked _ p) = not $ null $ toPaths p
    compilePrimitive (Rez sh win) (Stroked (Stroke c cs w f cp) p) = do
        let ps = toPaths p
            shader = shProjectedPolyline sh
        rs <- forM ps $ \(Path vs) -> do
            let cs' = if null cs then repeat c else gradient
                gradient = [deCasteljau n cs | n <- map (/len) [0..len - 1]]
                len = realToFrac $ length vs
            projectedPolylineRenderer win shader w f cp vs cs'
        return $ foldl appendRenderer emptyRenderer rs

instance Primitive (FillPrimitives Font) where
    type PrimM (FillPrimitives Font) = IO
    type PrimR (FillPrimitives Font) = Rez
    type PrimT (FillPrimitives Font) = Transform
    canAllocPrimitive _ _ = True
    compilePrimitive (Rez sh win) (FillText fill font px str)
        | FillColor f <- fill = do
            let gsh = shGeometry sh
                bsh = shBezier sh
            colorFontRenderer win gsh bsh (FontString font px (0,0) str) f
        -- TODO: FillText with texture fill
        | otherwise = return (return (), const $ return ())
    compilePrimitive (Rez sh win) (FillBeziers fill bs) = do
        let bsh = shBezier sh
        filledBezierRenderer win bsh bs fill
    compilePrimitive (Rez sh win) (FillTriangles fill ts) = do
        let gsh = shGeometry sh
        filledTriangleRenderer win gsh ts fill
    compilePrimitive (Rez sh win) (FillPaths fill ps) = do
        -- We use a filled concave polygon technique instead of
        -- triangulating the path.
        -- http://www.glprogramming.com/red/chapter14.html#name13
        let gsh = shGeometry sh
            tss = map path2ConcavePoly ps
        rs <- forM tss $ \ts -> do
            (c,f) <- filledTriangleRenderer win gsh ts fill
            return (c,\t -> stencilMask (f t) (f t))
        return $ foldl appendRenderer emptyRenderer rs

instance Primitive (R2Primitives Font) where
    type PrimM (R2Primitives Font) = IO
    type PrimR (R2Primitives Font) = Rez
    type PrimT (R2Primitives Font) = Transform
    canAllocPrimitive rez (R2PathPrimitives sps) = canAllocPrimitive rez sps
    canAllocPrimitive rez (R2FillPrimitives fps) = canAllocPrimitive rez fps
    compilePrimitive rez (R2PathPrimitives sps) = compilePrimitive rez sps
    compilePrimitive rez (R2FillPrimitives fps) = compilePrimitive rez fps

deriving instance Generic FontStyle
instance Hashable FontStyle
deriving instance Generic FontDescriptor
instance Hashable FontDescriptor

instance Hashable Font where
    hashWithSalt s = hashWithSalt s . descriptorOf

instance FontClass Font where
    stringBoundingBox font dpi px str = (V2 nx ny, V2 xx xy)
        where BoundingBox nx ny xx xy _ =
                TT.stringBoundingBox font dpi (PointSize px) str

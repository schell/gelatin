{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
module Gelatin.Picture {-(
    -- * Types
    FontData(..),
    PictureCmd(..),
    Picture,
    Coloring(..),
    PaintedPrimitives(..),
    -- * Creating pictures
    blank,
    line,
    polyline,
    rectangle,
    curve,
    arc,
    ellipse,
    circle,
    letters,
    withStroke,
    withFill,
    withTransform,
    withFont,
    move,
    scale,
    rotate,
    -- * Querying pictures
    pictureBounds,
    pictureSize,
    pictureOrigin,
    pictureCenter,
    -- * Compilation helpers
    CompileData(..),
    emptyCompileData,
    freePic,
    withColoring,
    -- * Re-exports
    module Core,
    module Linear
)-} where

import Control.Monad.Free
import Control.Monad.Free.Church
import Gelatin.Core hiding (ellipse, arc)
import qualified Gelatin.Core as Core
import Linear hiding (rotate, trace)
import Data.Hashable
import GHC.Generics
--------------------------------------------------------------------------------
-- Defining shapes
--------------------------------------------------------------------------------
-- | A shape is one or more attributes combined with some extra data in order
-- to describe how to infer a full set of attributes.
data ShapeCmd a n where
    Line          :: Lines a () -> n -> ShapeCmd a n
    Triangle      :: a -> a -> a -> n -> ShapeCmd a n
    Curve         :: a -> a -> a -> n -> ShapeCmd a n
    deriving (Functor)

type Shape a = F (ShapeCmd a)

freeShape :: Shape a () -> Free (ShapeCmd a) ()
freeShape = fromF

polylines :: Lines a () -> Shape a ()
polylines p = liftF $ Line p ()

triangle :: a -> a -> a -> Shape a ()
triangle a b c = liftF $ Triangle a b c ()

curve :: a -> a -> a -> Shape a ()
curve a b c = liftF $ Curve a b c ()
--------------------------------------------------------------------------------
-- Defining drawings
--------------------------------------------------------------------------------
-- | A drawing is either a set of points and colors or a set of points and
-- texture coords. A single element of the set is known as an attribute.
data DrawCmd n where
    Colored       :: Geometry Color2d () -> n -> DrawCmd n
    Textured      :: FilePath -> Geometry Tex2d () -> n -> DrawCmd n
    deriving (Functor)

type Draw = F DrawCmd

freeDraw :: Draw () -> Free DrawCmd ()
freeDraw = fromF

colored :: Geometry Color2d () -> Draw ()
colored g = liftF $ Colored g ()

textured :: FilePath -> Geometry Tex2d () -> Draw ()
textured fp g = liftF $ Textured fp g ()

drawBounds :: Draw () -> (V2 Float, V2 Float)
drawBounds = boundsBounds . bounds . freeDraw
  where bounds (Pure ()) = []
        bounds (Free (Colored g n)) = geomBounds g : bounds n
        bounds (Free (Textured _ g n)) = geomBounds g : bounds n
--------------------------------------------------------------------------------
-- Some attribute types
--------------------------------------------------------------------------------
type Color2d = (V2 Float, V4 Float)
type Tex2d = (V2 Float, V2 Float)
--------------------------------------------------------------------------------
-- Defining whole pictures
--------------------------------------------------------------------------------
data PictureCmd n where
    Blank         :: n -> PictureCmd n
    Draw          :: Draw () -> n -> PictureCmd n
    --WithStroke    :: [StrokeAttr] -> Picture () -> n -> PictureCmd n
    --WithFill      :: Fill -> Picture () -> n -> PictureCmd n
    WithTransform :: Transform -> Picture () -> n -> PictureCmd n
    deriving (Functor)

type Picture = F PictureCmd

freePic :: Picture () -> Free PictureCmd ()
freePic = fromF

blank :: Picture ()
blank = liftF $ Blank ()

draw :: Draw () -> Picture ()
draw d = liftF $ Draw d ()

--withStroke :: [StrokeAttr] -> Picture () -> Picture ()
--withStroke attrs pic = liftF $ WithStroke attrs pic ()
--
--withFill :: Fill -> Picture () -> Picture ()
--withFill f pic = liftF $ WithFill f pic ()

withTransform :: Transform -> Picture () -> Picture ()
withTransform t pic = liftF $ WithTransform t pic ()

move :: V2 Float -> Picture () -> Picture ()
move v = withTransform (Transform v 1 0)

scale :: V2 Float -> Picture () -> Picture ()
scale v = withTransform (Transform 0 v 0)

rotate :: Float -> Picture () -> Picture ()
rotate r = withTransform (Transform 0 1 r)

instance Monoid (Picture ()) where
    mempty = blank
    mappend = (>>)
--------------------------------------------------------------------------------
-- Converting shapes into attribute geometry
--------------------------------------------------------------------------------
shapeToGeom :: Free (ShapeCmd a) () -> Geometry a ()
shapeToGeom (Pure ()) = none
shapeToGeom (Free (Line ls n)) = do
  line ls
  shapeToGeom n
shapeToGeom (Free (Triangle a b c n)) = do
  tri a b c
  shapeToGeom n
shapeToGeom (Free (Curve a b c n)) = do
  bez a b c
  shapeToGeom n

shapes :: Shape a () -> Geometry a ()
shapes = shapeToGeom . freeShape

instance Transformable Transform (Picture ()) where
    transform = withTransform

--data Coloring = StrokeColoring Stroke
--              | FillColoring Fill
--              -- | NoColoring
--
--data PaintedPrimitives = Stroked Stroke Primitives
--                       | Filled Fill Primitives
--                       deriving (Show)
--instance Hashable PaintedPrimitives where
--    hashWithSalt s (Stroked st prim) =
--        let vs = concatMap unPath $ primToPaths prim
--            sh = StrokeHash st vs
--        in s `hashWithSalt` sh `hashWithSalt` prim
--    hashWithSalt s (Filled f prim) =
--        let vs = concatMap unPath $ primToPaths prim
--            fh = FillHash f vs
--        in s `hashWithSalt` fh `hashWithSalt` prim
--
--withColoring :: Coloring -> Primitives -> PaintedPrimitives
--withColoring (FillColoring f) = Filled f
--withColoring (StrokeColoring s) = Stroked s
--
--data CompileData = CompileData { cdFont :: Maybe FontData
--                               , cdColoring :: Maybe Coloring
--                               , cdTransform :: Transform
--                               }
--
--emptyCompileData :: CompileData
--emptyCompileData = CompileData Nothing Nothing mempty
--------------------------------------------------------------------------------
-- Measuring pictures
--------------------------------------------------------------------------------
boundingBox :: Free PictureCmd () -> (V2 Float, V2 Float)
boundingBox (Pure ()) = (0,0)
boundingBox (Free (Blank n)) = boundingBox n
boundingBox (Free (Draw d n)) = boundsBounds [drawBounds d, boundingBox n]
boundingBox (Free (WithTransform t p n)) =
  boundsBounds [transform t $ boundingBox $ fromF p, boundingBox n]
--boundingBox (Free (Polyline vs n)) =
--    boundsBounds [polyBounds vs, boundingBox cd n]
--boundingBox cd (Free (Rectangle v n)) =
--    let vs = box v
--    in boundsBounds [polyBounds vs, boundingBox cd n]
--boundingBox cd (Free (Curve a b c n)) =
--    let vs = subdivideAdaptive 100 0 $ bez3 a b c
--    in boundsBounds [polyBounds vs, boundingBox cd n]
--boundingBox cd (Free (Arc (V2 xr yr) start stop n)) =
--    let vs = concatMap (subdivideAdaptive4 100 0) $ Core.arc xr yr start stop
--    in boundsBounds [polyBounds vs, boundingBox cd n]
--boundingBox cd (Free (Ellipse (V2 x y) n)) =
--    let vs = bez4sToPath 100 0 $ Core.ellipse x y
--    in boundsBounds [polyBounds vs, boundingBox cd n]
--boundingBox cd (Free (Circle r n)) =
--    let vs = bez4sToPath 100 0 $ Core.ellipse r r
--    in boundsBounds [polyBounds vs, boundingBox cd n]
--boundingBox cd (Free (Letters dpi px str n))
--    | Just font <- cdFont cd =
--        let (tl, br) = polyBounds $ trisToComp $ snd $
--                         fontStringGeom font dpi px str
--            vs = [tl, br]
--        in boundsBounds [polyBounds vs, boundingBox cd n]
--    | otherwise = boundingBox cd n
--boundingBox cd (Free (WithStroke _ p n)) =
--    boundsBounds [boundingBox cd $ freePic p, boundingBox cd n]
--boundingBox cd (Free (WithFill _ p n)) =
--    boundsBounds [boundingBox cd $ freePic p, boundingBox cd n]
--boundingBox cd (Free (WithTransform t p n)) =
--    let (tl,br) = boundingBox cd $ freePic p
--        (tl',br') = (transform t tl, transform t br)
--    in boundsBounds [ (tl',br')
--                    , boundingBox cd n
--                    ]
--boundingBox cd (Free (WithFont font p n)) =
--    let cd' = cd{cdFont = Just font}
--    in boundsBounds [boundingBox cd' $ freePic p, boundingBox cd n]
--
-- | Returns the bounding box of the picture.
pictureBounds :: Picture () -> BBox
pictureBounds = boundingBox . freePic

-- | Returns the size of the picture.
pictureSize :: Picture () -> V2 Float
pictureSize p =
    let (tl,br) = pictureBounds p
    in br - tl

-- Returns the center point of the picture, based on its bounding box.
pictureCenter :: Picture () -> V2 Float
pictureCenter p =
    let (tl,br) = pictureBounds p
    in tl + (br - tl) / 2

-- Returns the leftmost, uppermost point of the picture.
pictureOrigin :: Picture () -> V2 Float
pictureOrigin = fst . pictureBounds
--------------------------------------------------------------------------------
-- Compiling a picture
--------------------------------------------------------------------------------
data CompiledPicture = CompiledWithTex [GPrims (V2 Float, V2 Float)] FilePath
                     | CompiledWithColor [GPrims (V2 Float, V4 Float)]

instance Hashable CompiledPicture where
  hashWithSalt s (CompiledWithTex ps fp) =
    s `hashWithSalt` ps `hashWithSalt` fp
  hashWithSalt s (CompiledWithColor ps) =
    s `hashWithSalt` ps

compileDraw :: Draw () -> [CompiledPicture]
compileDraw = compile . fromF
  where compile (Pure ()) = []
        compile (Free (Colored g n)) =
          CompiledWithColor (compilePrims g) : compile n
        compile (Free (Textured fp g n)) =
          CompiledWithTex (compilePrims g) fp : compile n

compilePicture :: Picture () -> [(Transform, CompiledPicture)]
compilePicture = compile . fromF
  where compile (Pure ()) = []
        compile (Free (Blank n)) = compile n
        compile (Free (Draw d n)) =
          let ps = map (mempty,) $ compileDraw d
          in ps ++ compile n
        compile (Free (WithTransform t p n)) =
          let ps = map (\(t',p) -> (mappend t t', p)) (compilePicture p)
          in ps ++ compile n

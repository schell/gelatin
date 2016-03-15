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

import Control.Arrow (first)
import Control.Monad (forM_)
import Control.Monad.Free
import Control.Monad.Free.Church
import Gelatin.Core.Bounds
import Gelatin.Core.Stroke
import Gelatin.Core.Bezier
import Gelatin.Core.Path
import Gelatin.Core.Font
import Gelatin.Core.Fill
import qualified Gelatin.Core.Triangle as T
import Linear hiding (rotate, trace)

import Data.Hashable
import GHC.Generics
--------------------------------------------------------------------------------
-- Monadically defining paths geometry
--------------------------------------------------------------------------------
data LineAddCmd a n where
  LineTo  :: a -> n -> LineAddCmd a n
  CurveTo :: a -> a -> n -> LineAddCmd a n
  deriving (Show, Functor, Generic)

type LineAdd a = F (LineAddCmd a)

lineTo :: a -> LineAdd a ()
lineTo a = liftF $ LineTo a ()

curveTo :: a -> a -> LineAdd a ()
curveTo c a = liftF $ CurveTo c a ()

data LineSegment a = LineSegment a
                   | LineCurve a a
                   deriving (Generic)

instance Hashable a => Hashable (LineSegment a)

segmentsToPolyline :: Additive f
                   => LineSegment (V2 Float, f Float)
                   -> [LineSegment (V2 Float, f Float)]
                   -> [(V2 Float, f Float)]
segmentsToPolyline (LineSegment a) (LineSegment b:ls) =
  a: segmentsToPolyline (LineSegment b) ls
segmentsToPolyline (LineSegment (va,ca)) (LineCurve (vb,cb) (vc,cc):ls) =
  let vs = subdivideAdaptive 100 0 $ bez3 va vb vc
      n = fromIntegral (length vs) / 2
      r = fromIntegral (length vs) - n
      cs = reverse $ rcs ++ ncs
      ncs = map (\x -> lerp (x/n) ca cb) [0..n]
      rcs = map (\x -> lerp (x/r) cb cc) [0..r]
  in zip vs cs ++ segmentsToPolyline (LineCurve (vb,cb) (vc,cc)) ls
segmentsToPolyline (LineCurve _ _) (LineSegment c:ls) =
  segmentsToPolyline (LineSegment c) ls
segmentsToPolyline (LineCurve _ b) (LineCurve c d:ls) =
  segmentsToPolyline (LineSegment b) $ LineCurve c d:ls
segmentsToPolyline (LineSegment a) [] = [a]
segmentsToPolyline (LineCurve _ _) [] = []

addsToSegments :: Free (LineAddCmd a) () -> [LineSegment a]
addsToSegments (Pure ()) = []
addsToSegments (Free (LineTo a n)) = LineSegment a : addsToSegments n
addsToSegments (Free (CurveTo a b n)) = LineCurve a b : addsToSegments n

data LinesCmd a n where
  LineStart :: a -> LineAdd a () -> n -> LinesCmd a n
  deriving (Functor)

type Lines a = F (LinesCmd a)

lineStart :: a -> LineAdd a () -> Lines a ()
lineStart a ls = liftF $ LineStart a ls ()

toSegments :: Free (LinesCmd a) () -> [(LineSegment a , [LineSegment a])]
toSegments (Pure ()) = []
toSegments (Free (LineStart a adds n)) =
  (LineSegment a, addsToSegments $ fromF adds) : toSegments n

lineToSegments :: Lines a () -> [(LineSegment a, [LineSegment a])]
lineToSegments = toSegments . fromF

lineBounds :: Lines (V2 Float, a) () -> (V2 Float, V2 Float)
lineBounds = boundsBounds . segmentBounds . lineToSegments

segmentBounds :: [(LineSegment (V2 Float,a), [LineSegment (V2 Float, a)])]
              -> [(V2 Float, V2 Float)]
segmentBounds [] = []
segmentBounds xs = [polyBounds $ map fst $ concatMap toPoly $ concatMap (uncurry (:)) xs]
  where toPoly (LineSegment a) = [a]
        toPoly (LineCurve a b) = [a,b]

instance Hashable a => Hashable (Lines a ()) where
  hashWithSalt s = hashWithSalt s . lineToSegments
--------------------------------------------------------------------------------
-- Monadically defining attributes and geometry
--------------------------------------------------------------------------------
data Geom a n where
  GNone          :: n -> Geom a n
  GTriangle      :: a -> a -> a -> n -> Geom a n
  GBez           :: a -> a -> a -> n -> Geom a n
  GTriangleStrip :: a -> a -> a -> [a] -> n -> Geom a n
  GTriangleFan   :: a -> a -> a -> [a] -> n -> Geom a n
  GLine          :: Lines a () -> n -> Geom a n
  deriving (Functor)

type Geometry a = F (Geom a)

none :: Geometry a ()
none = liftF $ GNone ()

tri :: a -> a -> a -> Geometry a ()
tri a b c = liftF $ GTriangle a b c ()

bez :: a -> a -> a -> Geometry a ()
bez a b c = liftF $ GBez a b c ()

strip :: a -> a -> a -> [a] -> Geometry a ()
strip a b c ds = liftF $ GTriangleStrip a b c ds ()

fan :: a -> a -> a -> [a] -> Geometry a ()
fan a b c ds = liftF $ GTriangleFan a b c ds ()

line :: Lines a () -> Geometry a ()
line p = liftF $ GLine p ()

geomBounds :: Geometry (V2 Float, a) () -> (V2 Float, V2 Float)
geomBounds = boundsBounds . geomBounds' . fromF

geomBounds' :: Free (Geom (V2 Float, a)) () -> [(V2 Float, V2 Float)]
geomBounds' (Pure ()) = []
geomBounds' (Free (GNone n)) = geomBounds' n
geomBounds' (Free (GTriangle a b c n)) =
  polyBounds (map fst [a,b,c]) : geomBounds' n
geomBounds' (Free (GBez a b c n)) =
  polyBounds (map fst [a,b,c]) : geomBounds' n
geomBounds' (Free (GTriangleStrip a b c ds n)) =
  polyBounds (map fst $ a:b:c:ds) : geomBounds' n
geomBounds' (Free (GTriangleFan a b c ds n)) =
  polyBounds (map fst $ a:b:c:ds) : geomBounds' n
geomBounds' (Free (GLine l n)) =
  lineBounds l : geomBounds' n

instance Monoid (Geometry a ()) where
  mempty = none
  mappend = (>>)

instance Hashable a => Hashable (Geometry a ()) where
  hashWithSalt s0 = hashup s0 . fromF
    where hashup s (Pure ()) = s
          hashup s (Free (GNone n)) = hashup s n
          hashup s (Free (GTriangle a b c n)) =
            (s `hashWithSalt` [a,b,c]) `hashup` n
          hashup s (Free (GBez a b c n)) =
            (s `hashWithSalt` [a,b,c]) `hashup` n
          hashup s (Free (GTriangleStrip a b c ds n)) =
            (s `hashWithSalt` ([a,b,c] ++ ds)) `hashup` n
          hashup s (Free (GTriangleFan a b c ds n)) =
            (s `hashWithSalt` ([a,b,c] ++ ds)) `hashup` n
          hashup s (Free (GLine l n)) =
            (s `hashWithSalt` l) `hashup` n

--------------------------------------------------------------------------------
-- Defining shapes
--------------------------------------------------------------------------------
-- | A shape is one or more attributes combined with some extra data in order
-- to describe how to infer a full set of attributes.
--data ShapeCmd a n where
--    Triangle      :: a -> a -> a -> n -> ShapeCmd a n
--    Curve         :: a -> a -> a -> n -> ShapeCmd a n
--    Arc           :: a -> a -> a -> a -> Float -> Float -> n -> ShapCmd a n
--    deriving (Functor)

--type Shape a = F (ShapeCmd a)
--
--freeShape :: Shape a () -> Free (ShapeCmd a) ()
--freeShape = fromF
--
--triangle :: a -> a -> a -> Shape a ()
--triangle a b c = liftF $ Triangle a b c ()
--
--curve :: a -> a -> a -> Shape a ()
--curve a b c = liftF $ Curve a b c ()

--arc :: a -> a -> a -> Float -> Float -> Shape a ()
--arc a w h start stop = liftF $ Arc a w h start stop
--------------------------------------------------------------------------------
-- Defining Lettering
--------------------------------------------------------------------------------
newtype Name = Name { unName :: Int }
             deriving (Show, Eq, Ord)

data LetteringCmd a n where
  Stroked :: Name -> [StrokeAttr] -> FontData -> Int -> Float -> String
          -> (V2 Float -> V4 Float) -> n -> LetteringCmd a n
  Filled  :: Name -> FontData -> Int -> Float -> String -> Fill a -> n
          -> LetteringCmd a n
  deriving (Functor)

type Lettering a = F (LetteringCmd a)

freeL :: Lettering a () -> Free (LetteringCmd a) ()
freeL = fromF

stroked :: Name -> [StrokeAttr] -> FontData -> Int -> Float -> String
        -> (V2 Float -> V4 Float) -> Lettering a ()
stroked n ats fd dpi px str f = liftF $ Stroked n ats fd dpi px str f ()

filled :: Name -> FontData -> Int -> Float -> String -> Fill a -> Lettering a ()
filled n fd dpi px str f = liftF $ Filled n fd dpi px str f ()

-- | A convenience function for turning lettering into a tuple of a list of
-- inner or outer beziers and a list of triangles. The triangles should be
-- rendered using the concave polygon stencil method.
letteringToGeom :: FontData -> Int -> Float -> String
                -> (V2 Float -> f Float) -> (Geometry (V2 Float, f Float) ()
                                            ,Geometry (V2 Float, f Float) ()
                                            )
letteringToGeom fd dpi px str f =
  let (bs,ts) = fontStringGeom fd dpi px str
      bs1 = fmap (fmap g) bs
      ts1 = fmap (fmap g) ts
      g x = (x, f x)
      triangles (T.Triangle a b c :xs) = tri a b c >> triangles xs
      triangles [] = return ()
      beziers (Bezier _ a b c : xs) = bez a b c >> beziers xs
      beziers [] = return ()
  in (beziers bs1, triangles ts1)

letterBounds :: Lettering a () -> (V2 Float, V2 Float)
letterBounds = boundsBounds . bounds . freeL
  where bounds (Pure ()) = [(0,0)]
        bounds (Free (Stroked _ _ fd dpi px str _ n)) =
          calc fd dpi px str : bounds n
        bounds (Free (Filled _ fd dpi px str _ n)) =
          calc fd dpi px str : bounds n
        calc fd dpi px str =
          let (tl, br) = polyBounds $ T.trisToComp $ snd $
                           fontStringGeom fd dpi px str
              vs = [tl, br]
          in polyBounds vs

instance Hashable a => Hashable (Lettering a ()) where
  hashWithSalt s0 = hashup s0 . fromF
    where hashup s (Pure ()) = s
          hashup s (Free (Stroked nm st fd dpi px str _ n)) =
            let s1 = s `hashWithSalt` unName nm `hashWithSalt` st
                s2 = fontHash fd s1
                s3 = s2 `hashWithSalt` dpi `hashWithSalt` px `hashWithSalt` str
            in s3 `hashup` n
          hashup s (Free (Filled nm fd dpi px str f n)) =
            let s1 = s `hashWithSalt` unName nm
                s2 = fontHash fd s1
                s3 = s2 `hashWithSalt` dpi `hashWithSalt` px `hashWithSalt` str
                s4 = case f of
                       FillColor _ -> s3
                       FillTexture t _ -> s3 `hashWithSalt` t
                in s4 `hashup` n
--------------------------------------------------------------------------------
-- Defining drawings
--------------------------------------------------------------------------------
-- | A drawing is either a set of points and colors or a set of points and
-- texture coords. A single element of the set is known as an attribute. A
-- drawing is parameterized by its texture type, allowing textures to be handled
-- according to the specific backend being used.
data DrawCmd a n where
    Line     :: [StrokeAttr] -> Lines Color2d () -> n -> DrawCmd a n
    Colored  :: Geometry Color2d () -> n -> DrawCmd a n
    Textured :: a -> Geometry Tex2d () -> n -> DrawCmd a n
    Letters  :: Lettering a () -> n -> DrawCmd a n
    deriving (Functor)

type Draw a = F (DrawCmd a)

colored :: Geometry Color2d () -> Draw a ()
colored g = liftF $ Colored g ()

textured :: a -> Geometry Tex2d () -> Draw a ()
textured tex g = liftF $ Textured tex g ()

polylines :: [StrokeAttr] -> Lines Color2d () -> Draw a ()
polylines ss p = liftF $ Line ss p ()

letters :: Lettering a () -> Draw a ()
letters l = liftF $ Letters l ()

drawBounds :: Draw a () -> (V2 Float, V2 Float)
drawBounds = boundsBounds . bounds . fromF
  where bounds (Pure ()) = []
        bounds (Free (Colored g n)) = geomBounds g : bounds n
        bounds (Free (Textured _ g n)) = geomBounds g : bounds n
        bounds (Free (Line _ ls n)) = geomBounds (line ls) : bounds n
        bounds (Free (Letters l n)) = letterBounds l : bounds n

instance Hashable a => Hashable (Draw a ()) where
  hashWithSalt s0 = hashup s0 . fromF
    where hashup s (Pure ()) = s
          hashup s (Free (Colored g n)) = (s `hashWithSalt` g) `hashup` n
          hashup s (Free (Textured t g n)) =
            (s `hashWithSalt` t `hashWithSalt` g) `hashup` n
          hashup s (Free (Line st ls n)) =
            (s `hashWithSalt` st `hashWithSalt` ls) `hashup` n
          hashup s (Free (Letters l n)) = (s `hashWithSalt` l) `hashup` n
--------------------------------------------------------------------------------
-- Some attribute types
--------------------------------------------------------------------------------
type Color2d = (V2 Float, V4 Float)
type Tex2d = (V2 Float, V2 Float)
--------------------------------------------------------------------------------
-- Defining whole pictures
--------------------------------------------------------------------------------
data PictureCmd t a n where
    Blank         :: n -> PictureCmd t a n
    Draw          :: Draw a () -> n -> PictureCmd t a n
    WithTransform :: t -> Picture t a () -> n -> PictureCmd t a n
    deriving (Functor, Generic)

type Picture t a = F (PictureCmd t a)

freePic :: Picture t a () -> Free (PictureCmd t a) ()
freePic = fromF

blank :: Picture t a ()
blank = liftF $ Blank ()

draw :: Draw a () -> Picture t a ()
draw d = liftF $ Draw d ()

withTransform :: t -> Picture t a () -> Picture t a ()
withTransform t pic = liftF $ WithTransform t pic ()

instance Monoid (Picture t a ()) where
    mempty = blank
    mappend = (>>)

instance Hashable a => Hashable (Picture t a ()) where
  hashWithSalt x = hashup x . fromF
    where hashup s (Pure ()) = s
          hashup s (Free (Blank n)) = s `hashup` n
          hashup s (Free (Draw d n)) = (s `hashWithSalt` d) `hashup` n
          hashup s (Free (WithTransform _ p n)) =
            (s `hashWithSalt` p) `hashup` n
--------------------------------------------------------------------------------
-- Converting shapes into attribute geometry
--------------------------------------------------------------------------------
--shapeToGeom :: Free (ShapeCmd a) () -> Geometry a ()
--shapeToGeom (Pure ()) = none
--shapeToGeom (Free (Triangle a b c n)) = do
--  tri a b c
--  shapeToGeom n
--shapeToGeom (Free (Curve a b c n)) = do
--  bez a b c
--  shapeToGeom n

--shapes :: Shape a () -> Geometry a ()
--shapes = shapeToGeom . freeShape
--------------------------------------------------------------------------------
-- Measuring pictures
--------------------------------------------------------------------------------
type AffineTransform t = t -> V2 Float -> V2 Float

boundingBox :: AffineTransform t -> Free (PictureCmd t a) ()
            -> (V2 Float, V2 Float)
boundingBox _ (Pure ()) = (0,0)
boundingBox f (Free (Blank n)) = boundingBox f n
boundingBox f (Free (Draw d n)) = boundsBounds [drawBounds d, boundingBox f n]
boundingBox f (Free (WithTransform t p n)) =
  let (tl,br) = boundingBox f $ fromF p
  in boundsBounds [(f t tl, f t br), boundingBox f n]

-- | Returns the bounding box of the picture.
pictureBounds :: AffineTransform t -> Picture t a () -> BBox
pictureBounds f = boundingBox f . freePic

-- | Returns the size of the picture.
pictureSize :: AffineTransform t -> Picture t a () -> V2 Float
pictureSize f p =
    let (tl,br) = pictureBounds f p
    in br - tl

-- Returns the center point of the picture, based on its bounding box.
pictureCenter :: AffineTransform t -> Picture t a () -> V2 Float
pictureCenter f p =
    let (tl,br) = pictureBounds f p
    in tl + (br - tl) / 2

-- Returns the leftmost, uppermost point of the picture.
pictureOrigin :: AffineTransform t -> Picture t a () -> V2 Float
pictureOrigin f = fst . pictureBounds f

-- A list of rendering hashes contained within this picture.
pictureHashes :: Hashable a => Picture t a () -> [Int]
pictureHashes p = hash p : (compile $ fromF p)
  where compile (Pure ()) = []
        compile (Free (Blank n)) = compile n
        compile (Free (Draw _ n)) = compile n
        compile (Free (WithTransform _ p1 n)) = pictureHashes p1 ++ compile n

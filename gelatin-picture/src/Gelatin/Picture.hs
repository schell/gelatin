{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
module Gelatin.Picture (
  -- * Creating pictures
  withLetters,
  withStroke,
  withColor,
  withTexture,
  blank,
  draw,
  Picture,
  PictureCmd(..),
  -- * Transforming pictures
  withTransform,
  move,
  scale,
  rotate,
  withAlpha,
  withMult,
  PictureTransform(..),
  -- * Creating drawings within pictures
  colored,
  textured,
  polylines,
  letters,
  Draw,
  DrawCmd(..),
  drawBounds,
  -- * Lettering
  stroked,
  filled,
  Name(..),
  Lettering,
  LetteringCmd(..),
  letterBounds,
  letteringToGeom,
  -- * Geometry
  none,
  tris,
  triList,
  tri,
  bezs,
  bezList,
  bez,
  strip,
  fan,
  line,
  Geometry,
  Geom(..),
  geomBounds,
  -- * Creating geometry using shapes
  rectangle,
  -- * Geometry Attributes
  Color2d,
  Tex2d,
  -- * Lines
  lineStart,
  lineTo,
  curveTo,
  Lines,
  LinesCmd(..),
  LineAdd,
  LineAddCmd(..),
  linesToPolylines,
  lineBounds,
  -- * Measuring pictures
  pictureBounds,
  pictureOrigin,
  pictureCenter,
  pictureSize,
  -- * Other Helpers
  pictureHashes,
  freeL,
) where

import           Control.Monad.Free
import           Control.Monad.Free.Church
import           Gelatin.Core.Bounds
import           Gelatin.Core.Bezier
import           Gelatin.Core.Stroke
import           Gelatin.Core.Font
import           Gelatin.Core.Fill
import           Gelatin.Core.Transform
import           Gelatin.Core.Triangle
import           Linear hiding (rotate, trace)
import           Data.Hashable
import qualified Data.Vector as B
import qualified Data.Vector.Unboxed as V
import           Data.Vector.Unboxed (Vector, Unbox)
import           GHC.Generics
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

data LinesCmd a n where
  LineStart :: a -> LineAdd a () -> n -> LinesCmd a n
  deriving (Functor)

type Lines a = F (LinesCmd a)

fromL :: Lines a () -> Free (LinesCmd a) ()
fromL = fromF

lineStart :: a -> LineAdd a () -> Lines a ()
lineStart a ls = liftF $ LineStart a ls ()

addsToPolyline :: (Unbox (f Float), Additive f)
               => LineAdd (V2 Float, f Float) () -> Vector (V2 Float, f Float)
addsToPolyline = go Nothing . fromF
  where go _ (Pure ()) = V.empty
        go _ (Free (LineTo a n)) = a `V.cons` go (Just a) n
        go (Just (a,ca)) (Free (CurveTo (b,cb) (c,cc) n)) =
          let vs = subdivideAdaptive 100 0 $ bez3 a b c
              t = fromIntegral (V.length vs) / 2
              r = fromIntegral (V.length vs) - t
              cs = V.reverse $ rcs V.++ ncs
              ncs = V.map (\x -> lerp (x/t) ca cb) $ V.fromList [0..t]
              rcs = V.map (\x -> lerp (x/r) cb cc) $ V.fromList [0..r]
          in V.zip vs cs V.++ go (Just (c,cc)) n
        go _ (Free (CurveTo a b n)) = V.fromList [a,b] V.++ go (Just b) n

linesToPolylines :: (Unbox (f Float), Additive f)
                 => Lines (V2 Float, f Float) ()
                 -> B.Vector (Vector (V2 Float, f Float))
linesToPolylines = go . fromL
  where go (Pure ()) = B.empty
        go (Free (LineStart a adds n)) =
          (a `V.cons` addsToPolyline adds) `B.cons` go n

lineBounds :: (Unbox (f Float), Additive f)
           => Lines (V2 Float, f Float) () -> (V2 Float, V2 Float)
lineBounds = polyBounds . cat . B.map (V.map fst) . linesToPolylines

cat :: Unbox a => B.Vector (Vector a) -> Vector a
cat = V.convert . B.concatMap B.convert

instance (Hashable (f Float), Unbox (f Float), Additive f)
  => Hashable (Lines (V2 Float, f Float) ()) where
  hashWithSalt s = hashWithSalt s . linesToPolylines
--------------------------------------------------------------------------------
-- Monadically defining attributes and geometry
--------------------------------------------------------------------------------
data Geom a n where
  GNone          :: n -> Geom a n
  GTriangles     :: Vector (a, a, a) -> n -> Geom a n
  GBezs          :: Vector (a, a, a) -> n -> Geom a n
  GTriangleStrip :: a -> a -> a -> Vector a -> n -> Geom a n
  GTriangleFan   :: a -> a -> a -> Vector a -> n -> Geom a n
  GLine          :: Lines a () -> n -> Geom a n
  deriving (Functor)

type Geometry a = F (Geom a)

none :: Geometry a ()
none = liftF $ GNone ()

tris :: Vector (a, a, a) -> Geometry a ()
tris ts = liftF $ GTriangles ts ()

triList :: Unbox a => [(a,a,a)] -> Geometry a ()
triList = tris . V.fromList

tri :: Unbox a => a -> a -> a -> Geometry a ()
tri a b c = triList [(a,b,c)]

bezs :: Vector (a, a, a) -> Geometry a ()
bezs ts = liftF $ GBezs ts ()

bezList :: Unbox a => [(a,a,a)] -> Geometry a ()
bezList = bezs . V.fromList

bez :: Unbox a => a -> a -> a -> Geometry a ()
bez a b c = bezList [(a,b,c)]

strip :: a -> a -> a -> Vector a -> Geometry a ()
strip a b c ds = liftF $ GTriangleStrip a b c ds ()

fan :: a -> a -> a -> Vector a -> Geometry a ()
fan a b c ds = liftF $ GTriangleFan a b c ds ()

line :: Lines a () -> Geometry a ()
line p = liftF $ GLine p ()

geomBounds :: (Unbox (f Float), Additive f)
           => Geometry (V2 Float, f Float) () -> (V2 Float, V2 Float)
geomBounds = boundsBounds . geomBounds' . fromF

trisBounds :: ((V2 Float,a),(V2 Float,a),(V2 Float,a)) -> (V2 Float, V2 Float)
trisBounds (a,b,c) = polyBounds $ V.fromList $ map fst [a,b,c]

geomBounds' :: (Unbox (f Float), Additive f)
            => Free (Geom (V2 Float, f Float)) () -> Vector (V2 Float, V2 Float)
geomBounds' (Pure ()) = V.empty
geomBounds' (Free (GNone n)) = geomBounds' n
geomBounds' (Free (GTriangles ts n)) = V.map trisBounds ts V.++ geomBounds' n
geomBounds' (Free (GBezs bs n)) = V.map trisBounds bs V.++ geomBounds' n
geomBounds' (Free (GTriangleStrip a b c ds n)) =
  polyBounds (V.map fst $ V.fromList [a,b,c] V.++ ds) `V.cons` geomBounds' n
geomBounds' (Free (GTriangleFan a b c ds n)) =
  polyBounds (V.map fst $ V.fromList [a,b,c] V.++ ds) `V.cons` geomBounds' n
geomBounds' (Free (GLine l n)) =
  lineBounds l `V.cons` geomBounds' n

instance Monoid (Geometry a ()) where
  mempty = none
  mappend = (>>)

instance (Hashable (f Float), Unbox (f Float), Additive f)
  => Hashable (Geometry (V2 Float, f Float) ()) where
  hashWithSalt s0 = hashup s0 . fromF
    where hashup s (Pure ()) = s
          hashup s (Free (GNone n)) = hashup s n
          hashup s (Free (GTriangles ts n)) =
            (s `hashWithSalt` ts) `hashup` n
          hashup s (Free (GBezs bs n)) =
            (s `hashWithSalt` bs) `hashup` n
          hashup s (Free (GTriangleStrip a b c ds n)) =
            (s `hashWithSalt` (a,b,c) `hashWithSalt` ds) `hashup` n
          hashup s (Free (GTriangleFan a b c ds n)) =
            (s `hashWithSalt` (a,b,c) `hashWithSalt` ds) `hashup` n
          hashup s (Free (GLine l n)) =
            (s `hashWithSalt` l) `hashup` n
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
-- inner or outer beziers and a list of triangle fans. The triangles should be
-- rendered using the concave polygon stencil method.
letteringToGeom :: Unbox (f Float)
                => FontData -> Int -> Float -> String
                -> (V2 Float -> f Float) -> (Geometry (V2 Float, f Float) ()
                                            ,Geometry (V2 Float, f Float) ()
                                            )
letteringToGeom fd dpi px str f =
  let (bs,ts) = fontStringGeom fd dpi px str
      g x = (x, f x)
      triangles = map (V.map g) ts
      beziers = V.map ((\(_,a,b,c) -> (a,b,c)) . fmapBezier g) bs
      toTriFan xs = if V.length xs >= 3
                      then fan (xs V.! 0) (xs V.! 1) (xs V.! 2) $ V.drop 3 xs
                      else return ()
  in (bezs beziers, mapM_ toTriFan triangles)

letterBounds :: Lettering a () -> (V2 Float, V2 Float)
letterBounds = boundsBounds . bounds . freeL
  where bounds (Pure ()) = V.singleton (0,0)
        bounds (Free (Stroked _ _ fd dpi px str _ n)) =
          calc fd dpi px str `V.cons` bounds n
        bounds (Free (Filled _ fd dpi px str _ n)) =
          calc fd dpi px str `V.cons` bounds n
        calc fd dpi px str =
          let (tl, br) = polyBounds $ V.concat $ snd $
                           fontStringGeom fd dpi px str
              vs = [tl, br]
          in polyBounds $ V.fromList vs

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
  where bounds (Pure ()) = V.empty
        bounds (Free (Colored g n)) = geomBounds g `V.cons` bounds n
        bounds (Free (Textured _ g n)) = geomBounds g `V.cons` bounds n
        bounds (Free (Line _ ls n)) = geomBounds (line ls) `V.cons` bounds n
        bounds (Free (Letters l n)) = letterBounds l `V.cons` bounds n

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
-- Transforming pictures
--------------------------------------------------------------------------------
data PictureTransform = PictureTransform { ptAffine :: Transform
                                         , ptAlpha  :: Float
                                         , ptMult   :: V4 Float
                                         } deriving (Show)

instance Monoid PictureTransform where
  mempty = PictureTransform mempty 1 1
  mappend (PictureTransform at aa am) (PictureTransform bt ba bm) =
    PictureTransform (at `mappend` bt) (aa * ba) (am * bm)
--------------------------------------------------------------------------------
-- Defining whole pictures
--------------------------------------------------------------------------------
data PictureCmd a n where
    Blank         :: n -> PictureCmd a n
    Draw          :: Draw a () -> n -> PictureCmd a n
    WithTransform :: PictureTransform -> Picture a () -> n -> PictureCmd a n
    deriving (Functor, Generic)

type Picture a = F (PictureCmd a)

freePic :: Picture a () -> Free (PictureCmd a) ()
freePic = fromF

blank :: Picture a ()
blank = liftF $ Blank ()

draw :: Draw a () -> Picture a ()
draw d = liftF $ Draw d ()

withTransform :: PictureTransform -> Picture a () -> Picture a ()
withTransform t pic = liftF $ WithTransform t pic ()

move :: V2 Float -> Picture a () -> Picture a ()
move v = withTransform (PictureTransform (Transform v 1 0) 1 1)

scale :: V2 Float -> Picture a () -> Picture a ()
scale v = withTransform (PictureTransform (Transform 0 v 0) 1 1)

rotate :: Float -> Picture a () -> Picture a ()
rotate r = withTransform (PictureTransform (Transform 0 1 r) 1 1)

withAlpha :: Float -> Picture a () -> Picture a ()
withAlpha a = withTransform (PictureTransform mempty a 1)

withMult :: V4 Float -> Picture a () -> Picture a ()
withMult m = withTransform (PictureTransform mempty 1 m)

instance Monoid (Picture a ()) where
    mempty = blank
    mappend = (>>)

instance Hashable a => Hashable (Picture a ()) where
  hashWithSalt x = hashup x . fromF
    where hashup s (Pure ()) = s
          hashup s (Free (Blank n)) = s `hashup` n
          hashup s (Free (Draw d n)) = (s `hashWithSalt` d) `hashup` n
          hashup s (Free (WithTransform _ p n)) =
            (s `hashWithSalt` p) `hashup` n
--------------------------------------------------------------------------------
-- Semantic helpers
--------------------------------------------------------------------------------
withLetters :: Lettering a () -> Picture a ()
withLetters = draw . letters

withStroke :: [StrokeAttr] -> Lines Color2d () -> Picture a ()
withStroke = (draw .) . polylines

withColor :: Geometry Color2d () -> Picture a ()
withColor = draw . colored

withTexture :: a -> Geometry Tex2d () -> Picture a ()
withTexture = (draw .) . textured
--------------------------------------------------------------------------------
-- Using shapes to generate attribute geometry
--------------------------------------------------------------------------------
rectangle :: Unbox (f Float) => V2 Float -> V2 Float -> (V2 Float -> f Float)
          -> Geometry (V2 Float, f Float) ()
rectangle tl@(V2 tlx tly) br@(V2 brx bry) f =
  let tr = V2 brx tly
      bl = V2 tlx bry
  in fan (tl, f tl) (tr, f tr) (br, f br) $ V.singleton (bl, f bl)
--------------------------------------------------------------------------------
-- Measuring pictures
--------------------------------------------------------------------------------
boundingBox :: Free (PictureCmd a) ()
            -> (V2 Float, V2 Float)
boundingBox (Pure ()) = (0,0)
boundingBox (Free (Blank n)) = boundingBox n
boundingBox (Free (Draw d n)) =
  boundsBounds $ V.fromList [drawBounds d, boundingBox n]
boundingBox (Free (WithTransform pt p n)) =
  let (tl,br) = boundingBox $ fromF p
      t = ptAffine pt
  in boundsBounds $ V.fromList [(transform t tl, transform t br), boundingBox n]

-- | Returns the bounding box of the picture.
pictureBounds :: Picture a () -> BBox
pictureBounds = boundingBox . freePic

-- | Returns the size of the picture.
pictureSize :: Picture a () -> V2 Float
pictureSize p =
    let (tl,br) = pictureBounds p
    in br - tl

-- Returns the center point of the picture, based on its bounding box.
pictureCenter :: Picture a () -> V2 Float
pictureCenter p =
    let (tl,br) = pictureBounds p
    in tl + (br - tl) / 2

-- Returns the leftmost, uppermost point of the picture.
pictureOrigin :: Picture a () -> V2 Float
pictureOrigin = fst . pictureBounds

-- A list of rendering hashes contained within this picture.
pictureHashes :: Hashable a => Picture a () -> [Int]
pictureHashes p = hash p : (compile $ fromF p)
  where compile (Pure ()) = []
        compile (Free (Blank n)) = compile n
        compile (Free (Draw _ n)) = compile n
        compile (Free (WithTransform _ p1 n)) = pictureHashes p1 ++ compile n

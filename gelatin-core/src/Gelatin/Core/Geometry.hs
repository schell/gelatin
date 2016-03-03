{-# LANGUAGE GADTs #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module Gelatin.Core.Geometry where

import Control.Monad.Free
import Control.Monad.Free.Church
import Linear
import Gelatin.Core.Bounds
import Data.Hashable
--------------------------------------------------------------------------------
-- Monadically defining paths geometry
--------------------------------------------------------------------------------
data LineAddCmd a n where
  LineTo  :: a -> n -> LineAddCmd a n
  CurveTo :: a -> a -> n -> LineAddCmd a n
  deriving (Functor)

type LineAdd a = F (LineAddCmd a)

lineTo :: a -> LineAdd a ()
lineTo a = liftF $ LineTo a ()

curveTo :: a -> a -> LineAdd a ()
curveTo c a = liftF $ CurveTo c a ()

data LineSegment a = LineSegment a
                   | LineCurve a a

instance Hashable a => Hashable (LineSegment a) where
  hashWithSalt s (LineSegment a) = hashWithSalt s a
  hashWithSalt s (LineCurve a b) = hashWithSalt s [a,b]

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

freeG :: Geometry a () -> Free (Geom a) ()
freeG = fromF

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

instance Monoid (Geometry a ()) where
  mempty = none
  mappend = (>>)

geomBounds :: Geometry (V2 Float, a) () -> (V2 Float, V2 Float)
geomBounds = boundsBounds . geomBounds' . freeG

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
--------------------------------------------------------------------------------
-- Collecting attributes in lists.
--------------------------------------------------------------------------------
data SeqType = SeqStrip
             | SeqFan
             deriving (Show, Eq, Ord, Enum)

instance Hashable SeqType where
  hashWithSalt s = hashWithSalt s . fromEnum

data GPrims a = GPTris [(a,a,a)]
              | GPBezs [(a,a,a)]
              | GPLine (LineSegment a) [LineSegment a]
              | GPTriSeq SeqType a a a [a]

instance Hashable a => Hashable (GPrims a) where
  hashWithSalt s (GPTris ps) =
    s `hashWithSalt` ps
  hashWithSalt s (GPBezs ps) =
    s `hashWithSalt` ps
  hashWithSalt s (GPLine p ps) =
    s `hashWithSalt` p `hashWithSalt` ps
  hashWithSalt s (GPTriSeq t a b c ds) =
    s `hashWithSalt` t `hashWithSalt` (a:b:c:ds)

compilePrims :: Geometry a () -> [GPrims a]
compilePrims = collect Nothing . freeG

collect :: Maybe (GPrims a) -> Free (Geom a) () -> [GPrims a]
-- Ending scenario
collect Nothing (Pure ()) = []
collect Nothing (Free (GNone n)) = collect Nothing n
collect Nothing (Free (GTriangle a b c n)) =
  collect (Just $ GPTris [(a,b,c)]) n
collect Nothing (Free (GBez a b c n)) =
  collect (Just $ GPBezs [(a,b,c)]) n
collect Nothing (Free (GTriangleStrip a b c ds n)) =
  GPTriSeq SeqStrip a b c ds : collect Nothing n
collect Nothing (Free (GTriangleFan a b c ds n)) =
  GPTriSeq SeqFan a b c ds : collect Nothing n
collect Nothing (Free (GLine l n)) =
  let segs = map (uncurry GPLine) $ lineToSegments l
  in segs ++ collect Nothing n
collect (Just (GPTris ts)) (Free (GTriangle a b c n)) =
  collect (Just (GPTris $ ts ++ [(a,b,c)])) n
collect (Just (GPBezs ts)) (Free (GBez a b c n)) =
  collect (Just (GPBezs $ ts ++ [(a,b,c)])) n
collect (Just p) g =
  p : collect Nothing g

{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TupleSections #-}
module Gelatin.Picture (
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
    -- * Compilation helpers
    CompileData(..),
    emptyCompileData,
    freePic,
    withColoring,
    -- * Re-exports
    module Core,
    module Linear
) where

import Control.Monad.Free
import Control.Monad.Free.Church
import Gelatin.Core hiding (ellipse, arc)
import qualified Gelatin.Core as Core
import Linear hiding (rotate)
import Data.Hashable
--------------------------------------------------------------------------------
-- Picture
--------------------------------------------------------------------------------
-- | EDSL inspired by a language of pictures, from "Composing graphical user
-- interfaces in a purely functional language" Copyright 1998 by Sigbjørn Finne.
--
-- Implementation inspired by Rasterific.
data PictureCmd n where
    Blank         :: n -> PictureCmd n
    Polyline      :: [V2 Float] -> n -> PictureCmd n
    Rectangle     :: V2 Float -> n -> PictureCmd n
    Curve         :: V2 Float -> V2 Float -> V2 Float -> n -> PictureCmd n
    Arc           :: V2 Float -> Float -> Float -> n -> PictureCmd n
    Ellipse       :: V2 Float -> n -> PictureCmd n
    Circle        :: Float -> n -> PictureCmd n
    Letters       :: Int -> Float -> String -> n -> PictureCmd n
    WithStroke    :: [StrokeAttr] -> Picture () -> n -> PictureCmd n
    WithFill      :: Fill -> Picture () -> n -> PictureCmd n
    WithTransform :: Transform -> Picture () -> n -> PictureCmd n
    WithFont      :: FontData -> Picture () -> n -> PictureCmd n

instance Functor PictureCmd where
    fmap f (Blank n) = Blank $ f n
    fmap f (Polyline vs n) = Polyline vs $ f n
    fmap f (Rectangle v n) = Rectangle v $ f n
    fmap f (Curve a b c n) = Curve a b c $ f n
    fmap f (Arc v a b n) = Arc v a b $ f n
    fmap f (Ellipse v n) = Ellipse v $ f n
    fmap f (Circle r n) = Circle r $ f n
    fmap f (Letters dpi px s n) = Letters dpi px s $ f n
    fmap f (WithStroke as p n) = WithStroke as p $ f n
    fmap f (WithFill fill p n) = WithFill fill p $ f n
    fmap f (WithTransform t p n) = WithTransform t p $ f n
    fmap f (WithFont font p n) = WithFont font p $ f n

type Picture = F PictureCmd

freePic :: Picture () -> Free PictureCmd ()
freePic = fromF

instance Monoid (Picture ()) where
    mempty = blank
    mappend = (>>)

instance Transformable Transform (Picture ()) where
    transform = withTransform

data Coloring = StrokeColoring Stroke
              | FillColoring Fill
              -- | NoColoring

data PaintedPrimitives = Stroked Stroke Primitives
                       | Filled Fill Primitives
                       deriving (Show)
instance Hashable PaintedPrimitives where
    hashWithSalt s (Stroked st prim) =
        s `hashWithSalt` "Stroked" `hashWithSalt` st `hashWithSalt` prim 
    hashWithSalt s (Filled f prim) =
        let vs = concatMap unPath $ primToPaths prim
            fh = FillHash f vs
        in s `hashWithSalt` "Filled" `hashWithSalt` fh `hashWithSalt` prim

withColoring :: Coloring -> Primitives -> PaintedPrimitives 
withColoring (FillColoring f) = Filled f
withColoring (StrokeColoring s) = Stroked s

data CompileData = CompileData { cdFont :: Maybe FontData 
                               , cdColoring :: Maybe Coloring
                               , cdTransform :: Transform
                               }

emptyCompileData :: CompileData
emptyCompileData = CompileData Nothing Nothing mempty
--------------------------------------------------------------------------------
-- Creating Pictures
--------------------------------------------------------------------------------
blank :: Picture ()
blank = liftF $ Blank ()

line :: V2 Float -> Picture ()
line sz = liftF $ Polyline [sz] ()

polyline :: [V2 Float] -> Picture ()
polyline vs = liftF $ Polyline vs ()

rectangle :: V2 Float -> Picture ()
rectangle sz = liftF $ Rectangle sz ()

curve :: V2 Float -> V2 Float -> V2 Float -> Picture ()
curve a b c = liftF $ Curve a b c ()

arc :: V2 Float -> Float -> Float -> Picture ()
arc sz start stop = liftF $ Arc sz start stop ()

ellipse :: V2 Float -> Picture ()
ellipse sz = liftF $ Ellipse sz ()

circle :: Float -> Picture ()
circle r = liftF $ Circle r ()

letters :: Int -> Float -> String -> Picture ()
letters dpi px s = liftF $ Letters dpi px s ()

withStroke :: [StrokeAttr] -> Picture () -> Picture ()
withStroke attrs pic = liftF $ WithStroke attrs pic ()

withFill :: Fill -> Picture () -> Picture ()
withFill f pic = liftF $ WithFill f pic ()

withTransform :: Transform -> Picture () -> Picture ()
withTransform t pic = liftF $ WithTransform t pic ()

withFont :: FontData -> Picture () -> Picture ()
withFont f pic = liftF $ WithFont f pic ()

move :: V2 Float -> Picture () -> Picture ()
move v = withTransform (Transform v 1 0)

scale :: V2 Float -> Picture () -> Picture ()
scale v = withTransform (Transform 0 v 0)

rotate :: Float -> Picture () -> Picture ()
rotate r = withTransform (Transform 0 1 r)
--------------------------------------------------------------------------------
-- Measuring pictures
--------------------------------------------------------------------------------
boundingBox :: CompileData -> Free PictureCmd () -> (V2 Float, V2 Float)
boundingBox _ (Pure ()) = (0,0)
boundingBox cd (Free (Blank n)) = boundingBox cd n
boundingBox cd (Free (Polyline vs n)) =
    let t = cdTransform cd
        vs' = transform t vs
    in boundsBounds [polyBounds vs', boundingBox cd n]
boundingBox cd (Free (Rectangle v n)) =
    let t = cdTransform cd
        vs = box v
        vs' = transform t vs
    in boundsBounds [polyBounds vs', boundingBox cd n]
boundingBox cd (Free (Curve a b c n)) =
    let t = cdTransform cd
        vs = transform t $ subdivideAdaptive 100 0 $ bez3 a b c
    in boundsBounds [polyBounds vs, boundingBox cd n]
boundingBox cd (Free (Arc (V2 xr yr) start stop n)) =
    let t = cdTransform cd
        vs = transform t $ concatMap (subdivideAdaptive4 100 0) $
                 Core.arc xr yr start stop
    in boundsBounds [polyBounds vs, boundingBox cd n]
boundingBox cd (Free (Ellipse (V2 x y) n)) =
    let t = cdTransform cd
        vs = transform t $ bez4sToPath 100 0 $ Core.ellipse x y
    in boundsBounds [polyBounds vs, boundingBox cd n]
boundingBox cd (Free (Circle r n)) =
    let t = cdTransform cd
        vs = transform t $ bez4sToPath 100 0 $ Core.ellipse r r
    in boundsBounds [polyBounds vs, boundingBox cd n]
boundingBox cd (Free (Letters dpi px str n))
    | Just font <- cdFont cd =
        let t = cdTransform cd
            (V2 nx ny, V2 xx xy) = fontStringBoundingBox font dpi px str
            vs = transform t [V2 nx ny, V2 xx xy]
        in boundsBounds [polyBounds vs, boundingBox cd n]
    | otherwise = boundingBox cd n
boundingBox cd (Free (WithStroke _ p n)) =
    boundsBounds [boundingBox cd $ freePic p, boundingBox cd n]
boundingBox cd (Free (WithFill _ p n)) =
    boundsBounds [boundingBox cd $ freePic p, boundingBox cd n]
boundingBox cd (Free (WithTransform t p n)) =
    let t' = cdTransform cd
        cd' = cd{cdTransform = t `mappend` t'}
    in boundsBounds [boundingBox cd' $ freePic p, boundingBox cd n]
boundingBox cd (Free (WithFont font p n)) =
    let cd' = cd{cdFont = Just font}
    in boundsBounds [boundingBox cd' $ freePic p, boundingBox cd n]

-- Returns the bounding box of the picture.
pictureBounds :: Picture () -> BBox
pictureBounds = boundingBox emptyCompileData . freePic

-- Returns the size of the picuter.
pictureSize :: Picture () -> V2 Float
pictureSize p =
    let (tl,br) = pictureBounds p
    in br - tl

-- Returns the leftmost, uppermost point of the picture.
pictureOrigin :: Picture () -> V2 Float
pictureOrigin = fst . pictureBounds

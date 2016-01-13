{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TupleSections #-}
module Gelatin.Picture (
    -- * Types
    FontClass(..),
    PictureCmd(..),
    Picture,
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
    -- * Re-exports
    module Core,
    module Linear
) where

import Control.Monad.Free
import Control.Monad.Free.Church
import Gelatin.Core hiding (ellipse, arc)
import qualified Gelatin.Core as Core
import Linear hiding (rotate)

class FontClass f where
    stringBoundingBox :: f -> Int -> Float -> String -> (V2 Float, V2 Float)

--------------------------------------------------------------------------------
-- Picture
--------------------------------------------------------------------------------
-- | API inspired by a language of pictures, from "Composing graphical user
-- interfaces in a purely functional language" Copyright 1998 by Sigbjørn Finne.
--
-- Implementation inspired by Rasterific.
data PictureCmd f n where
    Blank         :: n -> PictureCmd f n
    Polyline      :: [V2 Float] -> n -> PictureCmd f n
    Rectangle     :: V2 Float -> n -> PictureCmd f n
    Curve         :: V2 Float -> V2 Float -> V2 Float -> n -> PictureCmd f n
    Arc           :: V2 Float -> Float -> Float -> n -> PictureCmd f n
    Ellipse       :: V2 Float -> n -> PictureCmd f n
    Circle        :: Float -> n -> PictureCmd f n
    Letters       :: Float -> String -> n -> PictureCmd f n
    WithStroke    :: [StrokeAttr] -> Picture f () -> n -> PictureCmd f n
    WithFill      :: Fill -> Picture f () -> n -> PictureCmd f n
    WithTransform :: Transform -> Picture f () -> n -> PictureCmd f n
    WithFont      :: FontClass f => f -> Picture f () -> n -> PictureCmd f n

instance Functor (PictureCmd f) where
    fmap f (Blank n) = Blank $ f n
    fmap f (Polyline vs n) = Polyline vs $ f n
    fmap f (Rectangle v n) = Rectangle v $ f n
    fmap f (Curve a b c n) = Curve a b c $ f n
    fmap f (Arc v a b n) = Arc v a b $ f n
    fmap f (Ellipse v n) = Ellipse v $ f n
    fmap f (Circle r n) = Circle r $ f n
    fmap f (Letters px s n) = Letters px s $ f n
    fmap f (WithStroke as p n) = WithStroke as p $ f n
    fmap f (WithFill fill p n) = WithFill fill p $ f n
    fmap f (WithTransform t p n) = WithTransform t p $ f n
    fmap f (WithFont font p n) = WithFont font p $ f n

type Picture f = F (PictureCmd f)

freePic :: Picture f () -> Free (PictureCmd f) ()
freePic = fromF

instance Monoid (Picture f ()) where
    mempty = blank
    mappend = (>>)

instance Transformable Transform (Picture f ()) where
    transform = withTransform

data CompileData f = CompileData { cdFont :: Maybe f
                                 , cdTransform :: Transform
                                 , cdFill :: Fill
                                 }

emptyCompileData :: CompileData f
emptyCompileData = CompileData Nothing mempty $ FillColor $ const 0
--------------------------------------------------------------------------------
-- Creating Pictures
--------------------------------------------------------------------------------
blank :: Picture f ()
blank = liftF $ Blank ()

line :: V2 Float -> Picture f ()
line sz = liftF $ Polyline [sz] ()

polyline :: [V2 Float] -> Picture f ()
polyline vs = liftF $ Polyline vs ()

rectangle :: V2 Float -> Picture f ()
rectangle sz = liftF $ Rectangle sz ()

curve :: V2 Float -> V2 Float -> V2 Float -> Picture f ()
curve a b c = liftF $ Curve a b c ()

arc :: V2 Float -> Float -> Float -> Picture f ()
arc sz start stop = liftF $ Arc sz start stop ()

ellipse :: V2 Float -> Picture f ()
ellipse sz = liftF $ Ellipse sz ()

circle :: Float -> Picture f ()
circle r = liftF $ Circle r ()

letters :: Float -> String -> Picture f ()
letters px s = liftF $ Letters px s ()

withStroke :: [StrokeAttr] -> Picture f () -> Picture f ()
withStroke attrs pic = liftF $ WithStroke attrs pic ()

withFill :: Fill -> Picture f () -> Picture f ()
withFill f pic = liftF $ WithFill f pic ()

withTransform :: Transform -> Picture f () -> Picture f ()
withTransform t pic = liftF $ WithTransform t pic ()

withFont :: FontClass f => f -> Picture f () -> Picture f ()
withFont f pic = liftF $ WithFont f pic ()

move :: V2 Float -> Picture f () -> Picture f ()
move v = withTransform (Transform v 1 0)

scale :: V2 Float -> Picture f () -> Picture f ()
scale v = withTransform (Transform 0 v 0)

rotate :: Float -> Picture f () -> Picture f ()
rotate r = withTransform (Transform 0 1 r)
--------------------------------------------------------------------------------
-- Measuring pictures
--------------------------------------------------------------------------------
boundingBox :: FontClass f
            => CompileData f -> Free (PictureCmd f) () -> (V2 Float, V2 Float)
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
boundingBox cd (Free (Letters px str n))
    | Just font <- cdFont cd =
        let t = cdTransform cd
            (V2 nx ny, V2 xx xy) = stringBoundingBox font 72 px str
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
pictureBounds :: FontClass f => Picture f () -> BBox
pictureBounds = boundingBox emptyCompileData . freePic

-- Returns the size of the picuter.
pictureSize :: FontClass f => Picture f () -> V2 Float
pictureSize p =
    let (tl,br) = pictureBounds p
    in br - tl

-- Returns the leftmost, uppermost point of the picture.
pictureOrigin :: FontClass f => Picture f () -> V2 Float
pictureOrigin = fst . pictureBounds

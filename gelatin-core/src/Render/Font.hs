{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE GADTs #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Render.Font (
    fontGeom,
    findFont,
    allFonts,
    withFont
) where

import Render.Types
import Render.Geometrical
import Linear hiding (trace)
import Prelude hiding (init)
import Graphics.Text.TrueType
import Data.List (sort)
import Control.Eff
import Control.Eff.Lift
import Control.Eff.State.Strict
import Control.Applicative
import Control.Concurrent.Async
import qualified Data.Vector.Unboxed as UV
import Debug.Trace

findFont :: Async FontCache -> FontDescriptor -> IO (Maybe FilePath)
findFont afCache desc = do
    -- Get the font cache from our async container
    mfCache <- poll afCache
    -- If it has loaded check if the font in question exists
    return $ do efCache <- mfCache
                case efCache of
                    Left _      -> Nothing
                    Right cache -> findFontInCache cache desc

allFonts :: (Member (State Resources) r,
             SetMember Lift (Lift IO) r)
         => Eff r (Maybe [FontDescriptor])
allFonts = do
    afcache <- rsrcFonts <$> get
    mfcache <- lift $ poll afcache
    return $ do efcache <- mfcache
                case efcache of
                    Left _ -> Nothing
                    Right fcache -> Just $ enumerateFonts fcache

withFont :: (Member (State Resources) r,
             SetMember Lift (Lift IO) r)
         => FontDescriptor -> (Font -> Eff r ()) -> Eff r ()
withFont desc f = do
    fs <- rsrcFonts <$> get
    mPath <- lift $ findFont fs desc
    case mPath of
        Nothing -> return ()
        Just path -> do ef <- lift $ loadFontFile path
                        case ef of
                            Left err   -> lift $ putStrLn err
                            Right font -> do -- TODO: store the font?
                                             f font

--------------------------------------------------------------------------------
-- Decomposition into triangles and beziers
--------------------------------------------------------------------------------
-- | Ephemeral types for creating polygons from font outlines.
-- Fonty gives us a [[Vector (Float, Float)]] for an entire string, which breaks down to
type Contours a = [Geometrical a] -- Beziers
type CharacterOutline a = [Contours a]
type StringOutline a = [CharacterOutline a]

-- | Merges poly a into poly b by "cutting" a and inserting b.
--cutMerge :: Poly -> Poly -> Poly
--cutMerge as bs = (take (ndx + 1) as) ++ bs ++ [head bs] ++ (drop ndx as)
--    where (ndx, _) = head $ sortBy (\a b -> snd a `compare` snd b) $
--                         zip [0..] $ map (`distance` (head bs)) as

fontGeom :: RealFrac a => Dpi -> [(Font, a, String)] -> [Geometrical a]
fontGeom dpi strs =
    let strs' = map (\(font, px, str) -> (font, pixelSizeInPointAtDpi (realToFrac px) dpi, str)) strs
        cs  = getStringCurveAtPoint dpi (0,0) strs'
        bs  = beziers cs
        bs' = concat $ concat bs
        ts  = concatMap (concatMap (concaveTriangles . onContourPoints)) bs
        gs  = bs' ++ ts
    in sort gs

fromFonty :: (UV.Unbox b1, Functor f1, Functor f) => ([V2 b1] -> b) -> f (f1 (UV.Vector (b1, b1))) -> f (f1 b)
fromFonty f = fmap $ fmap $ f . UV.toList . UV.map (uncurry V2)

beziers :: RealFrac a => [[UV.Vector (Float, Float)]] -> StringOutline a
beziers = fromFonty (toBeziers . (fmap (fmap realToFrac)))

-- | Turns a polygon into a list of triangles that can be rendered using the
-- Concave Polygon Stencil Test
-- @see http://www.glprogramming.com/red/chapter14.html#name13
concaveTriangles :: (Ord a, RealFrac a) => [V2 a] -> [Geometrical a]
concaveTriangles [] = []
concaveTriangles (a:as) = tris a as
    where tris p (p':p'':ps) = Triangle p p' p'' : tris p (p'':ps)
          tris _ _ = []

-- | Collects the points that lie directly on the contour of the font
-- outline.
onContourPoints :: [Geometrical a] -> [V2 a]
onContourPoints [] = []
onContourPoints ((Bezier LT a b c):bs) = [a,b,c] ++ onContourPoints bs
onContourPoints ((Bezier _ a _ c):bs) = [a,c] ++ onContourPoints bs
onContourPoints (_:bs) = onContourPoints bs


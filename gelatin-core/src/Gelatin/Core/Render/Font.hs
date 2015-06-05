{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE GADTs #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Gelatin.Core.Render.Font (
    fontGeom,
    findFont,
    allFonts,
    withFontAsync,
    withFont
) where

import Gelatin.Core.Render.Types
import Gelatin.Core.Render.Geometrical
import Prelude hiding (init)
import Control.Concurrent.Async
import qualified Data.Vector.Unboxed as UV

findFont :: Async FontCache -> FontDescriptor -> IO (Maybe FilePath)
findFont afCache desc = do
    -- Get the font cache from our async container
    mfCache <- poll afCache
    -- If it has loaded check if the font in question exists
    return $ do efCache <- mfCache
                case efCache of
                    Left _      -> Nothing
                    Right cache -> findFontInCache cache desc

allFonts :: Async FontCache -> IO (Maybe [FontDescriptor])
allFonts afcache = do
    mfcache <- poll afcache
    return $ do efcache <- mfcache
                case efcache of
                    Left _ -> Nothing
                    Right fcache -> Just $ enumerateFonts fcache

withFontAsync :: Async FontCache -> FontDescriptor -> (Font -> IO a) -> IO (Maybe a)
withFontAsync afcache desc f = do
    mPath <- findFont afcache desc
    case mPath of
        Nothing -> return Nothing
        Just path -> do ef <- loadFontFile path
                        case ef of
                            Left err   -> putStrLn err >> return Nothing
                            Right font -> Just `fmap` f font

withFont :: FontCache -> FontDescriptor -> (Font -> IO a) -> IO (Maybe a)
withFont cache desc f = do
    case findFontInCache cache desc of
        Nothing -> return Nothing
        Just fp -> do ef <- loadFontFile fp
                      case ef of
                          Left err   -> putStrLn err >> return Nothing
                          Right font -> Just `fmap` f font


--------------------------------------------------------------------------------
-- Decomposition into triangles and beziers
--------------------------------------------------------------------------------
-- | Ephemeral types for creating polygons from font outlines.
-- Fonty gives us a [[Vector (Float, Float)]] for an entire string, which breaks down to
type Contours = [Bezier (V2 Float)] -- Beziers
type CharacterOutline = [Contours]
type StringOutline = [CharacterOutline]

-- | Merges poly a into poly b by "cutting" a and inserting b.
--cutMerge :: Poly -> Poly -> Poly
--cutMerge as bs = (take (ndx + 1) as) ++ bs ++ [head bs] ++ (drop ndx as)
--    where (ndx, _) = head $ sortBy (\a b -> snd a `compare` snd b) $
--                         zip [0..] $ map (`distance` (head bs)) as

fontGeom :: Dpi -> FontString -> ([Bezier (V2 Float)], [Triangle (V2 Float)])
fontGeom dpi (FontString font px offset str) =
    let sz  = pixelSizeInPointAtDpi px dpi
        cs  = getStringCurveAtPoint dpi offset [(font, sz, str)]
        bs  = beziers cs
        ts  = concatMap (concatMap (concaveTriangles . onContourPoints)) bs
    in (concat $ concat bs,ts)

fromFonty :: (UV.Unbox b1, Functor f1, Functor f) => ([V2 b1] -> b) -> f (f1 (UV.Vector (b1, b1))) -> f (f1 b)
fromFonty f = fmap $ fmap $ f . UV.toList . UV.map (uncurry V2)

beziers :: [[UV.Vector (Float, Float)]] -> StringOutline
beziers = fromFonty (toBeziers . (fmap (fmap realToFrac)))

-- | Turns a polygon into a list of triangles that can be rendered using the
-- Concave Polygon Stencil Test
-- @see http://www.glprogramming.com/red/chapter14.html#name13
concaveTriangles :: [a] -> [Triangle a]
concaveTriangles [] = []
concaveTriangles (a:as) = tris a as
    where tris p (p':p'':ps) = Triangle p p' p'' : tris p (p'':ps)
          tris _ _ = []

-- | Collects the points that lie directly on the contour of the font
-- outline.
onContourPoints :: [Bezier a] -> [a]
onContourPoints [] = []
onContourPoints ((Bezier LT a b c):bs) = [a,b,c] ++ onContourPoints bs
onContourPoints ((Bezier _ a _ c):bs) = [a,c] ++ onContourPoints bs

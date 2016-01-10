{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE GADTs #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Gelatin.Core.Rendering.Font (
    compileFontCache,
    fontGeom,
    fontCurves,
    findFont,
    allFonts,
    usingFontAsync,
    usingFont,
    concaveTriangles
) where

import Gelatin.Core.Rendering.Types
import Gelatin.Core.Rendering.Bezier
import Prelude hiding (init)
import Control.Concurrent.Async
import Linear
import Graphics.Text.TrueType
import Data.List (intercalate)
import qualified Data.Vector.Unboxed as UV

compileFontCache :: IO (Async FontCache)
compileFontCache = async $ do
    putStrLn "Loading font cache."
    a <- buildCache
    putStrLn "Font cache loaded."
    return a

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

usingFontAsync :: Async FontCache -> FontDescriptor -> (Font -> IO a)
              -> IO (Maybe a)
usingFontAsync afcache desc f = do
    mPath <- findFont afcache desc
    case mPath of
        Nothing -> do putStrLn $ "could not find " ++ show desc
                      mfds <- allFonts afcache
                      case mfds of
                          Nothing -> putStrLn "cache is loading or err'd"
                          Just fds -> putStrLn $ intercalate "\n" $ map show fds
                      return Nothing
        Just path -> do ef <- loadFontFile path
                        case ef of
                            Left err   -> putStrLn err >> return Nothing
                            Right font -> Just `fmap` f font

usingFont :: FontCache -> FontDescriptor -> (Font -> IO a) -> IO (Maybe a)
usingFont cache desc f =
    case findFontInCache cache desc of
        Nothing -> return Nothing
        Just fp -> do ef <- loadFontFile fp
                      case ef of
                          Left err   -> putStrLn err >> return Nothing
                          Right font -> Just `fmap` f font
--------------------------------------------------------------------------------
-- Font decomposition into triangles and beziers
--------------------------------------------------------------------------------
-- | Ephemeral types for creating polygons from font outlines.
-- Fonty gives us a [[Vector (Float, Float)]] for an entire string, which breaks down to
type Contour = [Bezier (V2 Float)] -- Beziers
type CharacterOutline = [Contour]
type StringOutline = [CharacterOutline]

fontGeom :: Dpi -> FontString -> ([Bezier (V2 Float)], [Triangle (V2 Float)])
fontGeom dpi (FontString font px offset str) =
    let sz  = pixelSizeInPointAtDpi px dpi
        cs  = getStringCurveAtPoint dpi offset [(font, sz, str)]
        bs  = beziers cs
        ts  = concatMap (concatMap (concaveTriangles . onContourPoints)) bs
    in (concat $ concat bs,ts)

fontCurves :: Dpi -> Font -> Float -> String -> [[[QuadraticBezier (V2 Float)]]]
fontCurves dpi font px str =
    let sz = pixelSizeInPointAtDpi px dpi
        cs = getStringCurveAtPoint dpi (0,0) [(font, sz, str)]
        bs = beziers cs
    in fmap (fmap (fmap (\(Bezier _ a b c) -> bez3 a b c))) bs

fromFonty :: (UV.Unbox b1, Functor f1, Functor f) => ([V2 b1] -> b) -> f (f1 (UV.Vector (b1, b1))) -> f (f1 b)
fromFonty f = fmap $ fmap $ f . UV.toList . UV.map (uncurry V2)

beziers :: [[UV.Vector (Float, Float)]] -> StringOutline
beziers = fromFonty (toBeziers . fmap (fmap realToFrac))

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
onContourPoints (Bezier LT a b c :bs) = [a,b,c] ++ onContourPoints bs
onContourPoints (Bezier _ a _ c :bs) = [a,c] ++ onContourPoints bs

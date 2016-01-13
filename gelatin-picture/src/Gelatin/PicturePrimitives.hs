module Gelatin.PicturePrimitives where

import Control.Arrow (second)
import Control.Monad.Free
import Control.Monad.Free.Church
import Control.Monad.Reader
import Gelatin.Core as Core
import Gelatin.Picture

--------------------------------------------------------------------------------
-- Compiling pictures
--------------------------------------------------------------------------------
-- Returns the next picture command.
nextPicCmd :: Free (PictureCmd f) () -> Free (PictureCmd f) ()
nextPicCmd (Pure ()) = return ()
nextPicCmd (Free (Blank n)) = n
nextPicCmd (Free (Polyline _ n)) = n
nextPicCmd (Free (Rectangle _ n)) = n
nextPicCmd (Free (Curve _ _ _ n)) = n
nextPicCmd (Free (Arc _ _ _ n)) = n
nextPicCmd (Free (Ellipse _ n)) = n
nextPicCmd (Free (Circle _ n)) = n
nextPicCmd (Free (Letters _ _ n)) = n
nextPicCmd (Free (WithStroke _ _ n)) = n
nextPicCmd (Free (WithFill _ _ n)) = n
nextPicCmd (Free (WithTransform _ _ n)) = n
nextPicCmd (Free (WithFont _ _ n)) = n

continuePaths :: Free (PictureCmd f) () -> Picture f ()
              -> Reader (CompileData f) [(Transform, PathPrimitives f)]
continuePaths n p = do
    prims <- compilePaths n
    paths <- compilePaths $ fromF p
    return $ paths ++ prims

-- Compiles a list of paths from a free picture command
compilePaths :: Free (PictureCmd f) () -> Reader (CompileData f) [(Transform, PathPrimitives f)]
compilePaths (Pure ()) = return []
compilePaths (Free (Blank n)) = ([] ++) <$> compilePaths n
compilePaths (Free (Polyline vs n)) = do
    t     <- asks cdTransform
    prims <- compilePaths n
    return $ (t, Paths [Path vs]) : prims
compilePaths (Free (Rectangle sz n)) = do
    t     <- asks cdTransform
    prims <- compilePaths n
    let ps  = sizeToPaths (Size sz)
        ps' = map (\p -> (t,Paths [p])) ps
    return $ ps' ++ prims
compilePaths (Free (Curve a b c n)) = do
    t     <- asks cdTransform
    prims <- compilePaths n
    return $ (t, Paths [Path $ subdivideAdaptive 100 0 $ bez3 a b c]) : prims
compilePaths (Free (Arc (V2 xr yr) start stop n)) = do
    t     <- asks cdTransform
    prims <- compilePaths n
    let erc = concatMap (subdivideAdaptive4 100 0) $ Core.arc xr yr start stop
    return $ (t, Paths [Path erc]) : prims
compilePaths (Free (Ellipse (V2 x y) n)) = do
    t     <- asks cdTransform
    prims <- compilePaths n
    return $ (t, Paths [Path $ bez4sToPath 100 0 $ Core.ellipse x y]) : prims
compilePaths (Free (Circle rad n)) = do
    t     <- asks cdTransform
    prims <- compilePaths n
    return $ (t, Paths [Path $ bez4sToPath 100 0 $ Core.ellipse rad rad]) : prims
compilePaths (Free (Letters px s n)) = do
    t     <- asks cdTransform
    mf    <- asks cdFont
    prims <- compilePaths n
    let ps = case mf of
                 Just f -> [(t, PathText f px s)]
                 Nothing -> []
    return $ ps ++ prims
compilePaths (Free (WithStroke _ p n)) = continuePaths n p
compilePaths (Free (WithFill _ p n)) = continuePaths n p
compilePaths (Free (WithTransform t p n)) = do
    t'  <- asks cdTransform
    ps <- local (\cd -> cd{cdTransform = t' `mappend` t}) (compilePaths $ fromF p)
    (ps ++) <$> compilePaths n
compilePaths (Free (WithFont font p n)) = do
    ps <- local (\cd -> cd{cdFont = Just font}) (compilePaths $ fromF p)
    (ps ++) <$> compilePaths n

tfrmFillAndNext :: Free (PictureCmd f) ()
                -> Reader (CompileData f) (Transform,Fill,[(Transform, FillPrimitives f)])
tfrmFillAndNext n = do
    t <- asks cdTransform
    f <- asks cdFill
    prims <- compileFillPrims n
    return (t,f,prims)

-- Compiles a list of fills from a free picture command.
compileFillPrims :: Free (PictureCmd f) ()
                 -> Reader (CompileData f) [(Transform, FillPrimitives f)]
compileFillPrims (Pure ()) = return []
compileFillPrims (Free (Blank n)) = ([] ++) <$> compileFillPrims n
compileFillPrims (Free (Polyline vs n)) = do
    (t,f,prims) <- tfrmFillAndNext n
    return $ (t, FillPaths f [Path vs]) : prims
compileFillPrims (Free (Rectangle sz n)) = do
    (t,f,prims) <- tfrmFillAndNext n
    return $ (t, FillTriangles f $ sizeToTris $ Size sz) : prims
compileFillPrims (Free (Curve a b c n)) = do
    (t,f,prims) <- tfrmFillAndNext n
    return $ (t, FillBeziers f [bez a b c]) : prims
compileFillPrims (Free (Arc (V2 xr yr) start stop n)) = do
    (t,f,prims) <- tfrmFillAndNext n
    let erc = concatMap (subdivideAdaptive4 100 0) $ Core.arc xr yr start stop
    return $ (t, FillPaths f [Path erc]) : prims
compileFillPrims (Free (Ellipse (V2 x y) n)) = do
    (t,f,prims) <- tfrmFillAndNext n
    return $ (t, FillPaths f [Path $ bez4sToPath 100 0 $ Core.ellipse x y]) : prims
compileFillPrims (Free (Circle rad n)) = do
    (t,f,prims) <- tfrmFillAndNext n
    return $ (t, FillPaths f [Path $ bez4sToPath 100 0 $ Core.ellipse rad rad]) : prims
compileFillPrims (Free (Letters px s n)) = do
    (t,f,prims) <- tfrmFillAndNext n
    mf          <- asks cdFont
    let ps = case mf of
                 Just fnt -> [(t, FillText f fnt px s)]
                 Nothing  -> []
    return $ ps ++ prims
compileFillPrims (Free (WithStroke _ p n)) =
    (++) <$> compileFillPrims (fromF p) <*> compileFillPrims n
compileFillPrims (Free (WithFill f p n)) = do
    fs <- local (\cd -> cd{cdFill = f}) $ compileFillPrims $ fromF p
    (fs ++) <$> compileFillPrims n
compileFillPrims (Free (WithTransform t p n)) = do
    t'    <- asks cdTransform
    prims <- local (\cd -> cd{cdTransform = t' `mappend` t}) (compileFillPrims $ fromF p)
    (prims ++) <$> compileFillPrims n
compileFillPrims (Free (WithFont font p n)) = do
    ps <- local (\cd -> cd{cdFont = Just font}) $ compileFillPrims $ fromF p
    (ps ++) <$> compileFillPrims n

-- | Compile the picture commands into a list of renderable primitives.
compilePrimitives :: Free (PictureCmd f) ()
                  -> Reader (CompileData f) [(Transform, R2Primitives f)]
compilePrimitives (Pure ()) = return []
compilePrimitives (Free (WithTransform t p n)) = do
    t' <- asks cdTransform
    let f = compilePrimitives $ fromF p
    prims <- local (\cd -> cd{cdTransform = t `mappend` t'}) f
    (prims ++) <$> compilePrimitives n
compilePrimitives (Free (WithStroke attrs p n)) = do
    paths <- compilePaths $ fromF p
    let stroke = foldl strokeAttr Nothing attrs
        prims = case stroke of
                 Nothing -> []
                 Just s  -> map (second (R2PathPrimitives . Stroked s)) paths
    (prims ++) <$> compilePrimitives n
compilePrimitives (Free (WithFill fill p n)) = do
    prims <- local (\cd -> cd{cdFill = fill}) $ compileFillPrims $ fromF p
    let prims' = map (second R2FillPrimitives) prims
    (prims' ++) <$> compilePrimitives n
compilePrimitives (Free (WithFont font p n)) = do
    prims <- local (\cd -> cd{cdFont = Just font}) $ compilePrimitives $ fromF p
    (prims ++) <$> compilePrimitives n
compilePrimitives p = compilePrimitives $ nextPicCmd p

pictureToR2Primitives :: Picture f () -> [(Transform, R2Primitives f)]
pictureToR2Primitives pic =
    runReader (compilePrimitives $ freePic pic) emptyCompileData
--------------------------------------------------------------------------------
-- Showing a picture as a string
--------------------------------------------------------------------------------
compileLine :: Show f => Free (PictureCmd f) () -> String -> Reader Int String
compileLine n s = do
    t <- ask
    let s' = replicate t ' ' ++ s ++ "\n"
    (s' ++) <$> compileString n

-- | Compile the picture commands into a string.
compileString :: Show f => Free (PictureCmd f) () -> Reader Int String
compileString (Pure ()) = return ""
compileString (Free (Blank n)) = compileLine n "blank"
compileString (Free (Polyline vs n)) = compileLine n $ "polyline " ++ show vs
compileString (Free (Rectangle sz n)) = compileLine n $ "rectangle " ++ show sz
compileString (Free (Curve a b c n)) = compileLine n $ unwords $ "curve" : map show [a,b,c]
compileString (Free (Arc v a b n)) = compileLine n $ unwords $ "arc" : show v : map show [a,b]
compileString (Free (Ellipse sz n)) = compileLine n $ "ellipse " ++ show sz
compileString (Free (Circle u n)) = compileLine n $ "circle " ++ show u
compileString (Free (Letters px s n)) =
    compileLine n $ unwords ["letters", show px, show s]
compileString (Free (WithStroke attrs p n)) = do
    s <- local (+1) $ compileString $ fromF p
    compileLine n $ "withStroke " ++ show attrs ++ "\n" ++ s
compileString (Free (WithFill f p n)) = do
    s <- local (+1) $ compileString $ fromF p
    compileLine n $ "withFill " ++ show f ++ "\n" ++ s
compileString (Free (WithTransform t p n)) = do
    s <- local (+1) $ compileString $ fromF p
    compileLine n $ "withTransform " ++ show t ++ "\n" ++ s
compileString (Free (WithFont f p n)) = do
    s <- local (+1) $ compileString $ fromF p
    compileLine n $ "withFont " ++ show f ++ "\n" ++ s

showPic :: Show f => Picture f () -> String
showPic pic = runReader (compileString $ fromF pic) 0

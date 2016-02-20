module Gelatin.PicturePrimitives where

import Control.Arrow (second)
import Control.Monad.Free
import Control.Monad.Free.Church
import Control.Monad.Reader
import Control.Applicative
import Gelatin.Core as Core
import Gelatin.Picture
import Data.Hashable
import GHC.Generics

--------------------------------------------------------------------------------
-- Compiling pictures
--------------------------------------------------------------------------------
-- Returns the next picture command.
nextPicCmd :: Free PictureCmd () -> Free PictureCmd ()
nextPicCmd (Pure ()) = return ()
nextPicCmd (Free (Blank n)) = n
nextPicCmd (Free (Polyline _ n)) = n
nextPicCmd (Free (Rectangle _ n)) = n
nextPicCmd (Free (Curve _ _ _ n)) = n
nextPicCmd (Free (Arc _ _ _ n)) = n
nextPicCmd (Free (Ellipse _ n)) = n
nextPicCmd (Free (Circle _ n)) = n
nextPicCmd (Free (Letters _ _ _ n)) = n
nextPicCmd (Free (WithStroke _ _ n)) = n
nextPicCmd (Free (WithFill _ _ n)) = n
nextPicCmd (Free (WithTransform _ _ n)) = n
nextPicCmd (Free (WithFont _ _ n)) = n

-- | Compile only the nested picture within the command given coloring,
-- font and transform.
compileCurrentWith :: Free PictureCmd () ->  Maybe Coloring -> Maybe FontData 
                   -> Transform -> [(Transform, Painted Primitives)]
-- First list all the cases where primitives cannot be mapped.
compileCurrentWith (Pure ()) _ _ _ = []
compileCurrentWith (Free (Blank _)) _ _ _ = []
compileCurrentWith _ Nothing _ _ = []
compileCurrentWith (Free Letters{}) _ Nothing _ = []

-- Handle coloring everything that gets broken down into paths.
compileCurrentWith (Free (Polyline vs _)) 
                   (Just clr) _ t = 
    [(t, withColoring clr $ PathPrims [Path vs])]
compileCurrentWith (Free (Curve a b c n)) 
                   (Just clr) _ t = 
    let path = Path $ subdivideAdaptive 100 0 $ bez3 a b c
    in [(t, withColoring clr $ PathPrims [path])]
compileCurrentWith (Free (Arc (V2 xr yr) start stop _)) 
                   (Just clr) _ t = 
    let erc = concatMap (subdivideAdaptive4 100 0) $ Core.arc xr yr start stop 
    in [(t, withColoring clr $ PathPrims [Path erc])]
compileCurrentWith (Free (Ellipse (V2 x y) _)) 
                   (Just clr) _ t = 
    let path = Path $ bez4sToPath 100 0 $ Core.ellipse x y
    in [(t, withColoring clr $ PathPrims [path])]
compileCurrentWith (Free (Circle r _)) 
                   (Just clr) _ t = 
    let path = Path $ bez4sToPath 100 0 $ Core.ellipse r r
    in [(t, withColoring clr $ PathPrims [path])]
compileCurrentWith (Free (Letters dpi px str _)) 
                   (Just clr) (Just fd) t = 
    [(t, withColoring clr $ TextPrims fd dpi px str)]

-- Now do all the rest of the work for paths.
compileCurrentWith (Free (Rectangle sz _)) 
                   (Just (StrokeColoring s)) _ t = 
    [(t, Stroked s $ PathPrims $ sizeToPaths $ Size sz)]

-- Now do all the burte force work for fills.
compileCurrentWith (Free (Polyline vs _)) 
                   (Just (FillColoring s)) _ t = 
    [(t, Filled s $ PathPrims [Path vs])]
compileCurrentWith (Free (Rectangle sz _)) 
                   (Just (FillColoring s)) _ t = 
    [(t, Filled s $ TrianglePrims $ sizeToTris $ Size sz)]
compileCurrentWith (Free (Curve a b c _)) 
                   (Just (FillColoring s)) _ t = 
    [(t, Filled s $ BezierPrims [bez a b c])]

-- | Compile the picture commands into a list of renderable primitives.
compilePrimitives :: Free PictureCmd ()
                  -> Reader CompileData [(Transform, Painted Primitives)]
compilePrimitives (Pure ()) = return []
compilePrimitives (Free (WithTransform t p n)) = do
    t' <- asks cdTransform
    let f = compilePrimitives $ fromF p
    prims <- local (\cd -> cd{cdTransform = t `mappend` t'}) f
    (prims ++) <$> compilePrimitives n
compilePrimitives (Free (WithStroke attrs p n)) = do
    otherPrims <- compilePrimitives n 
    mColor     <- asks cdColoring
    let mStroke = StrokeColoring <$> foldl strokeAttr Nothing attrs
        mPaint  = mColor <|> mStroke 
    prims <- local (\cd -> cd{cdColoring = mPaint}) $ 
                   compilePrimitives $ fromF p
    return $ prims ++ otherPrims
compilePrimitives (Free (WithFill fill p n)) = do
    otherPrims <- compilePrimitives n 
    mColor     <- asks cdColoring
    t          <- asks cdTransform
    let mPaint = mColor <|> Just (FillColoring $ transform t fill) 
    prims <- local (\cd -> cd{cdColoring = mPaint}) $ 
                   compilePrimitives $ fromF p
    return $ prims ++ otherPrims
compilePrimitives (Free (WithFont font p n)) = do
    mFont <- asks cdFont
    prims <- local (\cd -> cd{cdFont = mFont <|> Just font}) $ 
                   compilePrimitives $ fromF p
    (prims ++) <$> compilePrimitives n
compilePrimitives cmd = do
    prims <- compileCurrentWith cmd <$> asks cdColoring <*> asks cdFont <*> asks cdTransform
    (prims ++) <$> compilePrimitives (nextPicCmd cmd)

toPaintedPrimitives :: Picture () -> [(Transform, Painted Primitives)]
toPaintedPrimitives pic =
    runReader (compilePrimitives $ freePic pic) emptyCompileData
--------------------------------------------------------------------------------
-- Showing a picture as a string
--------------------------------------------------------------------------------
compileLine :: Free PictureCmd () -> String -> Reader Int String
compileLine n s = do
    t <- ask
    let s' = replicate t ' ' ++ s ++ "\n"
    (s' ++) <$> compileString n

-- | Compile the picture commands into a string.
compileString :: Free PictureCmd () -> Reader Int String
compileString (Pure ()) = return ""
compileString (Free (Blank n)) = compileLine n "blank"
compileString (Free (Polyline vs n)) = compileLine n $ "polyline " ++ show vs
compileString (Free (Rectangle sz n)) = compileLine n $ "rectangle " ++ show sz
compileString (Free (Curve a b c n)) = compileLine n $ unwords $ "curve" : map show [a,b,c]
compileString (Free (Arc v a b n)) = compileLine n $ unwords $ "arc" : show v : map show [a,b]
compileString (Free (Ellipse sz n)) = compileLine n $ "ellipse " ++ show sz
compileString (Free (Circle u n)) = compileLine n $ "circle " ++ show u
compileString (Free (Letters dpi px s n)) =
    compileLine n $ unwords ["letters", show dpi, show px, show s]
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

showPic :: Picture () -> String
showPic pic = runReader (compileString $ fromF pic) 0

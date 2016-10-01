module Gelatin.Core.Font where

import Gelatin.Core.Bezier
import Gelatin.Core.Utils
import Linear
import qualified Data.Vector as B
import Data.Vector.Unboxed (Vector)

type CalcFontCurves = Int -> Float -> String -> [[Vector (QuadraticBezier (V2 Float))]]
type CalcFontGeom   = Int -> Float -> String -> (Vector (Bezier (V2 Float)), [Vector (V2 Float)])

data FontData = FontData { fontStringCurves :: CalcFontCurves
                         , fontStringGeom :: CalcFontGeom
                         , fontHash :: Int -> Int
                         , fontShow :: String
                         }

stringCurvesToPaths :: FontData -> Int -> Float -> String -> [Vector (V2 Float)]
stringCurvesToPaths fd dpi px str =
    let qs = fontStringCurves fd dpi px str
        sub = subdivideAdaptive 100 0
        mkPath :: Vector (QuadraticBezier (V2 Float)) -> Vector (V2 Float)
        mkPath = cleanSeqDupes
                   . B.convert
                   . B.concatMap (B.convert . sub)
                   . B.convert
    in concatMap (fmap mkPath) qs

instance Show FontData where
    show = fontShow

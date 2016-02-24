module Gelatin.Core.Font where

import Gelatin.Core.Bezier
import Gelatin.Core.Triangle
import Gelatin.Core.Path
import Data.Hashable
import Linear

type CalcFontCurves = Int -> Float -> String -> [[[QuadraticBezier (V2 Float)]]] 
type CalcFontGeom   = Int -> Float -> String -> ([Bezier (V2 Float)], [Triangle (V2 Float)]) 

data FontData = FontData { fontStringCurves :: CalcFontCurves
                         , fontStringGeom :: CalcFontGeom
                         , fontHash :: Int -> Int
                         , fontShow :: String
                         }

stringCurvesToPaths :: FontData -> Int -> Float -> String -> [Path (V2 Float)]
stringCurvesToPaths fd dpi px str = 
    let qs = fontStringCurves fd dpi px str
        sub = subdivideAdaptive 100 0
        mkPath = Path . cleanSeqDupes . concatMap sub
    in concatMap (fmap mkPath) qs

instance Show FontData where
    show = fontShow

instance Hashable FontData where
    hashWithSalt s f = fontHash f s

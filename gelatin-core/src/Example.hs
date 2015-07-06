module Main where

import System.Environment
import Gelatin.Core.Render
import Graphics.UI.GLFW
import Examples.PolylineTest
import Examples.PolylineWinding
import Examples.Masking

examples :: [(String, Window -> GeomRenderSource -> BezRenderSource -> IO ())]
examples = [("polylineTest", polylineTest)
           ,("polylineWinding", polylineWinding)
           ,("masking", masking)
           ]

main :: IO ()
main = do
    name:_ <- getArgs
    True   <- initGelatin
    win    <- newWindow 800 600 "Syndeca Mapper" Nothing Nothing
    grs    <- loadGeomRenderSource
    brs    <- loadBezRenderSource

    let Just example = lookup name examples

    example win grs brs

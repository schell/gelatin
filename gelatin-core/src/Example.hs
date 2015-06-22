module Main where

import System.Environment
import Gelatin.Core.Render
import Graphics.UI.GLFW
import Examples.PolylineTest

examples :: [(String, Window -> GeomRenderSource -> BezRenderSource -> IO ())]
examples = [("polylineTest", polylineTest)]

main :: IO ()
main = do
    name:_ <- getArgs
    True   <- initGelatin
    win    <- newWindow 800 600 "Syndeca Mapper" Nothing Nothing
    grs    <- loadGeomRenderSource
    brs    <- loadBezRenderSource

    let Just example = lookup name examples

    example win grs brs

module Main where

import System.Environment
import Gelatin.Core.Rendering
import Graphics.UI.GLFW
import Examples.ExpandedPolylines
import Examples.PolylineTest
import Examples.PolylineWinding
import Examples.AdaptiveBezierSubdivision
import Examples.Masking
import Examples.Text
import Examples.ClipTexture

examples :: [(String, Window -> SumShader -> IO ())]
examples = [("expandedPolylines", expandedPolylines)
           ,("polylineTest", polylineTest)
           ,("polylineWinding", polylineWinding)
           ,("adaptiveBezierSubdivision", adaptiveBezierSubdivision)
           ,("masking", masking)
           ,("text", text)
           ,("clipTexture", clippingTexture)
           ]

main :: IO ()
main = do
    name:_ <- getArgs
    True   <- initGelatin
    win    <- newWindow 800 600 "Syndeca Mapper" Nothing Nothing
    shaders<- loadShaders

    let Just example = lookup name examples

    example win shaders

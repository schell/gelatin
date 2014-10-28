{-# LANGUAGE GADTs #-}
module Gelatin.Shaders.Simple2dColorShader (
    simple2dColorShader
) where

import Graphics.Rendering.OpenGL hiding (Color, color)
import Graphics.GLUtil
import qualified Data.ByteString.Char8 as BS

colorShader2dVertSrc :: BS.ByteString
colorShader2dVertSrc = BS.pack $ unlines
    [ "uniform mat4 projection;"
    , "uniform mat4 modelview;"
    , "attribute vec2 position;"
    , "attribute vec4 color;"
    , "varying vec4 fcolor;"
    , "void main(void) {"
    , " fcolor = color;"
    , " gl_Position = projection * modelview * vec4(position, 0.0, 1.0);"
    , "}"
    ]

colorShader2dFragSrc :: BS.ByteString
colorShader2dFragSrc = BS.pack $ unlines
    [ "varying vec4 fcolor;"
    , "void main(void) {"
    , " gl_FragColor = fcolor;"
    , "}"
    ]

simple2dColorShader :: IO ShaderProgram
simple2dColorShader =
    loadShaderProgramBS [ (VertexShader, colorShader2dVertSrc)
                        , (FragmentShader, colorShader2dFragSrc)
                        ]

{-# LANGUAGE GADTs #-}
module Gelatin.Shaders.SimpleColorShader where

import Graphics.Rendering.OpenGL hiding (Color, color)
import Graphics.GLUtil
import qualified Data.ByteString.Char8 as BS

colorShaderVertSrc :: BS.ByteString
colorShaderVertSrc = BS.pack $ unlines
    [ "uniform mat4 projection;"
    , "uniform mat4 modelview;"
    , "attribute vec3 position;"
    , "attribute vec4 color;"
    , "varying vec4 fcolor;"
    , "void main(void) {"
    , " fcolor = color;"
    , " gl_Position = projection * modelview * vec4(position, 1.0);"
    , "}"
    ]

colorShaderFragSrc :: BS.ByteString
colorShaderFragSrc = BS.pack $ unlines
    [ "varying vec4 fcolor;"
    , "void main(void) {"
    , " gl_FragColor = fcolor;"
    , "}"
    ]

simpleColorShader :: IO ShaderProgram
simpleColorShader =
    loadShaderProgramBS [ (VertexShader, colorShaderVertSrc)
                        , (FragmentShader, colorShaderFragSrc)
                        ]

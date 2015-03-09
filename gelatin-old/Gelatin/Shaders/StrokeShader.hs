module Gelatin.Shaders.StrokeShader ( strokeShader ) where

import           Graphics.Rendering.OpenGL hiding (Color, color)
import           Graphics.GLUtil
import qualified Data.ByteString.Char8 as BS

shaderVertSrc :: BS.ByteString
shaderVertSrc = BS.pack $ unlines
    [ "uniform mat4 projection;"
    , "uniform mat4 modelview;"
    , "attribute vec2 position;"
    , "attribute vec4 color;"
    , "void main(void) {"
    , " fcolor = color;"
    , " gl_Position = projection * modelview * vec4(position, 0.0, 1.0);"
    , "}"
    ]

shaderFragSrc :: BS.ByteString
shaderFragSrc = BS.pack $ unlines
    [ "uniform float radius;"
    , "varying vec4 fcolor;"
    , "void main(void) {"
    , " gl_FragColor = texture2D(sampler, ftexcoord);"
    , "}"
    ]

simpleTextureShader :: IO ShaderProgram
simpleTextureShader =
    loadShaderProgramBS [ (VertexShader, shaderVertSrc)
                        , (FragmentShader, shaderFragSrc)
                        ]

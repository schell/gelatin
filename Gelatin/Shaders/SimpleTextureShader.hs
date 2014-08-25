module Gelatin.Shaders.SimpleTextureShader ( simpleTextureShader ) where

import           Graphics.Rendering.OpenGL hiding (Color, color)
import           Graphics.GLUtil
import qualified Data.ByteString.Char8 as BS

shaderVertSrc :: BS.ByteString
shaderVertSrc = BS.pack $ unlines
    [ "uniform mat4 projection;"
    , "uniform mat4 modelview;"
    , "attribute vec3 position;"
    , "attribute vec2 texcoord;"
    , "varying vec2 ftexcoord;"
    , "void main(void) {"
    , " ftexcoord = texcoord;"
    , " gl_Position = projection * modelview * vec4(position, 1.0);"
    , "}"
    ]

shaderFragSrc :: BS.ByteString
shaderFragSrc = BS.pack $ unlines
    [ "uniform sampler2D sampler;"
    , "varying vec2 ftexcoord;"
    , "void main(void) {"
    , " gl_FragColor = texture2D(sampler, ftexcoord);"
    , "}"
    ]

simpleTextureShader :: IO ShaderProgram
simpleTextureShader =
    loadShaderProgramBS [ (VertexShader, shaderVertSrc)
                        , (FragmentShader, shaderFragSrc)
                        ]

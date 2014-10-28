module Gelatin.Shaders.Simple2dTextureShader (
    simple2dTextureShader
) where

import           Graphics.Rendering.OpenGL hiding (Color, color)
import           Graphics.GLUtil
import qualified Data.ByteString.Char8 as BS

shader2dVertSrc :: BS.ByteString
shader2dVertSrc = BS.pack $ unlines
    [ "uniform mat4 projection;"
    , "uniform mat4 modelview;"
    , "attribute vec2 position;"
    , "attribute vec2 texcoord;"
    , "varying vec2 ftexcoord;"
    , "void main(void) {"
    , " ftexcoord = texcoord;"
    , " gl_Position = projection * modelview * vec4(position, 0.0, 1.0);"
    , "}"
    ]

shader2dFragSrc :: BS.ByteString
shader2dFragSrc = BS.pack $ unlines
    [ "uniform sampler2D sampler;"
    , "varying vec2 ftexcoord;"
    , "void main(void) {"
    , " gl_FragColor = texture2D(sampler, ftexcoord);"
    , "}"
    ]

simple2dTextureShader :: IO ShaderProgram
simple2dTextureShader =
    loadShaderProgramBS [ (VertexShader, shader2dVertSrc)
                        , (FragmentShader, shader2dFragSrc)
                        ]

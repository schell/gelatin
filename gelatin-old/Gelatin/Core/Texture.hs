module Gelatin.Core.Texture where

import Graphics.Rendering.OpenGL
import Foreign.Ptr
import Control.Monad
import System.Exit

-- | Renders the IO () action into a framebuffer texture.
renderToTexture :: Size -> PixelInternalFormat -> IO () -> IO TextureObject
renderToTexture (Size w h) fmt ioF = do
    fb <- genObjectName
    bindFramebuffer Framebuffer $= fb

    tex <- genObjectName
    textureBinding Texture2D $= Just tex
    textureFilter Texture2D $= ((Linear', Nothing), Linear')
    texImage2D
        Texture2D
        NoProxy
        0
        fmt
        (TextureSize2D w h)
        0
        (PixelData RGBA UnsignedByte nullPtr)
    framebufferTexture2D Framebuffer (ColorAttachment 0) Texture2D tex 0

    status <- get $ framebufferStatus Framebuffer
    unless (status == Complete) $ do
        print status
        exitFailure

    clearColor $= Color4 0 0 0 0
    clear [ColorBuffer]
    viewport $= (Position 0 0, Size w h)

    ioF
    bindFramebuffer Framebuffer $= defaultFramebufferObject
    deleteObjectName fb
    return tex

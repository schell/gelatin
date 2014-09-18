module Main where

import Linear hiding (rotate)
import Gelatin
import Gelatin.Transform
import Graphics.Rendering.OpenGL hiding (position, color, drawElements,
                                         translate, rotate, perspective,
                                         ortho, clearDepth, drawArrays)
import qualified Graphics.Rendering.OpenGL as GL
import Control.Monad
import Control.Concurrent
import Data.IORef
import Graphics.GLUtil hiding (setUniform)
import Graphics.VinylGL
import Data.Vinyl
import System.Exit

cubePoints :: [V3 GLfloat]
cubePoints =
    [ V3 (-0.5) ( 0.5) ( 0.5)
    , V3 ( 0.5) ( 0.5) ( 0.5)
    , V3 (-0.5) (-0.5) ( 0.5)
    , V3 ( 0.5) (-0.5) ( 0.5)

    , V3 (-0.5) ( 0.5) (-0.5)
    , V3 ( 0.5) ( 0.5) (-0.5)
    , V3 (-0.5) (-0.5) (-0.5)
    , V3 ( 0.5) (-0.5) (-0.5)
    ]

cubeColors :: [V4 GLfloat]
cubeColors = map (up . fmap (+0.5)) cubePoints
    where up (V3 x y z) = V4 x y z 1

cubeIndices :: [Word32]
cubeIndices = [ 0, 2, 3 -- front
              , 0, 1, 3
              , 4, 0, 1 -- top
              , 4, 5, 1
              , 4, 6, 7 -- bock
              , 4, 5, 7
              , 6, 2, 3 -- bottom
              , 6, 7, 3
              , 0, 2, 6 -- left
              , 0, 4, 6
              , 1, 3, 7 -- right
              , 1, 5, 7
              ]

renderCube :: ShaderProgram -> IO ()
renderCube shader = do
    let pj = perspective (pi/4) 1 0.1 10
        mv = mkM44 $ do translate $ V3 0 0 (-5)
                        rotate (pi/8) $ V3 1 0 0
    vbo <- bufferVertices (comp position cubePoints .+ comp color cubeColors)
    ebo <- bufferIndices cubeIndices
    clearColor $= Color4 0 0 0 1
    depthFunc $= Just Less
    clear [ColorBuffer, DepthBuffer]
    currentProgram $= (Just $ program shader)
    setUniforms shader (projection =: pj)
    setUniforms shader (modelview =: mv)
    bindVertices vbo
    enableVertices' shader vbo
    bindBuffer ElementArrayBuffer $= Just ebo
    drawIndexedTris $ floor $ (fromIntegral $ length cubeIndices) / 3
    bindBuffer ElementArrayBuffer $= Nothing
    depthFunc $= Nothing

renderTex :: MVar (Maybe TextureObject) -> ShaderProgram -> IO ()
renderTex mvar shader = do
    let pj = ortho 0 600 0 600 0 1
        mv = eye4
        t = rectangle (V2 0 0) (V2 1 1)
        tr = map embed (rectangle (V2 0 0) (V2 600 600) :: [V2 GLfloat])
        cs = take 6 $ cycle [red, green, blue]
    mTex <- takeMVar mvar
    tex  <- case mTex of
        Just tex -> return tex
        Nothing  -> do putStrLn "Loading texture."
                       texture Texture2D $= Enabled
                       (Right tex) <- loadTextureSrc $ Relative "img/quantum-foam.jpg"
                       textureFilter Texture2D $= ((Nearest, Nothing), Nearest)
                       textureWrapMode Texture2D S $= (Repeated, ClampToEdge)
                       textureWrapMode Texture2D T $= (Repeated, ClampToEdge)
                       return tex
    putMVar mvar $ Just tex
    vbo <- bufferVertices (comp position tr .+ comp texcoord t)
    clearColor $= Color4 0 0 0 1
    depthFunc $= Nothing
    clear [ColorBuffer, DepthBuffer]
    withTextures Texture2D [tex] $ do
    --activeTexture $= TextureUnit 0
    --textureBinding Texture2D $= Just tex
        currentProgram $= (Just $ program shader)
        setUniforms shader (projection =: pj)
        setUniforms shader (modelview =: mv)
        setUniforms shader (sampler =: 0)
        bindVertices vbo
        enableVertices' shader vbo
        GL.drawArrays Triangles 0 6
        deleteVertices vbo

colorCube :: ShaderProgram -> Rendering ()
colorCube shader = do
    usingDepthFunc Less $ usingShader shader $ do
        setUniform projection pj
        setUniform modelview mv
        withVertices (comp position cubePoints .+ comp color cubeColors) $
            drawIndexedTriangles cubeIndices 12
    where pj = perspective (pi/4) 1 0.1 10
          mv = mkM44 $ do translate $ V3 0 0 (-5)
                          rotate (pi/8) $ V3 1 0 0

sceneRendering :: ShaderProgram -> ShaderProgram -> Rendering ()
sceneRendering colorShader textureShader = do
    -- Clear the stage.
    clearDepth
    clearColorWith (black :: V4 GLfloat)
    -- Draw a background using a texture.
    usingTexture Texture2D (Relative "img/quantum-foam.jpg") params $
        usingShader textureShader $ do
            setUniform projection pj
            setUniform modelview eye4
            setUniform sampler 0
            withVertices (comp position tr .+ comp texcoord t) $
                drawArrays Triangles 6
    -- Draw a gradient box in the upper left corner.
    usingShader colorShader $ do
        setUniform projection pj
        setUniform modelview eye4
        withVertices (comp position gr .+ comp color cs) $
            drawArrays Triangles 6
    colorCube colorShader
    where pj = ortho 0 600 0 600 0 1
          tr = map embed (rectangle (V2 0 0) (V2 600 600) :: [V2 GLfloat])
          t  = rectangle (V2 0 0) (V2 1 1)
          gr = map embed $ (rectangle (V2 0 0) (V2 100 100) :: [V2 GLfloat])
          cs = take 6 $ cycle [red, green, blue]
          params = do setFilter (Nearest, Nothing) Nearest
                      setWrapMode S Repeated Clamp
                      setWrapMode T Repeated Clamp

main :: IO ()
main = do
    wref  <- initWindow (V2 0 0) (V2 600 600) "Gelatin"
    scs   <- simpleColorShader
    sts   <- simpleTextureShader
    scene <- compileRendering $ sceneRendering scs sts

    forever $ do
        pollEvents
        (_, window) <- readIORef wref
        writeIORef wref ([], window)
        makeContextCurrent $ Just window
        render scene
        printError
        swapBuffers window
        shouldClose <- windowShouldClose window
        when shouldClose exitSuccess

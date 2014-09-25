**gelatin** is a nice OpenGL drawing API inspired by Rasterific and diagrams. It wraps the Haskell OpenGL bindings with a pure declarative interface. It's very experimental and very much in flux, but there's always room for jello.

*gelatin* includes an API for monadically constructing 2d drawings which is influenced by Rasterific. It aims to make drawing shapes with colors and textures pretty easy:

```haskell
boxes :: Rendering2d ()
boxes = do
    -- Draw a gradient box using red, green and blue.
    -- The color definitions come from Gelatin.Color, which includes all
    -- the familiar named colors from CSS :)
    let box  = rectangle (V2 0 0) (V2 100 100)
        grad = take 6 $ cycle [red, green, blue]
    gradient box grad
    -- Translate (100,100) and draw a yellow box
    withPosition (V2 100 100) $ do
        fill box yellow
        -- Translate again, scale down and draw some more.
        withPosition (V2 100 (-50)) $ withScale (V2 0.5 0.5) $ do
            fill box red
            -- Scale down again and rotate and draw again.
            withScale (V2 0.5 0.5) $ withRotation (pi/4) $ do
                fill box blue
```

    image for boxes


However, the main focus of the library is wrapping the OpenGL API and simplifying the process of talking to shaders and sending geometry to the GPU:

    haskell code for drawing a cube and a textured cube
```haskell
cube :: ShaderProgram -> M44 Double -> VertexBufferCommand () -> Rendering ()
cube shader mv addColor = do
    usingDepthFunc Less $ usingShader shader $ do
        setProjection pj
        setModelview mv
        withVertices cubeVertices $ drawIndexedTriangles cubeIndices 12
    where pj = perspective (pi/4) 1 0.1 10
          cubeVertices = do addComponent $ position cubePoints
                            addColor

cubes :: ShaderProgram -> ShaderProgram -> Rendering ()
cubes colorShader textureShader = do
    -- Clear the stage.
    clearDepth
    clearColorWith black
    let leftmv = mkM44 $ do translate $ V3 (-1) 0 (-5)
                            rotate (pi/8) $ V3 1 0 0
        ritemv = mkM44 $ do translate $ V3 1 0 (-5)
                            rotate (pi/8) $ V3 1 0 0
        params = do setFilter (Nearest, Nothing) Nearest
                    setWrapMode S Repeated Clamp
                    setWrapMode T Repeated Clamp
    -- Draw the cube with colors, translated left and slightly rotated.
    cube colorShader leftmv $ addComponent $ color cubeColors
    -- Draw the cube with a texture, translated right and slightly rotated.
    usingTexture Texture2D (Relative "img/quantum-foam.jpg") params $
        cube textureShader ritemv $ addComponent $ texcoord cubeTexs
```
    
    image for cubes

For examples on how to set up your shaders and communicate with them see 
`Gelatin.Shaders.Core`. Currently you have to define your vertex components by 
hand, but gelatin provides some convenient functions to help you. A 
TemplateHaskell solution is in the works. 

The gelatin API is incomplete and if it's missing anything you need please feel free to submit a feature request or a pull request :)

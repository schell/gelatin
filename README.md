**gelatin** is a nice Haskell drawing API inspired by Rasterific and diagrams. 
It provides a pure interface to rendering in OpenGL. 
It's very experimental and very much in flux, but there's always room for jello.

Included is an API for monadically constructing 2d drawings which is 
heavily influenced by Rasterific. It aims to make drawing shapes with colors 
and textures pretty easy:

```haskell
circles :: Rendering2d ()
circles = withSize 300 300 $ do
    clear
    fill (solid white) $ [rectangle (V2 0 0) 300 300]
    let solidWhite = solid white         -- using css style colors 
        canary = solid $ hex 0xFFFF00FF  -- using RGBA hex colors
        c1 = circle (V2 50 50) 10
        c2 = circle (V2 150 150) 75
    fill (solid canary) [c1, c2]
    withPosition (V2 5 5) $
        outline (solid $ hex 0xFF00FFFF) $ concatMap triangulate [c1, c2]
```

This snippet demonstrates some of the underlying polygon triangulation which
displays the following:

<img src="https://raw.githubusercontent.com/schell/gelatin/master/img/circles.png" />

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

<img src="https://raw.githubusercontent.com/schell/gelatin/master/img/boxes.png" />

However, the main focus of the library is wrapping OpenGL calls and simplifying
the process of talking to shaders and sending geometry to the GPU:

```haskell
cube :: ShaderProgram -> M44 Double -> VertexBufferCommand () -> Rendering ()
cube shader mv addColor = do
    usingDepthFunc Less $ usingShader shader $ do
        setProjection pj
        setModelview mv
        withVertices cubeVertices $ drawIndexedTriangles cubeIndices 12
    where pj = perspective (pi/4) 1 0.1 10 :: M44 Float
          cubeVertices = do addComponent $ position3 cubePoints
                            addColor

cubes :: ShaderProgram -> ShaderProgram -> Rendering ()
cubes colorShader textureShader = do
    -- Clear the stage.
    clearDepth
    clearColorWith black
                 -- using familiar imperative transformations
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
    
<img src="https://raw.githubusercontent.com/schell/gelatin/master/img/cubes.png" />

For examples on how to set up your shaders and communicate with them see 
`Gelatin.Shaders.Core`. Currently you have to define your vertex components by 
hand, but gelatin provides some convenient functions to help you. A 
TemplateHaskell solution is in the works. 

The gelatin API is incomplete and if it's missing anything you need please feel 
free to submit a feature request or a pull request :)

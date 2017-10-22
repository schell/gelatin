gelatin
=======
[![Build Status](https://travis-ci.org/schell/gelatin.svg?branch=master)](https://travis-ci.org/schell/gelatin)

This is a mega-repo for a real-time graphics renderer using Haskell.

<img src="https://www.dropbox.com/s/foveaypptgbemrh/Screenshot%202016-09-20%2009.49.55.png?dl=1" />

features
--------
[x] drawing with triangles, triangle strips, triangle fans, beziers (fill below curve), inverted beziers (fill above curve) and thick, feathered stroked lines [gelatin (core)](https://github.com/schell/gelatin/tree/master/gelatin)

[x] drawing with 2d color or texture using gpu resources [gelatin-gl](https://github.com/schell/gelatin/tree/master/gelatin-gl)

[x] post compilation affine transformation, color multiply, red channel replacement (for font textures) [gelatin-gl](https://github.com/schell/gelatin/tree/master/gelatin-gl)

[x] freetype2 text with character atlas and word map [gelatin-freetype2](https://github.com/schell/gelatin/tree/master/gelatin-freetype2)

[x] text geometry [gelatin-fruity](https://github.com/schell/gelatin/tree/master/gelatin-fruity)

[x] sdl2 backend [gelatin-sdl2](https://github.com/schell/gelatin/tree/master/gelatin-sdl2)

[-] glfw backend (exists, but is broken an stalled) [gelatin-glfw](https://github.com/schell/gelatin/tree/master/gelatin-glfw)

[x] webgl via ghcjs backend

[ ] drawing with 3d color or texture using gpu resources

examples
--------
For examples please see [gelatin-example](https://github.com/schell/gelatin/tree/master/gelatin-example) and [odin](https://github.com/schell/odin)

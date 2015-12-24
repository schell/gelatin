gelatin
=======
`gelatin` provides an easy way to render 2D graphics in Haskell using OpenGL 
3.3.

features
--------
* colored or textured triangles
* alpha masking
* antialiased truetype font rendering
* colored or textured, [resolution independent, antialiased quadratic and cubic beziers][1]
* [adaptive quadratic bezier subdivision][2]
* colored [antialiased thick polylines with multiple end caps][3] 


future work
-----------
* lots of cleanup (API refinement, etc)
* dashed lines
* textured lines
* 3d

[1]:https://www.dropbox.com/s/gnxb9bhkww2cgr5/Resolution-Independent-Curve-Rendering-using-Programmable-Graphics-Hardware.pdf?dl=0
[2]:http://www.antigrain.com/research/adaptive_bezier/index.html
[3]:https://www.dropbox.com/s/0ahawis7qg1mnf8/Shader-Based-Antialiased-Dashed-Stroked-Polylines.pdf?dl=0

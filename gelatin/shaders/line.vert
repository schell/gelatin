// For more infor drawing expanded polylines please see
// https://www.mapbox.com/blog/drawing-antialiased-lines/
// http://mattdesl.svbtle.com/drawing-lines-is-hard
// https://github.com/mattdesl/three-line-2d

#version 330 core

layout(location = 0) in vec2 position;
layout(location = 1) in vec4 color;
layout(location = 4) in vec2 normal;
layout(location = 5) in float miter;

uniform mat4 projection;
uniform mat4 modelview;
uniform float thickness;

// the fragment color
out vec4 fcolor;
// the distance from center
out float fedge;

void main() {
    fcolor = color;
    fedge = sign(miter);
    vec4 delta = vec4(normal * miter, 0.0, 0.0);
    vec4 pos = modelview * vec4(position, 0.0, 1.0);
    gl_Position = projection * (pos + delta);
}

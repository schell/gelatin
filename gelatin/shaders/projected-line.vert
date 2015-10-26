#version 330 core

layout(location = 0) in vec2 position;
layout(location = 1) in vec4 color;
layout(location = 2) in vec2 uv;
layout(location = 7) in vec2 next;
layout(location = 8) in vec2 previous;

uniform mat4 projection;
uniform mat4 modelview;
uniform float thickness;
uniform float feather;
uniform float sumlength;

out vec4 fcolor;
out vec2 fuv;

void main() {
    fcolor = color;
    fuv = uv;
    vec2 a = previous;
    vec2 b = position;
    vec2 c = next;
    vec2 ab = normalize(b - a);
    vec2 bc = normalize(c - b);
    vec2 tangent = normalize(ab + bc);
    vec2 extrusion = vec2(-tangent.y, tangent.x);
    float direction = sign(uv.y);
    float len = thickness;
    //if (miter) {
        // find the length of the miter line
        vec2 perpab = vec2(-ab.y,ab.x);
        len = len / dot(extrusion, perpab);
    //}
    vec2 delta = extrusion * len * direction;
    gl_Position = projection * modelview * vec4(position + delta, 0.0, 1.0);
}

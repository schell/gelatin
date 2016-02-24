#version 330 core

in vec2 position;
in vec4 color;
in vec2 bezuv;
in vec2 next;
in vec2 previous;
in vec2 uv;

uniform mat4 projection;
uniform mat4 modelview;
uniform float thickness;
uniform float feather;
uniform float sumlength;

out vec4 fcolor;
out vec2 fuv;
out vec2 fbezuv;

void main() {
    fcolor = color;
    fuv = uv;
    fbezuv = bezuv;

    vec2 a = previous;
    vec2 b = position;
    vec2 c = next;
    vec2 ab = normalize(b - a);
    vec2 bc = normalize(c - b);
    vec2 tangent = normalize(ab + bc);
    vec2 extrusion = vec2(-tangent.y, tangent.x);
    float direction = sign(bezuv.y);
    float len = thickness;

    // find the length of the miter line
    vec2 perpab = vec2(-ab.y,ab.x);
    len = len / dot(extrusion, perpab);

    vec2 delta = extrusion * len * direction;
    gl_Position = projection * modelview * vec4(position + delta, 0.0, 1.0);
}

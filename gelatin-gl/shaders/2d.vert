#version 330 core
in vec2 position;
in vec4 color;
in vec2 uv;

uniform mat4 projection;
uniform mat4 modelview;
uniform bool hasUV;
uniform sampler2D sampler;

out vec4 fcolor;
out vec2 fuv;

void main() {
    fcolor = color;
    fuv = uv;
    gl_Position = projection * modelview * vec4(position.xy, 0.0, 1.0);
}

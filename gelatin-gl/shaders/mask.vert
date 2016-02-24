#version 330 core
in vec2 position;
in vec2 uv;

uniform mat4 projection;
uniform mat4 modelview;
uniform sampler2D mainTex;
uniform sampler2D maskTex;

out vec2 fuv;

void main() {
    fuv = uv;
    gl_Position = projection * modelview * vec4(position.xy, 0.0, 1.0);
}

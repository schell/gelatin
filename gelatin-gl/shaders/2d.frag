#version 330 core
in vec4 fcolor;
in vec2 fuv;
out vec4 fragColor;

uniform bool hasUV;
uniform sampler2D sampler;
uniform vec4 mult;


void main() {
    if (hasUV) {
        fragColor = texture(sampler, fuv.st) * mult;
    } else {
        fragColor = fcolor * mult;
    }
}

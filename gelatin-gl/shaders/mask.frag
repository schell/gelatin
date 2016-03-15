#version 330 core
uniform sampler2D mainTex;
uniform sampler2D maskTex;
uniform vec4 mult;

in vec2 fuv;

out vec4 fragColor;

void main() {
    vec4 color = texture(mainTex, fuv.st);
    vec4 mask  = texture(maskTex, fuv.st);
    fragColor = vec4(color.rgb, color.a * mask.a) * mult;
}

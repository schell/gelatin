#version 330 core

in vec4 fcolor;
in float fedge;
out vec4 fragColor;

uniform float opacity;
uniform float thickness;

void main() {
    float edge = 1.0 - abs(fedge);
    float mult = 2;
    float alpha = smoothstep(0.0, 2/thickness, edge);
    if (thickness <= 1) {
        alpha = thickness * edge;
    }
    fragColor = vec4(fcolor.rgb, fcolor.a * opacity * alpha);
}

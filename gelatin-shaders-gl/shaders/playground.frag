#version 330 core

uniform float utime;
uniform vec2 uresolution;
uniform vec2 umouse;

out vec4 fragColor;

void main() {
  fragColor = vec4(abs(sin(utime)), 0, 0, 1);
}

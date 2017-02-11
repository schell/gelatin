#version 330 core

in vec2 position;

uniform mat4 uprojection;

void main() {
  gl_Position = uprojection * vec4(position.xy, 0, 1);
}

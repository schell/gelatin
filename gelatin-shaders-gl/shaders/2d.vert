#version 330 core

in vec2 position;
in vec4 color;

out vec4 fcolor;

uniform mat4 projection;

void main () {
  fcolor = color;
  gl_Position = projection * vec4(position.xy, 0, 1);
}

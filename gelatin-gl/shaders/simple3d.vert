#version 330 core

in vec3 position;
in vec4 color;
in vec2 uv;
in vec3 normal;

out vec4 fcolor;
out vec2 fuv;
out vec3 fnormal;

uniform mat4 projection;
uniform mat4 modelview;

void main () {
  fcolor  = color;
  fuv     = uv;
  fnormal = fnormal;

  gl_Position = projection * modelview * vec4(position.xyz, 1.0);;
}

#version 330 core
layout(location = 0) in vec2 position;
layout(location = 1) in vec4 color;

uniform mat4 projection;
uniform mat4 modelview;

out vec4 fcolor;

void main() {
    fcolor = color;
    gl_Position = projection * modelview * vec4(position.xy, 0.0, 1.0);
}

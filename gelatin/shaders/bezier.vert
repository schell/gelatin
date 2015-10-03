// Loop-Blinn curve rendering

#version 330 core

layout(location = 0) in vec2 position;
layout(location = 1) in vec4 color;
layout(location = 2) in vec2 uv;
layout(location = 3) in vec3 bez;

uniform mat4 projection;
uniform mat4 modelview;

out vec3 fbez;
out vec4 fcolor;
out vec2 fuv;

void main() {
    fbez = bez;
    fuv = uv;
    fcolor = color;
    gl_Position = projection * modelview * vec4(position.xy, 0.0, 1.0);
}

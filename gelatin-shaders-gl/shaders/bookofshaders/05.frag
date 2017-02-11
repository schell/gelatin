#version 330 core

uniform vec2 uresolution;
uniform vec2 umouse;
uniform float utime;

out vec4 fragColor;

float plot (vec2 st, float pct) {
  return smoothstep(pct - 0.02, pct, st.y) - smoothstep(pct, pct + 0.02, st.y);
}

void main () {
  vec2 st = gl_FragCoord.xy/uresolution;
  float y = st.x;
  vec3 color = vec3(y);
  float pct = plot(st, y);
  color = (1.0 - pct) * color + pct * vec3(0.0, 1.0, 0.0);

  fragColor = vec4 (color, 1.0);
}

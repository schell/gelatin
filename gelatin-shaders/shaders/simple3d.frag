#version 330 core

in vec4 fcolor;
in vec2 fuv;
in vec3 fnormal;

out vec4 fragColor;

uniform bool hasUV;
uniform float alpha;
uniform vec4 mult;
uniform sampler2D sampler;
uniform vec4 replaceColor;
uniform bool shouldColorReplace;

// Colors a fragment based solely on either an input color or a texture.
vec4 coord_fragment(bool isUV,
                    sampler2D s,
                    vec4 clr,
                    vec2 uvs) {
  if (isUV) {
    return texture(s, uvs.st);
  } else {
    return clr;
  }
}

// Runs a color op on the fragment.
vec4 color_op_fragment(vec4 c, float a, vec4 m) {
  vec4 c1 = vec4(0);
  if (shouldColorReplace) {
    // Use a replacement color multiplied by the current red channel value.
    c1 = vec4(replaceColor.r, replaceColor.g, replaceColor.b, replaceColor.a * c.r) * m;
  } else {
    c1 = c * m;
  }
  return vec4(c1.rgb, c1.a * a);
}

void main() {
  vec4 out_color = vec4(0);
  out_color = coord_fragment(hasUV, sampler, fcolor, fuv);
  fragColor = color_op_fragment(out_color, alpha, mult);
}

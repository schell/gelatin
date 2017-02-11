#version 330 core

in vec2 position;
in vec4 color;
in vec2 uv;
in vec3 bez;
in vec2 bezuv;
in vec2 next;
in vec2 previous;

out vec4 fcolor;
out vec2 fuv;
out vec3 fbez;
out vec2 fbezuv;

uniform int primitive;
uniform mat4 projection;
uniform mat4 modelview;
uniform bool hasUV;
uniform sampler2D sampler;
uniform sampler2D mainTex;
uniform sampler2D maskTex;
uniform float thickness;

// Primitive types
const int PrimTri  = 0;
const int PrimBez  = 1;
const int PrimLine = 2;
const int PrimMask = 4;

// Projects a polyline segment into screen coordinates.
vec4 project_line(mat4 pj,
                  mat4 mv,
                  float thick,
                  vec2 pos,
                  vec2 bzuv,
                  vec2 nxt,
                  vec2 prev) {
  vec2 a = prev;
  vec2 b = pos;
  vec2 c = nxt;
  vec2 ab = normalize(b - a);
  vec2 bc = normalize(c - b);
  vec2 tangent = normalize(ab + bc);
  vec2 extrusion = vec2(-tangent.y, tangent.x);
  float direction = sign(bzuv.y);
  float len = thick;

  // find the length of the miter line
  vec2 perpab = vec2(-ab.y,ab.x);
  len = len / dot(extrusion, perpab);

  vec2 delta = extrusion * len * direction;
  return pj * mv * vec4(pos + delta, 0.0, 1.0);
}

// Projects a plain point into screen coords.
// Used for alpha masking and "regular" uv mapping and coloring.
vec4 project_position(mat4 pj, mat4 mv, vec2 pos) {
    return pj * mv * vec4(pos.xy, 0.0, 1.0);
}

void main () {
  // Figure out what kind of projection to use.
  vec4 out_position = vec4(0);

  switch (primitive) {
    case PrimTri:
    case PrimBez:
    case PrimMask:
      out_position = project_position(projection, modelview, position);
      break;

    case PrimLine: {
      out_position = project_line(projection, modelview, thickness, position,
                                  bezuv, next, previous);
      break;
    }
    default:
    out_position = project_position(projection, modelview, position);
  }

  fcolor = color;
  fuv = uv;
  fbez = bez;
  fbezuv = bezuv;

  gl_Position = out_position;
}

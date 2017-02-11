#extension GL_OES_standard_derivatives : enable

precision highp float;
precision highp int;

varying vec4 fcolor;
varying vec2 fuv;
varying vec3 fbez;
varying vec2 fbezuv;

uniform int primitive;
uniform bool hasUV;
uniform sampler2D sampler;
uniform sampler2D mainTex;
uniform sampler2D maskTex;
uniform float thickness;
uniform float feather;
uniform float sumlength;
uniform vec2 cap;
uniform float alpha;
uniform vec4 mult;

uniform vec4 replaceColor;
uniform bool shouldColorReplace;

// Primitive types
const int PrimTri  = 0;
const int PrimBez  = 1;
const int PrimLine = 2;
const int PrimMask = 4;

// Types for rendering line caps
const float CapNone   = 0.0;
const float CapButt   = 1.0;
const float CapSquare = 2.0;
const float CapRound  = 3.0;
const float CapTriOut = 4.0;
const float CapTriIn  = 5.0;

// Colors a fragment based solely on either an input color or a texture.
vec4 coord_fragment(bool isUV,
                    sampler2D s,
                    vec4 clr,
                    vec2 uvs) {
  if (isUV) {
    return texture2D(s, uvs.st);
  } else {
    return clr;
  }
}

// Colors a fragment using Loop-Blinn curve rendering.
vec4 bez_fragment(bool isUV,
                  sampler2D s,
                  vec3 bz,
                  vec4 clr,
                  vec2 uvs) {
    vec2 p = bz.xy;
    // when cw is true, winding is clockwise and we're drawing outside the
    // curve.
    bool cw = bool(bz.z);
    // gradients
    vec2 px = dFdx(p);
    vec2 py = dFdy(p);
    // chain rule
    float fx = (2.0*p.x)*px.x - px.y;
    float fy = (2.0*p.x)*py.x - py.y;
    // signed distance
    float sd = (p.x*p.x - p.y) / sqrt(fx*fx + fy*fy);
    // linear alpha
    float alpha = 0.5 - sd;
    alpha = cw ? 1.0 - alpha : alpha;
    // find the resulting fragment color
    float a = 0.0;

    if (alpha > 1.0) {
      a = 1.0;
    } else if (alpha < 0.0) {
      discard;
    } else {
      // we are right on the boundary, interpolate the color intensity.
      a = alpha;
    }

    vec4 color = vec4(0);
    if (isUV) {
      color = texture2D(s, uvs.st);
    } else {
      color = clr;
    }
    return vec4(color.rgb, color.a * a);
}

// Renders a polyline cap fragment.
float capd(float type, float u, float v, float t ) {
    // None
    if ( type == CapNone) discard;
    // Round
    else if (type == CapRound) return sqrt(u*u+v*v);
    // Triangle out
    else if (type == CapTriOut) return (u+abs(v));
    // Triangle in
    else if (type == CapTriIn) return max(abs(v),(t+u-abs(v)));
    // Square
    else if (type == CapSquare) return max(u,v);
    // Butt
    else if (type == CapButt) return max(u+t,v);
    discard;
}

vec4 line_fragment(float thick,
                   float fthr,
                   float slen,
                   vec2 cp,
                   bool isUV,
                   sampler2D s,
                   vec4 clr,
                   vec2 bzuv,
                   vec2 uvs) {
    float u = bzuv.x;
    float v = bzuv.y;
    float l = slen;
    float dx = abs(min(u, u - l));
    float dy = abs(v);
    float d = dy;

    vec4 color = vec4(0);
    if (isUV) {
        color = texture2D(s, uvs.st);
    } else {
        color = clr;
    }

    float t = thick/2.0 - fthr;

    if (u < 0.0) {
        // fragment is in the start cap
        d = capd(cp.x, abs(u), dy, t);
    } else if (u > slen) {
        // fragment is in the end cap
        d = capd(cp.y, u - l, dy, t);
    }

    d -= t;
    if (d < 0.0) {
        return color;
    } else {
        d /= fthr;
        return vec4(color.rgb, exp(-d*d)*color.a);
    }
}

// Colors a fragment using two textures, one the input texture and one as the
// alpha masking texture.
vec4 mask_fragment(sampler2D main,
                   sampler2D mask,
                   vec2 uvs) {
    vec4 color = texture2D(main, uvs.st);
    vec4 msk  = texture2D(mask, uvs.st);
    return vec4(color.rgb, color.a * msk.a);
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
  if (primitive == PrimTri) {
      out_color = coord_fragment(hasUV, sampler, fcolor, fuv);
  }
  if (primitive == PrimBez) {
      out_color = bez_fragment(hasUV, sampler, fbez, fcolor, fuv);

  }
  if (primitive == PrimLine) {
      out_color = line_fragment(thickness, feather, sumlength, cap, hasUV,
                                sampler, fcolor, fbezuv, fuv);
  }
  if (primitive == PrimMask) {
      out_color = mask_fragment(mainTex, maskTex, fuv);
  }

  gl_FragColor = color_op_fragment(out_color, alpha, mult);
}

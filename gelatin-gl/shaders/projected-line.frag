// Inspired by
// Nicolas Rougier. Shader-Based Antialiased Dashed Stroked Polylines.
// Journal of Computer Graphics Techniques, Williams College, 2013, 2 (2), pp.91-107.
// <http://jcgt.org/published/0002/02/08/>. <hal-00907326>
#version 330 core

in vec4 fcolor;
in vec2 fbezuv;
in vec2 fuv;
out vec4 fragColor;

// The thickness of the line
uniform float thickness;
// The length of the antialiasing/feather effect
uniform float feather;
// The total length of the line, i.e. the sum of all segment lengths
uniform float sumlength;
// The start and end cap type.
uniform vec2 cap;
// Whether or not to use a texture for color.
uniform bool hasUV;
// Our texture sampler.
uniform sampler2D sampler;

float capNone   = 0;
float capButt   = 1;
float capSquare = 2;
float capRound  = 3;
float capTriOut = 4;
float capTriIn  = 5;

float capd(float type, float u, float v, float t ) {
    // None
    if ( type == capNone) discard;
    // Round
    else if (type == capRound) return sqrt(u*u+v*v);
    // Triangle out
    else if (type == capTriOut) return (u+abs(v));
    // Triangle in
    else if (type == capTriIn) return max(abs(v),(t+u-abs(v)));
    // Square
    else if (type == capSquare) return max(u,v);
    // Butt
    else if (type == capButt) return max(u+t,v);
    discard;
}

void main() {
    float u = fbezuv.x;
    float v = fbezuv.y;
    float l = sumlength;
    float dx = abs(min(u, u - l));
    float dy = abs(v);
    float d = dy;

    vec4 color = vec4(0);
    if (hasUV) {
        color = texture(sampler, fuv.st);
    } else {
        color = fcolor;
    }

    float t = thickness/2.0 - feather;

    if (u < 0) {
        // fragment is in the start cap
        d = capd(cap.x, abs(u), dy, t);
    } else if (u > sumlength) {
        // fragment is in the end cap
        d = capd(cap.y, u - l, dy, t);
    }

    d -= t;
    if (d < 0.0) {
        fragColor = color;
    } else {
        d /= feather;
        fragColor = vec4(color.rgb, exp(-d*d)*color.a);
    }
}

// t = linewidth/2.0 - antialias;


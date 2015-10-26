#version 330 core

in vec4 fcolor;
in vec2 fuv;
out vec4 fragColor;

uniform float thickness;
uniform float feather;
uniform float sumlength;
uniform vec2 cap;

float capNone = 0;
float capButt = 1;
float capSquare = 2;
float capRound = 3;
float capTriOut = 4;
float capTriIn = 5;

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
    float u = fuv.x;
    float v = fuv.y;
    float l = sumlength;
    float dx = abs(min(u, u - l));
    float dy = abs(v);
    float d = dy;
    vec4 color = fcolor;
    float t = thickness/2.0 - feather;

    if (u < 0) {
        // fragment is in the start cap
        d = capd(cap.x, abs(u), dy, t);
    } else if (u > sumlength) {
        // fragment is in the end cap
        d = capd(cap.y, u - l, dy, t);
    } else {
        // fragment is in the body
    }

    d -= t;
    if (d < 0.0) {
        fragColor = fcolor;
    } else {
        d /= feather;
        fragColor = vec4(fcolor.rgb, exp(-d*d)*fcolor.a);
    }
}

// t = linewidth/2.0 - antialias;


#version 330 core
in vec3 fbez;
in vec4 fcolor;
out vec4 fragColor;

void main() {
    vec2 p = fbez.xy;
    // When cw is true, winding is clockwise and we're drawing outside the
    // curve.
    bool cw = bool(fbez.z);
    // Gradients
    vec2 px = dFdx(p);
    vec2 py = dFdy(p);
    // Chain rule
    float fx = (2*p.x)*px.x - px.y;
    float fy = (2*p.x)*py.x - py.y;
    // Signed distance
    float sd = (p.x*p.x - p.y) / sqrt(fx*fx + fy*fy);
    //Linear alpha
    float alpha = 0.5 - sd;
    alpha = cw ? 1 - alpha : alpha;
    // Find the resulting fragment color
    float a = 0;

    if (alpha > 1) {
        a = 1;
    } else if (alpha < 0) {
        discard;
    } else {
        // We are right on the boundary, interpolate the color intensity.
        a = alpha;
    }
    fragColor = fcolor * vec4(1.0, 1.0, 1.0, a);
}

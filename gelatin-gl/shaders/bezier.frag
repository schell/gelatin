// Loop-Blinn curve rendering

#version 330 core
in vec3 fbez;
in vec4 fcolor;
in vec2 fuv;
out vec4 fragColor;

uniform bool hasUV;
uniform sampler2D sampler;
uniform vec4 mult;

void main() {
    vec2 p = fbez.xy;
    // when cw is true, winding is clockwise and we're drawing outside the
    // curve.
    bool cw = bool(fbez.z);
    // gradients
    vec2 px = dFdx(p);
    vec2 py = dFdy(p);
    // chain rule
    float fx = (2*p.x)*px.x - px.y;
    float fy = (2*p.x)*py.x - py.y;
    // signed distance
    float sd = (p.x*p.x - p.y) / sqrt(fx*fx + fy*fy);
    // linear alpha
    float alpha = 0.5 - sd;
    alpha = cw ? 1 - alpha : alpha;
    // find the resulting fragment color
    float a = 0;

    if (alpha > 1) {
        a = 1;
    } else if (alpha < 0) {
        discard;
    } else {
        // we are right on the boundary, interpolate the color intensity.
        a = alpha;
    }

    vec4 color = vec4(0);
    if (hasUV) {
        color = texture(sampler, fuv.st);
    } else {
        color = fcolor;
    }
    fragColor = vec4(color.rgb, color.a * a) * mult;
}

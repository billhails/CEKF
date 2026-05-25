#version 330

out vec4 finalColor;

in float fragDepth;

vec3 packDepth24(float depth) {
    float d = clamp(depth, 0.0, 1.0);
    vec3 enc = fract(d * vec3(1.0, 255.0, 65025.0));
    enc -= enc.yzz * vec3(1.0 / 255.0, 1.0 / 255.0, 0.0);
    return enc;
}

void main(void) {
    finalColor = vec4(packDepth24(fragDepth), 1.0);
}

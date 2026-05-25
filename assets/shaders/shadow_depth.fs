#version 330

out vec4 finalColor;

in float fragDepth;

void main(void) {
    float d = clamp(fragDepth, 0.0, 1.0);
    finalColor = vec4(d, d, d, 1.0);
}

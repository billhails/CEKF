#version 330

in vec2 fragTexCoord;
in vec4 fragColor;

uniform sampler2D texture0;
uniform vec4 colDiffuse;
uniform vec4 uTint4;
uniform float uAmount;
uniform float uExposure;
uniform float uGamma;

out vec4 finalColor;

void main(void) {
    vec4 base = texture(texture0, fragTexCoord) * colDiffuse * fragColor;
    vec4 tinted = mix(base, base * uTint4, clamp(uAmount, 0.0, 1.0));

    float exposure = max(uExposure, 0.0);
    float gamma = max(uGamma, 0.001);
    vec3 hdr = max(tinted.rgb * exposure, vec3(0.0));
    vec3 mapped = hdr / (hdr + vec3(1.0));
    vec3 corrected = pow(mapped, vec3(1.0 / gamma));

    finalColor = vec4(corrected, base.a);
}

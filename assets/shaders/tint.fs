#version 330

in vec2 fragTexCoord;
in vec4 fragColor;

uniform sampler2D texture0;
uniform vec4 colDiffuse;
uniform vec3 uTint;
uniform float uAmount;

out vec4 finalColor;

void main(void) {
    vec4 base = texture(texture0, fragTexCoord) * colDiffuse * fragColor;
    vec3 tinted = mix(base.rgb, base.rgb * uTint, clamp(uAmount, 0.0, 1.0));
    finalColor = vec4(tinted, base.a);
}

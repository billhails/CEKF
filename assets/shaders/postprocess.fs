#version 330

in vec2 fragTexCoord;
in vec4 fragColor;

uniform sampler2D texture0;
uniform vec4 colDiffuse;
uniform vec4 uTint4;
uniform float uAmount;

out vec4 finalColor;

void main(void) {
    vec4 base = texture(texture0, fragTexCoord) * colDiffuse * fragColor;
    vec4 tinted = mix(base, base * uTint4, clamp(uAmount, 0.0, 1.0));
    finalColor = vec4(tinted.rgb, base.a);
}

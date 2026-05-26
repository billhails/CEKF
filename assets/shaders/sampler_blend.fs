#version 330

in vec2 fragTexCoord;
in vec4 fragColor;

uniform sampler2D uTex0;
uniform sampler2D uTex1;
uniform vec4 colDiffuse;
uniform float uBlend;

out vec4 finalColor;

void main(void) {
    vec4 c0 = texture(uTex0, fragTexCoord);
    vec4 c1 = texture(uTex1, fragTexCoord);
    vec4 mixed = mix(c0, c1, clamp(uBlend, 0.0, 1.0));
    finalColor = mixed * colDiffuse * fragColor;
}

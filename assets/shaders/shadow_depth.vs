#version 330

in vec3 vertexPosition;

out float fragDepth;

uniform mat4 matModel;
uniform mat4 uLightVP;

void main(void) {
    vec4 worldPosition = matModel * vec4(vertexPosition, 1.0);
    vec4 lightClip = uLightVP * worldPosition;
    gl_Position = lightClip;
    float w = lightClip.w;
    if (abs(w) < 0.0001) {
        w = (w < 0.0) ? -0.0001 : 0.0001;
    }
    fragDepth = (lightClip.z / w) * 0.5 + 0.5;
}

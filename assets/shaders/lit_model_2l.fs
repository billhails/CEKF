#version 330

in vec3 fragPosition;
in vec3 fragNormal;
in vec2 fragTexCoord;
in vec4 fragColor;

uniform sampler2D texture0;
uniform vec4 colDiffuse;

uniform vec3 uViewPos;
uniform vec3 uLightPos0;
uniform vec3 uLightColor0;
uniform vec3 uLightPos1;
uniform vec3 uLightColor1;
uniform vec3 uAmbientColor;
uniform float uSpecStrength;
uniform float uShininess;

out vec4 finalColor;

vec3 evalLight(vec3 normal, vec3 viewDir, vec3 lightPos, vec3 lightColor) {
    vec3 lightDir = normalize(lightPos - fragPosition);
    float diffuse = max(dot(normal, lightDir), 0.0);

    vec3 halfwayDir = normalize(lightDir + viewDir);
    float specular = pow(max(dot(normal, halfwayDir), 0.0), max(uShininess, 1.0));
    specular *= uSpecStrength;

    return (lightColor * diffuse) + (lightColor * specular);
}

void main(void) {
    vec4 base = texture(texture0, fragTexCoord) * colDiffuse * fragColor;

    vec3 normal = normalize(fragNormal);
    if (length(normal) < 0.001) {
        normal = normalize(cross(dFdx(fragPosition), dFdy(fragPosition)));
    }

    vec3 viewDir = normalize(uViewPos - fragPosition);

    vec3 lighting = uAmbientColor;
    lighting += evalLight(normal, viewDir, uLightPos0, uLightColor0);
    lighting += evalLight(normal, viewDir, uLightPos1, uLightColor1);

    finalColor = vec4(base.rgb * lighting, base.a);
}

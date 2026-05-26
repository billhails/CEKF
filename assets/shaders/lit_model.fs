#version 330

in vec3 fragPosition;
in vec3 fragNormal;
in vec2 fragTexCoord;
in vec4 fragColor;

uniform sampler2D texture0;
uniform vec4 colDiffuse;

uniform vec3 uViewPos;
uniform vec3 uLightPos;
uniform vec3 uLightColor;
uniform vec3 uAmbientColor;
uniform float uSpecStrength;
uniform float uShininess;

out vec4 finalColor;

void main(void) {
    vec4 base = texture(texture0, fragTexCoord) * colDiffuse * fragColor;

    vec3 normal = normalize(fragNormal);
    // Some meshes may lack usable normals; reconstruct a face normal as fallback.
    if (length(normal) < 0.001) {
        normal = normalize(cross(dFdx(fragPosition), dFdy(fragPosition)));
    }

    vec3 lightDir = normalize(uLightPos - fragPosition);

    float diffuse = max(dot(normal, lightDir), 0.0);

    vec3 viewDir = normalize(uViewPos - fragPosition);
    vec3 halfwayDir = normalize(lightDir + viewDir);
    float specular = pow(max(dot(normal, halfwayDir), 0.0), max(uShininess, 1.0));
    specular *= uSpecStrength;

    vec3 lighting = uAmbientColor + (uLightColor * diffuse) + (uLightColor * specular);
    finalColor = vec4(base.rgb * lighting, base.a);
}
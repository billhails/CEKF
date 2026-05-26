#version 330

in vec3 fragPosition;
in vec3 fragNormal;
in vec2 fragTexCoord;
in vec4 fragColor;
in vec4 fragLightClip;

uniform sampler2D texture0;
uniform sampler2D uShadowMap;
uniform vec4 colDiffuse;

uniform vec3 uViewPos;
uniform vec3 uLightPos;
uniform vec3 uLightColor;
uniform vec3 uAmbientColor;
uniform float uSpecStrength;
uniform float uShininess;
uniform float uShadowBias;
uniform float uDebugShadowMode;
uniform float uShadowFlipY;

out vec4 finalColor;

const float SHADOW_DEBUG_OK = 0.0;
const float SHADOW_DEBUG_W = 1.0;
const float SHADOW_DEBUG_UV = 2.0;
const float SHADOW_DEBUG_DEPTH = 3.0;

float unpackDepth24(vec3 enc) {
    return dot(enc, vec3(1.0, 1.0 / 255.0, 1.0 / 65025.0));
}

bool sampleShadowDepth(out float depth, out float sampledDepth, out vec2 uvOut,
                       out float failureReason) {
    float w = fragLightClip.w;
    if (abs(w) < 0.0001) {
        sampledDepth = 0.0;
        depth = 0.0;
        uvOut = vec2(0.0);
        failureReason = SHADOW_DEBUG_W;
        return false;
    }
    vec3 proj = fragLightClip.xyz / w;
    vec2 uv = proj.xy * 0.5 + 0.5;
    vec2 uvFlipped = vec2(uv.x, 1.0 - uv.y);
    depth = proj.z * 0.5 + 0.5;
    uvOut = uv;
    failureReason = SHADOW_DEBUG_OK;

    if (uv.x < 0.0 || uv.x > 1.0 || uv.y < 0.0 || uv.y > 1.0) {
        sampledDepth = 0.0;
        failureReason = SHADOW_DEBUG_UV;
        return false;
    }
    if (depth <= 0.0 || depth >= 1.0) {
        sampledDepth = 0.0;
        failureReason = SHADOW_DEBUG_DEPTH;
        return false;
    }

    vec2 uvSample = (uShadowFlipY > 0.5) ? uvFlipped : uv;
    sampledDepth = unpackDepth24(texture(uShadowMap, uvSample).rgb);
    return true;
}

float shadowFactor(vec3 normal, vec3 lightDir) {
    float depth = 0.0;
    float sampledDepth = 0.0;
    vec2 uv = vec2(0.0);
    float failureReason = SHADOW_DEBUG_OK;
    if (!sampleShadowDepth(depth, sampledDepth, uv, failureReason)) {
        return 0.0;
    }

    float slopeBias = max(uShadowBias * (1.0 - max(dot(normal, lightDir), 0.0)),
                          uShadowBias * 0.05);
    vec2 uvSample = (uShadowFlipY > 0.5) ? vec2(uv.x, 1.0 - uv.y) : uv;
    vec2 texel = 1.0 / vec2(textureSize(uShadowMap, 0));

    float shadowSum = 0.0;
    float taps = 0.0;
    for (int y = -1; y <= 1; y++) {
        for (int x = -1; x <= 1; x++) {
            vec2 tapUv = clamp(uvSample + vec2(float(x), float(y)) * texel * 1.25,
                               vec2(0.001), vec2(0.999));
            float tapDepth = unpackDepth24(texture(uShadowMap, tapUv).rgb);
            shadowSum += (depth - slopeBias > tapDepth) ? 1.0 : 0.0;
            taps += 1.0;
        }
    }

    return shadowSum / taps;
}

void main(void) {
    vec4 base = texture(texture0, fragTexCoord) * colDiffuse * fragColor;

    vec3 normal = normalize(fragNormal);
    if (length(normal) < 0.001) {
        normal = normalize(cross(dFdx(fragPosition), dFdy(fragPosition)));
    }

    vec3 lightDir = normalize(uLightPos - fragPosition);
    vec3 viewDir = normalize(uViewPos - fragPosition);

    float diffuse = max(dot(normal, lightDir), 0.0);
    vec3 halfwayDir = normalize(lightDir + viewDir);
    float specular = pow(max(dot(normal, halfwayDir), 0.0), max(uShininess, 1.0));
    specular *= uSpecStrength;

    float shadow = shadowFactor(normal, lightDir);

    float depth = 0.0;
    float sampledDepth = 0.0;
    vec2 uv = vec2(0.0);
    float failureReason = SHADOW_DEBUG_OK;
    bool hasDepthSample = sampleShadowDepth(depth, sampledDepth, uv, failureReason);

    if (uDebugShadowMode > 4.5) {
        finalColor = vec4(uv, 0.0, 1.0);
        return;
    }

    if (uDebugShadowMode > 3.5) {
        if (hasDepthSample) {
            finalColor = vec4(0.0, 1.0, 0.0, 1.0);
        } else if (failureReason == SHADOW_DEBUG_W) {
            finalColor = vec4(1.0, 0.0, 1.0, 1.0);
        } else if (failureReason == SHADOW_DEBUG_UV) {
            finalColor = vec4(1.0, 0.5, 0.0, 1.0);
        } else {
            finalColor = vec4(0.0, 0.0, 1.0, 1.0);
        }
        return;
    }

    if (uDebugShadowMode > 2.5) {
        if (hasDepthSample) {
            finalColor = vec4(vec3(sampledDepth), 1.0);
        } else {
            finalColor = vec4(1.0, 0.0, 0.0, 1.0);
        }
        return;
    }

    if (uDebugShadowMode > 1.5) {
        if (hasDepthSample) {
            finalColor = vec4(vec3(depth), 1.0);
        } else {
            finalColor = vec4(1.0, 0.0, 0.0, 1.0);
        }
        return;
    }

    if (uDebugShadowMode > 0.5) {
        finalColor = vec4(vec3(shadow), 1.0);
        return;
    }

    vec3 direct = (uLightColor * diffuse) + (uLightColor * specular);
    vec3 lighting = uAmbientColor + ((1.0 - (0.78 * shadow)) * direct);

    finalColor = vec4(base.rgb * lighting, base.a);
}

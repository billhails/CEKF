/*
 * CEKF - VM supporting amb
 * Copyright (C) 2022-2024  Bill Hails
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <https://www.gnu.org/licenses/>.
 */

#include "builtin_graphics.h"
#include "builtins_helper.h"
#include "cekf.h"
#include "cekfs.h"
#include "common.h"
#include "opaque.h"
#include "symbol.h"
#include "tc_analyze.h"
#include "value.h"
#include <math.h>
#include <stdbool.h>

#ifdef ENABLE_RAYLIB
#include <raylib.h>
#endif

static void registerGfxOpen(BuiltIns *registry);
static void registerGfxClose(BuiltIns *registry);
static void registerGfxShouldClose(BuiltIns *registry);
static void registerGfxSetTargetFps(BuiltIns *registry);
static void registerGfxBeginFrame(BuiltIns *registry);
static void registerGfxEndFrame(BuiltIns *registry);
static void registerGfxClear(BuiltIns *registry);
static void registerGfxFillRect(BuiltIns *registry);
static void registerGfxDrawText(BuiltIns *registry);
static void registerGfxIsKeyDown(BuiltIns *registry);
static void registerGfxIsKeyPressed(BuiltIns *registry);
static void registerGfxIsKeyReleased(BuiltIns *registry);
static void registerGfxMouseX(BuiltIns *registry);
static void registerGfxMouseY(BuiltIns *registry);
static void registerGfxMouseDeltaX(BuiltIns *registry);
static void registerGfxMouseDeltaY(BuiltIns *registry);
static void registerGfxIsMouseDown(BuiltIns *registry);
static void registerGfxIsMousePressed(BuiltIns *registry);
static void registerGfxIsMouseReleased(BuiltIns *registry);
static void registerGfxMouseWheel(BuiltIns *registry);
static void registerGfxDisableCursor(BuiltIns *registry);
static void registerGfxEnableCursor(BuiltIns *registry);
static void registerGfxHideCursor(BuiltIns *registry);
static void registerGfxShowCursor(BuiltIns *registry);
static void registerGfxScreenWidth(BuiltIns *registry);
static void registerGfxScreenHeight(BuiltIns *registry);
static void registerGfxFrameTimeMs(BuiltIns *registry);
static void registerGfxDrawLine(BuiltIns *registry);
static void registerGfxDrawCircle(BuiltIns *registry);
static void registerGfxFillCircle(BuiltIns *registry);
static void registerGfxDrawRect(BuiltIns *registry);
static void registerGfxDrawTriangle(BuiltIns *registry);
static void registerGfxFillTriangle(BuiltIns *registry);
static void registerGfxDrawPolygon(BuiltIns *registry);
static void registerGfxFillPolygon(BuiltIns *registry);
static void registerGfxDrawArc(BuiltIns *registry);
static void registerGfxDrawRectRounded(BuiltIns *registry);
static void registerGfxMat2DIdentity(BuiltIns *registry);
static void registerGfxMat2DTranslate(BuiltIns *registry);
static void registerGfxMat2DRotate(BuiltIns *registry);
static void registerGfxMat2DScale(BuiltIns *registry);
static void registerGfxMat2DCompose(BuiltIns *registry);
static void registerGfxMat2DApplyX(BuiltIns *registry);
static void registerGfxMat2DApplyY(BuiltIns *registry);
static void registerGfxTransform3D(BuiltIns *registry);
static void registerGfxTransform3DPositionX(BuiltIns *registry);
static void registerGfxTransform3DPositionY(BuiltIns *registry);
static void registerGfxTransform3DPositionZ(BuiltIns *registry);
static void registerGfxTransform3DRotationX(BuiltIns *registry);
static void registerGfxTransform3DRotationY(BuiltIns *registry);
static void registerGfxTransform3DRotationZ(BuiltIns *registry);
static void registerGfxTransform3DScaleX(BuiltIns *registry);
static void registerGfxTransform3DScaleY(BuiltIns *registry);
static void registerGfxTransform3DScaleZ(BuiltIns *registry);
static void registerGfxLoadFont(BuiltIns *registry);
static void registerGfxUnloadFont(BuiltIns *registry);
static void registerGfxDrawTextFont(BuiltIns *registry);
static void registerGfxMeasureTextWidth(BuiltIns *registry);
static void registerGfxLoadTexture(BuiltIns *registry);
static void registerGfxUnloadTexture(BuiltIns *registry);
static void registerGfxDrawTexture(BuiltIns *registry);
static void registerGfxDrawTextureRec(BuiltIns *registry);
static void registerGfxDrawTexturePro(BuiltIns *registry);
static void registerGfxTextureWidth(BuiltIns *registry);
static void registerGfxTextureHeight(BuiltIns *registry);
static void registerGfxLoadRenderTexture(BuiltIns *registry);
static void registerGfxUnloadRenderTexture(BuiltIns *registry);
static void registerGfxBeginTextureMode(BuiltIns *registry);
static void registerGfxEndTextureMode(BuiltIns *registry);
static void registerGfxDrawRenderTexture(BuiltIns *registry);
static void registerGfxLoadShader(BuiltIns *registry);
static void registerGfxUnloadShader(BuiltIns *registry);
static void registerGfxBeginShaderMode(BuiltIns *registry);
static void registerGfxEndShaderMode(BuiltIns *registry);
static void registerGfxSetShaderInt(BuiltIns *registry);
static void registerGfxSetShaderFloat(BuiltIns *registry);
static void registerGfxSetShaderVec2(BuiltIns *registry);
static void registerGfxSetShaderVec3(BuiltIns *registry);
static void registerGfxBeginMode2D(BuiltIns *registry);
static void registerGfxEndMode2D(BuiltIns *registry);
static void registerGfxBeginMode3D(BuiltIns *registry);
static void registerGfxEndMode3D(BuiltIns *registry);
static void registerGfxDrawCube(BuiltIns *registry);
static void registerGfxDrawCubeWires(BuiltIns *registry);
static void registerGfxDrawGrid(BuiltIns *registry);
static void registerGfxDrawSphere(BuiltIns *registry);
static void registerGfxDrawSphereWires(BuiltIns *registry);
static void registerGfxDrawCylinder(BuiltIns *registry);
static void registerGfxDrawCylinderWires(BuiltIns *registry);
static void registerGfxDrawPlane(BuiltIns *registry);
static void registerGfxDrawLine3D(BuiltIns *registry);
static void registerGfxLoadModel(BuiltIns *registry);
static void registerGfxUnloadModel(BuiltIns *registry);
static void registerGfxDrawModel(BuiltIns *registry);
static void registerGfxDrawModelWires(BuiltIns *registry);
static void registerGfxAudioOpen(BuiltIns *registry);
static void registerGfxAudioClose(BuiltIns *registry);
static void registerGfxLoadSound(BuiltIns *registry);
static void registerGfxUnloadSound(BuiltIns *registry);
static void registerGfxPlaySound(BuiltIns *registry);
static void registerGfxStopSound(BuiltIns *registry);
static void registerGfxSetSoundVolume(BuiltIns *registry);
static void registerGfxIsSoundPlaying(BuiltIns *registry);
static void registerGfxLoadMusic(BuiltIns *registry);
static void registerGfxUnloadMusic(BuiltIns *registry);
static void registerGfxPlayMusic(BuiltIns *registry);
static void registerGfxPauseMusic(BuiltIns *registry);
static void registerGfxResumeMusic(BuiltIns *registry);
static void registerGfxStopMusic(BuiltIns *registry);
static void registerGfxSetMusicVolume(BuiltIns *registry);
static void registerGfxIsMusicPlaying(BuiltIns *registry);
static void registerGfxUpdateMusicStream(BuiltIns *registry);
static void registerGfxSeekMusicMs(BuiltIns *registry);
static void registerGfxMusicTimePlayedMs(BuiltIns *registry);
static void registerGfxMusicTimeLengthMs(BuiltIns *registry);
static void registerGfxMusicProgressWidth(BuiltIns *registry);

void registerGraphics(BuiltIns *registry) {
    registerGfxOpen(registry);
    registerGfxClose(registry);
    registerGfxShouldClose(registry);
    registerGfxSetTargetFps(registry);
    registerGfxBeginFrame(registry);
    registerGfxEndFrame(registry);
    registerGfxClear(registry);
    registerGfxFillRect(registry);
    registerGfxDrawText(registry);
    registerGfxIsKeyDown(registry);
    registerGfxIsKeyPressed(registry);
    registerGfxIsKeyReleased(registry);
    registerGfxMouseX(registry);
    registerGfxMouseY(registry);
    registerGfxMouseDeltaX(registry);
    registerGfxMouseDeltaY(registry);
    registerGfxIsMouseDown(registry);
    registerGfxIsMousePressed(registry);
    registerGfxIsMouseReleased(registry);
    registerGfxMouseWheel(registry);
    registerGfxDisableCursor(registry);
    registerGfxEnableCursor(registry);
    registerGfxHideCursor(registry);
    registerGfxShowCursor(registry);
    registerGfxScreenWidth(registry);
    registerGfxScreenHeight(registry);
    registerGfxFrameTimeMs(registry);
    registerGfxDrawLine(registry);
    registerGfxDrawCircle(registry);
    registerGfxFillCircle(registry);
    registerGfxDrawRect(registry);
    registerGfxDrawTriangle(registry);
    registerGfxFillTriangle(registry);
    registerGfxDrawPolygon(registry);
    registerGfxFillPolygon(registry);
    registerGfxDrawArc(registry);
    registerGfxDrawRectRounded(registry);
    registerGfxMat2DIdentity(registry);
    registerGfxMat2DTranslate(registry);
    registerGfxMat2DRotate(registry);
    registerGfxMat2DScale(registry);
    registerGfxMat2DCompose(registry);
    registerGfxMat2DApplyX(registry);
    registerGfxMat2DApplyY(registry);
    registerGfxTransform3D(registry);
    registerGfxTransform3DPositionX(registry);
    registerGfxTransform3DPositionY(registry);
    registerGfxTransform3DPositionZ(registry);
    registerGfxTransform3DRotationX(registry);
    registerGfxTransform3DRotationY(registry);
    registerGfxTransform3DRotationZ(registry);
    registerGfxTransform3DScaleX(registry);
    registerGfxTransform3DScaleY(registry);
    registerGfxTransform3DScaleZ(registry);
    registerGfxLoadFont(registry);
    registerGfxUnloadFont(registry);
    registerGfxDrawTextFont(registry);
    registerGfxMeasureTextWidth(registry);
    registerGfxLoadTexture(registry);
    registerGfxUnloadTexture(registry);
    registerGfxDrawTexture(registry);
    registerGfxDrawTextureRec(registry);
    registerGfxDrawTexturePro(registry);
    registerGfxTextureWidth(registry);
    registerGfxTextureHeight(registry);
    registerGfxLoadRenderTexture(registry);
    registerGfxUnloadRenderTexture(registry);
    registerGfxBeginTextureMode(registry);
    registerGfxEndTextureMode(registry);
    registerGfxDrawRenderTexture(registry);
    registerGfxLoadShader(registry);
    registerGfxUnloadShader(registry);
    registerGfxBeginShaderMode(registry);
    registerGfxEndShaderMode(registry);
    registerGfxSetShaderInt(registry);
    registerGfxSetShaderFloat(registry);
    registerGfxSetShaderVec2(registry);
    registerGfxSetShaderVec3(registry);
    registerGfxBeginMode2D(registry);
    registerGfxEndMode2D(registry);
    registerGfxBeginMode3D(registry);
    registerGfxEndMode3D(registry);
    registerGfxDrawCube(registry);
    registerGfxDrawCubeWires(registry);
    registerGfxDrawGrid(registry);
    registerGfxDrawSphere(registry);
    registerGfxDrawSphereWires(registry);
    registerGfxDrawCylinder(registry);
    registerGfxDrawCylinderWires(registry);
    registerGfxDrawPlane(registry);
    registerGfxDrawLine3D(registry);
    registerGfxLoadModel(registry);
    registerGfxUnloadModel(registry);
    registerGfxDrawModel(registry);
    registerGfxDrawModelWires(registry);
    registerGfxAudioOpen(registry);
    registerGfxAudioClose(registry);
    registerGfxLoadSound(registry);
    registerGfxUnloadSound(registry);
    registerGfxPlaySound(registry);
    registerGfxStopSound(registry);
    registerGfxSetSoundVolume(registry);
    registerGfxIsSoundPlaying(registry);
    registerGfxLoadMusic(registry);
    registerGfxUnloadMusic(registry);
    registerGfxPlayMusic(registry);
    registerGfxPauseMusic(registry);
    registerGfxResumeMusic(registry);
    registerGfxStopMusic(registry);
    registerGfxSetMusicVolume(registry);
    registerGfxIsMusicPlaying(registry);
    registerGfxUpdateMusicStream(registry);
    registerGfxSeekMusicMs(registry);
    registerGfxMusicTimePlayedMs(registry);
    registerGfxMusicTimeLengthMs(registry);
    registerGfxMusicProgressWidth(registry);
}

typedef struct FontNode {
    Opaque *wrapper;
    struct FontNode *next;
} FontNode;

static FontNode *fontRegistry = NULL;

typedef struct TextureNode {
    Opaque *wrapper;
    struct TextureNode *next;
} TextureNode;

static TextureNode *textureRegistry = NULL;

typedef struct RenderTextureNode {
    Opaque *wrapper;
    struct RenderTextureNode *next;
} RenderTextureNode;

static RenderTextureNode *renderTextureRegistry = NULL;

typedef struct ShaderNode {
    Opaque *wrapper;
    struct ShaderNode *next;
} ShaderNode;

static ShaderNode *shaderRegistry = NULL;

typedef struct SoundNode {
    Opaque *wrapper;
    struct SoundNode *next;
} SoundNode;

static SoundNode *soundRegistry = NULL;

typedef struct MusicNode {
    Opaque *wrapper;
    struct MusicNode *next;
} MusicNode;

static MusicNode *musicRegistry = NULL;

typedef struct ModelNode {
    Opaque *wrapper;
    struct ModelNode *next;
} ModelNode;

static ModelNode *modelRegistry = NULL;

static HashSymbol *fontSymbol(void) { return newSymbol("font"); }

static TcType *makeFontType(void) { return newTcType_Opaque(fontSymbol()); }

static TcType *pushFontArg(BuiltInArgs *args) {
    TcType *t = makeFontType();
    int save = PROTECT(t);
    pushBuiltInArgs(args, t);
    UNPROTECT(save);
    return t;
}

static HashSymbol *textureSymbol(void) { return newSymbol("texture"); }

static TcType *makeTextureType(void) {
    return newTcType_Opaque(textureSymbol());
}

static TcType *pushTextureArg(BuiltInArgs *args) {
    TcType *t = makeTextureType();
    int save = PROTECT(t);
    pushBuiltInArgs(args, t);
    UNPROTECT(save);
    return t;
}

static HashSymbol *soundSymbol(void) { return newSymbol("sound"); }

static TcType *makeSoundType(void) { return newTcType_Opaque(soundSymbol()); }

static TcType *pushSoundArg(BuiltInArgs *args) {
    TcType *t = makeSoundType();
    int save = PROTECT(t);
    pushBuiltInArgs(args, t);
    UNPROTECT(save);
    return t;
}

static HashSymbol *musicSymbol(void) { return newSymbol("music"); }

static TcType *makeMusicType(void) { return newTcType_Opaque(musicSymbol()); }

static TcType *pushMusicArg(BuiltInArgs *args) {
    TcType *t = makeMusicType();
    int save = PROTECT(t);
    pushBuiltInArgs(args, t);
    UNPROTECT(save);
    return t;
}

static HashSymbol *modelSymbol(void) { return newSymbol("model"); }

static TcType *makeModelType(void) { return newTcType_Opaque(modelSymbol()); }

static TcType *pushModelArg(BuiltInArgs *args) {
    TcType *t = makeModelType();
    int save = PROTECT(t);
    pushBuiltInArgs(args, t);
    UNPROTECT(save);
    return t;
}

static HashSymbol *mat2DSymbol(void) { return newSymbol("mat2d"); }

static TcType *makeMat2DType(void) { return newTcType_Opaque(mat2DSymbol()); }

static TcType *pushMat2DArg(BuiltInArgs *args) {
    TcType *t = makeMat2DType();
    int save = PROTECT(t);
    pushBuiltInArgs(args, t);
    UNPROTECT(save);
    return t;
}

static HashSymbol *transform3DSymbol(void) { return newSymbol("transform3d"); }

static TcType *makeTransform3DType(void) {
    return newTcType_Opaque(transform3DSymbol());
}

static TcType *pushTransform3DArg(BuiltInArgs *args) {
    TcType *t = makeTransform3DType();
    int save = PROTECT(t);
    pushBuiltInArgs(args, t);
    UNPROTECT(save);
    return t;
}

static HashSymbol *shaderSymbol(void) { return newSymbol("shader"); }

static TcType *makeShaderType(void) { return newTcType_Opaque(shaderSymbol()); }

static TcType *pushShaderArg(BuiltInArgs *args) {
    TcType *t = makeShaderType();
    int save = PROTECT(t);
    pushBuiltInArgs(args, t);
    UNPROTECT(save);
    return t;
}

typedef struct Mat2DData {
    float m00;
    float m01;
    float m02;
    float m10;
    float m11;
    float m12;
} Mat2DData;

typedef struct Transform3DData {
    float px;
    float py;
    float pz;
    float rx;
    float ry;
    float rz;
    float sx;
    float sy;
    float sz;
} Transform3DData;

static void opaque_gfx_mat2d_free(void *data) {
    if (data == NULL)
        return;
    Mat2DData *mat = (Mat2DData *)data;
    FREE_ARRAY(Mat2DData, mat, 1);
}

static void opaque_gfx_transform3d_free(void *data) {
    if (data == NULL)
        return;
    Transform3DData *t = (Transform3DData *)data;
    FREE_ARRAY(Transform3DData, t, 1);
}

static Mat2DData mat2dIdentityData(void) {
    return (Mat2DData){1.0f, 0.0f, 0.0f, 0.0f, 1.0f, 0.0f};
}

static Mat2DData mat2dMul(Mat2DData a, Mat2DData b) {
    Mat2DData out;
    out.m00 = a.m00 * b.m00 + a.m01 * b.m10;
    out.m01 = a.m00 * b.m01 + a.m01 * b.m11;
    out.m02 = a.m00 * b.m02 + a.m01 * b.m12 + a.m02;
    out.m10 = a.m10 * b.m00 + a.m11 * b.m10;
    out.m11 = a.m10 * b.m01 + a.m11 * b.m11;
    out.m12 = a.m10 * b.m02 + a.m11 * b.m12 + a.m12;
    return out;
}

static Opaque *newMat2DOpaque(Mat2DData data) {
    Mat2DData *storage = NEW_ARRAY(Mat2DData, 1);
    *storage = data;
    return newOpaque(storage, opaque_gfx_mat2d_free, NULL, NULL);
}

static Opaque *newTransform3DOpaque(Transform3DData data) {
    Transform3DData *storage = NEW_ARRAY(Transform3DData, 1);
    *storage = data;
    return newOpaque(storage, opaque_gfx_transform3d_free, NULL, NULL);
}

static Mat2DData *asMat2D(Value v) {
    if (v.type != VALUE_TYPE_OPAQUE || v.val.opaque == NULL ||
        v.val.opaque->data == NULL)
        return NULL;
    return (Mat2DData *)v.val.opaque->data;
}

static Transform3DData *asTransform3D(Value v) {
    if (v.type != VALUE_TYPE_OPAQUE || v.val.opaque == NULL ||
        v.val.opaque->data == NULL)
        return NULL;
    return (Transform3DData *)v.val.opaque->data;
}

#ifdef ENABLE_RAYLIB
static void fontRegistryRemove(Opaque *wrapper) {
    FontNode **prev = &fontRegistry;
    while (*prev != NULL) {
        if ((*prev)->wrapper == wrapper) {
            FontNode *dead = *prev;
            *prev = dead->next;
            FREE_ARRAY(FontNode, dead, 1);
            return;
        }
        prev = &(*prev)->next;
    }
}

static void textureRegistryRemove(Opaque *wrapper) {
    TextureNode **prev = &textureRegistry;
    while (*prev != NULL) {
        if ((*prev)->wrapper == wrapper) {
            TextureNode *dead = *prev;
            *prev = dead->next;
            FREE_ARRAY(TextureNode, dead, 1);
            return;
        }
        prev = &(*prev)->next;
    }
}

static void soundRegistryRemove(Opaque *wrapper) {
    SoundNode **prev = &soundRegistry;
    while (*prev != NULL) {
        if ((*prev)->wrapper == wrapper) {
            SoundNode *dead = *prev;
            *prev = dead->next;
            FREE_ARRAY(SoundNode, dead, 1);
            return;
        }
        prev = &(*prev)->next;
    }
}

static void musicRegistryRemove(Opaque *wrapper) {
    MusicNode **prev = &musicRegistry;
    while (*prev != NULL) {
        if ((*prev)->wrapper == wrapper) {
            MusicNode *dead = *prev;
            *prev = dead->next;
            FREE_ARRAY(MusicNode, dead, 1);
            return;
        }
        prev = &(*prev)->next;
    }
}

static void modelRegistryRemove(Opaque *wrapper) {
    ModelNode **prev = &modelRegistry;
    while (*prev != NULL) {
        if ((*prev)->wrapper == wrapper) {
            ModelNode *dead = *prev;
            *prev = dead->next;
            FREE_ARRAY(ModelNode, dead, 1);
            return;
        }
        prev = &(*prev)->next;
    }
}
#endif

void markGraphicsGlobals(void) {
    FontNode *node = fontRegistry;
    while (node != NULL) {
        markOpaque(node->wrapper);
        node = node->next;
    }
    TextureNode *tnode = textureRegistry;
    while (tnode != NULL) {
        markOpaque(tnode->wrapper);
        tnode = tnode->next;
    }
    RenderTextureNode *rtnode = renderTextureRegistry;
    while (rtnode != NULL) {
        markOpaque(rtnode->wrapper);
        rtnode = rtnode->next;
    }
    ShaderNode *shaderNode = shaderRegistry;
    while (shaderNode != NULL) {
        markOpaque(shaderNode->wrapper);
        shaderNode = shaderNode->next;
    }
    SoundNode *snode = soundRegistry;
    while (snode != NULL) {
        markOpaque(snode->wrapper);
        snode = snode->next;
    }
    MusicNode *mnode = musicRegistry;
    while (mnode != NULL) {
        markOpaque(mnode->wrapper);
        mnode = mnode->next;
    }
    ModelNode *modelNode = modelRegistry;
    while (modelNode != NULL) {
        markOpaque(modelNode->wrapper);
        modelNode = modelNode->next;
    }
}

#ifdef ENABLE_RAYLIB
static struct {
    bool initialized;
    bool in_frame;
    bool in_texture_mode;
    bool in_shader_mode;
    bool in_camera_mode;
    bool in_3d_mode;
    bool audio_initialized;
} gfx_state = {false, false, false, false, false, false, false};

static bool canDrawTarget(void) {
    return gfx_state.in_frame || gfx_state.in_texture_mode;
}

static int extractChannel(Value v) {
    int n = (int)getValue_Stdint(v);
    if (n < 0 || n > 255)
        return -1;
    return n;
}

// Accept STDINT or rational (e.g. result of w/2) as a pixel coordinate.
// Rationals are truncated toward zero.
static int valueAsInt(Value v) {
    if (v.type == VALUE_TYPE_STDINT)
        return (int)getValue_Stdint(v);
    if (v.type == VALUE_TYPE_IRRATIONAL)
        return (int)getValue_Irrational(v);
    if (v.type == VALUE_TYPE_RATIONAL) {
        Vec *r = getValue_Rational(v);
        if (r->entries[0].type == VALUE_TYPE_STDINT &&
            r->entries[1].type == VALUE_TYPE_STDINT) {
            int num = (int)getValue_Stdint(r->entries[0]);
            int den = (int)getValue_Stdint(r->entries[1]);
            return den != 0 ? num / den : 0;
        }
    }
    return (int)getValue_Stdint(
        v); // aborts with a clear message for other types
}

// Accept STDINT, rational, or irrational as a float (e.g. rotation degrees,
// pivot coordinates). Rationals are converted to their exact float quotient;
// irrationals are cast directly.
static float valueAsFloat(Value v) {
    if (v.type == VALUE_TYPE_STDINT)
        return (float)getValue_Stdint(v);
    if (v.type == VALUE_TYPE_IRRATIONAL)
        return (float)getValue_Irrational(v);
    if (v.type == VALUE_TYPE_RATIONAL) {
        Vec *r = getValue_Rational(v);
        if (r->entries[0].type == VALUE_TYPE_STDINT &&
            r->entries[1].type == VALUE_TYPE_STDINT) {
            float num = (float)getValue_Stdint(r->entries[0]);
            float den = (float)getValue_Stdint(r->entries[1]);
            return den != 0.0f ? num / den : 0.0f;
        }
    }
    return (float)getValue_Stdint(
        v); // aborts with a clear message for other types
}

static void opaque_gfx_font_unload(void *data) {
    if (data == NULL)
        return;
    Font *f = (Font *)data;
    UnloadFont(*f);
    FREE_ARRAY(Font, f, 1);
}

static void fontRegistryDrain(void) {
    FontNode *node = fontRegistry;
    while (node != NULL) {
        FontNode *next = node->next;
        if (node->wrapper->data != NULL) {
            opaque_gfx_font_unload(node->wrapper->data);
            node->wrapper->data = NULL;
        }
        FREE_ARRAY(FontNode, node, 1);
        node = next;
    }
    fontRegistry = NULL;
}

static void opaque_gfx_texture_unload(void *data) {
    if (data == NULL)
        return;
    Texture2D *t = (Texture2D *)data;
    UnloadTexture(*t);
    FREE_ARRAY(Texture2D, t, 1);
}

static void textureRegistryDrain(void) {
    TextureNode *node = textureRegistry;
    while (node != NULL) {
        TextureNode *next = node->next;
        if (node->wrapper->data != NULL) {
            opaque_gfx_texture_unload(node->wrapper->data);
            node->wrapper->data = NULL;
        }
        FREE_ARRAY(TextureNode, node, 1);
        node = next;
    }
    textureRegistry = NULL;
}

static void renderTextureRegistryRemove(Opaque *wrapper) {
    RenderTextureNode **prev = &renderTextureRegistry;
    while (*prev != NULL) {
        if ((*prev)->wrapper == wrapper) {
            RenderTextureNode *dead = *prev;
            *prev = dead->next;
            FREE_ARRAY(RenderTextureNode, dead, 1);
            return;
        }
        prev = &(*prev)->next;
    }
}

static void shaderRegistryRemove(Opaque *wrapper) {
    ShaderNode **prev = &shaderRegistry;
    while (*prev != NULL) {
        if ((*prev)->wrapper == wrapper) {
            ShaderNode *dead = *prev;
            *prev = dead->next;
            FREE_ARRAY(ShaderNode, dead, 1);
            return;
        }
        prev = &(*prev)->next;
    }
}

static void opaque_gfx_render_texture_unload(void *data) {
    if (data == NULL)
        return;
    RenderTexture2D *rt = (RenderTexture2D *)data;
    UnloadRenderTexture(*rt);
    FREE_ARRAY(RenderTexture2D, rt, 1);
}

static void renderTextureRegistryDrain(void) {
    RenderTextureNode *node = renderTextureRegistry;
    while (node != NULL) {
        RenderTextureNode *next = node->next;
        if (node->wrapper->data != NULL) {
            opaque_gfx_render_texture_unload(node->wrapper->data);
            node->wrapper->data = NULL;
        }
        FREE_ARRAY(RenderTextureNode, node, 1);
        node = next;
    }
    renderTextureRegistry = NULL;
}

static void opaque_gfx_shader_unload(void *data) {
    if (data == NULL)
        return;
    Shader *shader = (Shader *)data;
    UnloadShader(*shader);
    FREE_ARRAY(Shader, shader, 1);
}

static void shaderRegistryDrain(void) {
    ShaderNode *node = shaderRegistry;
    while (node != NULL) {
        ShaderNode *next = node->next;
        if (node->wrapper->data != NULL) {
            opaque_gfx_shader_unload(node->wrapper->data);
            node->wrapper->data = NULL;
        }
        FREE_ARRAY(ShaderNode, node, 1);
        node = next;
    }
    shaderRegistry = NULL;
}

static void opaque_gfx_sound_unload(void *data) {
    if (data == NULL)
        return;
    Sound *snd = (Sound *)data;
    UnloadSound(*snd);
    FREE_ARRAY(Sound, snd, 1);
}

static void soundRegistryDrain(void) {
    SoundNode *node = soundRegistry;
    while (node != NULL) {
        SoundNode *next = node->next;
        if (node->wrapper->data != NULL) {
            opaque_gfx_sound_unload(node->wrapper->data);
            node->wrapper->data = NULL;
        }
        FREE_ARRAY(SoundNode, node, 1);
        node = next;
    }
    soundRegistry = NULL;
}

static void opaque_gfx_music_unload(void *data) {
    if (data == NULL)
        return;
    Music *music = (Music *)data;
    UnloadMusicStream(*music);
    FREE_ARRAY(Music, music, 1);
}

static void musicRegistryDrain(void) {
    MusicNode *node = musicRegistry;
    while (node != NULL) {
        MusicNode *next = node->next;
        if (node->wrapper->data != NULL) {
            opaque_gfx_music_unload(node->wrapper->data);
            node->wrapper->data = NULL;
        }
        FREE_ARRAY(MusicNode, node, 1);
        node = next;
    }
    musicRegistry = NULL;
}

static void opaque_gfx_model_unload(void *data) {
    if (data == NULL)
        return;
    Model *model = (Model *)data;
    UnloadModel(*model);
    FREE_ARRAY(Model, model, 1);
}

static void modelRegistryDrain(void) {
    ModelNode *node = modelRegistry;
    while (node != NULL) {
        ModelNode *next = node->next;
        if (node->wrapper->data != NULL) {
            opaque_gfx_model_unload(node->wrapper->data);
            node->wrapper->data = NULL;
        }
        FREE_ARRAY(ModelNode, node, 1);
        node = next;
    }
    modelRegistry = NULL;
}
#endif

static Value failMsg(const char *text) {
    Value msg = utf8ToList(text);
    int save = protectValue(msg);
    Value result = makeTryResult(0, msg);
    UNPROTECT(save);
    return result;
}

Value builtin_gfx_open(Vec *args) {
#ifndef ENABLE_RAYLIB
    (void)args;
    return failMsg("graphics not available (built without ENABLE_RAYLIB)");
#else
    if (gfx_state.initialized) {
        return failMsg("gfx_open: graphics already open");
    }
    int w = (int)getValue_Stdint(args->entries[0]);
    int h = (int)getValue_Stdint(args->entries[1]);
    if (w <= 0 || h <= 0) {
        return failMsg("gfx_open: width and height must be positive");
    }
    SCharVec *title = listToUtf8(args->entries[2]);
    int save = PROTECT(title);
    SetTraceLogLevel(LOG_ERROR);
    InitWindow(w, h, title->entries);
    UNPROTECT(save);
    if (!IsWindowReady()) {
        return failMsg("gfx_open: failed to create window");
    }
    gfx_state.initialized = true;
    gfx_state.in_frame = false;
    gfx_state.in_texture_mode = false;
    gfx_state.in_shader_mode = false;
    gfx_state.in_camera_mode = false;
    gfx_state.in_3d_mode = false;
    return makeTryResult(1, value_Stdint(1));
#endif
}

Value builtin_gfx_close(Vec *args) {
    (void)args;
#ifndef ENABLE_RAYLIB
    return value_Stdint(0);
#else
    if (!gfx_state.initialized && !gfx_state.audio_initialized) {
        return value_Stdint(0);
    }
    if (gfx_state.initialized) {
        if (gfx_state.in_shader_mode) {
            EndShaderMode();
            gfx_state.in_shader_mode = false;
        }
        if (gfx_state.in_3d_mode) {
            EndMode3D();
            gfx_state.in_3d_mode = false;
        }
        if (gfx_state.in_camera_mode) {
            EndMode2D();
            gfx_state.in_camera_mode = false;
        }
        if (gfx_state.in_frame) {
            EndDrawing();
            gfx_state.in_frame = false;
        }
        if (gfx_state.in_texture_mode) {
            EndTextureMode();
            gfx_state.in_texture_mode = false;
        }
        shaderRegistryDrain();
        renderTextureRegistryDrain();
        textureRegistryDrain();
        modelRegistryDrain();
        fontRegistryDrain();
        CloseWindow();
        gfx_state.initialized = false;
    }
    if (gfx_state.audio_initialized) {
        musicRegistryDrain();
        soundRegistryDrain();
        CloseAudioDevice();
        gfx_state.audio_initialized = false;
    }
    return value_Stdint(1);
#endif
}

Value builtin_gfx_should_close(Vec *args) {
    (void)args;
#ifndef ENABLE_RAYLIB
    return value_Stdint(0);
#else
    if (!gfx_state.initialized) {
        return value_Stdint(0);
    }
    return value_Stdint(WindowShouldClose() ? 1 : 0);
#endif
}

Value builtin_gfx_set_target_fps(Vec *args) {
#ifndef ENABLE_RAYLIB
    (void)args;
    return value_Stdint(0);
#else
    if (!gfx_state.initialized) {
        return value_Stdint(0);
    }
    int fps = (int)getValue_Stdint(args->entries[0]);
    if (fps <= 0) {
        return value_Stdint(0);
    }
    SetTargetFPS(fps);
    return value_Stdint(1);
#endif
}

Value builtin_gfx_begin_frame(Vec *args) {
    (void)args;
#ifndef ENABLE_RAYLIB
    return value_Stdint(0);
#else
    if (!gfx_state.initialized || gfx_state.in_frame) {
        return value_Stdint(0);
    }
    BeginDrawing();
    gfx_state.in_frame = true;
    return value_Stdint(1);
#endif
}

Value builtin_gfx_end_frame(Vec *args) {
    (void)args;
#ifndef ENABLE_RAYLIB
    return value_Stdint(0);
#else
    if (!gfx_state.initialized || !gfx_state.in_frame) {
        return value_Stdint(0);
    }
    EndDrawing();
    gfx_state.in_frame = false;
    return value_Stdint(1);
#endif
}

Value builtin_gfx_clear(Vec *args) {
#ifndef ENABLE_RAYLIB
    (void)args;
    return value_Stdint(0);
#else
    if (!canDrawTarget()) {
        return value_Stdint(0);
    }
    int r = extractChannel(args->entries[0]);
    int g = extractChannel(args->entries[1]);
    int b = extractChannel(args->entries[2]);
    int a = extractChannel(args->entries[3]);
    if (r < 0 || g < 0 || b < 0 || a < 0) {
        return value_Stdint(0);
    }
    Color color = {(unsigned char)r, (unsigned char)g, (unsigned char)b,
                   (unsigned char)a};
    ClearBackground(color);
    return value_Stdint(1);
#endif
}

Value builtin_gfx_fill_rect(Vec *args) {
#ifndef ENABLE_RAYLIB
    (void)args;
    return value_Stdint(0);
#else
    if (!canDrawTarget()) {
        return value_Stdint(0);
    }
    int x = (int)getValue_Stdint(args->entries[0]);
    int y = (int)getValue_Stdint(args->entries[1]);
    int w = (int)getValue_Stdint(args->entries[2]);
    int h = (int)getValue_Stdint(args->entries[3]);
    int r = extractChannel(args->entries[4]);
    int g = extractChannel(args->entries[5]);
    int b = extractChannel(args->entries[6]);
    int a = extractChannel(args->entries[7]);
    if (w < 0 || h < 0 || r < 0 || g < 0 || b < 0 || a < 0) {
        return value_Stdint(0);
    }
    Color color = {(unsigned char)r, (unsigned char)g, (unsigned char)b,
                   (unsigned char)a};
    DrawRectangle(x, y, w, h, color);
    return value_Stdint(1);
#endif
}

Value builtin_gfx_draw_text(Vec *args) {
#ifndef ENABLE_RAYLIB
    (void)args;
    return value_Stdint(0);
#else
    if (!canDrawTarget()) {
        return value_Stdint(0);
    }
    SCharVec *text = listToUtf8(args->entries[0]);
    int save = PROTECT(text);
    int x = valueAsInt(args->entries[1]);
    int y = valueAsInt(args->entries[2]);
    int sz = valueAsInt(args->entries[3]);
    int r = extractChannel(args->entries[4]);
    int g = extractChannel(args->entries[5]);
    int b = extractChannel(args->entries[6]);
    int a = extractChannel(args->entries[7]);
    int ok;
    if (sz <= 0 || r < 0 || g < 0 || b < 0 || a < 0) {
        ok = 0;
    } else {
        Color color = {(unsigned char)r, (unsigned char)g, (unsigned char)b,
                       (unsigned char)a};
        DrawText(text->entries, x, y, sz, color);
        ok = 1;
    }
    UNPROTECT(save);
    return value_Stdint(ok);
#endif
}

// --- Phase A: input ---

Value builtin_gfx_is_key_down(Vec *args) {
#ifndef ENABLE_RAYLIB
    (void)args;
    return value_Stdint(0);
#else
    if (!gfx_state.initialized)
        return value_Stdint(0);
    return value_Stdint(IsKeyDown((int)getValue_Stdint(args->entries[0])) ? 1
                                                                          : 0);
#endif
}

Value builtin_gfx_is_key_pressed(Vec *args) {
#ifndef ENABLE_RAYLIB
    (void)args;
    return value_Stdint(0);
#else
    if (!gfx_state.initialized)
        return value_Stdint(0);
    return value_Stdint(
        IsKeyPressed((int)getValue_Stdint(args->entries[0])) ? 1 : 0);
#endif
}

Value builtin_gfx_is_key_released(Vec *args) {
#ifndef ENABLE_RAYLIB
    (void)args;
    return value_Stdint(0);
#else
    if (!gfx_state.initialized)
        return value_Stdint(0);
    return value_Stdint(
        IsKeyReleased((int)getValue_Stdint(args->entries[0])) ? 1 : 0);
#endif
}

Value builtin_gfx_mouse_x(Vec *args) {
    (void)args;
#ifndef ENABLE_RAYLIB
    return value_Stdint(0);
#else
    if (!gfx_state.initialized)
        return value_Stdint(0);
    return value_Stdint(GetMouseX());
#endif
}

Value builtin_gfx_mouse_y(Vec *args) {
    (void)args;
#ifndef ENABLE_RAYLIB
    return value_Stdint(0);
#else
    if (!gfx_state.initialized)
        return value_Stdint(0);
    return value_Stdint(GetMouseY());
#endif
}

Value builtin_gfx_mouse_delta_x(Vec *args) {
    (void)args;
#ifndef ENABLE_RAYLIB
    return value_Stdint(0);
#else
    if (!gfx_state.initialized)
        return value_Stdint(0);
    Vector2 delta = GetMouseDelta();
    return value_Stdint((int)delta.x);
#endif
}

Value builtin_gfx_mouse_delta_y(Vec *args) {
    (void)args;
#ifndef ENABLE_RAYLIB
    return value_Stdint(0);
#else
    if (!gfx_state.initialized)
        return value_Stdint(0);
    Vector2 delta = GetMouseDelta();
    return value_Stdint((int)delta.y);
#endif
}

Value builtin_gfx_is_mouse_down(Vec *args) {
#ifndef ENABLE_RAYLIB
    (void)args;
    return value_Stdint(0);
#else
    if (!gfx_state.initialized)
        return value_Stdint(0);
    return value_Stdint(
        IsMouseButtonDown((int)getValue_Stdint(args->entries[0])) ? 1 : 0);
#endif
}

Value builtin_gfx_is_mouse_pressed(Vec *args) {
#ifndef ENABLE_RAYLIB
    (void)args;
    return value_Stdint(0);
#else
    if (!gfx_state.initialized)
        return value_Stdint(0);
    return value_Stdint(
        IsMouseButtonPressed((int)getValue_Stdint(args->entries[0])) ? 1 : 0);
#endif
}

Value builtin_gfx_is_mouse_released(Vec *args) {
#ifndef ENABLE_RAYLIB
    (void)args;
    return value_Stdint(0);
#else
    if (!gfx_state.initialized)
        return value_Stdint(0);
    return value_Stdint(
        IsMouseButtonReleased((int)getValue_Stdint(args->entries[0])) ? 1 : 0);
#endif
}

Value builtin_gfx_mouse_wheel(Vec *args) {
    (void)args;
#ifndef ENABLE_RAYLIB
    return value_Stdint(0);
#else
    if (!gfx_state.initialized)
        return value_Stdint(0);
    return value_Stdint((int)GetMouseWheelMove());
#endif
}

Value builtin_gfx_disable_cursor(Vec *args) {
    (void)args;
#ifndef ENABLE_RAYLIB
    return value_Stdint(0);
#else
    if (!gfx_state.initialized)
        return value_Stdint(0);
    DisableCursor();
    return value_Stdint(1);
#endif
}

Value builtin_gfx_enable_cursor(Vec *args) {
    (void)args;
#ifndef ENABLE_RAYLIB
    return value_Stdint(0);
#else
    if (!gfx_state.initialized)
        return value_Stdint(0);
    EnableCursor();
    return value_Stdint(1);
#endif
}

Value builtin_gfx_hide_cursor(Vec *args) {
    (void)args;
#ifndef ENABLE_RAYLIB
    return value_Stdint(0);
#else
    if (!gfx_state.initialized)
        return value_Stdint(0);
    HideCursor();
    return value_Stdint(1);
#endif
}

Value builtin_gfx_show_cursor(Vec *args) {
    (void)args;
#ifndef ENABLE_RAYLIB
    return value_Stdint(0);
#else
    if (!gfx_state.initialized)
        return value_Stdint(0);
    ShowCursor();
    return value_Stdint(1);
#endif
}

Value builtin_gfx_screen_width(Vec *args) {
    (void)args;
#ifndef ENABLE_RAYLIB
    return value_Stdint(0);
#else
    if (!gfx_state.initialized)
        return value_Stdint(0);
    return value_Stdint(GetScreenWidth());
#endif
}

Value builtin_gfx_screen_height(Vec *args) {
    (void)args;
#ifndef ENABLE_RAYLIB
    return value_Stdint(0);
#else
    if (!gfx_state.initialized)
        return value_Stdint(0);
    return value_Stdint(GetScreenHeight());
#endif
}

Value builtin_gfx_frame_time_ms(Vec *args) {
    (void)args;
#ifndef ENABLE_RAYLIB
    return value_Stdint(0);
#else
    if (!gfx_state.initialized)
        return value_Stdint(0);
    return value_Stdint((int)(GetFrameTime() * 1000.0f));
#endif
}

// --- Phase B: primitives ---

Value builtin_gfx_draw_line(Vec *args) {
#ifndef ENABLE_RAYLIB
    (void)args;
    return value_Stdint(0);
#else
    if (!canDrawTarget())
        return value_Stdint(0);
    int x1 = valueAsInt(args->entries[0]);
    int y1 = valueAsInt(args->entries[1]);
    int x2 = valueAsInt(args->entries[2]);
    int y2 = valueAsInt(args->entries[3]);
    int r = extractChannel(args->entries[4]);
    int g = extractChannel(args->entries[5]);
    int b = extractChannel(args->entries[6]);
    int a = extractChannel(args->entries[7]);
    if (r < 0 || g < 0 || b < 0 || a < 0)
        return value_Stdint(0);
    Color color = {(unsigned char)r, (unsigned char)g, (unsigned char)b,
                   (unsigned char)a};
    DrawLine(x1, y1, x2, y2, color);
    return value_Stdint(1);
#endif
}

Value builtin_gfx_draw_circle(Vec *args) {
#ifndef ENABLE_RAYLIB
    (void)args;
    return value_Stdint(0);
#else
    if (!canDrawTarget())
        return value_Stdint(0);
    int cx = valueAsInt(args->entries[0]);
    int cy = valueAsInt(args->entries[1]);
    int radius = valueAsInt(args->entries[2]);
    int r = extractChannel(args->entries[3]);
    int g = extractChannel(args->entries[4]);
    int b = extractChannel(args->entries[5]);
    int a = extractChannel(args->entries[6]);
    if (radius < 0 || r < 0 || g < 0 || b < 0 || a < 0)
        return value_Stdint(0);
    Color color = {(unsigned char)r, (unsigned char)g, (unsigned char)b,
                   (unsigned char)a};
    DrawCircleLines(cx, cy, (float)radius, color);
    return value_Stdint(1);
#endif
}

Value builtin_gfx_fill_circle(Vec *args) {
#ifndef ENABLE_RAYLIB
    (void)args;
    return value_Stdint(0);
#else
    if (!canDrawTarget())
        return value_Stdint(0);
    int cx = valueAsInt(args->entries[0]);
    int cy = valueAsInt(args->entries[1]);
    int radius = valueAsInt(args->entries[2]);
    int r = extractChannel(args->entries[3]);
    int g = extractChannel(args->entries[4]);
    int b = extractChannel(args->entries[5]);
    int a = extractChannel(args->entries[6]);
    if (radius < 0 || r < 0 || g < 0 || b < 0 || a < 0)
        return value_Stdint(0);
    Color color = {(unsigned char)r, (unsigned char)g, (unsigned char)b,
                   (unsigned char)a};
    DrawCircle(cx, cy, (float)radius, color);
    return value_Stdint(1);
#endif
}

Value builtin_gfx_draw_rect(Vec *args) {
#ifndef ENABLE_RAYLIB
    (void)args;
    return value_Stdint(0);
#else
    if (!canDrawTarget())
        return value_Stdint(0);
    int x = valueAsInt(args->entries[0]);
    int y = valueAsInt(args->entries[1]);
    int w = valueAsInt(args->entries[2]);
    int h = valueAsInt(args->entries[3]);
    int r = extractChannel(args->entries[4]);
    int g = extractChannel(args->entries[5]);
    int b = extractChannel(args->entries[6]);
    int a = extractChannel(args->entries[7]);
    if (w < 0 || h < 0 || r < 0 || g < 0 || b < 0 || a < 0)
        return value_Stdint(0);
    Color color = {(unsigned char)r, (unsigned char)g, (unsigned char)b,
                   (unsigned char)a};
    DrawRectangleLines(x, y, w, h, color);
    return value_Stdint(1);
#endif
}

Value builtin_gfx_draw_triangle(Vec *args) {
#ifndef ENABLE_RAYLIB
    (void)args;
    return value_Stdint(0);
#else
    if (!canDrawTarget())
        return value_Stdint(0);
    float x1 = valueAsFloat(args->entries[0]);
    float y1 = valueAsFloat(args->entries[1]);
    float x2 = valueAsFloat(args->entries[2]);
    float y2 = valueAsFloat(args->entries[3]);
    float x3 = valueAsFloat(args->entries[4]);
    float y3 = valueAsFloat(args->entries[5]);
    int r = extractChannel(args->entries[6]);
    int g = extractChannel(args->entries[7]);
    int b = extractChannel(args->entries[8]);
    int a = extractChannel(args->entries[9]);
    if (r < 0 || g < 0 || b < 0 || a < 0)
        return value_Stdint(0);
    Color color = {(unsigned char)r, (unsigned char)g, (unsigned char)b,
                   (unsigned char)a};
    DrawTriangleLines((Vector2){x1, y1}, (Vector2){x2, y2}, (Vector2){x3, y3},
                      color);
    return value_Stdint(1);
#endif
}

Value builtin_gfx_fill_triangle(Vec *args) {
#ifndef ENABLE_RAYLIB
    (void)args;
    return value_Stdint(0);
#else
    if (!canDrawTarget())
        return value_Stdint(0);
    float x1 = valueAsFloat(args->entries[0]);
    float y1 = valueAsFloat(args->entries[1]);
    float x2 = valueAsFloat(args->entries[2]);
    float y2 = valueAsFloat(args->entries[3]);
    float x3 = valueAsFloat(args->entries[4]);
    float y3 = valueAsFloat(args->entries[5]);
    int r = extractChannel(args->entries[6]);
    int g = extractChannel(args->entries[7]);
    int b = extractChannel(args->entries[8]);
    int a = extractChannel(args->entries[9]);
    if (r < 0 || g < 0 || b < 0 || a < 0)
        return value_Stdint(0);
    Color color = {(unsigned char)r, (unsigned char)g, (unsigned char)b,
                   (unsigned char)a};
    DrawTriangle((Vector2){x1, y1}, (Vector2){x2, y2}, (Vector2){x3, y3},
                 color);
    return value_Stdint(1);
#endif
}

Value builtin_gfx_draw_polygon(Vec *args) {
#ifndef ENABLE_RAYLIB
    (void)args;
    return value_Stdint(0);
#else
    if (!canDrawTarget())
        return value_Stdint(0);
    float cx = valueAsFloat(args->entries[0]);
    float cy = valueAsFloat(args->entries[1]);
    int sides = valueAsInt(args->entries[2]);
    float radius = valueAsFloat(args->entries[3]);
    float rotation = valueAsFloat(args->entries[4]);
    int r = extractChannel(args->entries[5]);
    int g = extractChannel(args->entries[6]);
    int b = extractChannel(args->entries[7]);
    int a = extractChannel(args->entries[8]);
    if (sides < 3 || radius <= 0.0f || r < 0 || g < 0 || b < 0 || a < 0)
        return value_Stdint(0);
    Color color = {(unsigned char)r, (unsigned char)g, (unsigned char)b,
                   (unsigned char)a};
    DrawPolyLines((Vector2){cx, cy}, sides, radius, rotation, color);
    return value_Stdint(1);
#endif
}

Value builtin_gfx_fill_polygon(Vec *args) {
#ifndef ENABLE_RAYLIB
    (void)args;
    return value_Stdint(0);
#else
    if (!canDrawTarget())
        return value_Stdint(0);
    float cx = valueAsFloat(args->entries[0]);
    float cy = valueAsFloat(args->entries[1]);
    int sides = valueAsInt(args->entries[2]);
    float radius = valueAsFloat(args->entries[3]);
    float rotation = valueAsFloat(args->entries[4]);
    int r = extractChannel(args->entries[5]);
    int g = extractChannel(args->entries[6]);
    int b = extractChannel(args->entries[7]);
    int a = extractChannel(args->entries[8]);
    if (sides < 3 || radius <= 0.0f || r < 0 || g < 0 || b < 0 || a < 0)
        return value_Stdint(0);
    Color color = {(unsigned char)r, (unsigned char)g, (unsigned char)b,
                   (unsigned char)a};
    DrawPoly((Vector2){cx, cy}, sides, radius, rotation, color);
    return value_Stdint(1);
#endif
}

Value builtin_gfx_draw_arc(Vec *args) {
#ifndef ENABLE_RAYLIB
    (void)args;
    return value_Stdint(0);
#else
    if (!canDrawTarget())
        return value_Stdint(0);
    float cx = valueAsFloat(args->entries[0]);
    float cy = valueAsFloat(args->entries[1]);
    float radius = valueAsFloat(args->entries[2]);
    float startDeg = valueAsFloat(args->entries[3]);
    float endDeg = valueAsFloat(args->entries[4]);
    int segments = valueAsInt(args->entries[5]);
    int r = extractChannel(args->entries[6]);
    int g = extractChannel(args->entries[7]);
    int b = extractChannel(args->entries[8]);
    int a = extractChannel(args->entries[9]);
    if (radius <= 0.0f || segments <= 0 || r < 0 || g < 0 || b < 0 || a < 0)
        return value_Stdint(0);
    Color color = {(unsigned char)r, (unsigned char)g, (unsigned char)b,
                   (unsigned char)a};
    DrawCircleSectorLines((Vector2){cx, cy}, radius, startDeg, endDeg, segments,
                          color);
    return value_Stdint(1);
#endif
}

Value builtin_gfx_draw_rect_rounded(Vec *args) {
#ifndef ENABLE_RAYLIB
    (void)args;
    return value_Stdint(0);
#else
    if (!canDrawTarget())
        return value_Stdint(0);
    float x = valueAsFloat(args->entries[0]);
    float y = valueAsFloat(args->entries[1]);
    float w = valueAsFloat(args->entries[2]);
    float h = valueAsFloat(args->entries[3]);
    float roundness = valueAsFloat(args->entries[4]);
    int segments = valueAsInt(args->entries[5]);
    float lineThick = valueAsFloat(args->entries[6]);
    int r = extractChannel(args->entries[7]);
    int g = extractChannel(args->entries[8]);
    int b = extractChannel(args->entries[9]);
    int a = extractChannel(args->entries[10]);
    if (w <= 0.0f || h <= 0.0f || roundness < 0.0f || roundness > 1.0f ||
        segments <= 0 || lineThick <= 0.0f || r < 0 || g < 0 || b < 0 || a < 0)
        return value_Stdint(0);
    Color color = {(unsigned char)r, (unsigned char)g, (unsigned char)b,
                   (unsigned char)a};
    DrawRectangleRoundedLinesEx((Rectangle){x, y, w, h}, roundness, segments,
                                lineThick, color);
    return value_Stdint(1);
#endif
}

Value builtin_gfx_mat2d_identity(Vec *args) {
    (void)args;
    Opaque *mat = newMat2DOpaque(mat2dIdentityData());
    return value_Opaque(mat);
}

Value builtin_gfx_mat2d_translate(Vec *args) {
    Mat2DData *base = asMat2D(args->entries[0]);
    Mat2DData current = base != NULL ? *base : mat2dIdentityData();
    float dx = valueAsFloat(args->entries[1]);
    float dy = valueAsFloat(args->entries[2]);
    Mat2DData t = {1.0f, 0.0f, dx, 0.0f, 1.0f, dy};
    Opaque *mat = newMat2DOpaque(mat2dMul(current, t));
    return value_Opaque(mat);
}

Value builtin_gfx_mat2d_rotate(Vec *args) {
    Mat2DData *base = asMat2D(args->entries[0]);
    Mat2DData current = base != NULL ? *base : mat2dIdentityData();
    float degrees = valueAsFloat(args->entries[1]);
    float radians = degrees * ((float)M_PI / 180.0f);
    float c = cosf(radians);
    float s = sinf(radians);
    Mat2DData r = {c, -s, 0.0f, s, c, 0.0f};
    Opaque *mat = newMat2DOpaque(mat2dMul(current, r));
    return value_Opaque(mat);
}

Value builtin_gfx_mat2d_scale(Vec *args) {
    Mat2DData *base = asMat2D(args->entries[0]);
    Mat2DData current = base != NULL ? *base : mat2dIdentityData();
    float sx = valueAsFloat(args->entries[1]);
    float sy = valueAsFloat(args->entries[2]);
    Mat2DData s = {sx, 0.0f, 0.0f, 0.0f, sy, 0.0f};
    Opaque *mat = newMat2DOpaque(mat2dMul(current, s));
    return value_Opaque(mat);
}

Value builtin_gfx_mat2d_compose(Vec *args) {
    Mat2DData *a = asMat2D(args->entries[0]);
    Mat2DData *b = asMat2D(args->entries[1]);
    Mat2DData left = a != NULL ? *a : mat2dIdentityData();
    Mat2DData right = b != NULL ? *b : mat2dIdentityData();
    Opaque *mat = newMat2DOpaque(mat2dMul(left, right));
    return value_Opaque(mat);
}

Value builtin_gfx_mat2d_apply_x(Vec *args) {
    Mat2DData *mat = asMat2D(args->entries[0]);
    if (mat == NULL)
        return value_Irrational(0.0);
    float x = valueAsFloat(args->entries[1]);
    float y = valueAsFloat(args->entries[2]);
    float out = mat->m00 * x + mat->m01 * y + mat->m02;
    return value_Irrational((double)out);
}

Value builtin_gfx_mat2d_apply_y(Vec *args) {
    Mat2DData *mat = asMat2D(args->entries[0]);
    if (mat == NULL)
        return value_Irrational(0.0);
    float x = valueAsFloat(args->entries[1]);
    float y = valueAsFloat(args->entries[2]);
    float out = mat->m10 * x + mat->m11 * y + mat->m12;
    return value_Irrational((double)out);
}

Value builtin_gfx_transform3d(Vec *args) {
    Transform3DData t;
    t.px = valueAsFloat(args->entries[0]);
    t.py = valueAsFloat(args->entries[1]);
    t.pz = valueAsFloat(args->entries[2]);
    t.rx = valueAsFloat(args->entries[3]);
    t.ry = valueAsFloat(args->entries[4]);
    t.rz = valueAsFloat(args->entries[5]);
    t.sx = valueAsFloat(args->entries[6]);
    t.sy = valueAsFloat(args->entries[7]);
    t.sz = valueAsFloat(args->entries[8]);
    Opaque *wrapped = newTransform3DOpaque(t);
    return value_Opaque(wrapped);
}

Value builtin_gfx_transform3d_position_x(Vec *args) {
    Transform3DData *t = asTransform3D(args->entries[0]);
    return value_Irrational((double)(t != NULL ? t->px : 0.0f));
}

Value builtin_gfx_transform3d_position_y(Vec *args) {
    Transform3DData *t = asTransform3D(args->entries[0]);
    return value_Irrational((double)(t != NULL ? t->py : 0.0f));
}

Value builtin_gfx_transform3d_position_z(Vec *args) {
    Transform3DData *t = asTransform3D(args->entries[0]);
    return value_Irrational((double)(t != NULL ? t->pz : 0.0f));
}

Value builtin_gfx_transform3d_rotation_x(Vec *args) {
    Transform3DData *t = asTransform3D(args->entries[0]);
    return value_Irrational((double)(t != NULL ? t->rx : 0.0f));
}

Value builtin_gfx_transform3d_rotation_y(Vec *args) {
    Transform3DData *t = asTransform3D(args->entries[0]);
    return value_Irrational((double)(t != NULL ? t->ry : 0.0f));
}

Value builtin_gfx_transform3d_rotation_z(Vec *args) {
    Transform3DData *t = asTransform3D(args->entries[0]);
    return value_Irrational((double)(t != NULL ? t->rz : 0.0f));
}

Value builtin_gfx_transform3d_scale_x(Vec *args) {
    Transform3DData *t = asTransform3D(args->entries[0]);
    return value_Irrational((double)(t != NULL ? t->sx : 0.0f));
}

Value builtin_gfx_transform3d_scale_y(Vec *args) {
    Transform3DData *t = asTransform3D(args->entries[0]);
    return value_Irrational((double)(t != NULL ? t->sy : 0.0f));
}

Value builtin_gfx_transform3d_scale_z(Vec *args) {
    Transform3DData *t = asTransform3D(args->entries[0]);
    return value_Irrational((double)(t != NULL ? t->sz : 0.0f));
}

Value builtin_gfx_load_font(Vec *args) {
#ifndef ENABLE_RAYLIB
    (void)args;
    return failMsg("graphics not available (built without ENABLE_RAYLIB)");
#else
    if (!gfx_state.initialized)
        return failMsg("gfx_load_font: no graphics context");
    SCharVec *buf = listToUtf8(args->entries[0]);
    int save = PROTECT(buf);
    int baseSize = (int)getValue_Stdint(args->entries[1]);
    if (baseSize <= 0) {
        UNPROTECT(save);
        return failMsg("gfx_load_font: base_size must be positive");
    }
    Font loaded = LoadFontEx(buf->entries, baseSize, NULL, 0);
    UNPROTECT(save);
    if (!IsFontValid(loaded)) {
        UnloadFont(loaded);
        return failMsg("gfx_load_font: failed to load font");
    }
    Font *f = NEW_ARRAY(Font, 1);
    *f = loaded;
    Opaque *wrapper = newOpaque(f, opaque_gfx_font_unload, NULL, NULL);
    int wSave = PROTECT(wrapper);
    FontNode *node = NEW_ARRAY(FontNode, 1);
    node->wrapper = wrapper;
    node->next = fontRegistry;
    fontRegistry = node;
    Value opaque = value_Opaque(wrapper);
    Value result = makeTryResult(1, opaque);
    UNPROTECT(wSave);
    return result;
#endif
}

Value builtin_gfx_unload_font(Vec *args) {
#ifndef ENABLE_RAYLIB
    (void)args;
    return value_Stdint(0);
#else
    if (args->entries[0].type != VALUE_TYPE_OPAQUE)
        return value_Stdint(0);
    Opaque *wrapper = args->entries[0].val.opaque;
    fontRegistryRemove(wrapper);
    opaque_gfx_font_unload(wrapper->data);
    wrapper->data = NULL;
    return value_Stdint(1);
#endif
}

Value builtin_gfx_draw_text_font(Vec *args) {
#ifndef ENABLE_RAYLIB
    (void)args;
    return value_Stdint(0);
#else
    if (!canDrawTarget())
        return value_Stdint(0);
    if (args->entries[0].type != VALUE_TYPE_OPAQUE)
        return value_Stdint(0);
    Opaque *wrapper = args->entries[0].val.opaque;
    if (wrapper->data == NULL)
        return value_Stdint(0);
    Font *f = (Font *)wrapper->data;
    SCharVec *buf = listToUtf8(args->entries[1]);
    int save = PROTECT(buf);
    int x = valueAsInt(args->entries[2]);
    int y = valueAsInt(args->entries[3]);
    int size = valueAsInt(args->entries[4]);
    int spacing = valueAsInt(args->entries[5]);
    int r = extractChannel(args->entries[6]);
    int g = extractChannel(args->entries[7]);
    int b = extractChannel(args->entries[8]);
    int a = extractChannel(args->entries[9]);
    if (size <= 0 || r < 0 || g < 0 || b < 0 || a < 0) {
        UNPROTECT(save);
        return value_Stdint(0);
    }
    Color color = {(unsigned char)r, (unsigned char)g, (unsigned char)b,
                   (unsigned char)a};
    Vector2 pos = {(float)x, (float)y};
    DrawTextEx(*f, buf->entries, pos, (float)size, (float)spacing, color);
    UNPROTECT(save);
    return value_Stdint(1);
#endif
}

Value builtin_gfx_measure_text_width(Vec *args) {
#ifndef ENABLE_RAYLIB
    (void)args;
    return failMsg("graphics not available (built without ENABLE_RAYLIB)");
#else
    if (args->entries[0].type != VALUE_TYPE_OPAQUE)
        return failMsg("gfx_measure_text_width: invalid font handle");
    Opaque *wrapper = args->entries[0].val.opaque;
    if (wrapper->data == NULL)
        return failMsg("gfx_measure_text_width: font has been unloaded");
    Font *f = (Font *)wrapper->data;
    SCharVec *buf = listToUtf8(args->entries[1]);
    int save = PROTECT(buf);
    int size = valueAsInt(args->entries[2]);
    int spacing = valueAsInt(args->entries[3]);
    if (size <= 0) {
        UNPROTECT(save);
        return failMsg("gfx_measure_text_width: size must be positive");
    }
    Vector2 v = MeasureTextEx(*f, buf->entries, (float)size, (float)spacing);
    UNPROTECT(save);
    Value w = value_Stdint((int)v.x);
    int wSave = protectValue(w);
    Value result = makeTryResult(1, w);
    UNPROTECT(wSave);
    return result;
#endif
}

// --- Phase D: textures ---

Value builtin_gfx_load_texture(Vec *args) {
#ifndef ENABLE_RAYLIB
    (void)args;
    return failMsg("graphics not available (built without ENABLE_RAYLIB)");
#else
    if (!gfx_state.initialized)
        return failMsg("gfx_load_texture: no graphics context");
    SCharVec *buf = listToUtf8(args->entries[0]);
    int save = PROTECT(buf);
    Texture2D loaded = LoadTexture(buf->entries);
    UNPROTECT(save);
    if (!IsTextureValid(loaded)) {
        UnloadTexture(loaded);
        return failMsg("gfx_load_texture: failed to load texture");
    }
    Texture2D *t = NEW_ARRAY(Texture2D, 1);
    *t = loaded;
    Opaque *wrapper = newOpaque(t, opaque_gfx_texture_unload, NULL, NULL);
    int wSave = PROTECT(wrapper);
    TextureNode *node = NEW_ARRAY(TextureNode, 1);
    node->wrapper = wrapper;
    node->next = textureRegistry;
    textureRegistry = node;
    Value opaque = value_Opaque(wrapper);
    Value result = makeTryResult(1, opaque);
    UNPROTECT(wSave);
    return result;
#endif
}

Value builtin_gfx_unload_texture(Vec *args) {
#ifndef ENABLE_RAYLIB
    (void)args;
    return value_Stdint(0);
#else
    if (args->entries[0].type != VALUE_TYPE_OPAQUE)
        return value_Stdint(0);
    Opaque *wrapper = args->entries[0].val.opaque;
    textureRegistryRemove(wrapper);
    opaque_gfx_texture_unload(wrapper->data);
    wrapper->data = NULL;
    return value_Stdint(1);
#endif
}

Value builtin_gfx_draw_texture(Vec *args) {
#ifndef ENABLE_RAYLIB
    (void)args;
    return value_Stdint(0);
#else
    if (!canDrawTarget())
        return value_Stdint(0);
    if (args->entries[0].type != VALUE_TYPE_OPAQUE)
        return value_Stdint(0);
    Opaque *wrapper = args->entries[0].val.opaque;
    if (wrapper->data == NULL)
        return value_Stdint(0);
    Texture2D *t = (Texture2D *)wrapper->data;
    int x = valueAsInt(args->entries[1]);
    int y = valueAsInt(args->entries[2]);
    int r = extractChannel(args->entries[3]);
    int g = extractChannel(args->entries[4]);
    int b = extractChannel(args->entries[5]);
    int a = extractChannel(args->entries[6]);
    if (r < 0 || g < 0 || b < 0 || a < 0)
        return value_Stdint(0);
    Color tint = {(unsigned char)r, (unsigned char)g, (unsigned char)b,
                  (unsigned char)a};
    DrawTexture(*t, x, y, tint);
    return value_Stdint(1);
#endif
}

Value builtin_gfx_draw_texture_rec(Vec *args) {
#ifndef ENABLE_RAYLIB
    (void)args;
    return value_Stdint(0);
#else
    if (!canDrawTarget())
        return value_Stdint(0);
    if (args->entries[0].type != VALUE_TYPE_OPAQUE)
        return value_Stdint(0);
    Opaque *wrapper = args->entries[0].val.opaque;
    if (wrapper->data == NULL)
        return value_Stdint(0);
    Texture2D *t = (Texture2D *)wrapper->data;
    float sx = (float)valueAsInt(args->entries[1]);
    float sy = (float)valueAsInt(args->entries[2]);
    float sw = (float)valueAsInt(args->entries[3]);
    float sh = (float)valueAsInt(args->entries[4]);
    float dx = (float)valueAsInt(args->entries[5]);
    float dy = (float)valueAsInt(args->entries[6]);
    int r = extractChannel(args->entries[7]);
    int g = extractChannel(args->entries[8]);
    int b = extractChannel(args->entries[9]);
    int a = extractChannel(args->entries[10]);
    if (sw <= 0 || sh <= 0 || r < 0 || g < 0 || b < 0 || a < 0)
        return value_Stdint(0);
    Rectangle src = {sx, sy, sw, sh};
    Vector2 dst = {dx, dy};
    Color tint = {(unsigned char)r, (unsigned char)g, (unsigned char)b,
                  (unsigned char)a};
    DrawTextureRec(*t, src, dst, tint);
    return value_Stdint(1);
#endif
}

Value builtin_gfx_draw_texture_pro(Vec *args) {
#ifndef ENABLE_RAYLIB
    (void)args;
    return value_Stdint(0);
#else
    if (!canDrawTarget())
        return value_Stdint(0);
    if (args->entries[0].type != VALUE_TYPE_OPAQUE)
        return value_Stdint(0);
    Opaque *wrapper = args->entries[0].val.opaque;
    if (wrapper->data == NULL)
        return value_Stdint(0);
    Texture2D *t = (Texture2D *)wrapper->data;
    // source rect: src_x, src_y, src_w, src_h
    float src_x = valueAsFloat(args->entries[1]);
    float src_y = valueAsFloat(args->entries[2]);
    float src_w = valueAsFloat(args->entries[3]);
    float src_h = valueAsFloat(args->entries[4]);
    // destination rect: dst_x, dst_y, dst_w, dst_h
    float dst_x = valueAsFloat(args->entries[5]);
    float dst_y = valueAsFloat(args->entries[6]);
    float dst_w = valueAsFloat(args->entries[7]);
    float dst_h = valueAsFloat(args->entries[8]);
    // origin pivot: ox, oy
    float ox = valueAsFloat(args->entries[9]);
    float oy = valueAsFloat(args->entries[10]);
    // rotation in degrees
    float rotation = valueAsFloat(args->entries[11]);
    // tint: r, g, b, a
    int r = extractChannel(args->entries[12]);
    int g = extractChannel(args->entries[13]);
    int b = extractChannel(args->entries[14]);
    int a = extractChannel(args->entries[15]);
    if (src_w == 0.0f || src_h == 0.0f || r < 0 || g < 0 || b < 0 || a < 0)
        return value_Stdint(0);
    Rectangle src = {src_x, src_y, src_w, src_h};
    Rectangle dst = {dst_x, dst_y, dst_w, dst_h};
    Vector2 origin = {ox, oy};
    Color tint = {(unsigned char)r, (unsigned char)g, (unsigned char)b,
                  (unsigned char)a};
    DrawTexturePro(*t, src, dst, origin, rotation, tint);
    return value_Stdint(1);
#endif
}

Value builtin_gfx_texture_width(Vec *args) {
#ifndef ENABLE_RAYLIB
    (void)args;
    return value_Stdint(0);
#else
    if (args->entries[0].type != VALUE_TYPE_OPAQUE)
        return value_Stdint(0);
    Opaque *wrapper = args->entries[0].val.opaque;
    if (wrapper->data == NULL)
        return value_Stdint(0);
    Texture2D *t = (Texture2D *)wrapper->data;
    return value_Stdint(t->width);
#endif
}

Value builtin_gfx_texture_height(Vec *args) {
#ifndef ENABLE_RAYLIB
    (void)args;
    return value_Stdint(0);
#else
    if (args->entries[0].type != VALUE_TYPE_OPAQUE)
        return value_Stdint(0);
    Opaque *wrapper = args->entries[0].val.opaque;
    if (wrapper->data == NULL)
        return value_Stdint(0);
    Texture2D *t = (Texture2D *)wrapper->data;
    return value_Stdint(t->height);
#endif
}

static HashSymbol *renderTextureSymbol(void) {
    return newSymbol("render_texture");
}

static TcType *makeRenderTextureType(void) {
    return newTcType_Opaque(renderTextureSymbol());
}

static TcType *pushRenderTextureArg(BuiltInArgs *args) {
    TcType *t = makeRenderTextureType();
    int save = PROTECT(t);
    pushBuiltInArgs(args, t);
    UNPROTECT(save);
    return t;
}

Value builtin_gfx_load_render_texture(Vec *args) {
#ifndef ENABLE_RAYLIB
    (void)args;
    return failMsg("graphics not available (built without ENABLE_RAYLIB)");
#else
    if (!gfx_state.initialized)
        return failMsg("gfx_load_render_texture: no graphics context");
    int w = valueAsInt(args->entries[0]);
    int h = valueAsInt(args->entries[1]);
    if (w <= 0 || h <= 0)
        return failMsg(
            "gfx_load_render_texture: width and height must be positive");
    RenderTexture2D loaded = LoadRenderTexture(w, h);
    if (!IsRenderTextureValid(loaded)) {
        UnloadRenderTexture(loaded);
        return failMsg(
            "gfx_load_render_texture: failed to create render texture");
    }
    RenderTexture2D *rt = NEW_ARRAY(RenderTexture2D, 1);
    *rt = loaded;
    Opaque *wrapper =
        newOpaque(rt, opaque_gfx_render_texture_unload, NULL, NULL);
    int wSave = PROTECT(wrapper);
    RenderTextureNode *node = NEW_ARRAY(RenderTextureNode, 1);
    node->wrapper = wrapper;
    node->next = renderTextureRegistry;
    renderTextureRegistry = node;
    Value opaque = value_Opaque(wrapper);
    Value result = makeTryResult(1, opaque);
    UNPROTECT(wSave);
    return result;
#endif
}

Value builtin_gfx_unload_render_texture(Vec *args) {
#ifndef ENABLE_RAYLIB
    (void)args;
    return value_Stdint(0);
#else
    if (args->entries[0].type != VALUE_TYPE_OPAQUE)
        return value_Stdint(0);
    Opaque *wrapper = args->entries[0].val.opaque;
    renderTextureRegistryRemove(wrapper);
    opaque_gfx_render_texture_unload(wrapper->data);
    wrapper->data = NULL;
    return value_Stdint(1);
#endif
}

Value builtin_gfx_begin_texture_mode(Vec *args) {
#ifndef ENABLE_RAYLIB
    (void)args;
    return value_Stdint(0);
#else
    if (!gfx_state.initialized || gfx_state.in_texture_mode)
        return value_Stdint(0);
    if (args->entries[0].type != VALUE_TYPE_OPAQUE)
        return value_Stdint(0);
    Opaque *wrapper = args->entries[0].val.opaque;
    if (wrapper->data == NULL)
        return value_Stdint(0);
    RenderTexture2D *rt = (RenderTexture2D *)wrapper->data;
    BeginTextureMode(*rt);
    gfx_state.in_texture_mode = true;
    return value_Stdint(1);
#endif
}

Value builtin_gfx_end_texture_mode(Vec *args) {
    (void)args;
#ifndef ENABLE_RAYLIB
    return value_Stdint(0);
#else
    if (!gfx_state.in_texture_mode)
        return value_Stdint(0);
    EndTextureMode();
    gfx_state.in_texture_mode = false;
    return value_Stdint(1);
#endif
}

// Draw a render texture's color attachment to the current frame.
// The Y-axis is flipped here to correct for raylib's upside-down FBO
// convention.
Value builtin_gfx_draw_render_texture(Vec *args) {
#ifndef ENABLE_RAYLIB
    (void)args;
    return value_Stdint(0);
#else
    if (!canDrawTarget())
        return value_Stdint(0);
    if (args->entries[0].type != VALUE_TYPE_OPAQUE)
        return value_Stdint(0);
    Opaque *wrapper = args->entries[0].val.opaque;
    if (wrapper->data == NULL)
        return value_Stdint(0);
    RenderTexture2D *rt = (RenderTexture2D *)wrapper->data;
    int dx = valueAsInt(args->entries[1]);
    int dy = valueAsInt(args->entries[2]);
    int r = extractChannel(args->entries[3]);
    int g = extractChannel(args->entries[4]);
    int b = extractChannel(args->entries[5]);
    int a = extractChannel(args->entries[6]);
    if (r < 0 || g < 0 || b < 0 || a < 0)
        return value_Stdint(0);
    // Flip source vertically: raylib renders FBOs upside-down.
    float texW = (float)rt->texture.width;
    float texH = (float)rt->texture.height;
    Rectangle src = {0.0f, 0.0f, texW, -texH};
    Vector2 dst = {(float)dx, (float)dy};
    Color tint = {(unsigned char)r, (unsigned char)g, (unsigned char)b,
                  (unsigned char)a};
    DrawTextureRec(rt->texture, src, dst, tint);
    return value_Stdint(1);
#endif
}

Value builtin_gfx_load_shader(Vec *args) {
#ifndef ENABLE_RAYLIB
    (void)args;
    return failMsg("graphics not available (built without ENABLE_RAYLIB)");
#else
    if (!gfx_state.initialized)
        return failMsg("gfx_load_shader: no graphics context");

    SCharVec *vertexPath = listToUtf8(args->entries[0]);
    int save = PROTECT(vertexPath);
    SCharVec *fragmentPath = listToUtf8(args->entries[1]);
    PROTECT(fragmentPath);

    Shader loaded = LoadShader(vertexPath->entries, fragmentPath->entries);
    if (!IsShaderValid(loaded)) {
        UNPROTECT(save);
        return failMsg("gfx_load_shader: failed to load shader");
    }

    Shader *shader = NEW_ARRAY(Shader, 1);
    *shader = loaded;
    Opaque *wrapper = newOpaque(shader, opaque_gfx_shader_unload, NULL, NULL);
    int wSave = PROTECT(wrapper);

    ShaderNode *node = NEW_ARRAY(ShaderNode, 1);
    node->wrapper = wrapper;
    node->next = shaderRegistry;
    shaderRegistry = node;

    Value opaque = value_Opaque(wrapper);
    Value result = makeTryResult(1, opaque);
    UNPROTECT(wSave);
    UNPROTECT(save);
    return result;
#endif
}

Value builtin_gfx_unload_shader(Vec *args) {
#ifndef ENABLE_RAYLIB
    (void)args;
    return value_Stdint(0);
#else
    if (args->entries[0].type != VALUE_TYPE_OPAQUE)
        return value_Stdint(0);
    Opaque *wrapper = args->entries[0].val.opaque;
    shaderRegistryRemove(wrapper);
    opaque_gfx_shader_unload(wrapper->data);
    wrapper->data = NULL;
    return value_Stdint(1);
#endif
}

Value builtin_gfx_begin_shader_mode(Vec *args) {
#ifndef ENABLE_RAYLIB
    (void)args;
    return value_Stdint(0);
#else
    if (!gfx_state.initialized || gfx_state.in_shader_mode || !canDrawTarget())
        return value_Stdint(0);
    if (args->entries[0].type != VALUE_TYPE_OPAQUE)
        return value_Stdint(0);
    Opaque *wrapper = args->entries[0].val.opaque;
    if (wrapper->data == NULL)
        return value_Stdint(0);
    Shader *shader = (Shader *)wrapper->data;
    BeginShaderMode(*shader);
    gfx_state.in_shader_mode = true;
    return value_Stdint(1);
#endif
}

Value builtin_gfx_end_shader_mode(Vec *args) {
    (void)args;
#ifndef ENABLE_RAYLIB
    return value_Stdint(0);
#else
    if (!gfx_state.in_shader_mode)
        return value_Stdint(0);
    EndShaderMode();
    gfx_state.in_shader_mode = false;
    return value_Stdint(1);
#endif
}

Value builtin_gfx_set_shader_int(Vec *args) {
#ifndef ENABLE_RAYLIB
    (void)args;
    return value_Stdint(0);
#else
    if (!gfx_state.initialized)
        return value_Stdint(0);
    if (args->entries[0].type != VALUE_TYPE_OPAQUE)
        return value_Stdint(0);
    Opaque *wrapper = args->entries[0].val.opaque;
    if (wrapper->data == NULL)
        return value_Stdint(0);
    Shader *shader = (Shader *)wrapper->data;
    SCharVec *uniformName = listToUtf8(args->entries[1]);
    int save = PROTECT(uniformName);
    int location = GetShaderLocation(*shader, uniformName->entries);
    UNPROTECT(save);
    if (location < 0)
        return value_Stdint(0);
    int value = valueAsInt(args->entries[2]);
    SetShaderValue(*shader, location, &value, SHADER_UNIFORM_INT);
    return value_Stdint(1);
#endif
}

Value builtin_gfx_set_shader_float(Vec *args) {
#ifndef ENABLE_RAYLIB
    (void)args;
    return value_Stdint(0);
#else
    if (!gfx_state.initialized)
        return value_Stdint(0);
    if (args->entries[0].type != VALUE_TYPE_OPAQUE)
        return value_Stdint(0);
    Opaque *wrapper = args->entries[0].val.opaque;
    if (wrapper->data == NULL)
        return value_Stdint(0);
    Shader *shader = (Shader *)wrapper->data;
    SCharVec *uniformName = listToUtf8(args->entries[1]);
    int save = PROTECT(uniformName);
    int location = GetShaderLocation(*shader, uniformName->entries);
    UNPROTECT(save);
    if (location < 0)
        return value_Stdint(0);
    float value = valueAsFloat(args->entries[2]);
    SetShaderValue(*shader, location, &value, SHADER_UNIFORM_FLOAT);
    return value_Stdint(1);
#endif
}

Value builtin_gfx_set_shader_vec2(Vec *args) {
#ifndef ENABLE_RAYLIB
    (void)args;
    return value_Stdint(0);
#else
    if (!gfx_state.initialized)
        return value_Stdint(0);
    if (args->entries[0].type != VALUE_TYPE_OPAQUE)
        return value_Stdint(0);
    Opaque *wrapper = args->entries[0].val.opaque;
    if (wrapper->data == NULL)
        return value_Stdint(0);
    Shader *shader = (Shader *)wrapper->data;
    SCharVec *uniformName = listToUtf8(args->entries[1]);
    int save = PROTECT(uniformName);
    int location = GetShaderLocation(*shader, uniformName->entries);
    UNPROTECT(save);
    if (location < 0)
        return value_Stdint(0);
    float values[2] = {valueAsFloat(args->entries[2]),
                       valueAsFloat(args->entries[3])};
    SetShaderValue(*shader, location, values, SHADER_UNIFORM_VEC2);
    return value_Stdint(1);
#endif
}

Value builtin_gfx_set_shader_vec3(Vec *args) {
#ifndef ENABLE_RAYLIB
    (void)args;
    return value_Stdint(0);
#else
    if (!gfx_state.initialized)
        return value_Stdint(0);
    if (args->entries[0].type != VALUE_TYPE_OPAQUE)
        return value_Stdint(0);
    Opaque *wrapper = args->entries[0].val.opaque;
    if (wrapper->data == NULL)
        return value_Stdint(0);
    Shader *shader = (Shader *)wrapper->data;
    SCharVec *uniformName = listToUtf8(args->entries[1]);
    int save = PROTECT(uniformName);
    int location = GetShaderLocation(*shader, uniformName->entries);
    UNPROTECT(save);
    if (location < 0)
        return value_Stdint(0);
    float values[3] = {valueAsFloat(args->entries[2]),
                       valueAsFloat(args->entries[3]),
                       valueAsFloat(args->entries[4])};
    SetShaderValue(*shader, location, values, SHADER_UNIFORM_VEC3);
    return value_Stdint(1);
#endif
}

Value builtin_gfx_begin_mode_2d(Vec *args) {
#ifndef ENABLE_RAYLIB
    (void)args;
    return value_Stdint(0);
#else
    if (!gfx_state.initialized || gfx_state.in_camera_mode ||
        gfx_state.in_3d_mode || !canDrawTarget())
        return value_Stdint(0);
    float targetX = valueAsFloat(args->entries[0]);
    float targetY = valueAsFloat(args->entries[1]);
    float offsetX = valueAsFloat(args->entries[2]);
    float offsetY = valueAsFloat(args->entries[3]);
    float rotation = valueAsFloat(args->entries[4]);
    float zoom = valueAsFloat(args->entries[5]);
    if (zoom <= 0.0f)
        return value_Stdint(0);
    Camera2D camera = {{offsetX, offsetY}, {targetX, targetY}, rotation, zoom};
    BeginMode2D(camera);
    gfx_state.in_camera_mode = true;
    return value_Stdint(1);
#endif
}

Value builtin_gfx_end_mode_2d(Vec *args) {
    (void)args;
#ifndef ENABLE_RAYLIB
    return value_Stdint(0);
#else
    if (!gfx_state.in_camera_mode)
        return value_Stdint(0);
    EndMode2D();
    gfx_state.in_camera_mode = false;
    return value_Stdint(1);
#endif
}

Value builtin_gfx_begin_mode_3d(Vec *args) {
#ifndef ENABLE_RAYLIB
    (void)args;
    return value_Stdint(0);
#else
    if (!gfx_state.initialized || gfx_state.in_camera_mode ||
        gfx_state.in_3d_mode || !canDrawTarget())
        return value_Stdint(0);

    float posX = valueAsFloat(args->entries[0]);
    float posY = valueAsFloat(args->entries[1]);
    float posZ = valueAsFloat(args->entries[2]);
    float targetX = valueAsFloat(args->entries[3]);
    float targetY = valueAsFloat(args->entries[4]);
    float targetZ = valueAsFloat(args->entries[5]);
    float upX = valueAsFloat(args->entries[6]);
    float upY = valueAsFloat(args->entries[7]);
    float upZ = valueAsFloat(args->entries[8]);
    float fovy = valueAsFloat(args->entries[9]);
    int projection = valueAsInt(args->entries[10]);

    if (fovy <= 0.0f)
        return value_Stdint(0);
    if (projection != CAMERA_PERSPECTIVE && projection != CAMERA_ORTHOGRAPHIC)
        return value_Stdint(0);

    Camera3D camera = {
        .position = {posX, posY, posZ},
        .target = {targetX, targetY, targetZ},
        .up = {upX, upY, upZ},
        .fovy = fovy,
        .projection = projection,
    };

    BeginMode3D(camera);
    gfx_state.in_3d_mode = true;
    return value_Stdint(1);
#endif
}

Value builtin_gfx_end_mode_3d(Vec *args) {
    (void)args;
#ifndef ENABLE_RAYLIB
    return value_Stdint(0);
#else
    if (!gfx_state.in_3d_mode)
        return value_Stdint(0);
    EndMode3D();
    gfx_state.in_3d_mode = false;
    return value_Stdint(1);
#endif
}

Value builtin_gfx_draw_cube(Vec *args) {
#ifndef ENABLE_RAYLIB
    (void)args;
    return value_Stdint(0);
#else
    if (!gfx_state.in_3d_mode)
        return value_Stdint(0);

    float centerX = valueAsFloat(args->entries[0]);
    float centerY = valueAsFloat(args->entries[1]);
    float centerZ = valueAsFloat(args->entries[2]);
    float sizeX = valueAsFloat(args->entries[3]);
    float sizeY = valueAsFloat(args->entries[4]);
    float sizeZ = valueAsFloat(args->entries[5]);
    int r = extractChannel(args->entries[6]);
    int g = extractChannel(args->entries[7]);
    int b = extractChannel(args->entries[8]);
    int a = extractChannel(args->entries[9]);

    if (sizeX <= 0.0f || sizeY <= 0.0f || sizeZ <= 0.0f || r < 0 || g < 0 ||
        b < 0 || a < 0)
        return value_Stdint(0);

    Color color = {(unsigned char)r, (unsigned char)g, (unsigned char)b,
                   (unsigned char)a};
    DrawCube((Vector3){centerX, centerY, centerZ}, sizeX, sizeY, sizeZ, color);
    return value_Stdint(1);
#endif
}

Value builtin_gfx_draw_cube_wires(Vec *args) {
#ifndef ENABLE_RAYLIB
    (void)args;
    return value_Stdint(0);
#else
    if (!gfx_state.in_3d_mode)
        return value_Stdint(0);

    float centerX = valueAsFloat(args->entries[0]);
    float centerY = valueAsFloat(args->entries[1]);
    float centerZ = valueAsFloat(args->entries[2]);
    float sizeX = valueAsFloat(args->entries[3]);
    float sizeY = valueAsFloat(args->entries[4]);
    float sizeZ = valueAsFloat(args->entries[5]);
    int r = extractChannel(args->entries[6]);
    int g = extractChannel(args->entries[7]);
    int b = extractChannel(args->entries[8]);
    int a = extractChannel(args->entries[9]);

    if (sizeX <= 0.0f || sizeY <= 0.0f || sizeZ <= 0.0f || r < 0 || g < 0 ||
        b < 0 || a < 0)
        return value_Stdint(0);

    Color color = {(unsigned char)r, (unsigned char)g, (unsigned char)b,
                   (unsigned char)a};
    DrawCubeWires((Vector3){centerX, centerY, centerZ}, sizeX, sizeY, sizeZ,
                  color);
    return value_Stdint(1);
#endif
}

Value builtin_gfx_draw_grid(Vec *args) {
#ifndef ENABLE_RAYLIB
    (void)args;
    return value_Stdint(0);
#else
    if (!gfx_state.in_3d_mode)
        return value_Stdint(0);

    int slices = valueAsInt(args->entries[0]);
    float spacing = valueAsFloat(args->entries[1]);
    if (slices <= 0 || spacing <= 0.0f)
        return value_Stdint(0);

    DrawGrid(slices, spacing);
    return value_Stdint(1);
#endif
}

Value builtin_gfx_draw_sphere(Vec *args) {
#ifndef ENABLE_RAYLIB
    (void)args;
    return value_Stdint(0);
#else
    if (!gfx_state.in_3d_mode)
        return value_Stdint(0);

    float centerX = valueAsFloat(args->entries[0]);
    float centerY = valueAsFloat(args->entries[1]);
    float centerZ = valueAsFloat(args->entries[2]);
    float radius = valueAsFloat(args->entries[3]);
    int r = extractChannel(args->entries[4]);
    int g = extractChannel(args->entries[5]);
    int b = extractChannel(args->entries[6]);
    int a = extractChannel(args->entries[7]);

    if (radius <= 0.0f || r < 0 || g < 0 || b < 0 || a < 0)
        return value_Stdint(0);

    Color color = {(unsigned char)r, (unsigned char)g, (unsigned char)b,
                   (unsigned char)a};
    DrawSphere((Vector3){centerX, centerY, centerZ}, radius, color);
    return value_Stdint(1);
#endif
}

Value builtin_gfx_draw_sphere_wires(Vec *args) {
#ifndef ENABLE_RAYLIB
    (void)args;
    return value_Stdint(0);
#else
    if (!gfx_state.in_3d_mode)
        return value_Stdint(0);

    float centerX = valueAsFloat(args->entries[0]);
    float centerY = valueAsFloat(args->entries[1]);
    float centerZ = valueAsFloat(args->entries[2]);
    float radius = valueAsFloat(args->entries[3]);
    int rings = valueAsInt(args->entries[4]);
    int slices = valueAsInt(args->entries[5]);
    int r = extractChannel(args->entries[6]);
    int g = extractChannel(args->entries[7]);
    int b = extractChannel(args->entries[8]);
    int a = extractChannel(args->entries[9]);

    if (radius <= 0.0f || rings <= 0 || slices <= 0 || r < 0 || g < 0 ||
        b < 0 || a < 0)
        return value_Stdint(0);

    Color color = {(unsigned char)r, (unsigned char)g, (unsigned char)b,
                   (unsigned char)a};
    DrawSphereWires((Vector3){centerX, centerY, centerZ}, radius, rings, slices,
                    color);
    return value_Stdint(1);
#endif
}

Value builtin_gfx_draw_cylinder(Vec *args) {
#ifndef ENABLE_RAYLIB
    (void)args;
    return value_Stdint(0);
#else
    if (!gfx_state.in_3d_mode)
        return value_Stdint(0);

    float posX = valueAsFloat(args->entries[0]);
    float posY = valueAsFloat(args->entries[1]);
    float posZ = valueAsFloat(args->entries[2]);
    float radiusTop = valueAsFloat(args->entries[3]);
    float radiusBottom = valueAsFloat(args->entries[4]);
    float height = valueAsFloat(args->entries[5]);
    int slices = valueAsInt(args->entries[6]);
    int r = extractChannel(args->entries[7]);
    int g = extractChannel(args->entries[8]);
    int b = extractChannel(args->entries[9]);
    int a = extractChannel(args->entries[10]);

    if (radiusTop < 0.0f || radiusBottom < 0.0f ||
        (radiusTop == 0.0f && radiusBottom == 0.0f) || height <= 0.0f ||
        slices < 3 || r < 0 || g < 0 || b < 0 || a < 0)
        return value_Stdint(0);

    Color color = {(unsigned char)r, (unsigned char)g, (unsigned char)b,
                   (unsigned char)a};
    DrawCylinder((Vector3){posX, posY, posZ}, radiusTop, radiusBottom, height,
                 slices, color);
    return value_Stdint(1);
#endif
}

Value builtin_gfx_draw_cylinder_wires(Vec *args) {
#ifndef ENABLE_RAYLIB
    (void)args;
    return value_Stdint(0);
#else
    if (!gfx_state.in_3d_mode)
        return value_Stdint(0);

    float posX = valueAsFloat(args->entries[0]);
    float posY = valueAsFloat(args->entries[1]);
    float posZ = valueAsFloat(args->entries[2]);
    float radiusTop = valueAsFloat(args->entries[3]);
    float radiusBottom = valueAsFloat(args->entries[4]);
    float height = valueAsFloat(args->entries[5]);
    int slices = valueAsInt(args->entries[6]);
    int r = extractChannel(args->entries[7]);
    int g = extractChannel(args->entries[8]);
    int b = extractChannel(args->entries[9]);
    int a = extractChannel(args->entries[10]);

    if (radiusTop < 0.0f || radiusBottom < 0.0f ||
        (radiusTop == 0.0f && radiusBottom == 0.0f) || height <= 0.0f ||
        slices < 3 || r < 0 || g < 0 || b < 0 || a < 0)
        return value_Stdint(0);

    Color color = {(unsigned char)r, (unsigned char)g, (unsigned char)b,
                   (unsigned char)a};
    DrawCylinderWires((Vector3){posX, posY, posZ}, radiusTop, radiusBottom,
                      height, slices, color);
    return value_Stdint(1);
#endif
}

Value builtin_gfx_draw_plane(Vec *args) {
#ifndef ENABLE_RAYLIB
    (void)args;
    return value_Stdint(0);
#else
    if (!gfx_state.in_3d_mode)
        return value_Stdint(0);

    float centerX = valueAsFloat(args->entries[0]);
    float centerY = valueAsFloat(args->entries[1]);
    float centerZ = valueAsFloat(args->entries[2]);
    float sizeX = valueAsFloat(args->entries[3]);
    float sizeZ = valueAsFloat(args->entries[4]);
    int r = extractChannel(args->entries[5]);
    int g = extractChannel(args->entries[6]);
    int b = extractChannel(args->entries[7]);
    int a = extractChannel(args->entries[8]);

    if (sizeX <= 0.0f || sizeZ <= 0.0f || r < 0 || g < 0 || b < 0 || a < 0)
        return value_Stdint(0);

    Color color = {(unsigned char)r, (unsigned char)g, (unsigned char)b,
                   (unsigned char)a};
    DrawPlane((Vector3){centerX, centerY, centerZ}, (Vector2){sizeX, sizeZ},
              color);
    return value_Stdint(1);
#endif
}

Value builtin_gfx_draw_line_3d(Vec *args) {
#ifndef ENABLE_RAYLIB
    (void)args;
    return value_Stdint(0);
#else
    if (!gfx_state.in_3d_mode)
        return value_Stdint(0);

    float startX = valueAsFloat(args->entries[0]);
    float startY = valueAsFloat(args->entries[1]);
    float startZ = valueAsFloat(args->entries[2]);
    float endX = valueAsFloat(args->entries[3]);
    float endY = valueAsFloat(args->entries[4]);
    float endZ = valueAsFloat(args->entries[5]);
    int r = extractChannel(args->entries[6]);
    int g = extractChannel(args->entries[7]);
    int b = extractChannel(args->entries[8]);
    int a = extractChannel(args->entries[9]);

    if (r < 0 || g < 0 || b < 0 || a < 0)
        return value_Stdint(0);

    Color color = {(unsigned char)r, (unsigned char)g, (unsigned char)b,
                   (unsigned char)a};
    DrawLine3D((Vector3){startX, startY, startZ}, (Vector3){endX, endY, endZ},
               color);
    return value_Stdint(1);
#endif
}

Value builtin_gfx_load_model(Vec *args) {
#ifndef ENABLE_RAYLIB
    (void)args;
    return failMsg("graphics not available (built without ENABLE_RAYLIB)");
#else
    if (!gfx_state.initialized)
        return failMsg("gfx_load_model: no graphics context");
    SCharVec *path = listToUtf8(args->entries[0]);
    int save = PROTECT(path);
    Model loaded = LoadModel(path->entries);
    UNPROTECT(save);
    if (loaded.meshCount <= 0)
        return failMsg("gfx_load_model: failed to load model");

    Model *model = NEW_ARRAY(Model, 1);
    *model = loaded;
    Opaque *wrapper = newOpaque(model, opaque_gfx_model_unload, NULL, NULL);
    int wSave = PROTECT(wrapper);
    ModelNode *node = NEW_ARRAY(ModelNode, 1);
    node->wrapper = wrapper;
    node->next = modelRegistry;
    modelRegistry = node;
    Value result = makeTryResult(1, value_Opaque(wrapper));
    UNPROTECT(wSave);
    return result;
#endif
}

Value builtin_gfx_unload_model(Vec *args) {
#ifndef ENABLE_RAYLIB
    (void)args;
    return value_Stdint(0);
#else
    if (args->entries[0].type != VALUE_TYPE_OPAQUE)
        return value_Stdint(0);
    Opaque *wrapper = args->entries[0].val.opaque;
    modelRegistryRemove(wrapper);
    opaque_gfx_model_unload(wrapper->data);
    wrapper->data = NULL;
    return value_Stdint(1);
#endif
}

Value builtin_gfx_draw_model(Vec *args) {
#ifndef ENABLE_RAYLIB
    (void)args;
    return value_Stdint(0);
#else
    if (!gfx_state.in_3d_mode)
        return value_Stdint(0);
    if (args->entries[0].type != VALUE_TYPE_OPAQUE)
        return value_Stdint(0);
    Opaque *wrapper = args->entries[0].val.opaque;
    if (wrapper->data == NULL)
        return value_Stdint(0);

    Model *model = (Model *)wrapper->data;
    float posX = valueAsFloat(args->entries[1]);
    float posY = valueAsFloat(args->entries[2]);
    float posZ = valueAsFloat(args->entries[3]);
    float scale = valueAsFloat(args->entries[4]);
    int r = extractChannel(args->entries[5]);
    int g = extractChannel(args->entries[6]);
    int b = extractChannel(args->entries[7]);
    int a = extractChannel(args->entries[8]);

    if (scale <= 0.0f || r < 0 || g < 0 || b < 0 || a < 0)
        return value_Stdint(0);

    Color tint = {(unsigned char)r, (unsigned char)g, (unsigned char)b,
                  (unsigned char)a};
    DrawModel(*model, (Vector3){posX, posY, posZ}, scale, tint);
    return value_Stdint(1);
#endif
}

Value builtin_gfx_draw_model_wires(Vec *args) {
#ifndef ENABLE_RAYLIB
    (void)args;
    return value_Stdint(0);
#else
    if (!gfx_state.in_3d_mode)
        return value_Stdint(0);
    if (args->entries[0].type != VALUE_TYPE_OPAQUE)
        return value_Stdint(0);
    Opaque *wrapper = args->entries[0].val.opaque;
    if (wrapper->data == NULL)
        return value_Stdint(0);

    Model *model = (Model *)wrapper->data;
    float posX = valueAsFloat(args->entries[1]);
    float posY = valueAsFloat(args->entries[2]);
    float posZ = valueAsFloat(args->entries[3]);
    float scale = valueAsFloat(args->entries[4]);
    int r = extractChannel(args->entries[5]);
    int g = extractChannel(args->entries[6]);
    int b = extractChannel(args->entries[7]);
    int a = extractChannel(args->entries[8]);

    if (scale <= 0.0f || r < 0 || g < 0 || b < 0 || a < 0)
        return value_Stdint(0);

    Color tint = {(unsigned char)r, (unsigned char)g, (unsigned char)b,
                  (unsigned char)a};
    DrawModelWires(*model, (Vector3){posX, posY, posZ}, scale, tint);
    return value_Stdint(1);
#endif
}

Value builtin_gfx_audio_open(Vec *args) {
    (void)args;
#ifndef ENABLE_RAYLIB
    return value_Stdint(0);
#else
    if (gfx_state.audio_initialized)
        return value_Stdint(0);
    InitAudioDevice();
    if (!IsAudioDeviceReady())
        return value_Stdint(0);
    gfx_state.audio_initialized = true;
    return value_Stdint(1);
#endif
}

Value builtin_gfx_audio_close(Vec *args) {
    (void)args;
#ifndef ENABLE_RAYLIB
    return value_Stdint(0);
#else
    if (!gfx_state.audio_initialized)
        return value_Stdint(0);
    musicRegistryDrain();
    soundRegistryDrain();
    CloseAudioDevice();
    gfx_state.audio_initialized = false;
    return value_Stdint(1);
#endif
}

Value builtin_gfx_load_sound(Vec *args) {
#ifndef ENABLE_RAYLIB
    (void)args;
    return failMsg("graphics not available (built without ENABLE_RAYLIB)");
#else
    if (!gfx_state.audio_initialized)
        return failMsg("gfx_load_sound: audio device not open");
    SCharVec *path = listToUtf8(args->entries[0]);
    int save = PROTECT(path);
    Sound loaded = LoadSound(path->entries);
    UNPROTECT(save);
    if (loaded.frameCount == 0) {
        return failMsg("gfx_load_sound: failed to load sound");
    }
    Sound *snd = NEW_ARRAY(Sound, 1);
    *snd = loaded;
    Opaque *wrapper = newOpaque(snd, opaque_gfx_sound_unload, NULL, NULL);
    int wSave = PROTECT(wrapper);
    SoundNode *node = NEW_ARRAY(SoundNode, 1);
    node->wrapper = wrapper;
    node->next = soundRegistry;
    soundRegistry = node;
    Value result = makeTryResult(1, value_Opaque(wrapper));
    UNPROTECT(wSave);
    return result;
#endif
}

Value builtin_gfx_unload_sound(Vec *args) {
#ifndef ENABLE_RAYLIB
    (void)args;
    return value_Stdint(0);
#else
    if (args->entries[0].type != VALUE_TYPE_OPAQUE)
        return value_Stdint(0);
    Opaque *wrapper = args->entries[0].val.opaque;
    soundRegistryRemove(wrapper);
    opaque_gfx_sound_unload(wrapper->data);
    wrapper->data = NULL;
    return value_Stdint(1);
#endif
}

Value builtin_gfx_play_sound(Vec *args) {
#ifndef ENABLE_RAYLIB
    (void)args;
    return value_Stdint(0);
#else
    if (!gfx_state.audio_initialized)
        return value_Stdint(0);
    if (args->entries[0].type != VALUE_TYPE_OPAQUE)
        return value_Stdint(0);
    Opaque *wrapper = args->entries[0].val.opaque;
    if (wrapper->data == NULL)
        return value_Stdint(0);
    Sound *snd = (Sound *)wrapper->data;
    PlaySound(*snd);
    return value_Stdint(1);
#endif
}

Value builtin_gfx_stop_sound(Vec *args) {
#ifndef ENABLE_RAYLIB
    (void)args;
    return value_Stdint(0);
#else
    if (!gfx_state.audio_initialized)
        return value_Stdint(0);
    if (args->entries[0].type != VALUE_TYPE_OPAQUE)
        return value_Stdint(0);
    Opaque *wrapper = args->entries[0].val.opaque;
    if (wrapper->data == NULL)
        return value_Stdint(0);
    Sound *snd = (Sound *)wrapper->data;
    StopSound(*snd);
    return value_Stdint(1);
#endif
}

Value builtin_gfx_set_sound_volume(Vec *args) {
#ifndef ENABLE_RAYLIB
    (void)args;
    return value_Stdint(0);
#else
    if (!gfx_state.audio_initialized)
        return value_Stdint(0);
    if (args->entries[0].type != VALUE_TYPE_OPAQUE)
        return value_Stdint(0);
    Opaque *wrapper = args->entries[0].val.opaque;
    if (wrapper->data == NULL)
        return value_Stdint(0);
    int percent = valueAsInt(args->entries[1]);
    if (percent < 0 || percent > 100)
        return value_Stdint(0);
    Sound *snd = (Sound *)wrapper->data;
    SetSoundVolume(*snd, (float)percent / 100.0f);
    return value_Stdint(1);
#endif
}

Value builtin_gfx_is_sound_playing(Vec *args) {
#ifndef ENABLE_RAYLIB
    (void)args;
    return value_Stdint(0);
#else
    if (!gfx_state.audio_initialized)
        return value_Stdint(0);
    if (args->entries[0].type != VALUE_TYPE_OPAQUE)
        return value_Stdint(0);
    Opaque *wrapper = args->entries[0].val.opaque;
    if (wrapper->data == NULL)
        return value_Stdint(0);
    Sound *snd = (Sound *)wrapper->data;
    return value_Stdint(IsSoundPlaying(*snd) ? 1 : 0);
#endif
}

Value builtin_gfx_load_music(Vec *args) {
#ifndef ENABLE_RAYLIB
    (void)args;
    return failMsg("graphics not available (built without ENABLE_RAYLIB)");
#else
    if (!gfx_state.audio_initialized)
        return failMsg("gfx_load_music: audio device not open");
    SCharVec *path = listToUtf8(args->entries[0]);
    int save = PROTECT(path);
    Music loaded = LoadMusicStream(path->entries);
    UNPROTECT(save);
    if (loaded.ctxData == NULL)
        return failMsg("gfx_load_music: failed to load music");
    Music *music = NEW_ARRAY(Music, 1);
    *music = loaded;
    Opaque *wrapper = newOpaque(music, opaque_gfx_music_unload, NULL, NULL);
    int wSave = PROTECT(wrapper);
    MusicNode *node = NEW_ARRAY(MusicNode, 1);
    node->wrapper = wrapper;
    node->next = musicRegistry;
    musicRegistry = node;
    Value result = makeTryResult(1, value_Opaque(wrapper));
    UNPROTECT(wSave);
    return result;
#endif
}

Value builtin_gfx_unload_music(Vec *args) {
#ifndef ENABLE_RAYLIB
    (void)args;
    return value_Stdint(0);
#else
    if (args->entries[0].type != VALUE_TYPE_OPAQUE)
        return value_Stdint(0);
    Opaque *wrapper = args->entries[0].val.opaque;
    musicRegistryRemove(wrapper);
    opaque_gfx_music_unload(wrapper->data);
    wrapper->data = NULL;
    return value_Stdint(1);
#endif
}

Value builtin_gfx_play_music(Vec *args) {
#ifndef ENABLE_RAYLIB
    (void)args;
    return value_Stdint(0);
#else
    if (!gfx_state.audio_initialized)
        return value_Stdint(0);
    if (args->entries[0].type != VALUE_TYPE_OPAQUE)
        return value_Stdint(0);
    Opaque *wrapper = args->entries[0].val.opaque;
    if (wrapper->data == NULL)
        return value_Stdint(0);
    Music *music = (Music *)wrapper->data;
    PlayMusicStream(*music);
    return value_Stdint(1);
#endif
}

Value builtin_gfx_pause_music(Vec *args) {
#ifndef ENABLE_RAYLIB
    (void)args;
    return value_Stdint(0);
#else
    if (!gfx_state.audio_initialized)
        return value_Stdint(0);
    if (args->entries[0].type != VALUE_TYPE_OPAQUE)
        return value_Stdint(0);
    Opaque *wrapper = args->entries[0].val.opaque;
    if (wrapper->data == NULL)
        return value_Stdint(0);
    Music *music = (Music *)wrapper->data;
    PauseMusicStream(*music);
    return value_Stdint(1);
#endif
}

Value builtin_gfx_resume_music(Vec *args) {
#ifndef ENABLE_RAYLIB
    (void)args;
    return value_Stdint(0);
#else
    if (!gfx_state.audio_initialized)
        return value_Stdint(0);
    if (args->entries[0].type != VALUE_TYPE_OPAQUE)
        return value_Stdint(0);
    Opaque *wrapper = args->entries[0].val.opaque;
    if (wrapper->data == NULL)
        return value_Stdint(0);
    Music *music = (Music *)wrapper->data;
    ResumeMusicStream(*music);
    return value_Stdint(1);
#endif
}

Value builtin_gfx_stop_music(Vec *args) {
#ifndef ENABLE_RAYLIB
    (void)args;
    return value_Stdint(0);
#else
    if (!gfx_state.audio_initialized)
        return value_Stdint(0);
    if (args->entries[0].type != VALUE_TYPE_OPAQUE)
        return value_Stdint(0);
    Opaque *wrapper = args->entries[0].val.opaque;
    if (wrapper->data == NULL)
        return value_Stdint(0);
    Music *music = (Music *)wrapper->data;
    StopMusicStream(*music);
    return value_Stdint(1);
#endif
}

Value builtin_gfx_set_music_volume(Vec *args) {
#ifndef ENABLE_RAYLIB
    (void)args;
    return value_Stdint(0);
#else
    if (!gfx_state.audio_initialized)
        return value_Stdint(0);
    if (args->entries[0].type != VALUE_TYPE_OPAQUE)
        return value_Stdint(0);
    Opaque *wrapper = args->entries[0].val.opaque;
    if (wrapper->data == NULL)
        return value_Stdint(0);
    int percent = valueAsInt(args->entries[1]);
    if (percent < 0 || percent > 100)
        return value_Stdint(0);
    Music *music = (Music *)wrapper->data;
    SetMusicVolume(*music, (float)percent / 100.0f);
    return value_Stdint(1);
#endif
}

Value builtin_gfx_is_music_playing(Vec *args) {
#ifndef ENABLE_RAYLIB
    (void)args;
    return value_Stdint(0);
#else
    if (!gfx_state.audio_initialized)
        return value_Stdint(0);
    if (args->entries[0].type != VALUE_TYPE_OPAQUE)
        return value_Stdint(0);
    Opaque *wrapper = args->entries[0].val.opaque;
    if (wrapper->data == NULL)
        return value_Stdint(0);
    Music *music = (Music *)wrapper->data;
    return value_Stdint(IsMusicStreamPlaying(*music) ? 1 : 0);
#endif
}

Value builtin_gfx_update_music_stream(Vec *args) {
#ifndef ENABLE_RAYLIB
    (void)args;
    return value_Stdint(0);
#else
    if (!gfx_state.audio_initialized)
        return value_Stdint(0);
    if (args->entries[0].type != VALUE_TYPE_OPAQUE)
        return value_Stdint(0);
    Opaque *wrapper = args->entries[0].val.opaque;
    if (wrapper->data == NULL)
        return value_Stdint(0);
    Music *music = (Music *)wrapper->data;
    UpdateMusicStream(*music);
    return value_Stdint(1);
#endif
}

Value builtin_gfx_seek_music_ms(Vec *args) {
#ifndef ENABLE_RAYLIB
    (void)args;
    return value_Stdint(0);
#else
    if (!gfx_state.audio_initialized)
        return value_Stdint(0);
    if (args->entries[0].type != VALUE_TYPE_OPAQUE)
        return value_Stdint(0);
    Opaque *wrapper = args->entries[0].val.opaque;
    if (wrapper->data == NULL)
        return value_Stdint(0);
    int ms = valueAsInt(args->entries[1]);
    if (ms < 0)
        return value_Stdint(0);
    Music *music = (Music *)wrapper->data;
    SeekMusicStream(*music, (float)ms / 1000.0f);
    return value_Stdint(1);
#endif
}

Value builtin_gfx_music_time_played_ms(Vec *args) {
#ifndef ENABLE_RAYLIB
    (void)args;
    return value_Stdint(0);
#else
    if (!gfx_state.audio_initialized)
        return value_Stdint(0);
    if (args->entries[0].type != VALUE_TYPE_OPAQUE)
        return value_Stdint(0);
    Opaque *wrapper = args->entries[0].val.opaque;
    if (wrapper->data == NULL)
        return value_Stdint(0);
    Music *music = (Music *)wrapper->data;
    float played = GetMusicTimePlayed(*music);
    int ms = (int)(played * 1000.0f);
    if (ms < 0)
        ms = 0;
    return value_Stdint(ms);
#endif
}

Value builtin_gfx_music_time_length_ms(Vec *args) {
#ifndef ENABLE_RAYLIB
    (void)args;
    return value_Stdint(0);
#else
    if (!gfx_state.audio_initialized)
        return value_Stdint(0);
    if (args->entries[0].type != VALUE_TYPE_OPAQUE)
        return value_Stdint(0);
    Opaque *wrapper = args->entries[0].val.opaque;
    if (wrapper->data == NULL)
        return value_Stdint(0);
    Music *music = (Music *)wrapper->data;
    float length = GetMusicTimeLength(*music);
    int ms = (int)(length * 1000.0f);
    if (ms < 0)
        ms = 0;
    return value_Stdint(ms);
#endif
}

Value builtin_gfx_music_progress_width(Vec *args) {
#ifndef ENABLE_RAYLIB
    (void)args;
    return value_Stdint(0);
#else
    if (!gfx_state.audio_initialized)
        return value_Stdint(0);
    if (args->entries[0].type != VALUE_TYPE_OPAQUE)
        return value_Stdint(0);
    Opaque *wrapper = args->entries[0].val.opaque;
    if (wrapper->data == NULL)
        return value_Stdint(0);
    int maxWidth = valueAsInt(args->entries[1]);
    if (maxWidth <= 0)
        return value_Stdint(0);
    Music *music = (Music *)wrapper->data;
    float length = GetMusicTimeLength(*music);
    if (length <= 0.0f)
        return value_Stdint(0);
    float played = GetMusicTimePlayed(*music);
    float ratio = played / length;
    if (ratio < 0.0f)
        ratio = 0.0f;
    if (ratio > 1.0f)
        ratio = 1.0f;
    int width = (int)(ratio * (float)maxWidth);
    return value_Stdint(width);
#endif
}

// registration helpers

static void registerGfxOpen(BuiltIns *registry) {
    BuiltInArgs *args = newBuiltInArgs();
    int save = PROTECT(args);
    pushIntegerArg(args);
    pushIntegerArg(args);
    pushStringArg(args);
    TcType *errType = makeStringType();
    PROTECT(errType);
    TcType *okType = makeBoolean();
    PROTECT(okType);
    TcType *retType = makeTryType(errType, okType);
    PROTECT(retType);
    pushNewBuiltIn(registry, "gfx_open", retType, args,
                   (void *)builtin_gfx_open, "builtin_gfx_open");
    UNPROTECT(save);
}

static void registerGfxClose(BuiltIns *registry) {
    BuiltInArgs *args = newBuiltInArgs();
    int save = PROTECT(args);
    TcType *b = makeBoolean();
    PROTECT(b);
    pushNewBuiltIn(registry, "gfx_close", b, args, (void *)builtin_gfx_close,
                   "builtin_gfx_close");
    UNPROTECT(save);
}

static void registerGfxShouldClose(BuiltIns *registry) {
    BuiltInArgs *args = newBuiltInArgs();
    int save = PROTECT(args);
    TcType *b = makeBoolean();
    PROTECT(b);
    pushNewBuiltIn(registry, "gfx_should_close", b, args,
                   (void *)builtin_gfx_should_close,
                   "builtin_gfx_should_close");
    UNPROTECT(save);
}

static void registerGfxSetTargetFps(BuiltIns *registry) {
    BuiltInArgs *args = newBuiltInArgs();
    int save = PROTECT(args);
    pushIntegerArg(args);
    TcType *b = makeBoolean();
    PROTECT(b);
    pushNewBuiltIn(registry, "gfx_set_target_fps", b, args,
                   (void *)builtin_gfx_set_target_fps,
                   "builtin_gfx_set_target_fps");
    UNPROTECT(save);
}

static void registerGfxBeginFrame(BuiltIns *registry) {
    BuiltInArgs *args = newBuiltInArgs();
    int save = PROTECT(args);
    TcType *b = makeBoolean();
    PROTECT(b);
    pushNewBuiltIn(registry, "gfx_begin_frame", b, args,
                   (void *)builtin_gfx_begin_frame, "builtin_gfx_begin_frame");
    UNPROTECT(save);
}

static void registerGfxEndFrame(BuiltIns *registry) {
    BuiltInArgs *args = newBuiltInArgs();
    int save = PROTECT(args);
    TcType *b = makeBoolean();
    PROTECT(b);
    pushNewBuiltIn(registry, "gfx_end_frame", b, args,
                   (void *)builtin_gfx_end_frame, "builtin_gfx_end_frame");
    UNPROTECT(save);
}

static void registerGfxClear(BuiltIns *registry) {
    BuiltInArgs *args = newBuiltInArgs();
    int save = PROTECT(args);
    pushIntegerArg(args);
    pushIntegerArg(args);
    pushIntegerArg(args);
    pushIntegerArg(args);
    TcType *b = makeBoolean();
    PROTECT(b);
    pushNewBuiltIn(registry, "gfx_clear", b, args, (void *)builtin_gfx_clear,
                   "builtin_gfx_clear");
    UNPROTECT(save);
}

static void registerGfxFillRect(BuiltIns *registry) {
    BuiltInArgs *args = newBuiltInArgs();
    int save = PROTECT(args);
    pushIntegerArg(args);
    pushIntegerArg(args);
    pushIntegerArg(args);
    pushIntegerArg(args);
    pushIntegerArg(args);
    pushIntegerArg(args);
    pushIntegerArg(args);
    pushIntegerArg(args);
    TcType *b = makeBoolean();
    PROTECT(b);
    pushNewBuiltIn(registry, "gfx_fill_rect", b, args,
                   (void *)builtin_gfx_fill_rect, "builtin_gfx_fill_rect");
    UNPROTECT(save);
}

static void registerGfxDrawText(BuiltIns *registry) {
    BuiltInArgs *args = newBuiltInArgs();
    int save = PROTECT(args);
    pushStringArg(args);
    pushIntegerArg(args);
    pushIntegerArg(args);
    pushIntegerArg(args);
    pushIntegerArg(args);
    pushIntegerArg(args);
    pushIntegerArg(args);
    pushIntegerArg(args);
    TcType *b = makeBoolean();
    PROTECT(b);
    pushNewBuiltIn(registry, "gfx_draw_text", b, args,
                   (void *)builtin_gfx_draw_text, "builtin_gfx_draw_text");
    UNPROTECT(save);
}

// input registration helpers

static void registerGfxIsKeyDown(BuiltIns *registry) {
    BuiltInArgs *args = newBuiltInArgs();
    int save = PROTECT(args);
    pushIntegerArg(args);
    TcType *b = makeBoolean();
    PROTECT(b);
    pushNewBuiltIn(registry, "gfx_is_key_down", b, args,
                   (void *)builtin_gfx_is_key_down, "builtin_gfx_is_key_down");
    UNPROTECT(save);
}

static void registerGfxIsKeyPressed(BuiltIns *registry) {
    BuiltInArgs *args = newBuiltInArgs();
    int save = PROTECT(args);
    pushIntegerArg(args);
    TcType *b = makeBoolean();
    PROTECT(b);
    pushNewBuiltIn(registry, "gfx_is_key_pressed", b, args,
                   (void *)builtin_gfx_is_key_pressed,
                   "builtin_gfx_is_key_pressed");
    UNPROTECT(save);
}

static void registerGfxIsKeyReleased(BuiltIns *registry) {
    BuiltInArgs *args = newBuiltInArgs();
    int save = PROTECT(args);
    pushIntegerArg(args);
    TcType *b = makeBoolean();
    PROTECT(b);
    pushNewBuiltIn(registry, "gfx_is_key_released", b, args,
                   (void *)builtin_gfx_is_key_released,
                   "builtin_gfx_is_key_released");
    UNPROTECT(save);
}

static void registerGfxMouseX(BuiltIns *registry) {
    BuiltInArgs *args = newBuiltInArgs();
    int save = PROTECT(args);
    TcType *n = newTcType_BigInteger();
    PROTECT(n);
    pushNewBuiltIn(registry, "gfx_mouse_x", n, args,
                   (void *)builtin_gfx_mouse_x, "builtin_gfx_mouse_x");
    UNPROTECT(save);
}

static void registerGfxMouseY(BuiltIns *registry) {
    BuiltInArgs *args = newBuiltInArgs();
    int save = PROTECT(args);
    TcType *n = newTcType_BigInteger();
    PROTECT(n);
    pushNewBuiltIn(registry, "gfx_mouse_y", n, args,
                   (void *)builtin_gfx_mouse_y, "builtin_gfx_mouse_y");
    UNPROTECT(save);
}

static void registerGfxMouseDeltaX(BuiltIns *registry) {
    BuiltInArgs *args = newBuiltInArgs();
    int save = PROTECT(args);
    TcType *n = newTcType_BigInteger();
    PROTECT(n);
    pushNewBuiltIn(registry, "gfx_mouse_delta_x", n, args,
                   (void *)builtin_gfx_mouse_delta_x,
                   "builtin_gfx_mouse_delta_x");
    UNPROTECT(save);
}

static void registerGfxMouseDeltaY(BuiltIns *registry) {
    BuiltInArgs *args = newBuiltInArgs();
    int save = PROTECT(args);
    TcType *n = newTcType_BigInteger();
    PROTECT(n);
    pushNewBuiltIn(registry, "gfx_mouse_delta_y", n, args,
                   (void *)builtin_gfx_mouse_delta_y,
                   "builtin_gfx_mouse_delta_y");
    UNPROTECT(save);
}

static void registerGfxIsMouseDown(BuiltIns *registry) {
    BuiltInArgs *args = newBuiltInArgs();
    int save = PROTECT(args);
    pushIntegerArg(args);
    TcType *b = makeBoolean();
    PROTECT(b);
    pushNewBuiltIn(registry, "gfx_is_mouse_down", b, args,
                   (void *)builtin_gfx_is_mouse_down,
                   "builtin_gfx_is_mouse_down");
    UNPROTECT(save);
}

static void registerGfxIsMousePressed(BuiltIns *registry) {
    BuiltInArgs *args = newBuiltInArgs();
    int save = PROTECT(args);
    pushIntegerArg(args);
    TcType *b = makeBoolean();
    PROTECT(b);
    pushNewBuiltIn(registry, "gfx_is_mouse_pressed", b, args,
                   (void *)builtin_gfx_is_mouse_pressed,
                   "builtin_gfx_is_mouse_pressed");
    UNPROTECT(save);
}

static void registerGfxIsMouseReleased(BuiltIns *registry) {
    BuiltInArgs *args = newBuiltInArgs();
    int save = PROTECT(args);
    pushIntegerArg(args);
    TcType *b = makeBoolean();
    PROTECT(b);
    pushNewBuiltIn(registry, "gfx_is_mouse_released", b, args,
                   (void *)builtin_gfx_is_mouse_released,
                   "builtin_gfx_is_mouse_released");
    UNPROTECT(save);
}

static void registerGfxMouseWheel(BuiltIns *registry) {
    BuiltInArgs *args = newBuiltInArgs();
    int save = PROTECT(args);
    TcType *n = newTcType_BigInteger();
    PROTECT(n);
    pushNewBuiltIn(registry, "gfx_mouse_wheel", n, args,
                   (void *)builtin_gfx_mouse_wheel, "builtin_gfx_mouse_wheel");
    UNPROTECT(save);
}

static void registerGfxDisableCursor(BuiltIns *registry) {
    BuiltInArgs *args = newBuiltInArgs();
    int save = PROTECT(args);
    TcType *b = makeBoolean();
    PROTECT(b);
    pushNewBuiltIn(registry, "gfx_disable_cursor", b, args,
                   (void *)builtin_gfx_disable_cursor,
                   "builtin_gfx_disable_cursor");
    UNPROTECT(save);
}

static void registerGfxEnableCursor(BuiltIns *registry) {
    BuiltInArgs *args = newBuiltInArgs();
    int save = PROTECT(args);
    TcType *b = makeBoolean();
    PROTECT(b);
    pushNewBuiltIn(registry, "gfx_enable_cursor", b, args,
                   (void *)builtin_gfx_enable_cursor,
                   "builtin_gfx_enable_cursor");
    UNPROTECT(save);
}

static void registerGfxHideCursor(BuiltIns *registry) {
    BuiltInArgs *args = newBuiltInArgs();
    int save = PROTECT(args);
    TcType *b = makeBoolean();
    PROTECT(b);
    pushNewBuiltIn(registry, "gfx_hide_cursor", b, args,
                   (void *)builtin_gfx_hide_cursor, "builtin_gfx_hide_cursor");
    UNPROTECT(save);
}

static void registerGfxShowCursor(BuiltIns *registry) {
    BuiltInArgs *args = newBuiltInArgs();
    int save = PROTECT(args);
    TcType *b = makeBoolean();
    PROTECT(b);
    pushNewBuiltIn(registry, "gfx_show_cursor", b, args,
                   (void *)builtin_gfx_show_cursor, "builtin_gfx_show_cursor");
    UNPROTECT(save);
}

static void registerGfxScreenWidth(BuiltIns *registry) {
    BuiltInArgs *args = newBuiltInArgs();
    int save = PROTECT(args);
    TcType *n = newTcType_BigInteger();
    PROTECT(n);
    pushNewBuiltIn(registry, "gfx_screen_width", n, args,
                   (void *)builtin_gfx_screen_width,
                   "builtin_gfx_screen_width");
    UNPROTECT(save);
}

static void registerGfxScreenHeight(BuiltIns *registry) {
    BuiltInArgs *args = newBuiltInArgs();
    int save = PROTECT(args);
    TcType *n = newTcType_BigInteger();
    PROTECT(n);
    pushNewBuiltIn(registry, "gfx_screen_height", n, args,
                   (void *)builtin_gfx_screen_height,
                   "builtin_gfx_screen_height");
    UNPROTECT(save);
}

static void registerGfxFrameTimeMs(BuiltIns *registry) {
    BuiltInArgs *args = newBuiltInArgs();
    int save = PROTECT(args);
    TcType *n = newTcType_BigInteger();
    PROTECT(n);
    pushNewBuiltIn(registry, "gfx_frame_time_ms", n, args,
                   (void *)builtin_gfx_frame_time_ms,
                   "builtin_gfx_frame_time_ms");
    UNPROTECT(save);
}

// primitive registration helpers

static void registerGfxDrawLine(BuiltIns *registry) {
    BuiltInArgs *args = newBuiltInArgs();
    int save = PROTECT(args);
    pushAnyArg(args);     // x1
    pushAnyArg(args);     // y1
    pushAnyArg(args);     // x2
    pushAnyArg(args);     // y2
    pushIntegerArg(args); // r
    pushIntegerArg(args); // g
    pushIntegerArg(args); // b
    pushIntegerArg(args); // a
    TcType *ret = makeBoolean();
    PROTECT(ret);
    pushNewBuiltIn(registry, "gfx_draw_line", ret, args,
                   (void *)builtin_gfx_draw_line, "builtin_gfx_draw_line");
    UNPROTECT(save);
}

static void registerGfxDrawCircle(BuiltIns *registry) {
    BuiltInArgs *args = newBuiltInArgs();
    int save = PROTECT(args);
    pushIntegerArg(args); // cx
    pushIntegerArg(args); // cy
    pushIntegerArg(args); // radius
    pushIntegerArg(args); // r
    pushIntegerArg(args); // g
    pushIntegerArg(args); // b
    pushIntegerArg(args); // a
    TcType *ret = makeBoolean();
    PROTECT(ret);
    pushNewBuiltIn(registry, "gfx_draw_circle", ret, args,
                   (void *)builtin_gfx_draw_circle, "builtin_gfx_draw_circle");
    UNPROTECT(save);
}

static void registerGfxFillCircle(BuiltIns *registry) {
    BuiltInArgs *args = newBuiltInArgs();
    int save = PROTECT(args);
    pushIntegerArg(args); // cx
    pushIntegerArg(args); // cy
    pushIntegerArg(args); // radius
    pushIntegerArg(args); // r
    pushIntegerArg(args); // g
    pushIntegerArg(args); // b
    pushIntegerArg(args); // a
    TcType *ret = makeBoolean();
    PROTECT(ret);
    pushNewBuiltIn(registry, "gfx_fill_circle", ret, args,
                   (void *)builtin_gfx_fill_circle, "builtin_gfx_fill_circle");
    UNPROTECT(save);
}

static void registerGfxDrawRect(BuiltIns *registry) {
    BuiltInArgs *args = newBuiltInArgs();
    int save = PROTECT(args);
    pushIntegerArg(args); // x
    pushIntegerArg(args); // y
    pushIntegerArg(args); // w
    pushIntegerArg(args); // h
    pushIntegerArg(args); // r
    pushIntegerArg(args); // g
    pushIntegerArg(args); // b
    pushIntegerArg(args); // a
    TcType *ret = makeBoolean();
    PROTECT(ret);
    pushNewBuiltIn(registry, "gfx_draw_rect", ret, args,
                   (void *)builtin_gfx_draw_rect, "builtin_gfx_draw_rect");
    UNPROTECT(save);
}

static void registerGfxDrawTriangle(BuiltIns *registry) {
    BuiltInArgs *args = newBuiltInArgs();
    int save = PROTECT(args);
    pushAnyArg(args);     // x1
    pushAnyArg(args);     // y1
    pushAnyArg(args);     // x2
    pushAnyArg(args);     // y2
    pushAnyArg(args);     // x3
    pushAnyArg(args);     // y3
    pushIntegerArg(args); // r
    pushIntegerArg(args); // g
    pushIntegerArg(args); // b
    pushIntegerArg(args); // a
    TcType *ret = makeBoolean();
    PROTECT(ret);
    pushNewBuiltIn(registry, "gfx_draw_triangle", ret, args,
                   (void *)builtin_gfx_draw_triangle,
                   "builtin_gfx_draw_triangle");
    UNPROTECT(save);
}

static void registerGfxFillTriangle(BuiltIns *registry) {
    BuiltInArgs *args = newBuiltInArgs();
    int save = PROTECT(args);
    pushAnyArg(args);     // x1
    pushAnyArg(args);     // y1
    pushAnyArg(args);     // x2
    pushAnyArg(args);     // y2
    pushAnyArg(args);     // x3
    pushAnyArg(args);     // y3
    pushIntegerArg(args); // r
    pushIntegerArg(args); // g
    pushIntegerArg(args); // b
    pushIntegerArg(args); // a
    TcType *ret = makeBoolean();
    PROTECT(ret);
    pushNewBuiltIn(registry, "gfx_fill_triangle", ret, args,
                   (void *)builtin_gfx_fill_triangle,
                   "builtin_gfx_fill_triangle");
    UNPROTECT(save);
}

static void registerGfxDrawPolygon(BuiltIns *registry) {
    BuiltInArgs *args = newBuiltInArgs();
    int save = PROTECT(args);
    pushIntegerArg(args); // center_x
    pushIntegerArg(args); // center_y
    pushIntegerArg(args); // sides
    pushIntegerArg(args); // radius
    pushIntegerArg(args); // rotation
    pushIntegerArg(args); // r
    pushIntegerArg(args); // g
    pushIntegerArg(args); // b
    pushIntegerArg(args); // a
    TcType *ret = makeBoolean();
    PROTECT(ret);
    pushNewBuiltIn(registry, "gfx_draw_polygon", ret, args,
                   (void *)builtin_gfx_draw_polygon,
                   "builtin_gfx_draw_polygon");
    UNPROTECT(save);
}

static void registerGfxFillPolygon(BuiltIns *registry) {
    BuiltInArgs *args = newBuiltInArgs();
    int save = PROTECT(args);
    pushIntegerArg(args); // center_x
    pushIntegerArg(args); // center_y
    pushIntegerArg(args); // sides
    pushIntegerArg(args); // radius
    pushIntegerArg(args); // rotation
    pushIntegerArg(args); // r
    pushIntegerArg(args); // g
    pushIntegerArg(args); // b
    pushIntegerArg(args); // a
    TcType *ret = makeBoolean();
    PROTECT(ret);
    pushNewBuiltIn(registry, "gfx_fill_polygon", ret, args,
                   (void *)builtin_gfx_fill_polygon,
                   "builtin_gfx_fill_polygon");
    UNPROTECT(save);
}

static void registerGfxDrawArc(BuiltIns *registry) {
    BuiltInArgs *args = newBuiltInArgs();
    int save = PROTECT(args);
    pushIntegerArg(args); // center_x
    pushIntegerArg(args); // center_y
    pushIntegerArg(args); // radius
    pushIntegerArg(args); // start_deg
    pushIntegerArg(args); // end_deg
    pushIntegerArg(args); // segments
    pushIntegerArg(args); // r
    pushIntegerArg(args); // g
    pushIntegerArg(args); // b
    pushIntegerArg(args); // a
    TcType *ret = makeBoolean();
    PROTECT(ret);
    pushNewBuiltIn(registry, "gfx_draw_arc", ret, args,
                   (void *)builtin_gfx_draw_arc, "builtin_gfx_draw_arc");
    UNPROTECT(save);
}

static void registerGfxDrawRectRounded(BuiltIns *registry) {
    BuiltInArgs *args = newBuiltInArgs();
    int save = PROTECT(args);
    pushIntegerArg(args); // x
    pushIntegerArg(args); // y
    pushIntegerArg(args); // w
    pushIntegerArg(args); // h
    pushIntegerArg(args); // roundness
    pushIntegerArg(args); // segments
    pushIntegerArg(args); // line_thickness
    pushIntegerArg(args); // r
    pushIntegerArg(args); // g
    pushIntegerArg(args); // b
    pushIntegerArg(args); // a
    TcType *ret = makeBoolean();
    PROTECT(ret);
    pushNewBuiltIn(registry, "gfx_draw_rect_rounded", ret, args,
                   (void *)builtin_gfx_draw_rect_rounded,
                   "builtin_gfx_draw_rect_rounded");
    UNPROTECT(save);
}

static void registerGfxMat2DIdentity(BuiltIns *registry) {
    BuiltInArgs *args = newBuiltInArgs();
    int save = PROTECT(args);
    TcType *ret = makeMat2DType();
    PROTECT(ret);
    pushNewBuiltIn(registry, "gfx_mat2d_identity", ret, args,
                   (void *)builtin_gfx_mat2d_identity,
                   "builtin_gfx_mat2d_identity");
    UNPROTECT(save);
}

static void registerGfxMat2DTranslate(BuiltIns *registry) {
    BuiltInArgs *args = newBuiltInArgs();
    int save = PROTECT(args);
    pushMat2DArg(args);
    pushAnyArg(args); // dx
    pushAnyArg(args); // dy
    TcType *ret = makeMat2DType();
    PROTECT(ret);
    pushNewBuiltIn(registry, "gfx_mat2d_translate", ret, args,
                   (void *)builtin_gfx_mat2d_translate,
                   "builtin_gfx_mat2d_translate");
    UNPROTECT(save);
}

static void registerGfxMat2DRotate(BuiltIns *registry) {
    BuiltInArgs *args = newBuiltInArgs();
    int save = PROTECT(args);
    pushMat2DArg(args);
    pushAnyArg(args); // degrees
    TcType *ret = makeMat2DType();
    PROTECT(ret);
    pushNewBuiltIn(registry, "gfx_mat2d_rotate", ret, args,
                   (void *)builtin_gfx_mat2d_rotate,
                   "builtin_gfx_mat2d_rotate");
    UNPROTECT(save);
}

static void registerGfxMat2DScale(BuiltIns *registry) {
    BuiltInArgs *args = newBuiltInArgs();
    int save = PROTECT(args);
    pushMat2DArg(args);
    pushAnyArg(args); // sx
    pushAnyArg(args); // sy
    TcType *ret = makeMat2DType();
    PROTECT(ret);
    pushNewBuiltIn(registry, "gfx_mat2d_scale", ret, args,
                   (void *)builtin_gfx_mat2d_scale, "builtin_gfx_mat2d_scale");
    UNPROTECT(save);
}

static void registerGfxMat2DCompose(BuiltIns *registry) {
    BuiltInArgs *args = newBuiltInArgs();
    int save = PROTECT(args);
    pushMat2DArg(args);
    pushMat2DArg(args);
    TcType *ret = makeMat2DType();
    PROTECT(ret);
    pushNewBuiltIn(registry, "gfx_mat2d_compose", ret, args,
                   (void *)builtin_gfx_mat2d_compose,
                   "builtin_gfx_mat2d_compose");
    UNPROTECT(save);
}

static void registerGfxMat2DApplyX(BuiltIns *registry) {
    BuiltInArgs *args = newBuiltInArgs();
    int save = PROTECT(args);
    pushMat2DArg(args);
    pushAnyArg(args); // x
    pushAnyArg(args); // y
    TcType *ret = newTcType_BigInteger();
    PROTECT(ret);
    pushNewBuiltIn(registry, "gfx_mat2d_apply_x", ret, args,
                   (void *)builtin_gfx_mat2d_apply_x,
                   "builtin_gfx_mat2d_apply_x");
    UNPROTECT(save);
}

static void registerGfxMat2DApplyY(BuiltIns *registry) {
    BuiltInArgs *args = newBuiltInArgs();
    int save = PROTECT(args);
    pushMat2DArg(args);
    pushAnyArg(args); // x
    pushAnyArg(args); // y
    TcType *ret = newTcType_BigInteger();
    PROTECT(ret);
    pushNewBuiltIn(registry, "gfx_mat2d_apply_y", ret, args,
                   (void *)builtin_gfx_mat2d_apply_y,
                   "builtin_gfx_mat2d_apply_y");
    UNPROTECT(save);
}

static void registerGfxTransform3D(BuiltIns *registry) {
    BuiltInArgs *args = newBuiltInArgs();
    int save = PROTECT(args);
    pushAnyArg(args); // px
    pushAnyArg(args); // py
    pushAnyArg(args); // pz
    pushAnyArg(args); // rx
    pushAnyArg(args); // ry
    pushAnyArg(args); // rz
    pushAnyArg(args); // sx
    pushAnyArg(args); // sy
    pushAnyArg(args); // sz
    TcType *ret = makeTransform3DType();
    PROTECT(ret);
    pushNewBuiltIn(registry, "gfx_transform3d", ret, args,
                   (void *)builtin_gfx_transform3d, "builtin_gfx_transform3d");
    UNPROTECT(save);
}

static void registerGfxTransform3DPositionX(BuiltIns *registry) {
    BuiltInArgs *args = newBuiltInArgs();
    int save = PROTECT(args);
    pushTransform3DArg(args);
    TcType *ret = newTcType_BigInteger();
    PROTECT(ret);
    pushNewBuiltIn(registry, "gfx_transform3d_position_x", ret, args,
                   (void *)builtin_gfx_transform3d_position_x,
                   "builtin_gfx_transform3d_position_x");
    UNPROTECT(save);
}

static void registerGfxTransform3DPositionY(BuiltIns *registry) {
    BuiltInArgs *args = newBuiltInArgs();
    int save = PROTECT(args);
    pushTransform3DArg(args);
    TcType *ret = newTcType_BigInteger();
    PROTECT(ret);
    pushNewBuiltIn(registry, "gfx_transform3d_position_y", ret, args,
                   (void *)builtin_gfx_transform3d_position_y,
                   "builtin_gfx_transform3d_position_y");
    UNPROTECT(save);
}

static void registerGfxTransform3DPositionZ(BuiltIns *registry) {
    BuiltInArgs *args = newBuiltInArgs();
    int save = PROTECT(args);
    pushTransform3DArg(args);
    TcType *ret = newTcType_BigInteger();
    PROTECT(ret);
    pushNewBuiltIn(registry, "gfx_transform3d_position_z", ret, args,
                   (void *)builtin_gfx_transform3d_position_z,
                   "builtin_gfx_transform3d_position_z");
    UNPROTECT(save);
}

static void registerGfxTransform3DRotationX(BuiltIns *registry) {
    BuiltInArgs *args = newBuiltInArgs();
    int save = PROTECT(args);
    pushTransform3DArg(args);
    TcType *ret = newTcType_BigInteger();
    PROTECT(ret);
    pushNewBuiltIn(registry, "gfx_transform3d_rotation_x", ret, args,
                   (void *)builtin_gfx_transform3d_rotation_x,
                   "builtin_gfx_transform3d_rotation_x");
    UNPROTECT(save);
}

static void registerGfxTransform3DRotationY(BuiltIns *registry) {
    BuiltInArgs *args = newBuiltInArgs();
    int save = PROTECT(args);
    pushTransform3DArg(args);
    TcType *ret = newTcType_BigInteger();
    PROTECT(ret);
    pushNewBuiltIn(registry, "gfx_transform3d_rotation_y", ret, args,
                   (void *)builtin_gfx_transform3d_rotation_y,
                   "builtin_gfx_transform3d_rotation_y");
    UNPROTECT(save);
}

static void registerGfxTransform3DRotationZ(BuiltIns *registry) {
    BuiltInArgs *args = newBuiltInArgs();
    int save = PROTECT(args);
    pushTransform3DArg(args);
    TcType *ret = newTcType_BigInteger();
    PROTECT(ret);
    pushNewBuiltIn(registry, "gfx_transform3d_rotation_z", ret, args,
                   (void *)builtin_gfx_transform3d_rotation_z,
                   "builtin_gfx_transform3d_rotation_z");
    UNPROTECT(save);
}

static void registerGfxTransform3DScaleX(BuiltIns *registry) {
    BuiltInArgs *args = newBuiltInArgs();
    int save = PROTECT(args);
    pushTransform3DArg(args);
    TcType *ret = newTcType_BigInteger();
    PROTECT(ret);
    pushNewBuiltIn(registry, "gfx_transform3d_scale_x", ret, args,
                   (void *)builtin_gfx_transform3d_scale_x,
                   "builtin_gfx_transform3d_scale_x");
    UNPROTECT(save);
}

static void registerGfxTransform3DScaleY(BuiltIns *registry) {
    BuiltInArgs *args = newBuiltInArgs();
    int save = PROTECT(args);
    pushTransform3DArg(args);
    TcType *ret = newTcType_BigInteger();
    PROTECT(ret);
    pushNewBuiltIn(registry, "gfx_transform3d_scale_y", ret, args,
                   (void *)builtin_gfx_transform3d_scale_y,
                   "builtin_gfx_transform3d_scale_y");
    UNPROTECT(save);
}

static void registerGfxTransform3DScaleZ(BuiltIns *registry) {
    BuiltInArgs *args = newBuiltInArgs();
    int save = PROTECT(args);
    pushTransform3DArg(args);
    TcType *ret = newTcType_BigInteger();
    PROTECT(ret);
    pushNewBuiltIn(registry, "gfx_transform3d_scale_z", ret, args,
                   (void *)builtin_gfx_transform3d_scale_z,
                   "builtin_gfx_transform3d_scale_z");
    UNPROTECT(save);
}

static void registerGfxLoadFont(BuiltIns *registry) {
    BuiltInArgs *args = newBuiltInArgs();
    int save = PROTECT(args);
    pushStringArg(args);
    pushIntegerArg(args);
    TcType *errType = makeStringType();
    PROTECT(errType);
    TcType *fontType = makeFontType();
    PROTECT(fontType);
    TcType *retType = makeTryType(errType, fontType);
    PROTECT(retType);
    pushNewBuiltIn(registry, "gfx_load_font", retType, args,
                   (void *)builtin_gfx_load_font, "builtin_gfx_load_font");
    UNPROTECT(save);
}

static void registerGfxUnloadFont(BuiltIns *registry) {
    BuiltInArgs *args = newBuiltInArgs();
    int save = PROTECT(args);
    pushFontArg(args);
    TcType *b = makeBoolean();
    PROTECT(b);
    pushNewBuiltIn(registry, "gfx_unload_font", b, args,
                   (void *)builtin_gfx_unload_font, "builtin_gfx_unload_font");
    UNPROTECT(save);
}

static void registerGfxDrawTextFont(BuiltIns *registry) {
    BuiltInArgs *args = newBuiltInArgs();
    int save = PROTECT(args);
    pushFontArg(args);
    pushStringArg(args);
    pushIntegerArg(args); // x
    pushIntegerArg(args); // y
    pushIntegerArg(args); // size
    pushIntegerArg(args); // spacing
    pushIntegerArg(args); // r
    pushIntegerArg(args); // g
    pushIntegerArg(args); // b
    pushIntegerArg(args); // a
    TcType *b = makeBoolean();
    PROTECT(b);
    pushNewBuiltIn(registry, "gfx_draw_text_font", b, args,
                   (void *)builtin_gfx_draw_text_font,
                   "builtin_gfx_draw_text_font");
    UNPROTECT(save);
}

static void registerGfxMeasureTextWidth(BuiltIns *registry) {
    BuiltInArgs *args = newBuiltInArgs();
    int save = PROTECT(args);
    pushFontArg(args);
    pushStringArg(args);
    pushIntegerArg(args); // size
    pushIntegerArg(args); // spacing
    TcType *errType = makeStringType();
    PROTECT(errType);
    TcType *numType = newTcType_BigInteger();
    PROTECT(numType);
    TcType *retType = makeTryType(errType, numType);
    PROTECT(retType);
    pushNewBuiltIn(registry, "gfx_measure_text_width", retType, args,
                   (void *)builtin_gfx_measure_text_width,
                   "builtin_gfx_measure_text_width");
    UNPROTECT(save);
}

static void registerGfxLoadTexture(BuiltIns *registry) {
    BuiltInArgs *args = newBuiltInArgs();
    int save = PROTECT(args);
    pushStringArg(args);
    TcType *errType = makeStringType();
    PROTECT(errType);
    TcType *texType = makeTextureType();
    PROTECT(texType);
    TcType *retType = makeTryType(errType, texType);
    PROTECT(retType);
    pushNewBuiltIn(registry, "gfx_load_texture", retType, args,
                   (void *)builtin_gfx_load_texture,
                   "builtin_gfx_load_texture");
    UNPROTECT(save);
}

static void registerGfxUnloadTexture(BuiltIns *registry) {
    BuiltInArgs *args = newBuiltInArgs();
    int save = PROTECT(args);
    pushTextureArg(args);
    TcType *b = makeBoolean();
    PROTECT(b);
    pushNewBuiltIn(registry, "gfx_unload_texture", b, args,
                   (void *)builtin_gfx_unload_texture,
                   "builtin_gfx_unload_texture");
    UNPROTECT(save);
}

static void registerGfxDrawTexture(BuiltIns *registry) {
    BuiltInArgs *args = newBuiltInArgs();
    int save = PROTECT(args);
    pushTextureArg(args);
    pushIntegerArg(args); // x
    pushIntegerArg(args); // y
    pushIntegerArg(args); // r
    pushIntegerArg(args); // g
    pushIntegerArg(args); // b
    pushIntegerArg(args); // a
    TcType *ret = makeBoolean();
    PROTECT(ret);
    pushNewBuiltIn(registry, "gfx_draw_texture", ret, args,
                   (void *)builtin_gfx_draw_texture,
                   "builtin_gfx_draw_texture");
    UNPROTECT(save);
}

static void registerGfxDrawTextureRec(BuiltIns *registry) {
    BuiltInArgs *args = newBuiltInArgs();
    int save = PROTECT(args);
    pushTextureArg(args);
    pushIntegerArg(args); // src_x
    pushIntegerArg(args); // src_y
    pushIntegerArg(args); // src_w
    pushIntegerArg(args); // src_h
    pushIntegerArg(args); // dst_x
    pushIntegerArg(args); // dst_y
    pushIntegerArg(args); // r
    pushIntegerArg(args); // g
    pushIntegerArg(args); // b
    pushIntegerArg(args); // a
    TcType *ret = makeBoolean();
    PROTECT(ret);
    pushNewBuiltIn(registry, "gfx_draw_texture_rec", ret, args,
                   (void *)builtin_gfx_draw_texture_rec,
                   "builtin_gfx_draw_texture_rec");
    UNPROTECT(save);
}

static void registerGfxDrawTexturePro(BuiltIns *registry) {
    BuiltInArgs *args = newBuiltInArgs();
    int save = PROTECT(args);
    pushTextureArg(args);
    pushIntegerArg(args); // src_x
    pushIntegerArg(args); // src_y
    pushIntegerArg(args); // src_w
    pushIntegerArg(args); // src_h
    pushIntegerArg(args); // dst_x
    pushIntegerArg(args); // dst_y
    pushIntegerArg(args); // dst_w
    pushIntegerArg(args); // dst_h
    pushIntegerArg(args); // ox
    pushIntegerArg(args); // oy
    pushIntegerArg(args); // rotation
    pushIntegerArg(args); // r
    pushIntegerArg(args); // g
    pushIntegerArg(args); // b
    pushIntegerArg(args); // a
    TcType *ret = makeBoolean();
    PROTECT(ret);
    pushNewBuiltIn(registry, "gfx_draw_texture_pro", ret, args,
                   (void *)builtin_gfx_draw_texture_pro,
                   "builtin_gfx_draw_texture_pro");
    UNPROTECT(save);
}

static void registerGfxTextureWidth(BuiltIns *registry) {
    BuiltInArgs *args = newBuiltInArgs();
    int save = PROTECT(args);
    pushTextureArg(args);
    TcType *n = newTcType_BigInteger();
    PROTECT(n);
    pushNewBuiltIn(registry, "gfx_texture_width", n, args,
                   (void *)builtin_gfx_texture_width,
                   "builtin_gfx_texture_width");
    UNPROTECT(save);
}

static void registerGfxTextureHeight(BuiltIns *registry) {
    BuiltInArgs *args = newBuiltInArgs();
    int save = PROTECT(args);
    pushTextureArg(args);
    TcType *n = newTcType_BigInteger();
    PROTECT(n);
    pushNewBuiltIn(registry, "gfx_texture_height", n, args,
                   (void *)builtin_gfx_texture_height,
                   "builtin_gfx_texture_height");
    UNPROTECT(save);
}

static void registerGfxLoadRenderTexture(BuiltIns *registry) {
    BuiltInArgs *args = newBuiltInArgs();
    int save = PROTECT(args);
    pushIntegerArg(args); // width
    pushIntegerArg(args); // height
    TcType *errType = makeStringType();
    PROTECT(errType);
    TcType *rtType = makeRenderTextureType();
    PROTECT(rtType);
    TcType *retType = makeTryType(errType, rtType);
    PROTECT(retType);
    pushNewBuiltIn(registry, "gfx_load_render_texture", retType, args,
                   (void *)builtin_gfx_load_render_texture,
                   "builtin_gfx_load_render_texture");
    UNPROTECT(save);
}

static void registerGfxUnloadRenderTexture(BuiltIns *registry) {
    BuiltInArgs *args = newBuiltInArgs();
    int save = PROTECT(args);
    pushRenderTextureArg(args);
    TcType *ret = makeBoolean();
    PROTECT(ret);
    pushNewBuiltIn(registry, "gfx_unload_render_texture", ret, args,
                   (void *)builtin_gfx_unload_render_texture,
                   "builtin_gfx_unload_render_texture");
    UNPROTECT(save);
}

static void registerGfxBeginTextureMode(BuiltIns *registry) {
    BuiltInArgs *args = newBuiltInArgs();
    int save = PROTECT(args);
    pushRenderTextureArg(args);
    TcType *ret = makeBoolean();
    PROTECT(ret);
    pushNewBuiltIn(registry, "gfx_begin_texture_mode", ret, args,
                   (void *)builtin_gfx_begin_texture_mode,
                   "builtin_gfx_begin_texture_mode");
    UNPROTECT(save);
}

static void registerGfxEndTextureMode(BuiltIns *registry) {
    BuiltInArgs *args = newBuiltInArgs();
    int save = PROTECT(args);
    TcType *ret = makeBoolean();
    PROTECT(ret);
    pushNewBuiltIn(registry, "gfx_end_texture_mode", ret, args,
                   (void *)builtin_gfx_end_texture_mode,
                   "builtin_gfx_end_texture_mode");
    UNPROTECT(save);
}

static void registerGfxDrawRenderTexture(BuiltIns *registry) {
    BuiltInArgs *args = newBuiltInArgs();
    int save = PROTECT(args);
    pushRenderTextureArg(args);
    pushIntegerArg(args); // x
    pushIntegerArg(args); // y
    pushIntegerArg(args); // r
    pushIntegerArg(args); // g
    pushIntegerArg(args); // b
    pushIntegerArg(args); // a
    TcType *ret = makeBoolean();
    PROTECT(ret);
    pushNewBuiltIn(registry, "gfx_draw_render_texture", ret, args,
                   (void *)builtin_gfx_draw_render_texture,
                   "builtin_gfx_draw_render_texture");
    UNPROTECT(save);
}

static void registerGfxLoadShader(BuiltIns *registry) {
    BuiltInArgs *args = newBuiltInArgs();
    int save = PROTECT(args);
    pushStringArg(args); // vertex_path
    pushStringArg(args); // fragment_path
    TcType *errType = makeStringType();
    PROTECT(errType);
    TcType *shaderType = makeShaderType();
    PROTECT(shaderType);
    TcType *retType = makeTryType(errType, shaderType);
    PROTECT(retType);
    pushNewBuiltIn(registry, "gfx_load_shader", retType, args,
                   (void *)builtin_gfx_load_shader, "builtin_gfx_load_shader");
    UNPROTECT(save);
}

static void registerGfxUnloadShader(BuiltIns *registry) {
    BuiltInArgs *args = newBuiltInArgs();
    int save = PROTECT(args);
    pushShaderArg(args);
    TcType *ret = makeBoolean();
    PROTECT(ret);
    pushNewBuiltIn(registry, "gfx_unload_shader", ret, args,
                   (void *)builtin_gfx_unload_shader,
                   "builtin_gfx_unload_shader");
    UNPROTECT(save);
}

static void registerGfxBeginShaderMode(BuiltIns *registry) {
    BuiltInArgs *args = newBuiltInArgs();
    int save = PROTECT(args);
    pushShaderArg(args);
    TcType *ret = makeBoolean();
    PROTECT(ret);
    pushNewBuiltIn(registry, "gfx_begin_shader_mode", ret, args,
                   (void *)builtin_gfx_begin_shader_mode,
                   "builtin_gfx_begin_shader_mode");
    UNPROTECT(save);
}

static void registerGfxEndShaderMode(BuiltIns *registry) {
    BuiltInArgs *args = newBuiltInArgs();
    int save = PROTECT(args);
    TcType *ret = makeBoolean();
    PROTECT(ret);
    pushNewBuiltIn(registry, "gfx_end_shader_mode", ret, args,
                   (void *)builtin_gfx_end_shader_mode,
                   "builtin_gfx_end_shader_mode");
    UNPROTECT(save);
}

static void registerGfxSetShaderInt(BuiltIns *registry) {
    BuiltInArgs *args = newBuiltInArgs();
    int save = PROTECT(args);
    pushShaderArg(args);
    pushStringArg(args);  // uniform name
    pushIntegerArg(args); // value
    TcType *ret = makeBoolean();
    PROTECT(ret);
    pushNewBuiltIn(registry, "gfx_set_shader_int", ret, args,
                   (void *)builtin_gfx_set_shader_int,
                   "builtin_gfx_set_shader_int");
    UNPROTECT(save);
}

static void registerGfxSetShaderFloat(BuiltIns *registry) {
    BuiltInArgs *args = newBuiltInArgs();
    int save = PROTECT(args);
    pushShaderArg(args);
    pushStringArg(args); // uniform name
    pushAnyArg(args);    // value
    TcType *ret = makeBoolean();
    PROTECT(ret);
    pushNewBuiltIn(registry, "gfx_set_shader_float", ret, args,
                   (void *)builtin_gfx_set_shader_float,
                   "builtin_gfx_set_shader_float");
    UNPROTECT(save);
}

static void registerGfxSetShaderVec2(BuiltIns *registry) {
    BuiltInArgs *args = newBuiltInArgs();
    int save = PROTECT(args);
    pushShaderArg(args);
    pushStringArg(args); // uniform name
    pushAnyArg(args);    // x
    pushAnyArg(args);    // y
    TcType *ret = makeBoolean();
    PROTECT(ret);
    pushNewBuiltIn(registry, "gfx_set_shader_vec2", ret, args,
                   (void *)builtin_gfx_set_shader_vec2,
                   "builtin_gfx_set_shader_vec2");
    UNPROTECT(save);
}

static void registerGfxSetShaderVec3(BuiltIns *registry) {
    BuiltInArgs *args = newBuiltInArgs();
    int save = PROTECT(args);
    pushShaderArg(args);
    pushStringArg(args); // uniform name
    pushAnyArg(args);    // x
    pushAnyArg(args);    // y
    pushAnyArg(args);    // z
    TcType *ret = makeBoolean();
    PROTECT(ret);
    pushNewBuiltIn(registry, "gfx_set_shader_vec3", ret, args,
                   (void *)builtin_gfx_set_shader_vec3,
                   "builtin_gfx_set_shader_vec3");
    UNPROTECT(save);
}

static void registerGfxBeginMode2D(BuiltIns *registry) {
    BuiltInArgs *args = newBuiltInArgs();
    int save = PROTECT(args);
    pushAnyArg(args); // target_x
    pushAnyArg(args); // target_y
    pushAnyArg(args); // offset_x
    pushAnyArg(args); // offset_y
    pushAnyArg(args); // rotation
    pushAnyArg(args); // zoom
    TcType *ret = makeBoolean();
    PROTECT(ret);
    pushNewBuiltIn(registry, "gfx_begin_mode_2d", ret, args,
                   (void *)builtin_gfx_begin_mode_2d,
                   "builtin_gfx_begin_mode_2d");
    UNPROTECT(save);
}

static void registerGfxEndMode2D(BuiltIns *registry) {
    BuiltInArgs *args = newBuiltInArgs();
    int save = PROTECT(args);
    TcType *ret = makeBoolean();
    PROTECT(ret);
    pushNewBuiltIn(registry, "gfx_end_mode_2d", ret, args,
                   (void *)builtin_gfx_end_mode_2d, "builtin_gfx_end_mode_2d");
    UNPROTECT(save);
}

static void registerGfxBeginMode3D(BuiltIns *registry) {
    BuiltInArgs *args = newBuiltInArgs();
    int save = PROTECT(args);
    pushAnyArg(args);     // pos_x
    pushAnyArg(args);     // pos_y
    pushAnyArg(args);     // pos_z
    pushAnyArg(args);     // target_x
    pushAnyArg(args);     // target_y
    pushAnyArg(args);     // target_z
    pushAnyArg(args);     // up_x
    pushAnyArg(args);     // up_y
    pushAnyArg(args);     // up_z
    pushAnyArg(args);     // fovy
    pushIntegerArg(args); // projection
    TcType *ret = makeBoolean();
    PROTECT(ret);
    pushNewBuiltIn(registry, "gfx_begin_mode_3d", ret, args,
                   (void *)builtin_gfx_begin_mode_3d,
                   "builtin_gfx_begin_mode_3d");
    UNPROTECT(save);
}

static void registerGfxEndMode3D(BuiltIns *registry) {
    BuiltInArgs *args = newBuiltInArgs();
    int save = PROTECT(args);
    TcType *ret = makeBoolean();
    PROTECT(ret);
    pushNewBuiltIn(registry, "gfx_end_mode_3d", ret, args,
                   (void *)builtin_gfx_end_mode_3d, "builtin_gfx_end_mode_3d");
    UNPROTECT(save);
}

static void registerGfxDrawCube(BuiltIns *registry) {
    BuiltInArgs *args = newBuiltInArgs();
    int save = PROTECT(args);
    pushAnyArg(args);     // center_x
    pushAnyArg(args);     // center_y
    pushAnyArg(args);     // center_z
    pushAnyArg(args);     // size_x
    pushAnyArg(args);     // size_y
    pushAnyArg(args);     // size_z
    pushIntegerArg(args); // r
    pushIntegerArg(args); // g
    pushIntegerArg(args); // b
    pushIntegerArg(args); // a
    TcType *ret = makeBoolean();
    PROTECT(ret);
    pushNewBuiltIn(registry, "gfx_draw_cube", ret, args,
                   (void *)builtin_gfx_draw_cube, "builtin_gfx_draw_cube");
    UNPROTECT(save);
}

static void registerGfxDrawCubeWires(BuiltIns *registry) {
    BuiltInArgs *args = newBuiltInArgs();
    int save = PROTECT(args);
    pushAnyArg(args);     // center_x
    pushAnyArg(args);     // center_y
    pushAnyArg(args);     // center_z
    pushAnyArg(args);     // size_x
    pushAnyArg(args);     // size_y
    pushAnyArg(args);     // size_z
    pushIntegerArg(args); // r
    pushIntegerArg(args); // g
    pushIntegerArg(args); // b
    pushIntegerArg(args); // a
    TcType *ret = makeBoolean();
    PROTECT(ret);
    pushNewBuiltIn(registry, "gfx_draw_cube_wires", ret, args,
                   (void *)builtin_gfx_draw_cube_wires,
                   "builtin_gfx_draw_cube_wires");
    UNPROTECT(save);
}

static void registerGfxDrawGrid(BuiltIns *registry) {
    BuiltInArgs *args = newBuiltInArgs();
    int save = PROTECT(args);
    pushIntegerArg(args); // slices
    pushIntegerArg(args); // spacing
    TcType *ret = makeBoolean();
    PROTECT(ret);
    pushNewBuiltIn(registry, "gfx_draw_grid", ret, args,
                   (void *)builtin_gfx_draw_grid, "builtin_gfx_draw_grid");
    UNPROTECT(save);
}

static void registerGfxDrawSphere(BuiltIns *registry) {
    BuiltInArgs *args = newBuiltInArgs();
    int save = PROTECT(args);
    pushIntegerArg(args); // center_x
    pushIntegerArg(args); // center_y
    pushIntegerArg(args); // center_z
    pushIntegerArg(args); // radius
    pushIntegerArg(args); // r
    pushIntegerArg(args); // g
    pushIntegerArg(args); // b
    pushIntegerArg(args); // a
    TcType *ret = makeBoolean();
    PROTECT(ret);
    pushNewBuiltIn(registry, "gfx_draw_sphere", ret, args,
                   (void *)builtin_gfx_draw_sphere, "builtin_gfx_draw_sphere");
    UNPROTECT(save);
}

static void registerGfxDrawSphereWires(BuiltIns *registry) {
    BuiltInArgs *args = newBuiltInArgs();
    int save = PROTECT(args);
    pushIntegerArg(args); // center_x
    pushIntegerArg(args); // center_y
    pushIntegerArg(args); // center_z
    pushIntegerArg(args); // radius
    pushIntegerArg(args); // rings
    pushIntegerArg(args); // slices
    pushIntegerArg(args); // r
    pushIntegerArg(args); // g
    pushIntegerArg(args); // b
    pushIntegerArg(args); // a
    TcType *ret = makeBoolean();
    PROTECT(ret);
    pushNewBuiltIn(registry, "gfx_draw_sphere_wires", ret, args,
                   (void *)builtin_gfx_draw_sphere_wires,
                   "builtin_gfx_draw_sphere_wires");
    UNPROTECT(save);
}

static void registerGfxDrawCylinder(BuiltIns *registry) {
    BuiltInArgs *args = newBuiltInArgs();
    int save = PROTECT(args);
    pushIntegerArg(args); // pos_x
    pushIntegerArg(args); // pos_y
    pushIntegerArg(args); // pos_z
    pushIntegerArg(args); // radius_top
    pushIntegerArg(args); // radius_bottom
    pushIntegerArg(args); // height
    pushIntegerArg(args); // slices
    pushIntegerArg(args); // r
    pushIntegerArg(args); // g
    pushIntegerArg(args); // b
    pushIntegerArg(args); // a
    TcType *ret = makeBoolean();
    PROTECT(ret);
    pushNewBuiltIn(registry, "gfx_draw_cylinder", ret, args,
                   (void *)builtin_gfx_draw_cylinder,
                   "builtin_gfx_draw_cylinder");
    UNPROTECT(save);
}

static void registerGfxDrawCylinderWires(BuiltIns *registry) {
    BuiltInArgs *args = newBuiltInArgs();
    int save = PROTECT(args);
    pushIntegerArg(args); // pos_x
    pushIntegerArg(args); // pos_y
    pushIntegerArg(args); // pos_z
    pushIntegerArg(args); // radius_top
    pushIntegerArg(args); // radius_bottom
    pushIntegerArg(args); // height
    pushIntegerArg(args); // slices
    pushIntegerArg(args); // r
    pushIntegerArg(args); // g
    pushIntegerArg(args); // b
    pushIntegerArg(args); // a
    TcType *ret = makeBoolean();
    PROTECT(ret);
    pushNewBuiltIn(registry, "gfx_draw_cylinder_wires", ret, args,
                   (void *)builtin_gfx_draw_cylinder_wires,
                   "builtin_gfx_draw_cylinder_wires");
    UNPROTECT(save);
}

static void registerGfxDrawPlane(BuiltIns *registry) {
    BuiltInArgs *args = newBuiltInArgs();
    int save = PROTECT(args);
    pushIntegerArg(args); // center_x
    pushIntegerArg(args); // center_y
    pushIntegerArg(args); // center_z
    pushIntegerArg(args); // size_x
    pushIntegerArg(args); // size_z
    pushIntegerArg(args); // r
    pushIntegerArg(args); // g
    pushIntegerArg(args); // b
    pushIntegerArg(args); // a
    TcType *ret = makeBoolean();
    PROTECT(ret);
    pushNewBuiltIn(registry, "gfx_draw_plane", ret, args,
                   (void *)builtin_gfx_draw_plane, "builtin_gfx_draw_plane");
    UNPROTECT(save);
}

static void registerGfxDrawLine3D(BuiltIns *registry) {
    BuiltInArgs *args = newBuiltInArgs();
    int save = PROTECT(args);
    pushIntegerArg(args); // start_x
    pushIntegerArg(args); // start_y
    pushIntegerArg(args); // start_z
    pushIntegerArg(args); // end_x
    pushIntegerArg(args); // end_y
    pushIntegerArg(args); // end_z
    pushIntegerArg(args); // r
    pushIntegerArg(args); // g
    pushIntegerArg(args); // b
    pushIntegerArg(args); // a
    TcType *ret = makeBoolean();
    PROTECT(ret);
    pushNewBuiltIn(registry, "gfx_draw_line_3d", ret, args,
                   (void *)builtin_gfx_draw_line_3d,
                   "builtin_gfx_draw_line_3d");
    UNPROTECT(save);
}

static void registerGfxLoadModel(BuiltIns *registry) {
    BuiltInArgs *args = newBuiltInArgs();
    int save = PROTECT(args);
    pushStringArg(args);
    TcType *errType = makeStringType();
    PROTECT(errType);
    TcType *modelType = makeModelType();
    PROTECT(modelType);
    TcType *retType = makeTryType(errType, modelType);
    PROTECT(retType);
    pushNewBuiltIn(registry, "gfx_load_model", retType, args,
                   (void *)builtin_gfx_load_model, "builtin_gfx_load_model");
    UNPROTECT(save);
}

static void registerGfxUnloadModel(BuiltIns *registry) {
    BuiltInArgs *args = newBuiltInArgs();
    int save = PROTECT(args);
    pushModelArg(args);
    TcType *ret = makeBoolean();
    PROTECT(ret);
    pushNewBuiltIn(registry, "gfx_unload_model", ret, args,
                   (void *)builtin_gfx_unload_model,
                   "builtin_gfx_unload_model");
    UNPROTECT(save);
}

static void registerGfxDrawModel(BuiltIns *registry) {
    BuiltInArgs *args = newBuiltInArgs();
    int save = PROTECT(args);
    pushModelArg(args);
    pushIntegerArg(args); // pos_x
    pushIntegerArg(args); // pos_y
    pushIntegerArg(args); // pos_z
    pushIntegerArg(args); // scale
    pushIntegerArg(args); // r
    pushIntegerArg(args); // g
    pushIntegerArg(args); // b
    pushIntegerArg(args); // a
    TcType *ret = makeBoolean();
    PROTECT(ret);
    pushNewBuiltIn(registry, "gfx_draw_model", ret, args,
                   (void *)builtin_gfx_draw_model, "builtin_gfx_draw_model");
    UNPROTECT(save);
}

static void registerGfxDrawModelWires(BuiltIns *registry) {
    BuiltInArgs *args = newBuiltInArgs();
    int save = PROTECT(args);
    pushModelArg(args);
    pushIntegerArg(args); // pos_x
    pushIntegerArg(args); // pos_y
    pushIntegerArg(args); // pos_z
    pushIntegerArg(args); // scale
    pushIntegerArg(args); // r
    pushIntegerArg(args); // g
    pushIntegerArg(args); // b
    pushIntegerArg(args); // a
    TcType *ret = makeBoolean();
    PROTECT(ret);
    pushNewBuiltIn(registry, "gfx_draw_model_wires", ret, args,
                   (void *)builtin_gfx_draw_model_wires,
                   "builtin_gfx_draw_model_wires");
    UNPROTECT(save);
}

static void registerGfxAudioOpen(BuiltIns *registry) {
    BuiltInArgs *args = newBuiltInArgs();
    int save = PROTECT(args);
    TcType *ret = makeBoolean();
    PROTECT(ret);
    pushNewBuiltIn(registry, "gfx_audio_open", ret, args,
                   (void *)builtin_gfx_audio_open, "builtin_gfx_audio_open");
    UNPROTECT(save);
}

static void registerGfxAudioClose(BuiltIns *registry) {
    BuiltInArgs *args = newBuiltInArgs();
    int save = PROTECT(args);
    TcType *ret = makeBoolean();
    PROTECT(ret);
    pushNewBuiltIn(registry, "gfx_audio_close", ret, args,
                   (void *)builtin_gfx_audio_close, "builtin_gfx_audio_close");
    UNPROTECT(save);
}

static void registerGfxLoadSound(BuiltIns *registry) {
    BuiltInArgs *args = newBuiltInArgs();
    int save = PROTECT(args);
    pushStringArg(args);
    TcType *errType = makeStringType();
    PROTECT(errType);
    TcType *sndType = makeSoundType();
    PROTECT(sndType);
    TcType *retType = makeTryType(errType, sndType);
    PROTECT(retType);
    pushNewBuiltIn(registry, "gfx_load_sound", retType, args,
                   (void *)builtin_gfx_load_sound, "builtin_gfx_load_sound");
    UNPROTECT(save);
}

static void registerGfxUnloadSound(BuiltIns *registry) {
    BuiltInArgs *args = newBuiltInArgs();
    int save = PROTECT(args);
    pushSoundArg(args);
    TcType *ret = makeBoolean();
    PROTECT(ret);
    pushNewBuiltIn(registry, "gfx_unload_sound", ret, args,
                   (void *)builtin_gfx_unload_sound,
                   "builtin_gfx_unload_sound");
    UNPROTECT(save);
}

static void registerGfxPlaySound(BuiltIns *registry) {
    BuiltInArgs *args = newBuiltInArgs();
    int save = PROTECT(args);
    pushSoundArg(args);
    TcType *ret = makeBoolean();
    PROTECT(ret);
    pushNewBuiltIn(registry, "gfx_play_sound", ret, args,
                   (void *)builtin_gfx_play_sound, "builtin_gfx_play_sound");
    UNPROTECT(save);
}

static void registerGfxStopSound(BuiltIns *registry) {
    BuiltInArgs *args = newBuiltInArgs();
    int save = PROTECT(args);
    pushSoundArg(args);
    TcType *ret = makeBoolean();
    PROTECT(ret);
    pushNewBuiltIn(registry, "gfx_stop_sound", ret, args,
                   (void *)builtin_gfx_stop_sound, "builtin_gfx_stop_sound");
    UNPROTECT(save);
}

static void registerGfxSetSoundVolume(BuiltIns *registry) {
    BuiltInArgs *args = newBuiltInArgs();
    int save = PROTECT(args);
    pushSoundArg(args);
    pushIntegerArg(args); // volume_percent 0..100
    TcType *ret = makeBoolean();
    PROTECT(ret);
    pushNewBuiltIn(registry, "gfx_set_sound_volume", ret, args,
                   (void *)builtin_gfx_set_sound_volume,
                   "builtin_gfx_set_sound_volume");
    UNPROTECT(save);
}

static void registerGfxIsSoundPlaying(BuiltIns *registry) {
    BuiltInArgs *args = newBuiltInArgs();
    int save = PROTECT(args);
    pushSoundArg(args);
    TcType *ret = makeBoolean();
    PROTECT(ret);
    pushNewBuiltIn(registry, "gfx_is_sound_playing", ret, args,
                   (void *)builtin_gfx_is_sound_playing,
                   "builtin_gfx_is_sound_playing");
    UNPROTECT(save);
}

static void registerGfxLoadMusic(BuiltIns *registry) {
    BuiltInArgs *args = newBuiltInArgs();
    int save = PROTECT(args);
    pushStringArg(args);
    TcType *errType = makeStringType();
    PROTECT(errType);
    TcType *musicType = makeMusicType();
    PROTECT(musicType);
    TcType *retType = makeTryType(errType, musicType);
    PROTECT(retType);
    pushNewBuiltIn(registry, "gfx_load_music", retType, args,
                   (void *)builtin_gfx_load_music, "builtin_gfx_load_music");
    UNPROTECT(save);
}

static void registerGfxUnloadMusic(BuiltIns *registry) {
    BuiltInArgs *args = newBuiltInArgs();
    int save = PROTECT(args);
    pushMusicArg(args);
    TcType *ret = makeBoolean();
    PROTECT(ret);
    pushNewBuiltIn(registry, "gfx_unload_music", ret, args,
                   (void *)builtin_gfx_unload_music,
                   "builtin_gfx_unload_music");
    UNPROTECT(save);
}

static void registerGfxPlayMusic(BuiltIns *registry) {
    BuiltInArgs *args = newBuiltInArgs();
    int save = PROTECT(args);
    pushMusicArg(args);
    TcType *ret = makeBoolean();
    PROTECT(ret);
    pushNewBuiltIn(registry, "gfx_play_music", ret, args,
                   (void *)builtin_gfx_play_music, "builtin_gfx_play_music");
    UNPROTECT(save);
}

static void registerGfxPauseMusic(BuiltIns *registry) {
    BuiltInArgs *args = newBuiltInArgs();
    int save = PROTECT(args);
    pushMusicArg(args);
    TcType *ret = makeBoolean();
    PROTECT(ret);
    pushNewBuiltIn(registry, "gfx_pause_music", ret, args,
                   (void *)builtin_gfx_pause_music, "builtin_gfx_pause_music");
    UNPROTECT(save);
}

static void registerGfxResumeMusic(BuiltIns *registry) {
    BuiltInArgs *args = newBuiltInArgs();
    int save = PROTECT(args);
    pushMusicArg(args);
    TcType *ret = makeBoolean();
    PROTECT(ret);
    pushNewBuiltIn(registry, "gfx_resume_music", ret, args,
                   (void *)builtin_gfx_resume_music,
                   "builtin_gfx_resume_music");
    UNPROTECT(save);
}

static void registerGfxStopMusic(BuiltIns *registry) {
    BuiltInArgs *args = newBuiltInArgs();
    int save = PROTECT(args);
    pushMusicArg(args);
    TcType *ret = makeBoolean();
    PROTECT(ret);
    pushNewBuiltIn(registry, "gfx_stop_music", ret, args,
                   (void *)builtin_gfx_stop_music, "builtin_gfx_stop_music");
    UNPROTECT(save);
}

static void registerGfxSetMusicVolume(BuiltIns *registry) {
    BuiltInArgs *args = newBuiltInArgs();
    int save = PROTECT(args);
    pushMusicArg(args);
    pushIntegerArg(args); // volume_percent 0..100
    TcType *ret = makeBoolean();
    PROTECT(ret);
    pushNewBuiltIn(registry, "gfx_set_music_volume", ret, args,
                   (void *)builtin_gfx_set_music_volume,
                   "builtin_gfx_set_music_volume");
    UNPROTECT(save);
}

static void registerGfxIsMusicPlaying(BuiltIns *registry) {
    BuiltInArgs *args = newBuiltInArgs();
    int save = PROTECT(args);
    pushMusicArg(args);
    TcType *ret = makeBoolean();
    PROTECT(ret);
    pushNewBuiltIn(registry, "gfx_is_music_playing", ret, args,
                   (void *)builtin_gfx_is_music_playing,
                   "builtin_gfx_is_music_playing");
    UNPROTECT(save);
}

static void registerGfxUpdateMusicStream(BuiltIns *registry) {
    BuiltInArgs *args = newBuiltInArgs();
    int save = PROTECT(args);
    pushMusicArg(args);
    TcType *ret = makeBoolean();
    PROTECT(ret);
    pushNewBuiltIn(registry, "gfx_update_music_stream", ret, args,
                   (void *)builtin_gfx_update_music_stream,
                   "builtin_gfx_update_music_stream");
    UNPROTECT(save);
}

static void registerGfxSeekMusicMs(BuiltIns *registry) {
    BuiltInArgs *args = newBuiltInArgs();
    int save = PROTECT(args);
    pushMusicArg(args);
    pushIntegerArg(args); // position_ms
    TcType *ret = makeBoolean();
    PROTECT(ret);
    pushNewBuiltIn(registry, "gfx_seek_music_ms", ret, args,
                   (void *)builtin_gfx_seek_music_ms,
                   "builtin_gfx_seek_music_ms");
    UNPROTECT(save);
}

static void registerGfxMusicTimePlayedMs(BuiltIns *registry) {
    BuiltInArgs *args = newBuiltInArgs();
    int save = PROTECT(args);
    pushMusicArg(args);
    TcType *ret = newTcType_BigInteger();
    PROTECT(ret);
    pushNewBuiltIn(registry, "gfx_music_time_played_ms", ret, args,
                   (void *)builtin_gfx_music_time_played_ms,
                   "builtin_gfx_music_time_played_ms");
    UNPROTECT(save);
}

static void registerGfxMusicTimeLengthMs(BuiltIns *registry) {
    BuiltInArgs *args = newBuiltInArgs();
    int save = PROTECT(args);
    pushMusicArg(args);
    TcType *ret = newTcType_BigInteger();
    PROTECT(ret);
    pushNewBuiltIn(registry, "gfx_music_time_length_ms", ret, args,
                   (void *)builtin_gfx_music_time_length_ms,
                   "builtin_gfx_music_time_length_ms");
    UNPROTECT(save);
}

static void registerGfxMusicProgressWidth(BuiltIns *registry) {
    BuiltInArgs *args = newBuiltInArgs();
    int save = PROTECT(args);
    pushMusicArg(args);
    pushIntegerArg(args);
    TcType *ret = newTcType_BigInteger();
    PROTECT(ret);
    pushNewBuiltIn(registry, "gfx_music_progress_width", ret, args,
                   (void *)builtin_gfx_music_progress_width,
                   "builtin_gfx_music_progress_width");
    UNPROTECT(save);
}

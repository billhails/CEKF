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
static void registerGfxIsMouseDown(BuiltIns *registry);
static void registerGfxIsMousePressed(BuiltIns *registry);
static void registerGfxScreenWidth(BuiltIns *registry);
static void registerGfxScreenHeight(BuiltIns *registry);
static void registerGfxFrameTimeMs(BuiltIns *registry);
static void registerGfxDrawLine(BuiltIns *registry);
static void registerGfxDrawCircle(BuiltIns *registry);
static void registerGfxFillCircle(BuiltIns *registry);
static void registerGfxDrawRect(BuiltIns *registry);
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
static void registerGfxBeginMode2D(BuiltIns *registry);
static void registerGfxEndMode2D(BuiltIns *registry);

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
    registerGfxIsMouseDown(registry);
    registerGfxIsMousePressed(registry);
    registerGfxScreenWidth(registry);
    registerGfxScreenHeight(registry);
    registerGfxFrameTimeMs(registry);
    registerGfxDrawLine(registry);
    registerGfxDrawCircle(registry);
    registerGfxFillCircle(registry);
    registerGfxDrawRect(registry);
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
    registerGfxBeginMode2D(registry);
    registerGfxEndMode2D(registry);
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
}

#ifdef ENABLE_RAYLIB
static struct {
    bool initialized;
    bool in_frame;
    bool in_texture_mode;
    bool in_camera_mode;
} gfx_state = {false, false, false, false};

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
    gfx_state.in_camera_mode = false;
    return makeTryResult(1, value_Stdint(1));
#endif
}

Value builtin_gfx_close(Vec *args) {
    (void)args;
#ifndef ENABLE_RAYLIB
    return value_Stdint(0);
#else
    if (!gfx_state.initialized) {
        return value_Stdint(0);
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
    renderTextureRegistryDrain();
    textureRegistryDrain();
    fontRegistryDrain();
    CloseWindow();
    gfx_state.initialized = false;
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
    int x1 = (int)getValue_Stdint(args->entries[0]);
    int y1 = (int)getValue_Stdint(args->entries[1]);
    int x2 = (int)getValue_Stdint(args->entries[2]);
    int y2 = (int)getValue_Stdint(args->entries[3]);
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
    int cx = (int)getValue_Stdint(args->entries[0]);
    int cy = (int)getValue_Stdint(args->entries[1]);
    int radius = (int)getValue_Stdint(args->entries[2]);
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
    int cx = (int)getValue_Stdint(args->entries[0]);
    int cy = (int)getValue_Stdint(args->entries[1]);
    int radius = (int)getValue_Stdint(args->entries[2]);
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
    int x = (int)getValue_Stdint(args->entries[0]);
    int y = (int)getValue_Stdint(args->entries[1]);
    int w = (int)getValue_Stdint(args->entries[2]);
    int h = (int)getValue_Stdint(args->entries[3]);
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

Value builtin_gfx_begin_mode_2d(Vec *args) {
#ifndef ENABLE_RAYLIB
    (void)args;
    return value_Stdint(0);
#else
    if (!gfx_state.initialized || gfx_state.in_camera_mode || !canDrawTarget())
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
    pushIntegerArg(args); // x1
    pushIntegerArg(args); // y1
    pushIntegerArg(args); // x2
    pushIntegerArg(args); // y2
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

static void registerGfxBeginMode2D(BuiltIns *registry) {
    BuiltInArgs *args = newBuiltInArgs();
    int save = PROTECT(args);
    pushIntegerArg(args); // target_x
    pushIntegerArg(args); // target_y
    pushIntegerArg(args); // offset_x
    pushIntegerArg(args); // offset_y
    pushIntegerArg(args); // rotation
    pushIntegerArg(args); // zoom
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

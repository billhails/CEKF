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
}

#ifdef ENABLE_RAYLIB
static struct {
    bool initialized;
    bool in_frame;
} gfx_state = {false, false};

static int extractChannel(Value v) {
    int n = (int)getValue_Stdint(v);
    if (n < 0 || n > 255)
        return -1;
    return n;
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
    if (gfx_state.in_frame) {
        EndDrawing();
        gfx_state.in_frame = false;
    }
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
    if (!gfx_state.in_frame) {
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
    if (!gfx_state.in_frame) {
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
    if (!gfx_state.in_frame) {
        return value_Stdint(0);
    }
    SCharVec *text = listToUtf8(args->entries[0]);
    int save = PROTECT(text);
    int x = (int)getValue_Stdint(args->entries[1]);
    int y = (int)getValue_Stdint(args->entries[2]);
    int sz = (int)getValue_Stdint(args->entries[3]);
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
    if (!gfx_state.in_frame)
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
    if (!gfx_state.in_frame)
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
    if (!gfx_state.in_frame)
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
    if (!gfx_state.in_frame)
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

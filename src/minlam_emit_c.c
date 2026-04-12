/*
 * CEKF - VM supporting amb
 * Copyright (C) 2022-2026  Bill Hails
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
 *
 * Minimal AST after desugaring
 * Generated from src/minlam.yaml by tools/generate.py
 */

#include "minlam_emit_c.h"
#include "memory.h"
#include "minlam.h"
#include "minlam_pp.h"
#include "symbol.h"
#include "utils_helper.h"
#include <ctype.h>
#include <math.h>
#include <stdarg.h>
#include <sys/param.h>

///////////////////////
// Emit Buffer Helpers
///////////////////////

typedef CEmitterContext EC;
typedef EmitCResult ER;

#define EMITLOC(name, node, ctx)                                               \
    if (node == NULL || CPI(node).lineNo == 0)                                 \
        fprintf(FH(ctx), "// %s\n", name);                                     \
    else                                                                       \
        fprintf(FH(ctx), "// %s +%d %s\n", name, CPI(node).lineNo,             \
                CPI(node).fileName)

typedef struct EmitBuffer {
    FILE *fh;
    char *buffer;
    size_t size;
} EmitBuffer;

static EmitBuffer *newEmitBuffer();
static char *getEmitBuffer(EmitBuffer *);
static void cleanEmitBuffer(void *);
static void printEmitBuffer(FILE *, void *);

static inline Opaque *newOpaque_EmitBuffer() {
    // no protection as buffer is manually cleaned
    EmitBuffer *buffer = newEmitBuffer();
    return newOpaque(buffer, cleanEmitBuffer, printEmitBuffer, NULL);
}

static inline char *opaqueEmitBufferContent(Opaque *container) {
    return getEmitBuffer((EmitBuffer *)container->data);
}

static inline FILE *opaqueEmitBufferFh(Opaque *container) {
    return ((EmitBuffer *)(container->data))->fh;
}

static inline FILE *FH(CEmitterContext *ctx) {
    return opaqueEmitBufferFh(ctx->body);
}

static EmitBuffer *newEmitBuffer() {
    EmitBuffer *result = ALLOCATE(EmitBuffer);
    result->buffer = NULL;
    result->size = 0;
    result->fh = open_memstream(&result->buffer, &result->size);
    return result;
}

static char *getEmitBuffer(EmitBuffer *buffer) {
    fflush(buffer->fh);
    if (buffer->buffer == NULL)
        return "";
    else
        return buffer->buffer;
}

static void cleanEmitBuffer(void *buffer) {
    EmitBuffer *b = (EmitBuffer *)buffer;
    fclose(b->fh);
    free(b->buffer);
    FREE(b, EmitBuffer);
}

static void printEmitBuffer(FILE *fp, void *buffer) {
    EmitBuffer *b = (EmitBuffer *)buffer;
    if (b != NULL) {
        fflush(b->fh);
        if (b->buffer != NULL)
            fprintf(fp, "%s", b->buffer);
    }
}

/////////////////
// Leaf Emitters
/////////////////

static ER *emitAddrResult(SCharArray *label) {
    Opaque *buf = newOpaque_EmitBuffer();
    int save = PROTECT(buf);
    fprintf(opaqueEmitBufferFh(buf), "value_Addr(&&%s)", label->entries);
    ER *result = newEmitCResult_Buf(buf);
    UNPROTECT(save);
    return result;
}

static ER *emitIntegerResult(Integer i) {
    Opaque *buf = newOpaque_EmitBuffer();
    int save = PROTECT(buf);
    fprintf(opaqueEmitBufferFh(buf), "value_Stdint(%d)", i);
    ER *result = newEmitCResult_Constant(buf);
    UNPROTECT(save);
    return result;
}

static ER *emitCharacterResult(Character c) {
    Opaque *buf = newOpaque_EmitBuffer();
    int save = PROTECT(buf);
    fprintf(opaqueEmitBufferFh(buf), "value_Character(%d)", (int)c);
    ER *result = newEmitCResult_Constant(buf);
    UNPROTECT(save);
    return result;
}

static void comment(EC *ctx, char *fmt, ...) {
    va_list args;
    va_start(args, fmt);
    fprintf(FH(ctx), "// ");
    vfprintf(FH(ctx), fmt, args);
    fprintf(FH(ctx), "\n");
    va_end(args);
}

static void emitCharacter(Character character, EC *ctx) {
    fprintf(FH(ctx), "value_Character(%d)", (int)character);
}

static void emitInteger(Integer i, EC *ctx) {
    fprintf(FH(ctx), "value_Stdint(%d)", i);
}

#include "minlam_emit.inc"

//////////////
// Public API
//////////////

void emitCProgram(MinExp *node, BuiltIns *builtIns, FILE *out) {
    Opaque *body = newOpaque_EmitBuffer();
    int save = PROTECT(body);
    HashSymbol *main = newSymbol("main");
    SymbolArray *heap = emitter_createHeap();
    PROTECT(heap);
    EmitterContext context = newEmitterContext(main, builtIns, heap);
    PROTECT(context.slots);
    CEmitterContext *ctx = newCEmitterContext(body, context);
    REPLACE_PROTECT(save, ctx);
    fprintf(out, "// GENERATED CODE DO NOT EDIT\n");
    fprintf(out, "#include \"minlam_runtime.h\"\n");
    fprintf(out, "extern int forceGcFlag;\n");
    fprintf(out, "#ifdef TRACE_GOTO\n");
    fprintf(out, "#  define TRACE(from, target, nargs) "
                 "fprintf(stderr, \"%%s -> %%p (%%d args)\\n\", (from), (void "
                 "*)(target), (nargs))\n");
    fprintf(out, "#else\n");
    fprintf(out, "#  define TRACE(from, target, nargs)\n");
    fprintf(out, "#endif\n");
    emitMinExp(node, ctx);
    fprintf(out, "#define MAX_REG %d\n", ctx->context.maxReg);
    fprintf(out, "static Value reg[MAX_REG];\n");
    fprintf(out, "\n");

    fprintf(out, "int main(int argc, char *argv[]) {\n");
    fprintf(out, "for (int i = 0; i < MAX_REG; i++) {\n");
    fprintf(out, "reg[i] = value_None();\n");
    fprintf(out, "}\n");
    fprintf(out, "minlam_runtime_init(reg, MAX_REG, argc, argv);\n");
    fprintf(out, "// forceGcFlag = 1;\n");
    fprintf(out, "goto ENTRY;\n");
    Index i = 0;
    HashSymbol *label = NULL;
    Opaque *buf = NULL;
    i = 0;
    while ((label = iterateCBufferBag(ctx->lambdas, &i, &buf)) != NULL) {
        SCharArray *l = makeLambdaLabel(label);
        fprintf(out, "%s:\n", l->entries);
        fprintf(out, "%s\n", opaqueEmitBufferContent(buf));
    }
    fprintf(out, "\nENTRY:\n");
    fprintf(out, "%s\n", opaqueEmitBufferContent(ctx->body));
    fprintf(out, "}\n");
}
/*
 * CEKF - VM supporting amb
 * Copyright (C) 2022-2023  Bill Hails
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

#include "module.h"
#include "parser.h"
#include "lexer.h"
#include "preamble.h"

typedef struct PmBufStack {
    int lineno;
    char *filename;
    YY_BUFFER_STATE bs;
    struct PmBufStack *next;
} PmBufStack;

static PmModule *newPmModule() {
    PmModule *x = malloc(sizeof(PmModule));
    yyscan_t scanner;
    yylex_init_extra(x, &scanner);
    x->bufStack = NULL;
    x->nest = NULL;
    x->scanner = scanner;
    return x;
}

static void pushPmBufStack(PmModule *mod, YY_BUFFER_STATE bs,
                           const char *origin) {
    PmBufStack *bufStack = malloc(sizeof(PmBufStack));
    bufStack->next = mod->bufStack;
    mod->bufStack = bufStack;
    bufStack->bs = bs;
    bufStack->filename = strdup(origin);
}

static FILE *safeFOpen(const char *filename) {
    FILE *f = fopen(filename, "r");
    if (f == NULL) {
        perror(filename);
        exit(1);
    }
    return f;
}

PmModule *newPmModuleFromFileHandle(FILE *f, const char *origin) {
    PmModule *mod = newPmModule();
    YY_BUFFER_STATE bs = yy_create_buffer(f, YY_BUF_SIZE, mod->scanner);
    pushPmBufStack(mod, bs, origin);
    return mod;
}

PmModule *newPmModuleFromStdin() {
    return newPmModuleFromFileHandle(stdin, "stdin");
}

PmModule *newPmModuleFromFile(const char *filename) {
    return newPmModuleFromFileHandle(safeFOpen(filename), filename);
}

PmModule *newPmModuleFromString(char *s, char *id) {
    PmModule *mod = newPmModule();
    YY_BUFFER_STATE bs = yy_scan_string(s, mod->scanner);
    pushPmBufStack(mod, bs, id);
    return mod;
}

static void pushPmToplevelFromBufState(PmModule *mod, YY_BUFFER_STATE bs,
                                       const char *origin) {
    pushPmBufStack(mod, yy_scan_string(postamble, mod->scanner), "postamble");
    pushPmBufStack(mod, bs, origin);
    pushPmBufStack(mod, yy_scan_string(preamble, mod->scanner), "preamble");
}

static void pushPmNamespaceFromBufState(PmModule *mod, YY_BUFFER_STATE bs,
                                        const char *origin) {
    pushPmBufStack(mod, bs, origin);
    pushPmBufStack(mod, yy_scan_string("__namespace__ ", mod->scanner), "namespace token");
}

PmModule *newPmToplevelFromFileHandle(FILE *f, const char *origin) {
    PmModule *mod = newPmModule();
    pushPmToplevelFromBufState(mod,
                               yy_create_buffer(f, YY_BUF_SIZE, mod->scanner),
                               origin);
    return mod;
}

PmModule *newPmNameSpaceFromFileHandle(FILE *f, const char *origin) {
    PmModule *mod = newPmModule();
    pushPmNamespaceFromBufState(mod,
                                yy_create_buffer(f, YY_BUF_SIZE, mod->scanner),
                                origin);
    return mod;
}

PmModule *newPmToplevelFromStdin() {
    return newPmToplevelFromFileHandle(stdin, "stdin");
}

PmModule *newPmToplevelFromFile(const char *filename) {
    return newPmToplevelFromFileHandle(safeFOpen(filename), filename);
}

PmModule *newPmToplevelFromString(char *s, char *id) {
    PmModule *mod = newPmModule();
    pushPmToplevelFromBufState(mod, yy_scan_string(s, mod->scanner), id);
    return mod;
}

static void freePmBufStack(PmModule *mod, PmBufStack * x) {
    if (x == NULL) {
        return;
    }
    freePmBufStack(mod, x->next);
    x->next = NULL;
    free(x->filename);
    yy_delete_buffer(x->bs, mod->scanner);
    free(x);
}

void freePmModule(PmModule *mod) {
    if (mod == NULL)
        return;
    freePmBufStack(mod, mod->bufStack);
    yylex_destroy(mod->scanner);
    free(mod);
}

int pmParseModule(PmModule *mod) {
    int res;

    yy_switch_to_buffer(mod->bufStack->bs, mod->scanner);
    res = yyparse(mod->scanner, mod);

    return res;
}

int popPmFile(PmModule *mod) {
    if (mod->bufStack == NULL)
        return 0;

    PmBufStack *old = mod->bufStack;
    free(old->filename);
    yy_delete_buffer(old->bs, mod->scanner);
    mod->bufStack = mod->bufStack->next;
    free(old);

    if (mod->bufStack == NULL)
        return 0;

    yy_switch_to_buffer(mod->bufStack->bs, mod->scanner);

    return 1;
}

void incLineNo(PmModule *mod) {
    if (mod != NULL && mod->bufStack != NULL)
        mod->bufStack->lineno++;
}

void showModuleState(FILE *fp, PmModule *mod) {
    if (mod == NULL) {
        fprintf(fp, "module is null\n");
        return;
    }
    if (mod->bufStack == NULL) {
        fprintf(fp, "module->bufStack is null\n");
        return;
    }
    fprintf(fp, "current file %s, line %d\n", mod->bufStack->filename,
            mod->bufStack->lineno + 1);
}

char *currentPmFile(PmModule *mod) {
    if (mod == NULL) {
        return NULL;
    }
    if (mod->bufStack == NULL) {
        return NULL;
    }
    return mod->bufStack->filename;
}

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

static void freePmModule(PmModule *mod) {
    if (mod == NULL)
        return;
    freePmBufStack(mod, mod->bufStack);
    yylex_destroy(mod->scanner);
    free(mod);
}

static void pushPmBufStack(PmModule *mod, YY_BUFFER_STATE bs,
                           const char *origin) {
    PmBufStack *bufStack = malloc(sizeof(PmBufStack));
    bufStack->next = mod->bufStack;
    mod->bufStack = bufStack;
    bufStack->bs = bs;
    bufStack->filename = strdup(origin);
}

static int pmParseModule(PmModule *mod) {
    yy_switch_to_buffer(mod->bufStack->bs, mod->scanner);
    return yyparse(mod->scanner, mod);
}

static FILE *safeFOpen(const char *filename) {
    FILE *f = fopen(filename, "r");
    if (f == NULL) {
        perror(filename);
        exit(1);
    }
    return f;
}

static AstDefinitions *parseNameSpace(PmModule *mod, YY_BUFFER_STATE bs, const char *origin) {
    pushPmBufStack(mod, bs, origin);
    pushPmBufStack(mod, yy_scan_string("__namespace__ ", mod->scanner), "namespace token");
    int res = pmParseModule(mod);
    if (res) {
        return NULL;
    }
    AstDefinitions *definitions = mod->nest->definitions;
    return definitions;
}

AstDefinitions *parseNameSpaceFromString(const char *namespace, const char *origin) {
    PmModule *mod = newPmModule();
    AstDefinitions *definitions = parseNameSpace(mod, yy_scan_string(namespace, mod->scanner), origin);
    freePmModule(mod);
    return definitions;
}

AstDefinitions *parseNameSpaceFromFileHandle(FILE *f, const char *origin) {
    PmModule *mod = newPmModule();
    AstDefinitions *definitions = parseNameSpace(mod, yy_create_buffer(f, YY_BUF_SIZE, mod->scanner), origin);
    freePmModule(mod);
    return definitions;
}

AstDefinitions *parseNameSpaceFromFileName(const char *fileName) {
    return parseNameSpaceFromFileHandle(safeFOpen(fileName), fileName);
}

static AstDefinitions *parsePreamble() {
    return parseNameSpaceFromString(preamble, "preamble");
}

static AstNest *parseTopLevel(PmModule *mod, YY_BUFFER_STATE bs, const char *origin) {
    AstDefinitions *definitions = parsePreamble();
    if (definitions == NULL) return NULL;
    pushPmBufStack(mod, bs, origin);
    int res = pmParseModule(mod);
    if (res) {
        return NULL;
    }
    AstExpression *expression = newAstExpression(AST_EXPRESSION_TYPE_NEST, AST_EXPRESSION_VAL_NEST(mod->nest));
    AstExpressions *expressions = newAstExpressions(expression, NULL);
    AstNest *nest = newAstNest(definitions, expressions);
    return nest;
}

AstNest *parseTopLevelFromFileHandle(FILE *f, const char *origin) {
    PmModule *mod = newPmModule();
    AstNest *nest = parseTopLevel(mod, yy_create_buffer(f, YY_BUF_SIZE, mod->scanner), origin);
    freePmModule(mod);
    return nest;
}

AstNest *parseTopLevelFromFileName(const char *fileName) {
    return parseTopLevelFromFileHandle(safeFOpen(fileName), fileName);
}

AstNest *parseTopLevelFromString(const char *string, const char *origin) {
    PmModule *mod = newPmModule();
    AstNest *nest = parseTopLevel(mod, yy_scan_string(string, mod->scanner), origin);
    freePmModule(mod);
    return nest;
}

static AstNest *parseSingleBuffer(PmModule *mod, YY_BUFFER_STATE bs, const char *origin) {
    pushPmBufStack(mod, bs, origin);
    int res = pmParseModule(mod);
    if (res) {
        return NULL;
    }
    return mod->nest;
}

AstNest *parseSingleString(char *string, char *origin) {
    PmModule *mod = newPmModule();
    AstNest *nest = parseSingleBuffer(mod, yy_scan_string(string, mod->scanner), origin);
    freePmModule(mod);
    return nest;
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

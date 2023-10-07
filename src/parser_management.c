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

// initial support for loading preamble etc.

#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>

#include "ast.h"
#include "parser.h"
#include "lexer.h"

typedef struct BufStack {
    struct BufStack *prev;
    YY_BUFFER_STATE bs;
    int lineno;
    char *filename;
    FILE *f;
} BufStack;

static BufStack *curbs = NULL;

extern AstNest *result;

int pm_newFile(char *filename);
int pm_popFile(void);

AstNest *pm_parseFile(char *filename) {
    if (pm_newFile("src/postamble.fn")) {
        if (pm_newFile(filename)) {
            if (pm_newFile("src/preamble.fn")) {
                yyparse();
            }
        }
    }
    return result;
}

int pm_newFile(char *filename) {
    FILE *f = fopen(filename, "r");
    if (!f) {
        perror(filename);
        return 0;
    }

    BufStack *bs = malloc(sizeof(BufStack));
    if (!bs) {
        perror("malloc");
        exit(1);
    }

    if (curbs) {
        curbs->lineno = yylineno;
    }
    bs->prev = curbs;

    bs->bs = yy_create_buffer(f, YY_BUF_SIZE);
    bs->f = f;
    bs->filename = strdup(filename);
    yy_switch_to_buffer(bs->bs);
    curbs = bs;
    yylineno = 1;
    return 1;
}

int pm_popFile(void) {
    BufStack *bs = curbs;
    BufStack *prevbs;

    if (!bs) return 0;

    fclose(bs->f);
    free(bs->filename);
    yy_delete_buffer(bs->bs);

    prevbs = bs->prev;
    free(bs);

    if (!prevbs) return 0;

    yy_switch_to_buffer(prevbs->bs);
    curbs = prevbs;
    yylineno = curbs->lineno;

    return 1;
}

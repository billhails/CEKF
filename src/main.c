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
#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>

#include "common.h"
#include "ast.h"
#include "debug_ast.h"
#include "debug_tin.h"
#include "debug_lambda.h"
#include "lambda_conversion.h"
#include "parser.h"
#include "analysis.h"
#include "exp.h"
#include "memory.h"
#include "step.h"
#include "debug.h"
#include "bytecode.h"
#include "desugaring.h"
#include "algorithm_W.h"
#include "tin.h"
#include "tin_helper.h"
#include "hash.h"

#ifdef DEBUG_RUN_TESTS
#if DEBUG_RUN_TESTS == 1

#include "tests/exp.inc"

#elif DEBUG_RUN_TESTS == 2 /* testing parser */

// extern int yydebug;

extern AstNest *result;

int main(int argc, char *argv[]) {
    disableGC();
    // yydebug = 1;
    yyparse();
    printAstNest(result, 0);
    enableGC();
    printf("\n");
    char *foo = NEW_ARRAY(char, 10); // force gc
}

#elif DEBUG_RUN_TESTS == 3

extern void testTin();

int main(int argc, char *argv[]) {
    testTin();
}

#elif DEBUG_RUN_TESTS == 4

extern AstNest *result;

int main(int argc, char *argv[]) {
    disableGC();
    // yydebug = 1;
    yyparse();
    PROTECT(result);
    enableGC();
    // quietPrintHashTable = true;
    WResult *wr = WTop(result);
    showTinMonoType(wr->monoType);
    printf("\n");
    if (hadErrors()) {
        printf("(errors detected)\n");
    }
}

#else

extern AstNest *result;

int main(int argc, char *argv[]) {
    disableGC();
    // yydebug = 1;
    yyparse();
    PROTECT(result);
    enableGC();
    // quietPrintHashTable = true;
    WResult *wr = WTop(result);
    showTinMonoType(wr->monoType);
    printf("\n");
    if (hadErrors()) {
        printf("(errors detected)\n");
    } else {
        LamExp *exp = lamConvertNest(result, NULL);
        printLamExp(exp, 0);
        printf("\n");
    }
}

#endif

#else

int main(int argc, char *argv[]) {
}

#endif

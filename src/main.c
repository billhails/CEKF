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
#include <getopt.h>
#include <time.h>

#include "common.h"
#include "ast.h"
#include "ast_debug.h"
#include "lambda_debug.h"
#include "lambda_conversion.h"
#include "module.h"
// #include "parser.h"
#include "analysis.h"
#include "exp.h"
#include "memory.h"
#include "step.h"
#include "debug.h"
#include "bytecode.h"
#include "desugaring.h"
#include "hash.h"
#include "lambda_pp.h"
#include "anf.h"
#include "bigint.h"
#include "tc_analyze.h"
#include "tc_debug.h"

#ifdef DEBUG_RUN_TESTS
#if DEBUG_RUN_TESTS == 1

#include "tests/exp.inc"

#elif DEBUG_RUN_TESTS == 2 /* testing parser */


int main(int argc, char *argv[]) {
    initProtection();
    disableGC();
    if (argc < 2) {
        eprintf("need filename\n");
        exit(1);
    }
    AstNest *result = pm_parseFile(argv[1]);
    printAstNest(result, 0);
    enableGC();
    printf("\n");
    char *foo = NEW_ARRAY(char, 10); // force gc
}

#elif DEBUG_RUN_TESTS == 3 // testing type inference

extern void testTin();

int main(int argc, char *argv[]) {
    initProtection();
    testTin();
}

#else // testing lambda conversion

extern AstNest *result;

int main(int argc, char *argv[]) {
    initProtection();
    disableGC();
    if (argc < 2) {
        eprintf("need filename\n");
        exit(1);
    }
    AstNest *result = pm_parseFile(argv[1]);
    PROTECT(result);
    enableGC();
    // quietPrintHashTable = true;
    WResult *wr = WTop(result);
    // showTinMonoType(wr->monoType);
    // printf("\n");
    validateLastAlloc();
    if (hadErrors()) {
        printf("(errors detected)\n");
    } else {
        LamExp *exp = lamConvertNest(result, NULL);
        int save = PROTECT(exp);
        // ppLamExp(exp);
        // printf("\n");
        Exp *anfExp = anfNormalize(exp);
        printExp(anfExp);
        eprintf("\n");
    }
}

#endif

#else

int report_mem_flag = 0;
static int report_time_flag = 0;

int main(int argc, char *argv[]) {
    int c;
    clock_t begin = clock();

    while (1) {
        static struct option long_options[] =
        {
          {"bigint", no_argument, &bigint_flag, 1},
          {"report-memory", no_argument, &report_mem_flag, 1},
          {"report-time", no_argument, &report_time_flag, 1},
          {0, 0, 0, 0}
        };
        int option_index = 0;

        c = getopt_long (argc, argv, "", long_options, &option_index);

        if (c == -1)
            break;
    }

    ByteCodeArray byteCodes;
    initProtection();
    disableGC();
    /*
    printf("char: %ld\n", sizeof(char));
    printf("word: %ld\n", sizeof(word));
    printf("int: %ld\n", sizeof(int));
    printf("bigint_word: %ld\n", sizeof(bigint_word));
    printf("void *: %ld\n", sizeof(void *));
    */

    if (optind >= argc) {
        eprintf("need filename\n");
        exit(1);
    }
    // parse => AST
    PmModule *mod = newPmToplevelFromFile(argv[optind]);
    PROTECT(mod);
    pmParseModule(mod);
    enableGC();
    // lambda conversion: AST => LamExp
    LamExp *exp = lamConvertNest(mod->nest, NULL);
    int save = PROTECT(exp);
#ifdef DEBUG_LAMBDA_CONVERT
    ppLamExp(exp);
    eprintf("\n");
#endif
    // type checking
    TcEnv *env = tc_init();
    PROTECT(env);
    TcType *res = tc_analyze(exp, env);
    if (hadErrors()) {
        return 1;
    }
    PROTECT(res);
    ppTcType(res);
    eprintf("\n");
    // normalization: LamExp => ANF
    Exp *anfExp = anfNormalize(exp);
    PROTECT(anfExp);
    disableGC();
    // desugaring
    anfExp = desugarExp(anfExp);
    PROTECT(anfExp);
    enableGC();
#ifdef DEBUG_ANF
    printExp(anfExp);
    eprintf("\n");
#endif
    // static analysis: ANF => annotated ANF (de bruijn)
    analizeExp(anfExp, NULL);
    // byte code generation
    initByteCodeArray(&byteCodes);
    writeExp(anfExp, &byteCodes);
    writeEnd(&byteCodes);
    UNPROTECT(save);
    // execution
    printContainedValue(run(byteCodes), 1);
    printf("\n");
    // report stats etc.
    if (report_mem_flag)
        reportMemory();
    if (report_time_flag) {
        clock_t end = clock();
        double time_spent = (double)(end - begin) / CLOCKS_PER_SEC;
        printf("elapsed time %.3lf\n", time_spent);
    }
}

#endif

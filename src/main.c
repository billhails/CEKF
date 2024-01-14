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
#include "analysis.h"
#include "anf.h"
#include "anf_normalize.h"
#include "memory.h"
#include "step.h"
#include "debug.h"
#include "bytecode.h"
#include "desugaring.h"
#include "hash.h"
#include "lambda_pp.h"
#include "anf_normalize.h"
#include "bigint.h"
#include "tc_analyze.h"
#include "tc_debug.h"

int report_flag = 0;
static int help_flag = 0;

int main(int argc, char *argv[]) {
    int c;
    clock_t begin = clock();

    while (1) {
        static struct option long_options[] =
        {
          {"bigint", no_argument, &bigint_flag, 1},
          {"report", no_argument, &report_flag, 1},
          {"help", no_argument, &help_flag, 1},
          {0, 0, 0, 0}
        };
        int option_index = 0;

        c = getopt_long (argc, argv, "", long_options, &option_index);

        if (c == -1)
            break;
    }

    if (help_flag) {
        printf("%s",
            "--bigint        use arbitrary precision integers\n"
            "--report        report statistics\n"
            "--help          this help\n"
        );
        exit(0);
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
    // static analysis: ANF => annotated ANF (de bruijn)
    analizeExp(anfExp, NULL);
#ifdef DEBUG_ANF
    printExp(anfExp);
    eprintf("\n");
#endif
    // byte code generation
    initByteCodeArray(&byteCodes);
    writeExp(anfExp, &byteCodes);
    writeEnd(&byteCodes);
    UNPROTECT(save);
    // execution
    printContainedValue(run(byteCodes), 1);
    printf("\n");
    // report stats etc.
    if (report_flag) {
        clock_t end = clock();
        double time_spent = (double)(end - begin) / CLOCKS_PER_SEC;
        printf("\nelapsed time %.3lf\n", time_spent);
        reportMemory();
        reportSteps();
    }
}


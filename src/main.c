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
#include "annotate.h"
#include "anf.h"
#include "anf_normalize.h"
#include "memory.h"
#include "step.h"
#include "debug.h"
#include "anf_pp.h"
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

static void processArgs(int argc, char *argv[]) {
    int c;

    while (1) {
        static struct option long_options[] = {
            { "bigint", no_argument, &bigint_flag, 1 },
            { "report", no_argument, &report_flag, 1 },
            { "help", no_argument, &help_flag, 1 },
            { 0, 0, 0, 0 }
        };
        int option_index = 0;

        c = getopt_long(argc, argv, "", long_options, &option_index);

        if (c == -1)
            break;
    }

    if (help_flag) {
        printf("%s",
               "--bigint        use arbitrary precision integers\n"
               "--report        report statistics\n"
               "--help          this help\n");
        exit(0);
    }

    if (optind >= argc) {
        eprintf("need filename\n");
        exit(1);
    }
}

static AstNest *parseFile(char *file) {
    disableGC();
    PmModule *mod = newPmToplevelFromFile(file);
    int save = PROTECT(mod);
    pmParseModule(mod);
    enableGC();
    UNPROTECT(save);
    return mod->nest;
}

static LamExp *convertNest(AstNest *nest) {
    LamExp *exp = lamConvertNest(nest, NULL);
    int save = PROTECT(exp);
#ifdef DEBUG_LAMBDA_CONVERT
    ppLamExp(exp);
    eprintf("\n");
#endif
    UNPROTECT(save);
    return exp;
}

static void typeCheck(LamExp *exp) {
    TcEnv *env = tc_init();
    int save = PROTECT(env);
    TcType *res __attribute__((unused)) = tc_analyze(exp, env);
    if (hadErrors()) {
        exit(1);
    }
#ifdef DEBUG_TC
    ppTcType(res);
    eprintf("\n");
#endif
    UNPROTECT(save);
}

static Exp *desugar(Exp *anfExp) {
    disableGC();
    anfExp = desugarExp(anfExp);
    int save = PROTECT(anfExp);
    enableGC();
    UNPROTECT(save);
    return anfExp;
}

static void annotate(Exp *anfExp) {
    annotateExp(anfExp, NULL);
#ifdef DEBUG_ANF
    ppExp(anfExp);
    eprintf("\n");
#endif
}

static ByteCodeArray generateByteCodes(Exp *anfExp) {
    ByteCodeArray byteCodes;
    initByteCodeArray(&byteCodes);
    writeExp(anfExp, &byteCodes);
    writeEnd(&byteCodes);
    return byteCodes;
}

static void report(clock_t begin) {
    if (report_flag) {
        clock_t end = clock();
        double time_spent = (double) (end - begin) / CLOCKS_PER_SEC;
        printf("\nelapsed time %.3lf\n", time_spent);
        reportMemory();
        reportSteps();
    }
}

int main(int argc, char *argv[]) {
    clock_t begin = clock();
    processArgs(argc, argv);
    initProtection();

    AstNest *nest = parseFile(argv[optind]);
    int save = PROTECT(nest);

    LamExp *exp = convertNest(nest);
    REPLACE_PROTECT(save, exp);

    typeCheck(exp);

    Exp *anfExp = anfNormalize(exp);
    REPLACE_PROTECT(save, anfExp);

    anfExp = desugar(anfExp);
    REPLACE_PROTECT(save, anfExp);

    annotate(anfExp);

    ByteCodeArray byteCodes = generateByteCodes(anfExp);

    UNPROTECT(save);

    run(byteCodes);

    report(begin);

    exit(0);
}

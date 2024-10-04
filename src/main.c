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
#include <errno.h>

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
#include "hash.h"
#include "lambda_pp.h"
#include "anf_normalize.h"
#include "bigint.h"
#include "tc_analyze.h"
#include "tc_debug.h"
#include "tpmc_mermaid.h"
#include "arithmetic.h"
#include "builtins_helper.h"
#include "inline.h"
#include "pratt.h"
#include "pratt_parser.h"
#include "pratt_scanner.h"

int report_flag = 0;
static int help_flag = 0;
static int anf_flag = 0;
static int ast_flag = 0;
static int lambda_flag = 0;
static int inline_flag = 0;
extern bool assertions_failed;
extern int assertions_accumulate;
static char *binary_output_file = NULL;
static char *binary_input_file = NULL;

extern AstStringArray *include_paths;

static void report_build_mode(char *prog) {
    printf("%s - ", prog);
#ifdef BUILD_MODE
    switch (BUILD_MODE) {
        case 0:
            printf("debug build\n");
            break;
        case 1:
            printf("test build\n");
            break;
        case 2:
            printf("production build\n");
            break;
        default:
            printf("unrecognised build\n");
            break;
    }
#else
    printf("unspecified build\n");
#endif
}

static void usage(char *prog, int status) {
    report_build_mode(prog);
    printf("usage: %s <options> [<filename>] [<arguments> ...]\n", prog);
    printf("options:\n%s",
           "    --anf                    Display the generated ANF.\n"
           "    --assertions-accumulate  Don't exit on the first assertion failure.\n"
           "    --ast                    Display the parsed AST before lambda conversion.\n"
           "    --binary-in=file         Read byte code from file.\n"
           "    --binary-out=file        Write byte code to file.\n"
           "    --dump-bytecode          Dump the generated bytecode.\n"
           "    --help                   This help.\n"
           "    --include=dir            Add dir to the list of directories to be\n"
           "                             searched.\n"
           "    --inline-all             Display the entire intermediate code\n"
           "                             after inlining.\n"
           "    --lambda=function        Display the intermediate code\n"
           "                             generated for the function.\n"
           "    --lambda-all             Display the entire intermediate code.\n"
           "    --report                 Report statistics.\n"
           "    --tpmc=function          Produce a mermaid graph of the\n"
           "                             function's TPMC state table.\n"
    );
    exit(status);
}

static int processArgs(int argc, char *argv[]) {
    int c;

    while (1) {
        static struct option long_options[] = {
            { "report", no_argument, &report_flag, 1 },
            { "anf", no_argument, &anf_flag, 1 },
            { "ast", no_argument, &ast_flag, 1 },
            { "dump-bytecode", no_argument, &dump_bytecode_flag, 1 },
            { "help", no_argument, &help_flag, 1 },
            { "lambda-all", no_argument, &lambda_flag, 1 },
            { "inline-all", no_argument, &inline_flag, 1 },
            { "assertions-accumulate", no_argument, &assertions_accumulate, 1 },
            { "tpmc", required_argument, 0, 'm' },
            { "lambda", required_argument, 0, 'l' },
            { "include", required_argument, 0, 'i' },
            { "binary-out", required_argument, 0, 'o' },
            { "binary-in", required_argument, 0, 'b' },
            { 0, 0, 0, 0 }
        };
        int option_index = 0;

        c = getopt_long(argc, argv, "", long_options, &option_index);

        if (c == -1)
            break;

        if (c == 'm') {
            tpmc_mermaid_function = optarg;
        }

        if (c == 'l') {
            lambda_conversion_function = optarg;
        }

        if (c == 'o') {
            binary_output_file = optarg;
        }

        if (c == 'b') {
            binary_input_file = optarg;
        }

        if (c == 'i') {
            pushAstStringArray(include_paths, strdup(optarg));
        }

        if (c == '?') {
            help_flag = 1;
        }
    }

    if (help_flag) {
        usage(argv[0], 0);
    }

    if (binary_input_file != NULL && binary_output_file != NULL) {
        printf("incompatible --binary-out and --binary-in arguments\n");
        usage(argv[0], 1);
    }

    if (optind >= argc && !binary_input_file) {
        eprintf("need filename or --binary-in argument\n");
        exit(1);
    }

    return optind;
}

static AstProg *parseFile(char *file) {
    PrattParser *parser = makePrattParser();
    int save = PROTECT(parser);
    PrattTrie *trie = makePrattTrie(parser, NULL);
    PROTECT(trie);
    parser->lexer = makePrattLexerFromFilename(trie, file);
    AstNest *nest = prattParseTopLevel(parser);
    PROTECT(nest);
    if (parser->lexer->bufList != NULL) {
        parserError(parser, "unconsumed tokens");
    }
    if (hadErrors()) {
        exit(1);
    }
    AstProg *prog = astNestToProg(nest);
    PROTECT(prog);
    if (ast_flag) {
        PrattUTF8 *dest = newPrattUTF8();
        PROTECT(dest);
        ppAstProg(dest, prog);
        printf("%s\n", dest->entries);
    }
    UNPROTECT(save);
    return prog;
}

static LamExp *convertProg(AstProg *prog) {
    LamExp *exp = lamConvertProg(prog);
    int save = PROTECT(exp);
    if (lambda_flag) {
        ppLamExp(exp);
        eprintf("\n");
    }
    UNPROTECT(save);
    return exp;
}

static LamExp *inlineExp(LamExp *exp) __attribute__((unused));

static LamExp *inlineExp(LamExp *exp) {
    exp = inlineLamExp(exp);
    int save = PROTECT(exp);
    if (inline_flag) {
        ppLamExp(exp);
        eprintf("\n");
    }
    UNPROTECT(save);
    return exp;
}

static void typeCheck(LamExp *exp, BuiltIns *builtIns) {
    TcEnv *env = tc_init(builtIns);
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

static void annotate(Exp *anfExp, BuiltIns *builtIns) {
    annotateAnf(anfExp, builtIns);
#ifdef DEBUG_ANF
    ppExp(anfExp);
    eprintf("\n");
#endif
}

static ByteCodeArray generateByteCodes(Exp *anfExp) {
    ByteCodeArray byteCodes;
    initByteCodeArray(&byteCodes, 8);
    writeExp(anfExp, &byteCodes);
    writeEnd(&byteCodes);
    return byteCodes;
}

static void report(char *prog, clock_t begin, clock_t compiled, clock_t end) {
    if (report_flag) {
        printf("\n");
        report_build_mode(prog);
        double time_spent = (double) (end - begin) / CLOCKS_PER_SEC;
        printf("elapsed time: %.4lf\n", time_spent);
        double compile_time = (double) (compiled - begin) / CLOCKS_PER_SEC;
        printf("compile time: %.4lf\n", compile_time);
        double run_time = (double) (end - compiled) / CLOCKS_PER_SEC;
        printf("run time: %.4lf\n", run_time);
        reportMemory();
        reportSteps();
    }
}

int main(int argc, char *argv[]) {
    clock_t begin = clock();
    initProtection();
    init_arithmetic();
    initNamespaces();
    include_paths = newAstStringArray();
    int save = PROTECT(include_paths);
    initFileIdStack();
    int nextargc = processArgs(argc, argv);
    BuiltIns *builtIns = registerBuiltIns(argc, binary_input_file ? nextargc : nextargc + 1, argv);
    PROTECT(builtIns);

    if (binary_input_file) {
        ByteCodeArray byteCodes;
        initByteCodeArray(&byteCodes, 8);
        switch (readBinaryInputFile(&byteCodes, binary_input_file)) {
            case BYTECODES_BADFILE:
                fprintf(stderr, "%s: %s\n", binary_input_file, strerror(errno));
                exit(1);
            case BYTECODES_BADHEADER:
                fprintf(stderr, "%s: invalid header\n", binary_input_file);
                exit(1);
            case BYTECODES_BADVERSION:
                fprintf(stderr, "%s: invalid version\n", binary_input_file);
                exit(1);
            case BYTECODES_OK:
                break;
            default:
                cant_happen("unrecognised status from bytecode reader");
        }
        clock_t compiled = clock();
        run(byteCodes, builtIns);
        UNPROTECT(save);
        clock_t end = clock();
        report(argv[0], begin, compiled, end);
    } else {
        AstProg *prog = parseFile(argv[nextargc++]);
        int save2 = PROTECT(prog);

        LamExp *exp = convertProg(prog);
        REPLACE_PROTECT(save2, exp);

        typeCheck(exp, builtIns);

        exp = inlineExp(exp);
        REPLACE_PROTECT(save2, exp);

        Exp *anfExp = anfNormalize(exp);
        REPLACE_PROTECT(save2, anfExp);

        if (anf_flag) {
            ppExp(anfExp);
            eprintf("\n");
        }

        annotate(anfExp, builtIns);

        ByteCodeArray byteCodes = generateByteCodes(anfExp);
        if (binary_output_file != NULL) {
            if (!writeBinaryOutputFile(&byteCodes, binary_output_file)) {
                fprintf(stderr, "%s: %s\n", binary_output_file, strerror(errno));
                exit(1);
            }
            exit(0);
        }

        UNPROTECT(save2);
        clock_t compiled = clock();
        run(byteCodes, builtIns);
        UNPROTECT(save);
        clock_t end = clock();
        report(argv[0], begin, compiled, end);
    }
    exit(assertions_failed ? 1 : 0);
}

#ifndef cekf_common_h
#define cekf_common_h
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

#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define NS_GLOBAL -1

// Null ParserInfo for use when location info is not available
#define NULLPI ((ParserInfo){.lineNo = 0, .fileName = NULL})

#ifndef PRODUCTION_BUILD
// #    define DEBUG_ALLOC
// #    define DEBUG_ANF
// #    define DEBUG_ANF_NORMALIZE
// #    define DEBUG_ANNOTATE
// #    define DEBUG_ARITHMETIC
// #    define DEBUG_BIGINT
// #    define DEBUG_BYTECODE
// #    define DEBUG_CEKFS
// #    define DEBUG_DESUGARING
#define DEBUG_DUMP_CORE
// #    define DEBUG_HASHTABLE
// #    define DEBUG_LAMBDA_CONVERT
// #    define DEBUG_LAMBDA_SUBSTITUTE
// #    define DEBUG_LEAK
// #    define DEBUG_LOG_GC
// #    define DEBUG_PRATT_PARSER
// #    define DEBUG_PRATT_SCANNER
// #    define DEBUG_PRINT_COMPILER
// #    define DEBUG_PRINT_GENERATOR
// #    define DEBUG_SLOW_STEP
// #    define DEBUG_SQLITE
// #    define DEBUG_STACK
// #define DEBUG_STEP
#ifndef NO_DEBUG_STRESS_GC
#define DEBUG_STRESS_GC
#endif
// #    define DEBUG_TC
// #    define DEBUG_TIN_INSTANTIATION
// #    define DEBUG_TIN_SUBSTITUTION
// #    define DEBUG_TIN_UNIFICATION
// #    define DEBUG_TPMC_COMPARE
// #    define DEBUG_TPMC_LOGIC
// #define DEBUG_TPMC_MATCH
// #    define DEBUG_TPMC_TRANSLATE
#define SAFETY_CHECKS
#endif

#ifndef __GNUC__
#define __attribute__(x)
#endif

#define errout stdout

// Forward declaration to avoid circular dependency with parser_info.h
struct ParserInfo;
typedef struct ParserInfo ParserInfo;

void _cant_happen(char *file, int line, const char *message, ...)
    __attribute__((noreturn, format(printf, 3, 4)));
void can_happen(ParserInfo I, const char *message, ...)
    __attribute__((format(printf, 2, 3)));
void eprintf(const char *message, ...) __attribute__((format(printf, 1, 2)));
bool hadErrors(void);
void clearErrors(void);

#define cant_happen(...) _cant_happen(__FILE__, __LINE__, __VA_ARGS__)

#define PAD_WIDTH 2

#define ASSERT(assertion)                                                      \
    do {                                                                       \
        if (!(assertion)) {                                                    \
            cant_happen("assertion failed " #assertion);                       \
        }                                                                      \
    } while (0);

#endif

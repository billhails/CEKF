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

 typedef uint32_t hash_t;

// #define DEBUG_RUN_TESTS 1
// #define TEST_STACK
// #define DEBUG_STACK
// #define DEBUG_STEP
// if DEBUG_STEP is defined, this sleeps for 1 second between each machine step
// #define DEBUG_SLOW_STEP
// define this to cause a GC at every possible step (catched memory leaks early)
#define DEBUG_STRESS_GC
// #define DEBUG_LOG_GC
// #define DEBUG_GC
// #define DEBUG_TPMC_MATCH
// #define DEBUG_TPMC_TRANSLATE
// #define DEBUG_TPMC_LOGIC
// #define DEBUG_ANALIZE
// #define DEBUG_DESUGARING
// #define DEBUG_HASHTABLE
// #define DEBUG_TIN_SUBSTITUTION
// #define DEBUG_TIN_INSTANTIATION
// #define DEBUG_TIN_UNIFICATION
// #define DEBUG_BYTECODE
// define this to make fatal errors dump core (if ulimit allows)
#define DEBUG_DUMP_CORE
// #define DEBUG_TC
// #define DEBUG_LAMBDA_CONVERT
// #define DEBUG_LEAK
// #define DEBUG_ANF
// #define DEBUG_ALLOC
// define this to turn on additional safety checks for things that shouldn't but just possibly might happen
#define SAFETY_CHECKS

#ifndef __GNUC__
#define __attribute__(x)
#endif
void cant_happen(const char *message, ...) __attribute__((noreturn, format(printf, 1, 2)));
void can_happen(const char *message, ...) __attribute__((format(printf, 1, 2)));
bool hadErrors(void);
#define eprintf(...) fprintf(stderr, __VA_ARGS__)

#endif

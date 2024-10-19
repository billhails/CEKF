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

#include <stdlib.h>
#include "builtins_impl.h"
#include "arithmetic.h"
#include "utf8.h"
#include "unicode.h"

bool assertions_failed;
int assertions_accumulate = 0;
int builtin_args_argc = 0;
int builtin_args_cargc = 0;
char **builtin_args_argv;

Value builtin_rand(Vec *args) {
    return nrand(args->entries[0]);
}

Value builtin_assert(Vec *args __attribute__((unused))) {
    assertions_failed = true;
    if (assertions_accumulate) {
        return vFalse;
    }
    exit(1);
}

Value builtin_real_part(Vec *args) {
    return real_part(args->entries[0]);
}

Value builtin_imag_part(Vec *args) {
    return imag_part(args->entries[0]);
}

Value builtin_mag_part(Vec *args) {
    return mag_part(args->entries[0]);
}

Value builtin_theta_part(Vec *args) {
    return theta_part(args->entries[0]);
}

Value builtin_ord(Vec *args) {
    return value_Stdint((int) args->entries[0].val.character);
}

#define GC_LL  0
#define GC_LM  1
#define GC_LO  2
#define GC_LT  3
#define GC_LU  4
#define GC_MC  5
#define GC_ME  6
#define GC_MN  7
#define GC_ND  8
#define GC_NL  9
#define GC_NO 10
#define GC_PC 11
#define GC_PD 12
#define GC_PE 13
#define GC_PF 14
#define GC_PI 15
#define GC_PO 16
#define GC_PS 17
#define GC_SC 18
#define GC_SK 19
#define GC_SM 20
#define GC_SO 21
#define GC_ZL 22
#define GC_ZP 23
#define GC_ZS 24
#define GC_CC 25
#define GC_CF 26
#define GC_CO 27
#define GC_CS 28
#define GC_CN 29


Value builtin_unicode_category(Vec *args) {
    switch (unicode_category(args->entries[0].val.character)) {
        case GC_Ll: return value_Stdint(GC_LL);
        case GC_Lm: return value_Stdint(GC_LM);
        case GC_Lo: return value_Stdint(GC_LO);
        case GC_Lt: return value_Stdint(GC_LT);
        case GC_Lu: return value_Stdint(GC_LU);
        case GC_Mc: return value_Stdint(GC_MC);
        case GC_Me: return value_Stdint(GC_ME);
        case GC_Mn: return value_Stdint(GC_MN);
        case GC_Nd: return value_Stdint(GC_ND);
        case GC_Nl: return value_Stdint(GC_NL);
        case GC_No: return value_Stdint(GC_NO);
        case GC_Pc: return value_Stdint(GC_PC);
        case GC_Pd: return value_Stdint(GC_PD);
        case GC_Pe: return value_Stdint(GC_PE);
        case GC_Pf: return value_Stdint(GC_PF);
        case GC_Pi: return value_Stdint(GC_PI);
        case GC_Po: return value_Stdint(GC_PO);
        case GC_Ps: return value_Stdint(GC_PS);
        case GC_Sc: return value_Stdint(GC_SC);
        case GC_Sk: return value_Stdint(GC_SK);
        case GC_Sm: return value_Stdint(GC_SM);
        case GC_So: return value_Stdint(GC_SO);
        case GC_Zl: return value_Stdint(GC_ZL);
        case GC_Zp: return value_Stdint(GC_ZP);
        case GC_Zs: return value_Stdint(GC_ZS);
        case GC_Cc: return value_Stdint(GC_CC);
        case GC_Cf: return value_Stdint(GC_CF);
        case GC_Co: return value_Stdint(GC_CO);
        case GC_Cs: return value_Stdint(GC_CS);
        case GC_Cn: return value_Stdint(GC_CN);
        default:
            cant_happen("unrecognised result");
    }
}

Value builtin_chr(Vec *args) {
#ifdef SAFETY_CHECKS
    if (args->entries[0].type != VALUE_TYPE_STDINT) {
        cant_happen("unsupported numeric type for chr: %s", valueTypeName(args->entries[0].type));
    }
#endif
    return value_Character((Character) args->entries[0].val.stdint);
}

Value builtin_args(Vec *args) {
#ifdef SAFETY_CHECKS
    if (args->entries[0].type != VALUE_TYPE_STDINT) {
        cant_happen("unsupported argument for args: %s", valueTypeName(args->entries[0].type));
    }
#endif
    int index = args->entries[0].val.stdint + builtin_args_cargc;
    if (index < 0 || index >= builtin_args_argc) {
        return makeNothing();
    }
    Value s = utf8ToList(builtin_args_argv[index]);
    int save = protectValue(s);
    Value result = makeSome(s);
    UNPROTECT(save);
    return result;
}

Value builtin_getenv(Vec *args) {
    char *name = listToUtf8(args->entries[0]);
    char *value = getenv(name);
    FREE_ARRAY(char, name, strlen(name) + 1);
    if (value == NULL) {
        return makeNothing();
    }
    Value string = utf8ToList(value);
    int save = protectValue(string);
    Value result = makeSome(string);
    UNPROTECT(save);
    return result;
}

void builtin_exit(Vec *args) {
    int status = args->entries[0].val.stdint;
    exit(status);
}

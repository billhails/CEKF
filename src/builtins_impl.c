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

Value builtin_ord(Vec *args) {
    return value_Stdint((int) args->entries[0].val.character);
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

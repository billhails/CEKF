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

#include "symbols.h"

#define MAKE_SYMBOL(NAME, STR)             \
HashSymbol * NAME ## Symbol() {            \
    static HashSymbol *res = NULL;         \
    if (res == NULL) res = newSymbol(STR); \
    return res;                            \
}

// symbols with a '$' suffix are internal, other symbols
// are accessible from the language.

MAKE_SYMBOL(neg, "NEGATION")
MAKE_SYMBOL(assert, "__assert__")
MAKE_SYMBOL(fnError, "__error__")
MAKE_SYMBOL(namespaces, "$namespaces")
MAKE_SYMBOL(namespace, "$namespace")
MAKE_SYMBOL(puts, "puts")
MAKE_SYMBOL(here, "callcc")
MAKE_SYMBOL(then, "amb")
MAKE_SYMBOL(back, "back")
MAKE_SYMBOL(error, "error")
MAKE_SYMBOL(eq, "EQUALTO")
MAKE_SYMBOL(ne, "NOTEQUALTO")
MAKE_SYMBOL(gt, "GREATERTHAN")
MAKE_SYMBOL(lt, "LESSTHAN")
MAKE_SYMBOL(ge, "GREATERTHANOREQUALTO")
MAKE_SYMBOL(le, "LESSTHANOREQUALTO")
MAKE_SYMBOL(cmp, "COMPARISON")
MAKE_SYMBOL(spaceship, "cmp")
MAKE_SYMBOL(cons, "cons")
MAKE_SYMBOL(append, "append")
MAKE_SYMBOL(currentFile, "currentFile")
MAKE_SYMBOL(currentLine, "currentLine")
MAKE_SYMBOL(add, "ADDITION")
MAKE_SYMBOL(sub, "SUBTRACTION")
MAKE_SYMBOL(mul, "MULTIPLICATION")
MAKE_SYMBOL(div, "DIVISION")
MAKE_SYMBOL(mod, "MODULUS")
MAKE_SYMBOL(pow, "EXPONENTIAL")
MAKE_SYMBOL(if, "if")
MAKE_SYMBOL(true, "true")
MAKE_SYMBOL(false, "false")
MAKE_SYMBOL(nil, "nil")
MAKE_SYMBOL(arrow, "->")
MAKE_SYMBOL(char, "char")
MAKE_SYMBOL(bool, "bool")
MAKE_SYMBOL(list, "list")
MAKE_SYMBOL(env, "env")
MAKE_SYMBOL(car, "car")
MAKE_SYMBOL(cdr, "cdr")
MAKE_SYMBOL(leftCurly, "{")
MAKE_SYMBOL(rightCurly, "}")
MAKE_SYMBOL(leftRound, "(")
MAKE_SYMBOL(rightRound, ")")
MAKE_SYMBOL(left, "[")
MAKE_SYMBOL(right, "]")
MAKE_SYMBOL(questinMark, "?")
MAKE_SYMBOL(colon, ":")

#undef MAKE_SYMBOL

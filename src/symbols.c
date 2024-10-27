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

// symbols with a '$' suffix are internal, other symbols
// are accessible from the language.

HashSymbol *negSymbol() {
    static HashSymbol *res = NULL;
    if (res == NULL) {
        res = newSymbol("NEGATION");
    }
    return res;
}

HashSymbol *assertSymbol() {
    static HashSymbol *res = NULL;
    if (res == NULL) {
        res = newSymbol("assert$");
    }
    return res;
}

HashSymbol *fnErrorSymbol() {
    static HashSymbol *res = NULL;
    if (res == NULL) {
        res = newSymbol("error$");
    }
    return res;
}

HashSymbol *namespacesSymbol() {
    static HashSymbol *res = NULL;
    if (res == NULL) {
        res = newSymbol("$namespaces");
    }
    return res;
}

HashSymbol *namespaceSymbol() {
    static HashSymbol *res = NULL;
    if (res == NULL) {
        res = newSymbol("$namespace");
    }
    return res;
}

HashSymbol *putsSymbol() {
    static HashSymbol *res = NULL;
    if (res == NULL) {
        res = newSymbol("puts");
    }
    return res;
}

HashSymbol *hereSymbol() {
    static HashSymbol *res = NULL;
    if (res == NULL) {
        res = newSymbol("callcc");
    }
    return res;
}

HashSymbol *thenSymbol() {
    static HashSymbol *res = NULL;
    if (res == NULL) {
        res = newSymbol("amb");
    }
    return res;
}

HashSymbol *backSymbol() {
    static HashSymbol *res = NULL;
    if (res == NULL) {
        res = newSymbol("back");
    }
    return res;
}

HashSymbol *errorSymbol() {
    static HashSymbol *res = NULL;
    if (res == NULL) {
        res = newSymbol("error");
    }
    return res;
}

HashSymbol *eqSymbol() {
    static HashSymbol *res = NULL;
    if (res == NULL) {
        res = newSymbol("EQUALTO");
    }
    return res;
}

HashSymbol *neSymbol() {
    static HashSymbol *res = NULL;
    if (res == NULL) {
        res = newSymbol("NOTEQUALTO");
    }
    return res;
}

HashSymbol *gtSymbol() {
    static HashSymbol *res = NULL;
    if (res == NULL) {
        res = newSymbol("GREATERTHAN");
    }
    return res;
}

HashSymbol *ltSymbol() {
    static HashSymbol *res = NULL;
    if (res == NULL) {
        res = newSymbol("LESSTHAN");
    }
    return res;
}

HashSymbol *geSymbol() {
    static HashSymbol *res = NULL;
    if (res == NULL) {
        res = newSymbol("GREATERTHANOREQUALTO");
    }
    return res;
}

HashSymbol *leSymbol() {
    static HashSymbol *res = NULL;
    if (res == NULL) {
        res = newSymbol("LESSTHANOREQUALTO");
    }
    return res;
}

HashSymbol *cmpSymbol() {
    static HashSymbol *res = NULL;
    if (res == NULL) {
        res = newSymbol("COMPARISON");
    }
    return res;
}

HashSymbol *spaceshipSymbol() {
    static HashSymbol *res = NULL;
    if (res == NULL) {
        res = newSymbol("cmp");
    }
    return res;
}

HashSymbol *consSymbol() {
    static HashSymbol *res = NULL;
    if (res == NULL) {
        res = newSymbol("cons");
    }
    return res;
}

HashSymbol *appendSymbol() {
    static HashSymbol *res = NULL;
    if (res == NULL) {
        res = newSymbol("append");
    }
    return res;
}

HashSymbol *addSymbol() {
    static HashSymbol *res = NULL;
    if (res == NULL) {
        res = newSymbol("ADDITION");
    }
    return res;
}

HashSymbol *subSymbol() {
    static HashSymbol *res = NULL;
    if (res == NULL) {
        res = newSymbol("SUBTRACTION");
    }
    return res;
}

HashSymbol *mulSymbol() {
    static HashSymbol *res = NULL;
    if (res == NULL) {
        res = newSymbol("MULTIPLICATION");
    }
    return res;
}

HashSymbol *divSymbol() {
    static HashSymbol *res = NULL;
    if (res == NULL) {
        res = newSymbol("DIVISION");
    }
    return res;
}

HashSymbol *modSymbol() {
    static HashSymbol *res = NULL;
    if (res == NULL) {
        res = newSymbol("MODULUS");
    }
    return res;
}

HashSymbol *powSymbol() {
    static HashSymbol *res = NULL;
    if (res == NULL) {
        res = newSymbol("EXPONENTIAL");
    }
    return res;
}

HashSymbol *ifSymbol() {
    static HashSymbol *res = NULL;
    if (res == NULL) {
        res = newSymbol("if");
    }
    return res;
}

HashSymbol *trueSymbol() {
    static HashSymbol *res = NULL;
    if (res == NULL) {
        res = newSymbol("true");
    }
    return res;
}

HashSymbol *falseSymbol() {
    static HashSymbol *res = NULL;
    if (res == NULL) {
        res = newSymbol("false");
    }
    return res;
}

HashSymbol *nilSymbol() {
    static HashSymbol *res = NULL;
    if (res == NULL) {
        res = newSymbol("nil");
    }
    return res;
}

HashSymbol *arrowSymbol() {
    static HashSymbol *res = NULL;
    if (res == NULL) {
        res = newSymbol("->");
    }
    return res;
}

HashSymbol *charSymbol() {
    static HashSymbol *res = NULL;
    if (res == NULL) {
        res = newSymbol("char");
    }
    return res;
}

HashSymbol *boolSymbol() {
    static HashSymbol *res = NULL;
    if (res == NULL) {
        res = newSymbol("bool");
    }
    return res;
}

HashSymbol *listSymbol() {
    static HashSymbol *res = NULL;
    if (res == NULL) {
        res = newSymbol("list");
    }
    return res;
}

HashSymbol *envSymbol() {
    static HashSymbol *res = NULL;
    if (res == NULL) {
        res = newSymbol("env");
    }
    return res;
}

HashSymbol *carSymbol() {
    static HashSymbol *res = NULL;
    if (res == NULL) {
        res = newSymbol("car");
    }
    return res;
}

HashSymbol *cdrSymbol() {
    static HashSymbol *res = NULL;
    if (res == NULL) {
        res = newSymbol("cdr");
    }
    return res;
}

HashSymbol *leftCurlySymbol() {
    static HashSymbol *res = NULL;
    if (res == NULL) {
        res = newSymbol("{");
    }
    return res;
}

HashSymbol *rightCurlySymbol() {
    static HashSymbol *res = NULL;
    if (res == NULL) {
        res = newSymbol("}");
    }
    return res;
}

HashSymbol *leftRoundSymbol() {
    static HashSymbol *res = NULL;
    if (res == NULL) {
        res = newSymbol("(");
    }
    return res;
}

HashSymbol *rightRoundSymbol() {
    static HashSymbol *res = NULL;
    if (res == NULL) {
        res = newSymbol(")");
    }
    return res;
}

HashSymbol *leftSquareSymbol() {
    static HashSymbol *res = NULL;
    if (res == NULL) {
        res = newSymbol("[");
    }
    return res;
}

HashSymbol *rightSquareSymbol() {
    static HashSymbol *res = NULL;
    if (res == NULL) {
        res = newSymbol("]");
    }
    return res;
}

HashSymbol *questinMarkSymbol() {
    static HashSymbol *res = NULL;
    if (res == NULL) {
        res = newSymbol("?");
    }
    return res;
}

HashSymbol *colonSymbol() {
    static HashSymbol *res = NULL;
    if (res == NULL) {
        res = newSymbol(":");
    }
    return res;
}

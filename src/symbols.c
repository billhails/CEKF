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
        res = newSymbol("neg$");
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

HashSymbol *notSymbol() {
    static HashSymbol *res = NULL;
    if (res == NULL) {
        res = newSymbol("not");
    }
    return res;
}

HashSymbol *hereSymbol() {
    static HashSymbol *res = NULL;
    if (res == NULL) {
        res = newSymbol("here");
    }
    return res;
}

HashSymbol *thenSymbol() {
    static HashSymbol *res = NULL;
    if (res == NULL) {
        res = newSymbol("then");
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

HashSymbol *andSymbol() {
    static HashSymbol *res = NULL;
    if (res == NULL) {
        res = newSymbol("and");
    }
    return res;
}

HashSymbol *nandSymbol() {
    static HashSymbol *res = NULL;
    if (res == NULL) {
        res = newSymbol("nand");
    }
    return res;
}

HashSymbol *orSymbol() {
    static HashSymbol *res = NULL;
    if (res == NULL) {
        res = newSymbol("or");
    }
    return res;
}

HashSymbol *norSymbol() {
    static HashSymbol *res = NULL;
    if (res == NULL) {
        res = newSymbol("nor");
    }
    return res;
}

HashSymbol *xorSymbol() {
    static HashSymbol *res = NULL;
    if (res == NULL) {
        res = newSymbol("xor");
    }
    return res;
}

HashSymbol *xnorSymbol() {
    static HashSymbol *res = NULL;
    if (res == NULL) {
        res = newSymbol("xnor");
    }
    return res;
}

HashSymbol *eqSymbol() {
    static HashSymbol *res = NULL;
    if (res == NULL) {
        res = newSymbol("eq$");
    }
    return res;
}

HashSymbol *neSymbol() {
    static HashSymbol *res = NULL;
    if (res == NULL) {
        res = newSymbol("ne$");
    }
    return res;
}

HashSymbol *gtSymbol() {
    static HashSymbol *res = NULL;
    if (res == NULL) {
        res = newSymbol("gt$");
    }
    return res;
}

HashSymbol *ltSymbol() {
    static HashSymbol *res = NULL;
    if (res == NULL) {
        res = newSymbol("lt$");
    }
    return res;
}

HashSymbol *geSymbol() {
    static HashSymbol *res = NULL;
    if (res == NULL) {
        res = newSymbol("ge$");
    }
    return res;
}

HashSymbol *leSymbol() {
    static HashSymbol *res = NULL;
    if (res == NULL) {
        res = newSymbol("le$");
    }
    return res;
}

HashSymbol *cmpSymbol() {
    static HashSymbol *res = NULL;
    if (res == NULL) {
        res = newSymbol("cmp$");
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
        res = newSymbol("add$");
    }
    return res;
}

HashSymbol *subSymbol() {
    static HashSymbol *res = NULL;
    if (res == NULL) {
        res = newSymbol("sub$");
    }
    return res;
}

HashSymbol *mulSymbol() {
    static HashSymbol *res = NULL;
    if (res == NULL) {
        res = newSymbol("mul$");
    }
    return res;
}

HashSymbol *divSymbol() {
    static HashSymbol *res = NULL;
    if (res == NULL) {
        res = newSymbol("div$");
    }
    return res;
}

HashSymbol *modSymbol() {
    static HashSymbol *res = NULL;
    if (res == NULL) {
        res = newSymbol("mod$");
    }
    return res;
}

HashSymbol *powSymbol() {
    static HashSymbol *res = NULL;
    if (res == NULL) {
        res = newSymbol("pow$");
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

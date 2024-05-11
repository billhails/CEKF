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

#include "lambda_helper.h"
#include "lambda_pp.h"
#include "symbol.h"

void printLambdaSymbol(HashSymbol *x, int depth) {
    eprintf("%*s", depth * PAD_WIDTH, "");
    if (x == NULL) {
        eprintf("LambdaSymbol (NULL)");
        return;
    }
    eprintf("AstSymbol[\"%s\"]", x->name);
}

void markLamExpFn(void *ptr) {
    markLamExp(*((LamExp **) ptr));
}

void printLamExpFn(void *ptr, int depth) {
    ppLamExpD(*((LamExp **) ptr), depth);
}

LamTypeConstructorInfo *lookupConstructorInLamContext(LamContext *context, HashSymbol *var) {
    if (context == NULL)
        return NULL; // not an error
    LamInfo *result;
    if (getLamInfoTable(context->frame, var, &result)) {
        switch (result->type) {
            case LAMINFO_TYPE_TYPECONSTRUCTORINFO:
                return result->val.typeConstructorInfo;
            case LAMINFO_TYPE_NAMESPACEINFO:
                cant_happen("expected type constructor found namespace called %s", var->name);
            default:
                cant_happen("unrecognized type %s", lamInfoTypeName(result->type));
        }
    }
    return lookupConstructorInLamContext(context->parent, var);
}

static LamContext *_lookupNamespaceInLamContext(LamContext *context, HashSymbol *var) {
    if (context == NULL)
        cant_happen("cannot find namespace %s", var->name); // always an error
    LamInfo *result;
    if (getLamInfoTable(context->frame, var, &result)) {
        switch (result->type) {
            case LAMINFO_TYPE_TYPECONSTRUCTORINFO:
                cant_happen("expected namespace found typeconstructor called %s", var->name);
            case LAMINFO_TYPE_NAMESPACEINFO:
                return result->val.namespaceInfo;
            default:
                cant_happen("unrecognized type %s", lamInfoTypeName(result->type));
        }
    }
    return _lookupNamespaceInLamContext(context->parent, var);
}

LamContext *lookupNamespaceInLamContext(LamContext *context, Index index) {
    char buf[80];
    sprintf(buf, "$ns%u", index);
    HashSymbol *var = newSymbol(buf);
    return _lookupNamespaceInLamContext(context, var);
}

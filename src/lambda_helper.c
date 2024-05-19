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
#include "symbols.h"

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
            case LAMINFO_TYPE_NAMESPACE:
                cant_happen("expected type constructor found namespace %d", result->val.namespace);
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
            case LAMINFO_TYPE_NAMESPACE:
                cant_happen("expected namespace info found namespace %d", result->val.namespace);
            default:
                cant_happen("unrecognized type %s", lamInfoTypeName(result->type));
        }
    }
    return _lookupNamespaceInLamContext(context->parent, var);
}

LamContext *lookupNamespaceInLamContext(LamContext *context, Index index) {
    char buf[80];
    sprintf(buf, NS_FORMAT, index);
    HashSymbol *var = newSymbol(buf);
    return _lookupNamespaceInLamContext(context, var);
}

LamTypeConstructorInfo *lookupScopedAstConstructorInLamContext(LamContext *context, AstLookupOrSymbol *scoped) {
    switch (scoped->type) {
        case AST_LOOKUPORSYMBOL_TYPE_SYMBOL:
            return lookupConstructorInLamContext(context, scoped->val.symbol);
        case AST_LOOKUPORSYMBOL_TYPE_LOOKUP:{
            LamContext *namespace = lookupNamespaceInLamContext(context, scoped->val.lookup->namespace);
            return lookupConstructorInLamContext(namespace, scoped->val.lookup->symbol);
        }
        default:
            cant_happen("unrecognized %s", astLookupOrSymbolTypeName(scoped->type));
    }
}

LamTypeConstructorInfo *lookupScopedLamSymbolInLamContext(LamContext *context, LamLookupSymbol *lookup) {
    LamContext *namespace = lookupNamespaceInLamContext(context, lookup->namespace);
    return lookupConstructorInLamContext(namespace, lookup->symbol);
}

LamTypeConstructorInfo *lookupScopedAstSymbolInLamContext(LamContext *context, AstLookupSymbol *lookup) {
    LamContext *namespace = lookupNamespaceInLamContext(context, lookup->namespace);
    return lookupConstructorInLamContext(namespace, lookup->symbol);
}

LamTypeConstructorInfo *lookupScopedLamConstructorInLamContext(LamContext *context, LamLookupOrSymbol *scoped) {
    switch (scoped->type) {
        case LAMLOOKUPORSYMBOL_TYPE_SYMBOL:
            return lookupConstructorInLamContext(context, scoped->val.symbol);
        case LAMLOOKUPORSYMBOL_TYPE_LOOKUP:{
            return lookupScopedLamSymbolInLamContext(context, scoped->val.lookup);
        }
        default:
            cant_happen("unrecognized %s", lamLookupOrSymbolTypeName(scoped->type));
    }
}

int lookupCurrentNamespaceInLamContext(LamContext *context) {
    if (context == NULL)
        cant_happen("cannot find current namespace"); // always an error
    LamInfo *result;
    if (getLamInfoTable(context->frame, namespaceSymbol(), &result)) {
        switch (result->type) {
            case LAMINFO_TYPE_TYPECONSTRUCTORINFO:
                cant_happen("expected namespace id found typeconstructor");
            case LAMINFO_TYPE_NAMESPACEINFO:
                cant_happen("expected namespace id found namespace info");
            case LAMINFO_TYPE_NAMESPACE:
                return result->val.namespace;
            default:
                cant_happen("unrecognized type %s", lamInfoTypeName(result->type));
        }
    }
    return lookupCurrentNamespaceInLamContext(context->parent);
}

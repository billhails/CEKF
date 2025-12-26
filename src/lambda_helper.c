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

LamTypeConstructorType *lookupConstructorTypeInLamContext(LamContext *context, HashSymbol *var) {
    if (context == NULL)
        return NULL; // not an error
    LamTypeConstructorType *result = NULL;
    if (getLamAliasTable(context->aliases, var, &result)) {
        return result;
    }
    return lookupConstructorTypeInLamContext(context->parent, var);
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
                cant_happen("expected type constructor found nameSpace called %s", var->name);
            case LAMINFO_TYPE_NSID:
                cant_happen("expected type constructor found nameSpace %d", result->val.nsid);
            default:
                cant_happen("unrecognized type %s", lamInfoTypeName(result->type));
        }
    }
    return lookupConstructorInLamContext(context->parent, var);
}

static LamContext *_lookupNameSpaceInLamContext(LamContext *context, HashSymbol *var) {
    if (context == NULL)
        cant_happen("cannot find nameSpace %s", var->name); // always an error
    LamInfo *result;
    if (getLamInfoTable(context->frame, var, &result)) {
        switch (result->type) {
            case LAMINFO_TYPE_TYPECONSTRUCTORINFO:
                cant_happen("expected nameSpace found typeconstructor called %s", var->name);
            case LAMINFO_TYPE_NAMESPACEINFO:
                return result->val.nameSpaceInfo;
            case LAMINFO_TYPE_NSID:
                cant_happen("expected nameSpace info found nameSpace %d", result->val.nsid);
            default:
                cant_happen("unrecognized type %s", lamInfoTypeName(result->type));
        }
    }
    return _lookupNameSpaceInLamContext(context->parent, var);
}

LamContext *lookupNameSpaceInLamContext(LamContext *context, Index index) {
    char buf[80];
    sprintf(buf, NS_FORMAT, index);
    HashSymbol *var = newSymbol(buf);
    return _lookupNameSpaceInLamContext(context, var);
}

LamTypeConstructorInfo *lookupScopedAstConstructorInLamContext(LamContext *context, AstLookupOrSymbol *scoped) {
    switch (scoped->type) {
        case AST_LOOKUPORSYMBOL_TYPE_SYMBOL:
            return lookupConstructorInLamContext(context, scoped->val.symbol);
        case AST_LOOKUPORSYMBOL_TYPE_LOOKUP:{
            LamContext *nameSpace = lookupNameSpaceInLamContext(context, scoped->val.lookup->nsid);
            return lookupConstructorInLamContext(nameSpace, scoped->val.lookup->symbol);
        }
        default:
            cant_happen("unrecognized %s", astLookupOrSymbolTypeName(scoped->type));
    }
}

LamTypeConstructorInfo *lookupScopedLamSymbolInLamContext(LamContext *context, LamLookupSymbol *lookup) {
    LamContext *nameSpace = lookupNameSpaceInLamContext(context, lookup->nsid);
    return lookupConstructorInLamContext(nameSpace, lookup->symbol);
}

LamTypeConstructorInfo *lookupScopedAstSymbolInLamContext(LamContext *context, AstLookupSymbol *lookup) {
    LamContext *nameSpace = lookupNameSpaceInLamContext(context, lookup->nsid);
    return lookupConstructorInLamContext(nameSpace, lookup->symbol);
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

int lookupCurrentNameSpaceInLamContext(LamContext *context) {
    if (context == NULL)
        cant_happen("cannot find current nameSpace"); // always an error
    LamInfo *result;
    if (getLamInfoTable(context->frame, nameSpaceSymbol(), &result)) {
        switch (result->type) {
            case LAMINFO_TYPE_TYPECONSTRUCTORINFO:
                cant_happen("expected nameSpace id found typeconstructor");
            case LAMINFO_TYPE_NAMESPACEINFO:
                cant_happen("expected nameSpace id found nameSpace info");
            case LAMINFO_TYPE_NSID:
                return result->val.nsid;
            default:
                cant_happen("unrecognized type %s", lamInfoTypeName(result->type));
        }
    }
    return lookupCurrentNameSpaceInLamContext(context->parent);
}

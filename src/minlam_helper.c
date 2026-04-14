/*
 * CEKF - VM supporting amb
 * Copyright (C) 2022-2026  Bill Hails
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

#include "minlam_helper.h"
#include "minlam_freeVars.h"
#include "symbol.h"
#include "utils_helper.h"

SymbolList *minBindingsToSymbolList(MinBindings *bindings) {
    if (bindings == NULL) {
        return NULL;
    }

    SymbolList *next = minBindingsToSymbolList(bindings->next);
    int save = PROTECT(next);
    SymbolList *this = newSymbolList(CPI(bindings), bindings->var, next);
    UNPROTECT(save);
    return this;
}

MinExp *makeDoneCont(ParserInfo PI, int status, bool hasArg) {
    MinExp *body = newMinExp_Done(PI, status);
    int save = PROTECT(body);
    SymbolList *args = NULL;
    if (hasArg) {
        args = newSymbolList(PI, genSymDollar("k"), NULL);
        PROTECT(args);
    }
    MinExp *lambda = makeMinExp_Lam(PI, args, body);
    UNPROTECT(save);
    return lambda;
}

// returns the the free variables in exp that are in keys
SymbolSet *computeRoots(SymbolSet *keys, MinExp *exp) {
    SymbolSet *free = newSymbolSet();
    int save = PROTECT(free);
    freeVarsMinExp(exp, free, NULL);
    free = intersectSymbolSet(free, keys);
    UNPROTECT(save);
    return free;
}

// returns the binding keys
SymbolSet *getAllKeys(MinBindings *bindings) {
    if (bindings == NULL)
        return newSymbolSet();
    SymbolSet *set = getAllKeys(bindings->next);
    int save = PROTECT(set);
    setSymbolSet(set, bindings->var);
    UNPROTECT(save);
    return set;
}

// returns a map from binding symbol to references to
// binding symbols (what calls what)
SymbolSetMap *buildDependencyGraph(MinBindings *bindings, SymbolSet *keys) {
    if (bindings == NULL) {
        return newSymbolSetMap();
    }
    SymbolSetMap *map = buildDependencyGraph(bindings->next, keys);
    int save = PROTECT(map);
    SymbolSet *free = computeRoots(keys, bindings->val);
    PROTECT(free);
    setSymbolSetMap(map, bindings->var, free);
    UNPROTECT(save);
    return map;
}

SymbolSet *computeLiveBindings(SymbolSetMap *deps, SymbolSet *rootSet) {
    SymbolArray *workList = symbolSetToArray(rootSet);
    int save = PROTECT(workList);
    SymbolSet *live = newSymbolSet();
    PROTECT(live);
    while (workList->size > 0) {
        HashSymbol *name = popSymbolArray(workList);
        if (!getSymbolSet(live, name)) {
            setSymbolSet(live, name);
            SymbolSet *nameDeps = NULL;
            if (getSymbolSetMap(deps, name, &nameDeps)) {
                Index i = 0;
                HashSymbol *dep = NULL;
                while ((dep = iterateSymbolSet(nameDeps, &i)) != NULL) {
                    pushSymbolArray(workList, dep);
                }
            }
        }
    }
    UNPROTECT(save);
    return live;
}

MinBindings *retainOnlyLive(MinBindings *bindings, SymbolSet *live) {
    if (bindings == NULL)
        return NULL;
    MinBindings *next = retainOnlyLive(bindings->next, live);
    int save = PROTECT(next);
    if (getSymbolSet(live, bindings->var)) {
        if (next == bindings->next) {
            UNPROTECT(save);
            return bindings;
        } else {
            MinBindings *result = newMinBindings(CPI(bindings), bindings->var,
                                                 bindings->val, next);
            UNPROTECT(save);
            return result;
        }
    } else {
        UNPROTECT(save);
        return next;
    }
}

MinExpTable *minBindingsToMap(MinBindings *bindings) {
    if (bindings == NULL)
        return newMinExpTable();
    MinExpTable *map = minBindingsToMap(bindings->next);
    int save = PROTECT(map);
    setMinExpTable(map, bindings->var, bindings->val);
    UNPROTECT(save);
    return map;
}

bool isAtomicMinExp(MinExp *exp) {
    switch (exp->type) {
    case MINEXP_TYPE_AVAR:
    case MINEXP_TYPE_LAM:
    case MINEXP_TYPE_STDINT:
    case MINEXP_TYPE_VAR:
    case MINEXP_TYPE_BIGINTEGER:
    case MINEXP_TYPE_CHARACTER:
        return true;
    case MINEXP_TYPE_AMB:
    case MINEXP_TYPE_APPLY:
    case MINEXP_TYPE_ARGS:
    case MINEXP_TYPE_BACK:
    case MINEXP_TYPE_BINDINGS:
    case MINEXP_TYPE_CALLCC:
    case MINEXP_TYPE_COND:
    case MINEXP_TYPE_DONE:
    case MINEXP_TYPE_IFF:
    case MINEXP_TYPE_LETREC:
    case MINEXP_TYPE_MAKEVEC:
    case MINEXP_TYPE_MATCH:
    case MINEXP_TYPE_SEQUENCE:
        return false;
    case MINEXP_TYPE_PRIM: {
        MinPrimApp *prim = getMinExp_Prim(exp);
        return isAtomicMinExp(prim->exp1) && isAtomicMinExp(prim->exp2);
    }
    default:
        cant_happen("unrecognised MinExp %s", minExpTypeName(exp->type));
    }
}

IntMap *newIntMapFromSymbolList(SymbolList *list) {
    IntMap *map = newIntMap();
    int save = PROTECT(map);
    int i = 0;
    while (list != NULL) {
        setIntMap(map, list->symbol, i);
        i++;
        list = list->next;
    }
    UNPROTECT(save);
    return map;
}

IntMap *extendIntMapWithBindings(IntMap *context, MinBindings *bindings) {
    IntMap *map = newIntMap();
    int save = PROTECT(map);
    // copy existing context
    HashSymbol *key = NULL;
    Index idx = 0;
    Integer val = 0;
    while ((key = iterateIntMap(context, &idx, &val)) != NULL) {
        setIntMap(map, key, val);
    }
    // add bindings starting at next position
    int i = (int)countIntMap(context);
    while (bindings != NULL) {
        setIntMap(map, bindings->var, i);
        i++;
        bindings = bindings->next;
    }
    UNPROTECT(save);
    return map;
}

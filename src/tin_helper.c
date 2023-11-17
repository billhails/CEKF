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
#include <unistd.h>
#include <ctype.h>

#include "common.h"
#include "tin_helper.h"
#include "debug_tin.h"
#include "symbol.h"

static TinFunctionApplication *applyFunSubstitution(TinSubstitution *s, TinFunctionApplication *funApp);
static TinMonoType *applyVarSubstitution(TinSubstitution *s, TinMonoType *mtype);
static TinTypeQuantifier *applyQuantifierSubstitution(TinSubstitution *s, TinTypeQuantifier *tq);

static TinMonoType *instantiateMonoType(TinMonoType *tmt, HashTable *map);
static TinMonoType *instantiateQuantifier(TinTypeQuantifier *ttq, HashTable *map);
static TinMonoType *instantiatePolyType(TinPolyType *tpt, HashTable *map);

static void findMonoTypeVariables(HashTable *map, TinMonoType *monoType);

static bool monoTypeContains(HashSymbol *var, TinMonoType *tmt);

static void showTinSymbol(HashSymbol *symbol);
static void showTinTypeQuantifier(TinTypeQuantifier *quantifier);
static void showTinFun(TinFunctionApplication *fun);
static void showTinSubstitution(TinSubstitution *sub);
static void showTinMonoTypeList(TinMonoTypeList *list);
static void showTinContext(TinContext *context);

static HashSymbol *getTinSymbol(char *name) {
    return newSymbol(name);
}

void printTinSymbol(struct HashSymbol * x, int depth) {
    printf("%*s", depth * 4, "");
    if (x == NULL) { printf("TinSymbol (NULL)"); return; }
    printf("TinSymbol[\"%s\"]", x->name);
}

static void printSubstitutionFn(void *ptr, int depth) {
    printf("%*s", depth * 4, "");
    showTinMonoType(*(TinMonoType **)ptr);
}

static void markSubstitutionFn(void *ptr) {
    markTinMonoType(*(TinMonoType **)ptr);
}

static void printContextFn(void *ptr, int depth) {
    printf("%*s", depth * 4, "");
    showTinPolyType(*(TinPolyType **)ptr);
}

static void markContextFn(void *ptr) {
    markTinPolyType(*(TinPolyType **)ptr);
}

static void markVarFn(void *ptr) {
    markHashSymbol(*(HashSymbol **) ptr);
}

static void printVarFn(void *ptr, int depth) {
    printHashSymbol(*(HashSymbol **) ptr);
}

static HashTable *newSubstitutionTable() {
    HashTable *h = newHashTable(
        sizeof(TinMonoType*),
        markSubstitutionFn,
        printSubstitutionFn
    );
    h->shortEntries = true;
    return h;
}

static HashTable *newContextTcTable() {
    HashTable *h = newHashTable(0, NULL, NULL);
    h->shortEntries = true;
    return h;
}

static HashTable *newContextVarTable() {
    HashTable *h = newHashTable(
        sizeof(TinPolyType*),
        markContextFn,
        printContextFn
    );
    h->shortEntries = true;
    return h;
}

static HashTable *newVarTable() {
    HashTable *h = newHashTable(
        sizeof(HashSymbol *),
        markVarFn,
        printVarFn
    );
    h->shortEntries = true;
    return h;
}

TinContext *freshTinContext() {
    HashTable *vars = newContextVarTable();
    int save = PROTECT(vars);
    HashTable *constructors = newContextTcTable();
    PROTECT(constructors);
    TinContext *c = newTinContext(vars, constructors, NULL);
    UNPROTECT(save);
    return c;
}

static HashTable *newFreeVariableTable() {
    return newHashTable(0, NULL, NULL);
}

void addToSubstitution(TinSubstitution *substitution, HashSymbol *symbol, TinMonoType *monoType) {
    hashSet(substitution->map, symbol, &monoType);
}

void addVarToContext(TinContext *context, HashSymbol *symbol, TinPolyType *polyType) {
    hashSet(context->varFrame, symbol, &polyType);
}

void addConstructorToContext(TinContext *context, HashSymbol *symbol, TinPolyType *polyType) {
    hashSet(context->varFrame, symbol, &polyType);
    hashSet(context->tcFrame, symbol, NULL);
}

TinPolyType *lookupInContext(TinContext *context, HashSymbol *var) {
    if (context == NULL) return NULL;
    TinPolyType *result;
    if (hashGet(context->varFrame, var, &result)) {
        return result;
    }
    return lookupInContext(context->next, var);
}

bool isTypeConstructor(TinContext *context, HashSymbol *var) {
    if (context == NULL) return false;
    if (hashGet(context->tcFrame, var, NULL)) {
        return true;
    }
    return isTypeConstructor(context->next, var);
}

static TinMonoType *lookupInSubstitution(TinSubstitution *substitution, HashSymbol *var) {
    TinMonoType *result;
    if (hashGet(substitution->map, var, &result)) return result;
    return NULL;
}

static HashSymbol *lookupInMap(HashTable *map, HashSymbol *symbol) {
    HashSymbol *result;
    if (hashGet(map, symbol, &result)) return result;
    return NULL;
}

static void setInMap(HashTable *map, HashSymbol *key, HashSymbol *value) {
    hashSet(map, key, &value);
}

static void addFreeVariable(HashTable *map, HashSymbol *key) {
    hashSet(map, key, NULL);
}

static TinContext *copyContext(TinContext *source) {
    if (source == NULL) {
        return freshTinContext();
    } else {
        TinContext *context = copyContext(source->next);
        int save = PROTECT(context);
        copyHashTable(context->varFrame, source->varFrame);
        copyHashTable(context->tcFrame, source->tcFrame);
        UNPROTECT(save);
        return context;
    }
}

TinContext *extendTinContext(TinContext *parent) {
    HashTable *varFrame = newContextVarTable();
    int save = PROTECT(varFrame);
    HashTable *ctFrame = newContextTcTable();
    PROTECT(ctFrame);
    TinContext *context = newTinContext(varFrame, ctFrame, parent);
    UNPROTECT(save);
    return context;
}

static TinSubstitution *copySubstitution(TinSubstitution *source) {
    TinSubstitution *ts = makeEmptySubstitution();
    int save = PROTECT(ts);
    copyHashTable(ts->map, source->map);
    UNPROTECT(save);
    return ts;
}

static TinMonoTypeList *applyArgsSubstitution(TinSubstitution *s, TinMonoTypeList *args) {
#ifdef DEBUG_TIN_SUBSTITUTION
    printf("applyArgsSubstitution ");
    showTinSubstitution(s);
    printf("\n");
    showTinMonoTypeList(args);
    printf("\n");
#ifdef DEBUG_TIN_SUBSTITUTION_SLOWLY
    sleep(1);
#endif
#endif
    if (args == NULL) return NULL;
    TinMonoTypeList *soFar = applyArgsSubstitution(s, args->next);
    int save = PROTECT(soFar);
    TinMonoType *t = applyMonoTypeSubstitution(s, args->monoType);
    if (soFar == args->next && t == args->monoType) {
        UNPROTECT(save);
        return args;
    }
    PROTECT(t);
    TinMonoTypeList *current = newTinMonoTypeList(t, soFar);
    UNPROTECT(save);
    return current;
}

static TinFunctionApplication *applyFunSubstitution(TinSubstitution *s, TinFunctionApplication *funApp) {
#ifdef DEBUG_TIN_SUBSTITUTION
    printf("applyFunSubstitution ");
    showTinFun(funApp);
    printf("\n");
#ifdef DEBUG_TIN_SUBSTITUTION_SLOWLY
    sleep(1);
#endif
#endif
    TinMonoTypeList *args = applyArgsSubstitution(s, funApp->args);
    int save = PROTECT(args);
    if (args == funApp->args) return funApp;
    TinFunctionApplication *result = newTinFunctionApplication(funApp->name, funApp->nargs, args);
    UNPROTECT(save);
    return result;
}

static TinMonoType *applyVarSubstitution(TinSubstitution *s, TinMonoType *mtype) {
#ifdef DEBUG_TIN_SUBSTITUTION
    printf("applyVarSubstitution ");
    showTinMonoType(mtype);
    printf("\n");
#ifdef DEBUG_TIN_SUBSTITUTION_SLOWLY
    sleep(1);
#endif
#endif
    TinMonoType *replacement = lookupInSubstitution(s, mtype->val.var);
    if (replacement != NULL) return replacement;
    return mtype;
}

TinMonoType *applyMonoTypeSubstitution(TinSubstitution *s, TinMonoType *mtype) {
#ifdef DEBUG_TIN_SUBSTITUTION
    printf("applyMonoTypeSubstitution ");
    showTinMonoType(mtype);
    printf("\n");
#ifdef DEBUG_TIN_SUBSTITUTION_SLOWLY
    sleep(1);
#endif
#endif
    switch (mtype->type) {
        case TINMONOTYPE_TYPE_VAR:
            return applyVarSubstitution(s, mtype);
        case TINMONOTYPE_TYPE_FUN: {
            TinFunctionApplication *tfa = applyFunSubstitution(s, mtype->val.fun);
            if (tfa == mtype->val.fun) return mtype;
            int save = PROTECT(tfa);
            TinMonoType *tmt = newTinMonoType(TINMONOTYPE_TYPE_FUN, TINMONOTYPE_VAL_FUN(tfa));
            UNPROTECT(save);
            return tmt;
        }
        default:
            cant_happen("unrecognised type %d in applyMonoTypeSubstitution", mtype->type);
    }
}

static TinTypeQuantifier *applyQuantifierSubstitution(TinSubstitution *s, TinTypeQuantifier *tq) {
#ifdef DEBUG_TIN_SUBSTITUTION
    printf("applyQuantifierSubstitution ");
    showTinTypeQuantifier(tq);
    printf("\n");
#ifdef DEBUG_TIN_SUBSTITUTION_SLOWLY
    sleep(1);
#endif
#endif
    TinPolyType *pt = applyPolyTypeSubstitution(s, tq->quantifiedType);
    if (pt == tq->quantifiedType) return tq;
    int save = PROTECT(pt);
    TinTypeQuantifier *result = newTinTypeQuantifier(tq->var, pt);
    UNPROTECT(save);
    return result;
}

TinPolyType *applyPolyTypeSubstitution(TinSubstitution *s, TinPolyType *ptype) {
#ifdef DEBUG_TIN_SUBSTITUTION
    printf("applyPolyTypeSubstitution ");
    showTinPolyType(ptype);
    printf("\n");
#ifdef DEBUG_TIN_SUBSTITUTION_SLOWLY
    sleep(1);
#endif
#endif
    switch (ptype->type) {
        case TINPOLYTYPE_TYPE_MONOTYPE: {
            TinMonoType *tmt = applyMonoTypeSubstitution(s, ptype->val.monoType);
            if (tmt == ptype->val.monoType) return ptype;
            int save = PROTECT(tmt);
            TinPolyType *pt = newTinPolyType(TINPOLYTYPE_TYPE_MONOTYPE, TINPOLYTYPE_VAL_MONOTYPE(tmt));
            UNPROTECT(save);
            return pt;
        }
        case TINPOLYTYPE_TYPE_QUANTIFIER: {
            TinTypeQuantifier *ttq = applyQuantifierSubstitution(s, ptype->val.quantifier);
            if (ttq == ptype->val.quantifier) return ptype;
            int save = PROTECT(ttq);
            TinPolyType *pt = newTinPolyType(TINPOLYTYPE_TYPE_QUANTIFIER, TINPOLYTYPE_VAL_QUANTIFIER(ttq));
            UNPROTECT(save);
            return pt;
        }
        default:
            cant_happen("unrecognised type %d in applyPolyTypeSubstitution", ptype->type);
    }
}

void applyContextSubstitutionInPlace(TinSubstitution *s, TinContext *context) {
#ifdef DEBUG_TIN_SUBSTITUTION
    printf("applyContextSubstitutionInPlace ");
    showTinContext(context);
    printf("\n");
#ifdef DEBUG_TIN_SUBSTITUTION_SLOWLY
    sleep(1);
#endif
#endif
    int i = 0;
    TinPolyType *t = NULL;
    HashSymbol *key;
    while ((key = iterateHashTable(context->varFrame, &i, &t)) != NULL) {
        TinPolyType *ts = applyPolyTypeSubstitution(s, t);
        addVarToContext(context, key, ts); // we know it's already in the context so this won't change the table
    }
}

TinContext *applyContextSubstitution(TinSubstitution *s, TinContext *context) {
#ifdef DEBUG_TIN_SUBSTITUTION
    printf("applyContextSubstitution ");
    showTinContext(context);
    printf("\n");
#ifdef DEBUG_TIN_SUBSTITUTION_SLOWLY
    sleep(1);
#endif
#endif
    TinContext *result = copyContext(context);
    int save = PROTECT(result);
    int i = 0;
    TinPolyType *t = NULL;
    HashSymbol *key;
    while ((key = iterateHashTable(result->varFrame, &i, &t)) != NULL) {
        TinPolyType *ts = applyPolyTypeSubstitution(s, t);
        addVarToContext(result, key, ts); // we know it's already in the context so this won't change the table
    }
    UNPROTECT(save);
    return result;
}

TinSubstitution *applySubstitutionSubstitution(TinSubstitution *s1, TinSubstitution *s2) {
#ifdef DEBUG_TIN_SUBSTITUTION
    printf("applySubstitutionSubstitution ");
    showTinSubstitution(s2);
    printf("\n");
#ifdef DEBUG_TIN_SUBSTITUTION_SLOWLY
    sleep(1);
#endif
#endif
    TinSubstitution *ts = copySubstitution(s1);
    int save = PROTECT(ts);
    int i = 0;
    TinMonoType *tmt1 = NULL;
    HashSymbol *key;
    while ((key = iterateHashTable(s2->map, &i, &tmt1)) != NULL) {
        TinMonoType *tmt2 = applyMonoTypeSubstitution(s1, tmt1);
        int save2 = PROTECT(tmt2);
        addToSubstitution(ts, key, tmt2);
        UNPROTECT(save2);
    }
    UNPROTECT(save);
    return ts;
}

HashSymbol *freshTypeVariable(const char *suffix) {
    char buf[128];
    sprintf(buf, "#%s", suffix);
    return genSym(buf);
}

static HashSymbol *instantiateVar(HashSymbol *symbol, HashTable *map) {
#ifdef DEBUG_TIN_INSTANTIATION
    printf("instantiateVar ");
    printHashSymbol(symbol);
    printf("\n");
#ifdef DEBUG_TIN_SUBSTITUTION_SLOWLY
    sleep(1);
#endif
#endif
    HashSymbol *replacement = lookupInMap(map, symbol);
    if (replacement != NULL) return replacement;
    return symbol;
}

static TinMonoTypeList *instantiateArgs(TinMonoTypeList *args, HashTable *map) {
#ifdef DEBUG_TIN_INSTANTIATION
    printf("instantiateArgs ");
    showTinMonoTypeList(args);
    printf("\n");
#ifdef DEBUG_TIN_SUBSTITUTION_SLOWLY
    sleep(1);
#endif
#endif
    if (args == NULL) return NULL;
    TinMonoTypeList *next = instantiateArgs(args->next, map);
    int save = PROTECT(next);
    TinMonoType *monoType = instantiateMonoType(args->monoType, map);
    if (next == args->next && monoType == args->monoType) {
        UNPROTECT(save);
        return args;
    }
    PROTECT(monoType);
    TinMonoTypeList *newArgs = newTinMonoTypeList(monoType, next);
    UNPROTECT(save);
    return newArgs;

}

static TinFunctionApplication *instantiateFun(TinFunctionApplication *tfa, HashTable *map) {
#ifdef DEBUG_TIN_INSTANTIATION
    printf("instantiateFun ");
    showTinFunctionApplication(tfa);
    printf("\n");
#ifdef DEBUG_TIN_SUBSTITUTION_SLOWLY
    sleep(1);
#endif
#endif
    TinMonoTypeList *args = instantiateArgs(tfa->args, map);
    if (args == tfa->args) return tfa;
    int save = PROTECT(tfa);
    PROTECT(args);
    TinFunctionApplication *newTfa = newTinFunctionApplication(tfa->name, tfa->nargs, args);
    UNPROTECT(save);
    return newTfa;
}

static TinMonoType *instantiateMonoType(TinMonoType *tmt, HashTable *map) {
#ifdef DEBUG_TIN_INSTANTIATION
    printf("instantiateMonoType ");
    showTinMonoType(tmt);
    printf("\n");
#ifdef DEBUG_TIN_SUBSTITUTION_SLOWLY
    sleep(1);
#endif
#endif
    switch (tmt->type) {
        case TINMONOTYPE_TYPE_VAR: {
            HashSymbol *var = instantiateVar(tmt->val.var, map);
            if (tmt->val.var == var) return tmt;
            int save = PROTECT(var);
            TinMonoType *newTmt = newTinMonoType(
                TINMONOTYPE_TYPE_VAR,
                TINMONOTYPE_VAL_VAR(var)
            );
            UNPROTECT(save);
            return newTmt;
        }
        case TINMONOTYPE_TYPE_FUN: {
            TinFunctionApplication *tfa = instantiateFun(tmt->val.fun, map);
            if (tfa == tmt->val.fun) return tmt;
            int save = PROTECT(tfa);
            TinMonoType *newTmt = newTinMonoType(
                TINMONOTYPE_TYPE_FUN,
                TINMONOTYPE_VAL_FUN(tfa)
            );
            UNPROTECT(save);
            return newTmt;
        }
        default:
            cant_happen("unrecognised type %d in instantiateMonoType", tmt->type);
    }
}

static TinMonoType *instantiateQuantifier(TinTypeQuantifier *ttq, HashTable *map) {
#ifdef DEBUG_TIN_INSTANTIATION
    printf("instantiateQuantifier ");
    showTinTypeQuantifier(ttq);
    printf("\n");
#ifdef DEBUG_TIN_SUBSTITUTION_SLOWLY
    sleep(1);
#endif
#endif
    HashSymbol *newVar = freshTypeVariable("instantiate");
    int save = PROTECT(newVar);
    setInMap(map, ttq->var, newVar);
    UNPROTECT(save);
    return instantiatePolyType(ttq->quantifiedType, map);
}


static TinMonoType *instantiatePolyType(TinPolyType *tpt, HashTable *map) {
#ifdef DEBUG_TIN_INSTANTIATION
    printf("instantiatePolyType ");
    showTinPolyType(tpt);
    printf("\n");
#ifdef DEBUG_TIN_SUBSTITUTION_SLOWLY
    sleep(1);
#endif
#endif
    switch (tpt->type) {
        case TINPOLYTYPE_TYPE_MONOTYPE:
            return instantiateMonoType(tpt->val.monoType, map);
        case TINPOLYTYPE_TYPE_QUANTIFIER:
            return instantiateQuantifier(tpt->val.quantifier, map);
        default:
            cant_happen("unrecognised type %d in instantiatePolyType", tpt->type);
    }
}

TinMonoType *instantiate(TinPolyType *tpt) {
    HashTable *map = newVarTable();
    int save = PROTECT(map);
    TinMonoType *result = instantiatePolyType(tpt, map);
    validateLastAlloc();
    UNPROTECT(save);
    return result;
}

static void findArgVariables(HashTable *map, TinMonoTypeList *args) {
    while (args != NULL) {
        findMonoTypeVariables(map, args->monoType);
        args = args->next;
    }
}

static void findFunVariables(HashTable *map, TinFunctionApplication *app) {
    findArgVariables(map, app->args);
}

static void findMonoTypeVariables(HashTable *map, TinMonoType *monoType) {
    switch (monoType->type) {
        case TINMONOTYPE_TYPE_VAR:
            addFreeVariable(map, monoType->val.var);
            break;
        case TINMONOTYPE_TYPE_FUN:
            findFunVariables(map, monoType->val.fun);
            break;
        default:
            cant_happen("unrecognised type %d in findMonoTypeVariables", monoType->type);
    }
}

TinPolyType *generalize(TinContext *context, TinMonoType *monoType) {
    HashTable *map = newFreeVariableTable();
    int save = PROTECT(map);
    findMonoTypeVariables(map, monoType);
    TinPolyType *tpt = newTinPolyType(
        TINPOLYTYPE_TYPE_MONOTYPE,
        TINPOLYTYPE_VAL_MONOTYPE(monoType)
    );
    HashSymbol *var;
    TinTypeQuantifier *tqt = NULL;
    int i = 0;
    while (var = iterateHashTable(map, &i, NULL)) {
        if (lookupInContext(context, var) == NULL) {
            int save2 = PROTECT(tpt);
            tqt = newTinTypeQuantifier(var, tpt);
            PROTECT(tqt);
            tpt = newTinPolyType(
                TINPOLYTYPE_TYPE_QUANTIFIER,
                TINPOLYTYPE_VAL_QUANTIFIER(tqt)
            );
            UNPROTECT(save2);
        }
    }
    UNPROTECT(save);
    return tpt;
}

TinSubstitution *makeEmptySubstitution() {
    HashTable *h = newSubstitutionTable();
    int save = PROTECT(h);
    TinSubstitution *s = newTinSubstitution(h);
    UNPROTECT(save);
    return s;
}

static bool argsContains(HashSymbol *var, TinMonoTypeList *args) {
    while (args != NULL) {
        if (monoTypeContains(var, args->monoType)) return true;
        args = args->next;
    }
    return false;
}

static bool funContains(HashSymbol *var, TinFunctionApplication *tfa) {
    return argsContains(var, tfa->args);
}

static bool monoTypeContains(HashSymbol *var, TinMonoType *tmt) {
    switch (tmt->type) {
        case TINMONOTYPE_TYPE_VAR:
            return var == tmt->val.var;
        case TINMONOTYPE_TYPE_FUN:
            return funContains(var, tmt->val.fun);
        default:
            cant_happen("unrecognised type %d in monoTypeContains", tmt->type);
    }
}

TinSubstitution *unify(TinMonoType *t1, TinMonoType *t2, const char *caller) {
#ifdef DEBUG_TIN_UNIFICATION
    printf("%s unify ", caller);
    showTinMonoType(t1);
    printf(" with ");
    showTinMonoType(t2);
    printf("\n\n");
#endif
    switch (t1->type) {
        case TINMONOTYPE_TYPE_VAR: {
            TinSubstitution *s = makeEmptySubstitution();
            HashSymbol *var = t1->val.var;
            if (t2->type == TINMONOTYPE_TYPE_VAR && var == t2->val.var) {
                return s;
            }
            if (monoTypeContains(var, t2)) {
                can_happen("occurs check failed: %s", t1->val.var->name);
                return s;
            }
            int save = PROTECT(s);
            addToSubstitution(s, var, t2);
            UNPROTECT(save);
            return s;
        }
        case TINMONOTYPE_TYPE_FUN: {
            switch (t2->type) {
                case TINMONOTYPE_TYPE_VAR:
                    return unify(t2, t1, caller);
                case TINMONOTYPE_TYPE_FUN: {
                    TinSubstitution *s = makeEmptySubstitution();
                    TinFunctionApplication *f1 = t1->val.fun;
                    TinFunctionApplication *f2 = t2->val.fun;
                    if (f1->name != f2->name) {
                        can_happen("%s cannot unify %s with %s", caller, f1->name->name, f2->name->name);
                        return s;
                    }
                    if (f1->nargs != f2->nargs) {
                        cant_happen("%s different argument counts for type function %s: %d vs. %d", caller, f1->name->name, f1->nargs, f2->nargs);
                    }
                    int save = PROTECT(s);
                    TinMonoTypeList *a1 = f1->args;
                    TinMonoTypeList *a2 = f2->args;
                    while (a1 != NULL) {
                        if (a2 == NULL) {
                            cant_happen("%s argument lengths underrun in unify %s", caller, f1->name->name);
                        }
                        TinMonoType *tmt1 = applyMonoTypeSubstitution(s, a1->monoType);
                        PROTECT(tmt1);
                        TinMonoType *tmt2 = applyMonoTypeSubstitution(s, a2->monoType);
                        PROTECT(tmt2);
                        TinSubstitution *s2 = unify(tmt1, tmt2, caller);
                        PROTECT(s2);
                        s = applySubstitutionSubstitution(s, s2);
                        UNPROTECT(save);
                        save = PROTECT(s);
                        a1 = a1->next;
                        a2 = a2->next;
                    }
                    if (a2 != NULL) {
                        cant_happen("%s, argument lengths overrun in unify %s", caller, f1->name->name);
                    }
                    UNPROTECT(save);
                    return s;
                }
                default:
                    cant_happen("%s unrecognised t2 %d in unify", caller, t2->type);
            }
        }
        default:
            cant_happen("%s unrecognised t1 %d in unify", caller, t1->type);
    }
}


static bool isAlphaSymbol(HashSymbol *symbol) {
    if (symbol == NULL) return true;
    return (bool) isalpha(symbol->name[0]);
}

static void showTinContext(TinContext *context) {
    printTinContext(context, 0);
}

static void showTinSubstitution(TinSubstitution *sub) {
    printHashTable(sub->map, 0);
}

static void showTinMonoTypeList(TinMonoTypeList *list) {
    printf("(");
    for (TinMonoTypeList *args = list; args != NULL; args = args->next) {
        showTinMonoType(args->monoType);
        if (args->next != NULL) printf(", ");
    }
    printf(")");
}

static void showTinFun(TinFunctionApplication *fun) {
    if (fun == NULL) { printf("<null>"); return; }
    if (isAlphaSymbol(fun->name) || fun->nargs != 2) {
        showTinSymbol(fun->name);
        showTinMonoTypeList(fun->args);
    } else {
        printf("(");
        for (TinMonoTypeList *args = fun->args; args != NULL; args = args->next) {
            showTinMonoType(args->monoType);
            if (args->next != NULL) {
                printf(" ");
                showTinSymbol(fun->name);
                printf(" ");
            }
        }
        printf(")");
    }
}

static void showTinSymbol(HashSymbol *symbol) {
    if (symbol == NULL) { printf("<null>"); return; }
    printf("%s", symbol->name);
}

static void showTinTypeQuantifier(TinTypeQuantifier *quantifier) {
    if (quantifier == NULL) { printf("<null>"); return; }
    printf("V");
    showTinSymbol(quantifier->var);
    printf(".");
    showTinPolyType(quantifier->quantifiedType);
}

void showTinMonoType(TinMonoType *monoType) {
    if (monoType == NULL) { printf("<null>"); return; }
    switch (monoType->type) {
        case TINMONOTYPE_TYPE_VAR:
            showTinSymbol(monoType->val.var);
            break;
        case TINMONOTYPE_TYPE_FUN:
            showTinFun(monoType->val.fun);
            break;
        default:
            cant_happen("unrecognised type %d in showTinMonoType", monoType->type);
    }
}

void showTinPolyType(TinPolyType *polyType) {
    if (polyType == NULL) { printf("<null>"); return; }
    switch (polyType->type) {
        case TINPOLYTYPE_TYPE_MONOTYPE:
            showTinMonoType(polyType->val.monoType);
            break;
        case TINPOLYTYPE_TYPE_QUANTIFIER:
            showTinTypeQuantifier(polyType->val.quantifier);
            break;
        default:
            cant_happen("unrecognised type %d in showTinPolyType", polyType->type);
    }
}

#ifdef DEBUG_RUN_TESTS
#if DEBUG_RUN_TESTS == 3

#include "tests/tin.inc"

#endif
#endif

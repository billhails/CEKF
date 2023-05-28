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

#include "common.h"
#include "tin_helper.h"
#include "debug_tin.h"
#include "symbol.h"

static TinFunctionApplication *applyFunSubstitution(TinSubstitution *s, TinFunctionApplication *funApp);
static TinMonoType *applyVarSubstitution(TinSubstitution *s, TinMonoType *mtype);
static TinMonoType *applyVarSubstitution(TinSubstitution *s, TinMonoType *mtype);
static TinMonoType *applyMonoTypeSubstitution(TinSubstitution *s, TinMonoType *mtype);
static TinTypeQuantifier *applyQuantifierSubstitution(TinSubstitution *s, TinTypeQuantifier *tq);
static TinPolyType *applyPolyTypeSubstitution(TinSubstitution *s, TinPolyType *ptype);
static TinContext *applyContextSubstitution(TinSubstitution *s, TinContext *context);
static TinSubstitution *applySubstitutionSubstitution(TinSubstitution *s1, TinSubstitution *s2);
static TinSubstitution *makeEmptySubstitution();

static TinMonoType *instantiateMonoType(TinMonoType *tmt, HashTable *map);
static TinMonoType *instantiateQuantifier(TinTypeQuantifier *ttq, HashTable *map);
static TinMonoType *instantiatePolyType(TinPolyType *tpt, HashTable *map);

static void findMonoTypeVariables(HashTable *map, TinMonoType *monoType);

static bool monoTypeContains(HashSymbol *var, TinMonoType *tmt);

static HashSymbol *getTinSymbol(char *name) {
    return newSymbol(name);
}

void printTinSymbol(struct HashSymbol * x, int depth) {
    if (x == NULL) { printf("TinSymbol (NULL)"); return; }
    printf("TinSymbol[\"%s\"]", x->name);
}

static void printSubstitutionFn(void *ptr, int depth) {
    printTinMonoType(*(TinMonoType **)ptr, depth);
}

static void markSubstitutionFn(void *ptr) {
    markTinMonoType(*(TinMonoType **)ptr);
}

static void printContextFn(void *ptr, int depth) {
    printTinPolyType(*(TinPolyType **)ptr, depth);
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
    return newHashTable(
        sizeof(TinMonoType*),
        markSubstitutionFn,
        printSubstitutionFn
    );
}

static HashTable *newContextTable() {
    return newHashTable(
        sizeof(TinPolyType*),
        markContextFn,
        printContextFn
    );
}

static HashTable *newVarTable() {
    return newHashTable(
        sizeof(HashSymbol *),
        markVarFn,
        printVarFn
    );
}

static HashTable *newFreeVariableTable() {
    return newHashTable(0, NULL, NULL);
}

static void addToSubstitution(TinSubstitution *substitution, HashSymbol *symbol, TinMonoType *monoType) {
    hashSet(substitution->map, symbol, &monoType);
}

static void addToContext(TinContext *context, HashSymbol *symbol, TinPolyType *polyType) {
    hashSet(context->frame, symbol, &polyType);
}

static TinPolyType *lookupInContext(TinContext *context, HashSymbol *var) {
    if (context == NULL) return NULL;
    TinPolyType *result;
    if (hashGet(context->frame, var, &result)) return result;
    return lookupInContext(context->next, var);
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
        HashTable *table = newContextTable();
        int save = PROTECT(table);
        TinContext * context = newTinContext(table, NULL);
        UNPROTECT(save);
        return context;
    } else {
        TinContext *context = copyContext(source->next);
        int save = PROTECT(context);
        copyHashTable(context->frame, source->frame);
        UNPROTECT(save);
        return context;
    }
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
    printTinMonoTypeList(funApp, 0);
    printf("\n");
    sleep(1);
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
    printTinFunctionApplication(funApp, 0);
    printf("\n");
    sleep(1);
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
    printTinMonoType(mtype, 0);
    printf("\n");
    sleep(1);
#endif
    TinMonoType *replacement = lookupInSubstitution(s, mtype->val.var);
    if (replacement != NULL) return replacement;
    return mtype;
}

static TinMonoType *applyMonoTypeSubstitution(TinSubstitution *s, TinMonoType *mtype) {
#ifdef DEBUG_TIN_SUBSTITUTION
    printf("applyMonoTypeSubstitution ");
    printTinMonoType(mtype, 0);
    printf("\n");
    sleep(1);
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
    printTinTypeQuantifier(tq, 0);
    printf("\n");
    sleep(1);
#endif
    TinPolyType *pt = applyPolyTypeSubstitution(s, tq->quantifiedType);
    if (pt == tq->quantifiedType) return tq;
    int save = PROTECT(pt);
    TinTypeQuantifier *result = newTinTypeQuantifier(tq->var, pt);
    UNPROTECT(save);
    return result;
}

static TinPolyType *applyPolyTypeSubstitution(TinSubstitution *s, TinPolyType *ptype) {
#ifdef DEBUG_TIN_SUBSTITUTION
    printf("applyPolyTypeSubstitution ");
    printTinPolyType(ptype, 0);
    printf("\n");
    sleep(1);
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

static TinContext *applyContextSubstitution(TinSubstitution *s, TinContext *context) {
#ifdef DEBUG_TIN_SUBSTITUTION
    printf("applyContextSubstitution ");
    printTinContext(context, 0);
    printf("\n");
    sleep(1);
#endif
    TinContext *result = copyContext(context);
    int save = PROTECT(result);
    int i = 0;
    TinPolyType *t = NULL;
    HashSymbol *key;
    while ((key = iterateHashTable(result->frame, &i, &t)) != NULL) {
        TinPolyType *ts = applyPolyTypeSubstitution(s, t);
        addToContext(result, key, ts); // we know it's already in the context so this won't change the table
    }
    UNPROTECT(save);
    return result;
}

static TinSubstitution *applySubstitutionSubstitution(TinSubstitution *s1, TinSubstitution *s2) {
#ifdef DEBUG_TIN_SUBSTITUTION
    printf("applySubstitutionSubstitution ");
    printTinSubstitution(s2, 0);
    printf("\n");
    sleep(1);
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

static HashSymbol *newTypeVariable() {
    return genSym("#");
}

static HashSymbol *instantiateVar(HashSymbol *symbol, HashTable *map) {
#ifdef DEBUG_TIN_INSTANTIATION
    printf("instantiateVar ");
    printHashSymbol(symbol);
    printf("\n");
    sleep(1);
#endif
    HashSymbol *replacement = lookupInMap(map, symbol);
    if (replacement != NULL) return replacement;
    return symbol;
}

static TinMonoTypeList *instantiateArgs(TinMonoTypeList *args, HashTable *map) {
#ifdef DEBUG_TIN_INSTANTIATION
    printf("instantiateArgs ");
    printTinMonoTypeList(args, 0);
    printf("\n");
    sleep(1);
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
    printTinFunctionApplication(tfa, 0);
    printf("\n");
    sleep(1);
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
    printTinMonoType(tmt, 0);
    printf("\n");
    sleep(1);
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
    printTinTypeQuantifier(ttq, 0);
    printf("\n");
    sleep(1);
#endif
    HashSymbol *newVar = newTypeVariable();
    int save = PROTECT(newVar);
    setInMap(map, ttq->var, newVar);
    UNPROTECT(save);
    return instantiatePolyType(ttq->quantifiedType, map);
}


static TinMonoType *instantiatePolyType(TinPolyType *tpt, HashTable *map) {
#ifdef DEBUG_TIN_INSTANTIATION
    printf("instantiatePolyType ");
    printTinPolyType(tpt, 0);
    printf("\n");
    sleep(1);
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

static TinSubstitution *makeEmptySubstitution() {
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

TinSubstitution *unify(TinMonoType *t1, TinMonoType *t2) {
#ifdef DEBUG_TIN_UNIFICATION
    printf("unify ");
    printTinMonoType(t1, 0);
    printf(" with ");
    printTinMonoType(t2, 0);
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
                    return unify(t2, t1);
                case TINMONOTYPE_TYPE_FUN: {
                    TinSubstitution *s = makeEmptySubstitution();
                    TinFunctionApplication *f1 = t1->val.fun;
                    TinFunctionApplication *f2 = t2->val.fun;
                    if (f1->name != f2->name) {
                        can_happen("cannot unify %s with %s", f1->name->name, f2->name->name);
                        return s;
                    }
                    if (f1->nargs != f2->nargs) {
                        cant_happen("different argument counts for type function %s: %d vs. %d", f1->name->name, f1->nargs, f2->nargs);
                    }
                    int save = PROTECT(s);
                    TinMonoTypeList *a1 = f1->args;
                    TinMonoTypeList *a2 = f2->args;
                    while (a1 != NULL) {
                        if (a2 == NULL) {
                            cant_happen("argument lengths underrun in unify %s", f1->name->name);
                        }
                        TinMonoType *tmt1 = applyMonoTypeSubstitution(s, a1->monoType);
                        PROTECT(tmt1);
                        TinMonoType *tmt2 = applyMonoTypeSubstitution(s, a2->monoType);
                        PROTECT(tmt2);
                        TinSubstitution *s2 = unify(tmt1, tmt2);
                        PROTECT(s2);
                        s = applySubstitutionSubstitution(s, s2);
                        UNPROTECT(save);
                        save = PROTECT(s);
                        a1 = a1->next;
                        a2 = a2->next;
                    }
                    if (a2 != NULL) {
                        cant_happen("argument lengths overrun in unify %s", f1->name->name);
                    }
                    UNPROTECT(save);
                    return s;
                }
                default:
                    cant_happen("unrecognised t2 %d in unify", t2->type);
            }
        }
        default:
            cant_happen("unrecognised t1 %d in unify", t1->type);
    }
}

#ifdef DEBUG_RUN_TESTS
#if DEBUG_RUN_TESTS == 3

#define EXPECT(EXP) do { if (EXP) printf("assertion " #EXP " passed\n"); else printf("ASSERTION " #EXP " FAILED\n"); } while(0)

static bool isTypeVariable(HashSymbol *symbol) {
    return symbol->name[0] == '#';
}

void testTin() {
    printf("testTin()\n");
    int save;
    TinMonoType *tmt1;
    TinMonoType *tmt2;
    TinPolyType *tpt;
    TinSubstitution *C1;
    TinSubstitution *C2;
    TinContext *tc;
    HashTable *htc;
    int totalSave = PROTECT(NULL);
    HashSymbol *x = newSymbol("x"); PROTECT(x);
    HashSymbol *y = newSymbol("y"); PROTECT(y);
    HashSymbol *z = newSymbol("z"); PROTECT(y);
    HashSymbol *arrow = newSymbol("->"); PROTECT(arrow);
    HashSymbol *boolean = newSymbol("Bool"); PROTECT(boolean);
    HashSymbol *integer = newSymbol("Int"); PROTECT(boolean);

    disableGC();
    tmt1 = newTinMonoType(TINMONOTYPE_TYPE_VAR, TINMONOTYPE_VAL_VAR(x));
    C1 = newTinSubstitution(newSubstitutionTable());
    addToSubstitution(C1, x, newTinMonoType(TINMONOTYPE_TYPE_VAR, TINMONOTYPE_VAL_VAR(y)));
    save = PROTECT(tmt1);
    PROTECT(C1);
    enableGC();
    tmt1 = applyMonoTypeSubstitution(C1, tmt1);
    EXPECT(tmt1->val.var == y);
    UNPROTECT(save);

    disableGC();
    tmt1 = newTinMonoType(
        TINMONOTYPE_TYPE_FUN,
        TINMONOTYPE_VAL_FUN(
            newTinFunctionApplication(
                arrow,
                2,
                newTinMonoTypeList(
                    newTinMonoType(
                        TINMONOTYPE_TYPE_FUN,
                        TINMONOTYPE_VAL_FUN(newTinFunctionApplication(boolean, 0, NULL))
                    ),
                    newTinMonoTypeList(
                        newTinMonoType(
                            TINMONOTYPE_TYPE_VAR,
                            TINMONOTYPE_VAL_VAR(x)
                        ),
                        NULL
                    )
                )
            )
        )
    );

    C1 = newTinSubstitution(newSubstitutionTable());
    addToSubstitution(C1, x, newTinMonoType(TINMONOTYPE_TYPE_VAR, TINMONOTYPE_VAL_VAR(y)));
    save = PROTECT(tmt1);
    PROTECT(C1);
    enableGC();
    tmt1 = applyMonoTypeSubstitution(C1, tmt1);
    EXPECT(tmt1->val.fun->name == arrow);
    EXPECT(tmt1->val.fun->args->monoType->val.fun->name == boolean);
    EXPECT(tmt1->val.fun->args->next->monoType->val.var == y);
    UNPROTECT(save);

    disableGC();
    tpt = newTinPolyType(
        TINPOLYTYPE_TYPE_MONOTYPE,
        TINPOLYTYPE_VAL_MONOTYPE(
            newTinMonoType(
                TINMONOTYPE_TYPE_FUN,
                TINMONOTYPE_VAL_FUN(
                    newTinFunctionApplication(
                        arrow,
                        2,
                        newTinMonoTypeList(
                            newTinMonoType(
                                TINMONOTYPE_TYPE_FUN,
                                TINMONOTYPE_VAL_FUN(newTinFunctionApplication(boolean, 0, NULL))
                            ),
                            newTinMonoTypeList(
                                newTinMonoType(
                                    TINMONOTYPE_TYPE_VAR,
                                    TINMONOTYPE_VAL_VAR(x)
                                ),
                                NULL
                            )
                        )
                    )
                )
            )
        )
    );
    C1 = newTinSubstitution(newSubstitutionTable());
    addToSubstitution(C1, x, newTinMonoType(TINMONOTYPE_TYPE_VAR, TINMONOTYPE_VAL_VAR(y)));
    save = PROTECT(tpt);
    PROTECT(C1);
    enableGC();
    tpt = applyPolyTypeSubstitution(C1, tpt);
    EXPECT(tpt->val.monoType->val.fun->name == arrow);
    EXPECT(tpt->val.monoType->val.fun->args->monoType->val.fun->name == boolean);
    EXPECT(tpt->val.monoType->val.fun->args->next->monoType->val.var == y);
    UNPROTECT(save);

    disableGC();
    tpt = newTinPolyType(
        TINPOLYTYPE_TYPE_QUANTIFIER,
        TINPOLYTYPE_VAL_QUANTIFIER(
            newTinTypeQuantifier(
                x,
                newTinPolyType(
                    TINPOLYTYPE_TYPE_MONOTYPE,
                    TINPOLYTYPE_VAL_MONOTYPE(
                        newTinMonoType(
                            TINMONOTYPE_TYPE_VAR,
                            TINMONOTYPE_VAL_VAR(x)
                        )
                    )
                )
            )
        )
    );
    C1 = newTinSubstitution(newSubstitutionTable());
    addToSubstitution(C1, x, newTinMonoType(TINMONOTYPE_TYPE_VAR, TINMONOTYPE_VAL_VAR(y)));
    save = PROTECT(tpt);
    PROTECT(C1);
    enableGC();
    tpt = applyPolyTypeSubstitution(C1, tpt);
    PROTECT(tpt);
    EXPECT(tpt->val.quantifier->var == x);
    EXPECT(tpt->val.quantifier->quantifiedType->val.monoType->val.var == y);
    UNPROTECT(save);

    disableGC();
    htc = newContextTable();
    tc = newTinContext(htc, NULL);
    addToContext(tc, x, newTinPolyType(
        TINPOLYTYPE_TYPE_MONOTYPE,
        TINPOLYTYPE_VAL_MONOTYPE(
            newTinMonoType(
                TINMONOTYPE_TYPE_VAR,
                TINMONOTYPE_VAL_VAR(x)
            )
        )
    ));

    C1 = newTinSubstitution(newSubstitutionTable());
    addToSubstitution(C1, x, newTinMonoType(TINMONOTYPE_TYPE_VAR, TINMONOTYPE_VAL_VAR(y)));
    save = PROTECT(tc);
    PROTECT(C1);
    enableGC();
    tc = applyContextSubstitution(C1, tc);
    PROTECT(tc);
    EXPECT(lookupInContext(tc, x)->val.monoType->val.var == y);
    UNPROTECT(save);

    // C1 = { x |-> y }
    // C2 = { y |-> z }
    // C1(C2) =  { x |-> y, y |-> z }
    // x | x | y
    // y | z | z

    disableGC();
    C1 = newTinSubstitution(newSubstitutionTable());
    addToSubstitution(C1, x, newTinMonoType(TINMONOTYPE_TYPE_VAR, TINMONOTYPE_VAL_VAR(y)));
    C2 = newTinSubstitution(newSubstitutionTable());
    addToSubstitution(C2, y, newTinMonoType(TINMONOTYPE_TYPE_VAR, TINMONOTYPE_VAL_VAR(z)));
    save = PROTECT(C1);
    PROTECT(C2);
    enableGC();
    C2 = applySubstitutionSubstitution(C1, C2);
    EXPECT(lookupInSubstitution(C2, y)->val.var == z);
    EXPECT(lookupInSubstitution(C2, x)->val.var == y);
    UNPROTECT(save);

    // C1 = { y |-> z }
    // C2 = { x |-> y }
    // C1(C2) =  { x |-> z, y |-> z }
    // x | y | z
    // y | y | z

    disableGC();
    C1 = newTinSubstitution(newSubstitutionTable());
    addToSubstitution(C1, y, newTinMonoType(TINMONOTYPE_TYPE_VAR, TINMONOTYPE_VAL_VAR(z)));
    C2 = newTinSubstitution(newSubstitutionTable());
    addToSubstitution(C2, x, newTinMonoType(TINMONOTYPE_TYPE_VAR, TINMONOTYPE_VAL_VAR(y)));
    save = PROTECT(C1);
    PROTECT(C2);
    enableGC();
    C2 = applySubstitutionSubstitution(C1, C2);
    EXPECT(lookupInSubstitution(C2, y)->val.var == z);
    EXPECT(lookupInSubstitution(C2, x)->val.var == z);
    UNPROTECT(save);

    disableGC();
    tpt = newTinPolyType(
        TINPOLYTYPE_TYPE_MONOTYPE,
        TINPOLYTYPE_VAL_MONOTYPE(
            newTinMonoType(
                TINMONOTYPE_TYPE_VAR,
                TINMONOTYPE_VAL_VAR(x)
            )
        )
    );
    save = PROTECT(tpt);
    enableGC();
    tmt1 = instantiate(tpt);
    EXPECT(tmt1->val.var == x);
    UNPROTECT(save);

    disableGC();
    tpt = newTinPolyType(
        TINPOLYTYPE_TYPE_MONOTYPE,
        TINPOLYTYPE_VAL_MONOTYPE(
            newTinMonoType(
                TINMONOTYPE_TYPE_FUN,
                TINMONOTYPE_VAL_FUN(
                    newTinFunctionApplication(
                        arrow,
                        2,
                        newTinMonoTypeList(
                            newTinMonoType(
                                TINMONOTYPE_TYPE_FUN,
                                TINMONOTYPE_VAL_FUN(newTinFunctionApplication(boolean, 0, NULL))
                            ),
                            newTinMonoTypeList(
                                newTinMonoType(
                                    TINMONOTYPE_TYPE_VAR,
                                    TINMONOTYPE_VAL_VAR(x)
                                ),
                                NULL
                            )
                        )
                    )
                )
            )
        )
    );
    save = PROTECT(tpt);
    enableGC();
    tmt1 = instantiate(tpt);
    EXPECT(tmt1->val.fun->name == arrow);
    EXPECT(tmt1->val.fun->args->monoType->val.fun->name == boolean);
    EXPECT(tmt1->val.fun->args->next->monoType->val.var == x);
    UNPROTECT(save);

    disableGC();
    tpt = newTinPolyType(
        TINPOLYTYPE_TYPE_QUANTIFIER,
        TINPOLYTYPE_VAL_QUANTIFIER(
            newTinTypeQuantifier(
                x,
                newTinPolyType(
                    TINPOLYTYPE_TYPE_MONOTYPE,
                    TINPOLYTYPE_VAL_MONOTYPE(
                        newTinMonoType(
                            TINMONOTYPE_TYPE_FUN,
                            TINMONOTYPE_VAL_FUN(
                                newTinFunctionApplication(
                                    arrow,
                                    2,
                                    newTinMonoTypeList(
                                        newTinMonoType(
                                            TINMONOTYPE_TYPE_FUN,
                                            TINMONOTYPE_VAL_FUN(newTinFunctionApplication(boolean, 0, NULL))
                                        ),
                                        newTinMonoTypeList(
                                            newTinMonoType(
                                                TINMONOTYPE_TYPE_VAR,
                                                TINMONOTYPE_VAL_VAR(x)
                                            ),
                                            NULL
                                        )
                                    )
                                )
                            )
                        )
                    )
                )
            )
        )
    );
    save = PROTECT(tpt);
    enableGC();
    tmt1 = instantiate(tpt);
    EXPECT(tmt1->val.fun->name == arrow);
    EXPECT(tmt1->val.fun->args->monoType->val.fun->name == boolean);
    EXPECT(isTypeVariable(tmt1->val.fun->args->next->monoType->val.var));
    UNPROTECT(save);

    disableGC();
    htc = newContextTable();
    tc = newTinContext(htc, NULL);
    addToContext(tc, x, newTinPolyType(
        TINPOLYTYPE_TYPE_MONOTYPE,
        TINPOLYTYPE_VAL_MONOTYPE(
            newTinMonoType(
                TINMONOTYPE_TYPE_VAR,
                TINMONOTYPE_VAL_VAR(x)
            )
        )
    ));
    save = PROTECT(tc);
    tmt1 = newTinMonoType(
        TINMONOTYPE_TYPE_VAR,
        TINMONOTYPE_VAL_VAR(x)
    );
    PROTECT(tmt1);
    enableGC();
    tpt = generalize(tc, tmt1);
    EXPECT(tpt->type == TINPOLYTYPE_TYPE_MONOTYPE);
    EXPECT(tpt->val.monoType->val.var == x);
    UNPROTECT(save);

    disableGC();
    htc = newContextTable();
    tc = newTinContext(htc, NULL);
    save = PROTECT(tc);
    tmt1 = newTinMonoType(
        TINMONOTYPE_TYPE_VAR,
        TINMONOTYPE_VAL_VAR(x)
    );
    PROTECT(tmt1);
    enableGC();
    tpt = generalize(tc, tmt1);
    EXPECT(tpt->type == TINPOLYTYPE_TYPE_QUANTIFIER);
    EXPECT(tpt->val.quantifier->var == x);
    EXPECT(tpt->val.quantifier->quantifiedType->type == TINPOLYTYPE_TYPE_MONOTYPE);
    UNPROTECT(save);

    disableGC();
    htc = newContextTable();
    tc = newTinContext(htc, NULL);
    save = PROTECT(tc);
    tmt1 = newTinMonoType(
        TINMONOTYPE_TYPE_FUN,
        TINMONOTYPE_VAL_FUN(
            newTinFunctionApplication(
                arrow,
                2,
                newTinMonoTypeList(
                    newTinMonoType(
                        TINMONOTYPE_TYPE_VAR,
                        TINMONOTYPE_VAL_VAR(x)
                    ),
                    newTinMonoTypeList(
                        newTinMonoType(
                            TINMONOTYPE_TYPE_VAR,
                            TINMONOTYPE_VAL_VAR(x)
                        ),
                        NULL
                    )
                )
            )
        )
    );
    PROTECT(tmt1);
    enableGC();
    tpt = generalize(tc, tmt1);
    EXPECT(tpt->type == TINPOLYTYPE_TYPE_QUANTIFIER);
    EXPECT(tpt->val.quantifier->var == x);
    EXPECT(tpt->val.quantifier->quantifiedType->type == TINPOLYTYPE_TYPE_MONOTYPE);
    UNPROTECT(save);

    disableGC();
    tmt1 = newTinMonoType(
        TINMONOTYPE_TYPE_FUN,
        TINMONOTYPE_VAL_FUN(
            newTinFunctionApplication(
                arrow,
                2,
                newTinMonoTypeList(
                    newTinMonoType(
                        TINMONOTYPE_TYPE_FUN,
                        TINMONOTYPE_VAL_FUN(newTinFunctionApplication(boolean, 0, NULL))
                    ),
                    newTinMonoTypeList(
                        newTinMonoType(
                            TINMONOTYPE_TYPE_VAR,
                            TINMONOTYPE_VAL_VAR(x)
                        ),
                        NULL
                    )
                )
            )
        )
    );
    save = PROTECT(tmt1);
    tmt2 = newTinMonoType(
        TINMONOTYPE_TYPE_FUN,
        TINMONOTYPE_VAL_FUN(
            newTinFunctionApplication(
                arrow,
                2,
                newTinMonoTypeList(
                    newTinMonoType(TINMONOTYPE_TYPE_VAR, TINMONOTYPE_VAL_VAR(y)),
                    newTinMonoTypeList(
                        newTinMonoType(TINMONOTYPE_TYPE_FUN, TINMONOTYPE_VAL_FUN(newTinFunctionApplication(integer, 0, NULL))),
                        NULL
                    )
                )
            )
        )
    );
    PROTECT(tmt2);
    enableGC();
    C1 = unify(tmt1, tmt2);
    UNPROTECT(save);
    EXPECT(C1->map->count == 2);
    EXPECT(lookupInSubstitution(C1, x)->type = TINMONOTYPE_TYPE_FUN);
    EXPECT(lookupInSubstitution(C1, x)->val.fun->name = integer);
    EXPECT(lookupInSubstitution(C1, y)->type = TINMONOTYPE_TYPE_FUN);
    EXPECT(lookupInSubstitution(C1, x)->val.fun->name = boolean);

    disableGC();
    tmt1 = newTinMonoType(
        TINMONOTYPE_TYPE_FUN,
        TINMONOTYPE_VAL_FUN(
            newTinFunctionApplication(
                arrow,
                2,
                newTinMonoTypeList(
                    newTinMonoType(
                        TINMONOTYPE_TYPE_FUN,
                        TINMONOTYPE_VAL_FUN(newTinFunctionApplication(boolean, 0, NULL))
                    ),
                    newTinMonoTypeList(
                        newTinMonoType(
                            TINMONOTYPE_TYPE_VAR,
                            TINMONOTYPE_VAL_VAR(x)
                        ),
                        NULL
                    )
                )
            )
        )
    );
    save = PROTECT(tmt1);
    tmt2 = newTinMonoType(
        TINMONOTYPE_TYPE_FUN,
        TINMONOTYPE_VAL_FUN(
            newTinFunctionApplication(
                boolean,
                0,
                NULL
            )
        )
    );
    PROTECT(tmt2);
    enableGC();
    C1 = unify(tmt1, tmt2);
    UNPROTECT(save);
    EXPECT(hadErrors());

    disableGC();
    tmt1 = newTinMonoType(
        TINMONOTYPE_TYPE_FUN,
        TINMONOTYPE_VAL_FUN(
            newTinFunctionApplication(
                arrow,
                2,
                newTinMonoTypeList(
                    newTinMonoType(
                        TINMONOTYPE_TYPE_FUN,
                        TINMONOTYPE_VAL_FUN(newTinFunctionApplication(boolean, 0, NULL))
                    ),
                    newTinMonoTypeList(
                        newTinMonoType(
                            TINMONOTYPE_TYPE_VAR,
                            TINMONOTYPE_VAL_VAR(x)
                        ),
                        NULL
                    )
                )
            )
        )
    );
    save = PROTECT(tmt1);
    tmt2 = newTinMonoType(
        TINMONOTYPE_TYPE_VAR,
        TINMONOTYPE_VAL_VAR(x)
    );
    PROTECT(tmt2);
    enableGC();
    C1 = unify(tmt1, tmt2);
    UNPROTECT(save);
    EXPECT(hadErrors());

    UNPROTECT(totalSave);
}
#endif
#endif

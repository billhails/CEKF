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
static TinType *applyTypeSubstitution(TinSubstitution *s, TinType *ttype);
static TinContext *applyContextSubstitution(TinSubstitution *s, TinContext *context);
static TinSubstitution *applySubstitutionSubstitution(TinSubstitution *s1, TinSubstitution *s2);


HashSymbol *getTinSymbol(char *name) {
    return newSymbol(name);
}

void printTinSymbol(struct HashSymbol * x, int depth) {
    if (x == NULL) { printf("TinSymbol (NULL)"); return; }
    printf("TinSymbol[\"%s\"]", x->name);
}

static void printSubstitutionFn(void *ptr) {
    printTinMonoType(*(TinMonoType **)ptr, 0);
}

static void markSubstitutionFn(void *ptr) {
    markTinMonoType(*(TinMonoType **)ptr);
}

static void printContextFn(void *ptr) {
    printTinType(*(TinType **)ptr, 0);
}

static void markContextFn(void *ptr) {
    markTinType(*(TinType **)ptr);
}

HashTable *newTinSubstitutionTable() {
    return newHashTable(
        sizeof(TinMonoType*),
        markSubstitutionFn,
        printSubstitutionFn
    );
}

HashTable *newTinContextTable() {
    return newHashTable(
        sizeof(TinType*),
        markContextFn,
        printContextFn
    );
}

void addToSubstitution(TinSubstitution *substitution, HashSymbol *symbol, TinMonoType *monotype) {
    hashSet(substitution->map, symbol, &monotype);
}

void addToContext(TinContext *context, HashSymbol *symbol, TinType *type) {
    hashSet(context->frame, symbol, &type);
}

static TinContext *copyContext(TinContext *source) {
    if (source == NULL) {
        HashTable *table = newTinContextTable();
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
    HashTable *table = newTinSubstitutionTable();
    int save = PROTECT(table);
    TinSubstitution *ts = newTinSubstitution(table);
    PROTECT(ts);
    copyHashTable(ts->map, source->map);
    UNPROTECT(save);
    return ts;
}

static TinMonoTypeList *mapMonoTypeList(TinSubstitution *s, TinMonoTypeList *args) {
    if (args == NULL) return NULL;
    TinMonoTypeList *soFar = mapMonoTypeList(s, args->next);
    int save = PROTECT(soFar);
    TinMonoType *t = applyMonoTypeSubstitution(s, args->monoType);
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
    TinMonoTypeList *args = mapMonoTypeList(s, funApp->args);
    int save = PROTECT(args);
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
    TinMonoType *replacement = NULL;
    if (hashGet(s->map, mtype->val.var, &replacement)) return replacement;
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
            int save = PROTECT(tmt);
            TinPolyType *pt = newTinPolyType(TINPOLYTYPE_TYPE_MONOTYPE, TINPOLYTYPE_VAL_MONOTYPE(tmt));
            UNPROTECT(save);
            return pt;
        }
        case TINPOLYTYPE_TYPE_QUANTIFIER: {
            TinTypeQuantifier *ttq = applyQuantifierSubstitution(s, ptype->val.quantifier);
            int save = PROTECT(ttq);
            TinPolyType *pt = newTinPolyType(TINPOLYTYPE_TYPE_QUANTIFIER, TINPOLYTYPE_VAL_QUANTIFIER(ttq));
            UNPROTECT(save);
            return pt;
        }
        default:
            cant_happen("unrecognised type %d in applyPolyTypeSubstitution", ptype->type);
    }
}

static TinType *applyTypeSubstitution(TinSubstitution *s, TinType *ttype) {
#ifdef DEBUG_TIN_SUBSTITUTION
    printf("applyTypeSubstitution ");
    printTinType(ttype, 0);
    printf("\n");
    sleep(1);
#endif
    switch (ttype->type) {
        case TINTYPE_TYPE_MONOTYPE: {
            TinMonoType *tmt = applyMonoTypeSubstitution(s, ttype->val.monoType);
            int save = PROTECT(tmt);
            TinType *tt = newTinType(TINTYPE_TYPE_MONOTYPE, TINTYPE_VAL_MONOTYPE(tmt));
            UNPROTECT(save);
            return tt;
        }
        case TINTYPE_TYPE_POLYTYPE: {
            TinPolyType *tpt = applyPolyTypeSubstitution(s, ttype->val.polyType);
            int save = PROTECT(tpt);
            TinType *tt = newTinType(TINTYPE_TYPE_POLYTYPE, TINTYPE_VAL_POLYTYPE(tpt));
            UNPROTECT(save);
            return tt;
        }
        default:
            cant_happen("unrecognised type %d in applyTypeSubstitution", ttype->type);
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
    TinType *t = NULL;
    HashSymbol *key;
    while ((key = iterateHashTable(result->frame, &i, &t)) != NULL) {
        TinType *ts = applyTypeSubstitution(s, t);
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

TinSubstitutionCurrency *applySubstitution(TinSubstitution *s, TinSubstitutionCurrency *arg) {
#ifdef DEBUG_TIN_SUBSTITUTION
    printf("applySubstitution ");
    printTinSubstitutionCurrency(arg, 0);
    printf("\n");
    sleep(1);
#endif
    switch (arg->type) {
        case TINSUBSTITUTIONCURRENCY_TYPE_TYPE: {
            TinType *tt = applyTypeSubstitution(s, arg->val.type);
            int save = PROTECT(tt);
            TinSubstitutionCurrency * tsc = newTinSubstitutionCurrency(TINSUBSTITUTIONCURRENCY_TYPE_TYPE, TINSUBSTITUTIONCURRENCY_VAL_TYPE(tt));
            UNPROTECT(save);
            return tsc;
        }
        case TINSUBSTITUTIONCURRENCY_TYPE_CONTEXT: {
            TinContext *tc = applyContextSubstitution(s, arg->val.context);
            int save = PROTECT(tc);
            TinSubstitutionCurrency * tsc = newTinSubstitutionCurrency(TINSUBSTITUTIONCURRENCY_TYPE_CONTEXT, TINSUBSTITUTIONCURRENCY_VAL_CONTEXT(tc));
            UNPROTECT(save);
            return tsc;
        }
        case TINSUBSTITUTIONCURRENCY_TYPE_SUBSTITUTION: {
            TinSubstitution *ts = applySubstitutionSubstitution(s, arg->val.substitution);
            int save = PROTECT(ts);
            TinSubstitutionCurrency * tsc = newTinSubstitutionCurrency(TINSUBSTITUTIONCURRENCY_TYPE_SUBSTITUTION, TINSUBSTITUTIONCURRENCY_VAL_SUBSTITUTION(ts));
            UNPROTECT(save);
            return tsc;
        }
        default:
            cant_happen("unrecognised type %d in applySubstitution", arg->type);
    }
}

#ifdef DEBUG_RUN_TESTS
#if DEBUG_RUN_TESTS == 3
void testTin() {
    printf("testTin()\n");
    int save;
    TinSubstitutionCurrency *tsc;
    TinSubstitution *ts;
    TinSubstitutionCurrency *result;
    HashSymbol *x = newSymbol("x"); PROTECT(x);
    HashSymbol *y = newSymbol("y"); PROTECT(y);
    HashSymbol *z = newSymbol("z"); PROTECT(y);
    HashSymbol *a = newSymbol("->"); PROTECT(a);
    HashSymbol *b = newSymbol("Bool"); PROTECT(b);

    printf("\nt### type-monotype-var\n\n");

    disableGC();
    tsc = newTinSubstitutionCurrency(
        TINSUBSTITUTIONCURRENCY_TYPE_TYPE,
        TINSUBSTITUTIONCURRENCY_VAL_TYPE(
            newTinType(
                TINTYPE_TYPE_MONOTYPE,
                TINTYPE_VAL_MONOTYPE(
                    newTinMonoType(
                        TINMONOTYPE_TYPE_VAR,
                        TINMONOTYPE_VAL_VAR(x)
                    )
                )
            )
        )
    );
    ts = newTinSubstitution(newTinSubstitutionTable());
    addToSubstitution(ts, x, newTinMonoType(TINMONOTYPE_TYPE_VAR, TINMONOTYPE_VAL_VAR(y)));
    save = PROTECT(tsc);
    PROTECT(ts);
    enableGC();

    printTinSubstitution(ts, 0);
    printf("\n");
    printTinSubstitutionCurrency(tsc, 0);
    printf("\n");
    result = applySubstitution(ts, tsc);
    PROTECT(result);
    printTinSubstitutionCurrency(result, 0);
    printf("\n");
    UNPROTECT(save);

    printf("\n### type-monotype-fun\n\n");

    disableGC();
    tsc = newTinSubstitutionCurrency(
        TINSUBSTITUTIONCURRENCY_TYPE_TYPE,
        TINSUBSTITUTIONCURRENCY_VAL_TYPE(
            newTinType(
                TINTYPE_TYPE_MONOTYPE,
                TINTYPE_VAL_MONOTYPE(
                    newTinMonoType(
                        TINMONOTYPE_TYPE_FUN,
                        TINMONOTYPE_VAL_FUN(
                            newTinFunctionApplication(
                                a,
                                2,
                                newTinMonoTypeList(
                                    newTinMonoType(
                                        TINMONOTYPE_TYPE_FUN,
                                        TINMONOTYPE_VAL_FUN(newTinFunctionApplication(b, 0, NULL))
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
    );
    ts = newTinSubstitution(newTinSubstitutionTable());
    addToSubstitution(ts, x, newTinMonoType(TINMONOTYPE_TYPE_VAR, TINMONOTYPE_VAL_VAR(y)));
    save = PROTECT(tsc);
    PROTECT(ts);
    enableGC();

    printTinSubstitution(ts, 0);
    printf("\n");
    printTinSubstitutionCurrency(tsc, 0);
    printf("\n");
    result = applySubstitution(ts, tsc);
    PROTECT(result);
    printTinSubstitutionCurrency(result, 0);
    printf("\n");
    UNPROTECT(save);

    printf("\n### type-polytype-monotype-fun\n\n");

    disableGC();
    tsc = newTinSubstitutionCurrency(
        TINSUBSTITUTIONCURRENCY_TYPE_TYPE,
        TINSUBSTITUTIONCURRENCY_VAL_TYPE(
            newTinType(
                TINTYPE_TYPE_POLYTYPE,
                TINTYPE_VAL_POLYTYPE(
                    newTinPolyType(
                        TINPOLYTYPE_TYPE_MONOTYPE,
                        TINPOLYTYPE_VAL_MONOTYPE(
                            newTinMonoType(
                                TINMONOTYPE_TYPE_FUN,
                                TINMONOTYPE_VAL_FUN(
                                    newTinFunctionApplication(
                                        a,
                                        2,
                                        newTinMonoTypeList(
                                            newTinMonoType(
                                                TINMONOTYPE_TYPE_FUN,
                                                TINMONOTYPE_VAL_FUN(newTinFunctionApplication(b, 0, NULL))
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
        )
    );
    ts = newTinSubstitution(newTinSubstitutionTable());
    addToSubstitution(ts, x, newTinMonoType(TINMONOTYPE_TYPE_VAR, TINMONOTYPE_VAL_VAR(y)));
    save = PROTECT(tsc);
    PROTECT(ts);
    enableGC();

    printTinSubstitution(ts, 0);
    printf("\n");
    printTinSubstitutionCurrency(tsc, 0);
    printf("\n");
    result = applySubstitution(ts, tsc);
    PROTECT(result);
    printTinSubstitutionCurrency(result, 0);
    printf("\n");
    UNPROTECT(save);

    printf("\n### type-polytype-quantifier\n\n");

    disableGC();
    tsc = newTinSubstitutionCurrency(
        TINSUBSTITUTIONCURRENCY_TYPE_TYPE,
        TINSUBSTITUTIONCURRENCY_VAL_TYPE(
            newTinType(
                TINTYPE_TYPE_POLYTYPE,
                TINTYPE_VAL_POLYTYPE(
                    newTinPolyType(
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
                    )
                )
            )
        )
    );
    ts = newTinSubstitution(newTinSubstitutionTable());
    addToSubstitution(ts, x, newTinMonoType(TINMONOTYPE_TYPE_VAR, TINMONOTYPE_VAL_VAR(y)));
    save = PROTECT(tsc);
    PROTECT(ts);
    enableGC();

    printTinSubstitution(ts, 0);
    printf("\n");
    printTinSubstitutionCurrency(tsc, 0);
    printf("\n");
    result = applySubstitution(ts, tsc);
    PROTECT(result);
    printTinSubstitutionCurrency(result, 0);
    printf("\n");
    UNPROTECT(save);

    printf("\n### type-context\n\n");

    disableGC();
    HashTable *t = newTinContextTable();
    TinContext *tc = newTinContext(t, NULL);
    addToContext(tc, x, newTinType(
        TINTYPE_TYPE_MONOTYPE,
        TINTYPE_VAL_MONOTYPE(
            newTinMonoType(
                TINMONOTYPE_TYPE_VAR,
                TINMONOTYPE_VAL_VAR(x)
            )
        )
    ));
    tsc = newTinSubstitutionCurrency(
        TINSUBSTITUTIONCURRENCY_TYPE_CONTEXT,
        TINSUBSTITUTIONCURRENCY_VAL_CONTEXT(tc)
    );
    ts = newTinSubstitution(newTinSubstitutionTable());
    addToSubstitution(ts, x, newTinMonoType(TINMONOTYPE_TYPE_VAR, TINMONOTYPE_VAL_VAR(y)));
    save = PROTECT(tsc);
    PROTECT(ts);
    enableGC();

    printTinSubstitution(ts, 0);
    printf("\n");
    printTinSubstitutionCurrency(tsc, 0);
    printf("\n");
    result = applySubstitution(ts, tsc);
    PROTECT(result);
    printTinSubstitutionCurrency(result, 0);
    printf("\n");
    UNPROTECT(save);

    printf("\n### type-substitution-1\n\n");

    // C1 = { x |-> y }
    // C2 = { y |-> z }
    // C1(C2) =  { x |-> y, y |-> z }
    // x | x | y
    // y | z | z

    disableGC();
    TinSubstitution *C1 = newTinSubstitution(newTinSubstitutionTable());
    addToSubstitution(C1, x, newTinMonoType(TINMONOTYPE_TYPE_VAR, TINMONOTYPE_VAL_VAR(y)));
    TinSubstitution *C2 = newTinSubstitution(newTinSubstitutionTable());
    addToSubstitution(C2, y, newTinMonoType(TINMONOTYPE_TYPE_VAR, TINMONOTYPE_VAL_VAR(z)));
    tsc = newTinSubstitutionCurrency(
        TINSUBSTITUTIONCURRENCY_TYPE_SUBSTITUTION,
        TINSUBSTITUTIONCURRENCY_VAL_SUBSTITUTION(C2)
    );
    save = PROTECT(tsc);
    PROTECT(C1);

    printTinSubstitution(C1, 0);
    printf("\n");
    printTinSubstitution(C2, 0);
    printf("\n");
    result = applySubstitution(C1, tsc);
    PROTECT(result);
    printTinSubstitutionCurrency(result, 0);
    printf("\n");
    UNPROTECT(save);

    printf("\n### type-substitution-2\n\n");

    // C1 = { y |-> z }
    // C2 = { x |-> y }
    // C1(C2) =  { x |-> z, y |-> z }
    // x | y | z
    // y | y | z

    disableGC();
    C1 = newTinSubstitution(newTinSubstitutionTable());
    addToSubstitution(C1, y, newTinMonoType(TINMONOTYPE_TYPE_VAR, TINMONOTYPE_VAL_VAR(z)));
    C2 = newTinSubstitution(newTinSubstitutionTable());
    addToSubstitution(C2, x, newTinMonoType(TINMONOTYPE_TYPE_VAR, TINMONOTYPE_VAL_VAR(y)));
    tsc = newTinSubstitutionCurrency(
        TINSUBSTITUTIONCURRENCY_TYPE_SUBSTITUTION,
        TINSUBSTITUTIONCURRENCY_VAL_SUBSTITUTION(C2)
    );
    save = PROTECT(tsc);
    PROTECT(C1);

    printTinSubstitution(C1, 0);
    printf("\n");
    printTinSubstitution(C2, 0);
    printf("\n");
    result = applySubstitution(C1, tsc);
    PROTECT(result);
    printTinSubstitutionCurrency(result, 0);
    printf("\n");
    UNPROTECT(save);

}
#endif
#endif

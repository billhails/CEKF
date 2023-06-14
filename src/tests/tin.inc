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
    save = PROTECT(tmt1);
    tmt2 = newTinMonoType(
        TINMONOTYPE_TYPE_FUN,
        TINMONOTYPE_VAL_FUN(
            newTinFunctionApplication(
                arrow,
                2,
                newTinMonoTypeList(
                    newTinMonoType(TINMONOTYPE_TYPE_FUN, TINMONOTYPE_VAL_FUN(newTinFunctionApplication(integer, 0, NULL))),
                    newTinMonoTypeList(
                        newTinMonoType(TINMONOTYPE_TYPE_FUN, TINMONOTYPE_VAL_FUN(newTinFunctionApplication(boolean, 0, NULL))),
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

// vi: ft=c

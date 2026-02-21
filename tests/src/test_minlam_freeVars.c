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

#include <assert.h>
#include <stdio.h>

#include "bigint.h"
#include "common.h"
#include "init.h"
#include "minlam_freeVars.h"
#include "symbol.h"
#include "utils.h"

#ifdef DEBUG_STRESS_GC
extern int forceGcFlag;
#endif

typedef void (*TestFn)(void);

static void runTest(char *name, TestFn testFn) {
    printf("%s\n", name);
    testFn();
}

static MinExp *V(char *name) {
    MinExp *result = newMinExp_Var(NULLPI, newSymbol(name));
    PROTECT(result);
    return result;
}

static MinExp *Add(MinExp *left, MinExp *right) {
    MinExp *result = makeMinExp_Prim(NULLPI, MINPRIMOP_TYPE_ADD, left, right);
    PROTECT(result);
    return result;
}

static MinExp *Amb(MinExp *left, MinExp *right) {
    MinExp *result = makeMinExp_Amb(NULLPI, left, right);
    PROTECT(result);
    return result;
}

static MinExprList *ExprList(MinExp *exp, MinExprList *next) {
    MinExprList *result = newMinExprList(NULLPI, exp, next);
    PROTECT(result);
    return result;
}

static MinExp *Apply(MinExp *function, MinExprList *args) {
    MinExp *result = makeMinExp_Apply(NULLPI, function, args);
    PROTECT(result);
    return result;
}

static MinExp *LookUp(Integer nsId, MinExp *exp) {
    MinExp *result = makeMinExp_LookUp(NULLPI, nsId, exp);
    PROTECT(result);
    return result;
}

static MinExp *Iff(MinExp *condition, MinExp *consequent, MinExp *alternative) {
    MinExp *result = makeMinExp_Iff(NULLPI, condition, consequent, alternative);
    PROTECT(result);
    return result;
}

static MaybeBigInt *MBI(int n) {
    MaybeBigInt *result = fakeBigInt(n, false);
    PROTECT(result);
    return result;
}

static MinExp *Big(int n) {
    MinExp *result = newMinExp_BigInteger(NULLPI, MBI(n));
    PROTECT(result);
    return result;
}

static MinExp *Std(int n) {
    MinExp *result = newMinExp_Stdint(NULLPI, n);
    PROTECT(result);
    return result;
}

static MinExp *Chr(Character c) {
    MinExp *result = newMinExp_Character(NULLPI, c);
    PROTECT(result);
    return result;
}

static MinExp *CallCC(MinExp *exp) {
    MinExp *result = newMinExp_CallCC(NULLPI, exp);
    PROTECT(result);
    return result;
}

static MinExp *NameSpaces(void) {
    MinExp *result = makeMinExp_NameSpaces(NULLPI);
    PROTECT(result);
    return result;
}

static SymbolList *Args1(char *name) {
    SymbolList *result = newSymbolList(NULLPI, newSymbol(name), NULL);
    PROTECT(result);
    return result;
}

static MinBindings *Bind(char *varName, MinExp *val, MinBindings *next) {
    MinBindings *result = newMinBindings(NULLPI, newSymbol(varName), val, next);
    PROTECT(result);
    return result;
}

static MinExp *Lam(SymbolList *args, MinExp *body) {
    MinExp *result = makeMinExp_Lam(NULLPI, args, body);
    PROTECT(result);
    return result;
}

static MinExp *LetRec(MinBindings *bindings, MinExp *body) {
    MinExp *result = makeMinExp_LetRec(NULLPI, bindings, body);
    PROTECT(result);
    return result;
}

static MinExp *MakeVec(MinExp *first, MinExprList *rest) {
    MinExp *result = makeMinExp_MakeVec(NULLPI, first, rest);
    PROTECT(result);
    return result;
}

static MinExp *Sequence(MinExp *first, MinExprList *rest) {
    MinExp *result = makeMinExp_Sequence(NULLPI, first, rest);
    PROTECT(result);
    return result;
}

static MinIntList *Ints1(Integer item) {
    MinIntList *result = newMinIntList(NULLPI, item, NULL);
    PROTECT(result);
    return result;
}

static MinMatchList *MatchCase(Integer tag, MinExp *body, MinMatchList *next) {
    MinIntList *matches = Ints1(tag);
    int save = PROTECT(matches);
    MinMatchList *result = newMinMatchList(NULLPI, matches, body, next);
    UNPROTECT(save);
    PROTECT(result);
    return result;
}

static MinExp *Match(MinExp *index, MinMatchList *cases) {
    MinExp *result = makeMinExp_Match(NULLPI, index, cases);
    PROTECT(result);
    return result;
}

static MinCondCases *IntCases(int k, MinExp *body, MinIntCondCases *next) {
    MinCondCases *result =
        makeMinCondCases_Integers(NULLPI, MBI(k), body, next);
    PROTECT(result);
    return result;
}

static MinCondCases *CharCases(Character c, MinExp *body,
                               MinCharCondCases *next) {
    MinCondCases *result = makeMinCondCases_Characters(NULLPI, c, body, next);
    PROTECT(result);
    return result;
}

static MinIntCondCases *IntCaseNode(int k, MinExp *body,
                                    MinIntCondCases *next) {
    MinIntCondCases *result = newMinIntCondCases(NULLPI, MBI(k), body, next);
    PROTECT(result);
    return result;
}

static MinCharCondCases *CharCaseNode(Character c, MinExp *body,
                                      MinCharCondCases *next) {
    MinCharCondCases *result = newMinCharCondCases(NULLPI, c, body, next);
    PROTECT(result);
    return result;
}

static MinExp *Cond(MinExp *value, MinCondCases *cases) {
    MinExp *result = makeMinExp_Cond(NULLPI, value, cases);
    PROTECT(result);
    return result;
}

static void assertSetContainsOnly(SymbolSet *set, HashSymbol *a, HashSymbol *b,
                                  HashSymbol *c) {
    Index expected = 0;
    if (a != NULL) {
        expected++;
        assert(getSymbolSet(set, a));
    }
    if (b != NULL) {
        expected++;
        assert(getSymbolSet(set, b));
    }
    if (c != NULL) {
        expected++;
        assert(getSymbolSet(set, c));
    }
    assert(countSymbolSet(set) == expected);
}

static void test_var_unbound_is_free(void) {
    int save = PROTECT(NULL);
    HashSymbol *x = newSymbol("x");
    MinExp *expr = V("x");
    SymbolSet *freeVars = newSymbolSet();
    PROTECT(freeVars);

    freeVarsMinExp(expr, freeVars, NULL);

    assertSetContainsOnly(freeVars, x, NULL, NULL);
    UNPROTECT(save);
}

static void test_var_bound_in_context_is_not_free(void) {
    int save = PROTECT(NULL);
    HashSymbol *x = newSymbol("x");
    MinExp *expr = V("x");
    SymbolSet *freeVars = newSymbolSet();
    PROTECT(freeVars);
    SymbolEnv *env = newSymbolEnv(NULL);
    PROTECT(env);
    setSymbolSet(env->bindings, x);

    freeVarsMinExp(expr, freeVars, env);

    assert(countSymbolSet(freeVars) == 0);
    UNPROTECT(save);
}

static void test_add_collects_both_sides(void) {
    int save = PROTECT(NULL);
    HashSymbol *x = newSymbol("x");
    HashSymbol *y = newSymbol("y");
    MinExp *expr = Add(V("x"), V("y"));
    SymbolSet *freeVars = newSymbolSet();
    PROTECT(freeVars);

    freeVarsMinExp(expr, freeVars, NULL);

    assertSetContainsOnly(freeVars, x, y, NULL);
    UNPROTECT(save);
}

static void test_lambda_binds_parameter(void) {
    int save = PROTECT(NULL);
    HashSymbol *y = newSymbol("y");
    MinExp *expr = Lam(Args1("x"), Add(V("x"), V("y")));
    SymbolSet *freeVars = newSymbolSet();
    PROTECT(freeVars);

    freeVarsMinExp(expr, freeVars, NULL);

    assertSetContainsOnly(freeVars, y, NULL, NULL);
    UNPROTECT(save);
}

static void test_letrec_binds_binding_names_in_values_and_body(void) {
    int save = PROTECT(NULL);
    HashSymbol *y = newSymbol("y");
    HashSymbol *z = newSymbol("z");

    MinBindings *bindings = Bind("x", V("y"), NULL);
    MinExp *body = Add(V("x"), V("z"));
    MinExp *expr = LetRec(bindings, body);

    SymbolSet *freeVars = newSymbolSet();
    PROTECT(freeVars);
    freeVarsMinExp(expr, freeVars, NULL);

    assertSetContainsOnly(freeVars, y, z, NULL);
    UNPROTECT(save);
}

static void test_letrec_binding_value_respects_lambda_bound_vars(void) {
    int save = PROTECT(NULL);
    HashSymbol *z = newSymbol("z");

    MinBindings *bindings =
        Bind("f", Lam(Args1("y"), Add(V("y"), V("z"))), NULL);
    MinExp *expr = LetRec(bindings, V("f"));

    SymbolSet *freeVars = newSymbolSet();
    PROTECT(freeVars);
    freeVarsMinExp(expr, freeVars, NULL);

    assertSetContainsOnly(freeVars, z, NULL, NULL);
    UNPROTECT(save);
}

static void test_apply_collects_function_and_args(void) {
    int save = PROTECT(NULL);
    HashSymbol *f = newSymbol("f");
    HashSymbol *x = newSymbol("x");
    HashSymbol *y = newSymbol("y");

    MinExprList *args = ExprList(V("x"), ExprList(V("y"), NULL));
    MinExp *expr = Apply(V("f"), args);

    SymbolSet *freeVars = newSymbolSet();
    PROTECT(freeVars);
    freeVarsMinExp(expr, freeVars, NULL);

    assert(getSymbolSet(freeVars, f));
    assert(getSymbolSet(freeVars, x));
    assert(getSymbolSet(freeVars, y));
    assert(countSymbolSet(freeVars) == 3);
    UNPROTECT(save);
}

static void test_lookup_iff_amb_collect_free_vars(void) {
    int save = PROTECT(NULL);
    HashSymbol *x = newSymbol("x");
    HashSymbol *c = newSymbol("c");
    HashSymbol *t = newSymbol("t");
    HashSymbol *e = newSymbol("e");
    HashSymbol *l = newSymbol("l");
    HashSymbol *r = newSymbol("r");

    SymbolSet *s1 = newSymbolSet();
    PROTECT(s1);
    freeVarsMinExp(LookUp(0, V("x")), s1, NULL);
    assertSetContainsOnly(s1, x, NULL, NULL);

    SymbolSet *s2 = newSymbolSet();
    PROTECT(s2);
    freeVarsMinExp(Iff(V("c"), V("t"), V("e")), s2, NULL);
    assert(getSymbolSet(s2, c));
    assert(getSymbolSet(s2, t));
    assert(getSymbolSet(s2, e));
    assert(countSymbolSet(s2) == 3);

    SymbolSet *s3 = newSymbolSet();
    PROTECT(s3);
    freeVarsMinExp(Amb(V("l"), V("r")), s3, NULL);
    assert(getSymbolSet(s3, l));
    assert(getSymbolSet(s3, r));
    assert(countSymbolSet(s3) == 2);
    UNPROTECT(save);
}

static void test_cond_integer_and_character_cases(void) {
    int save = PROTECT(NULL);
    HashSymbol *v = newSymbol("v");
    HashSymbol *x = newSymbol("x");
    HashSymbol *z = newSymbol("z");
    HashSymbol *p = newSymbol("p");
    HashSymbol *q = newSymbol("q");

    MinIntCondCases *intNext = IntCaseNode(1, V("z"), NULL);
    MinExp *intCond = Cond(V("v"), IntCases(0, V("x"), intNext));
    SymbolSet *s1 = newSymbolSet();
    PROTECT(s1);
    freeVarsMinExp(intCond, s1, NULL);
    assert(getSymbolSet(s1, v));
    assert(getSymbolSet(s1, x));
    assert(getSymbolSet(s1, z));
    assert(countSymbolSet(s1) == 3);

    MinCharCondCases *charNext = CharCaseNode('b', V("q"), NULL);
    MinExp *charCond = Cond(V("p"), CharCases('a', V("q"), charNext));
    SymbolSet *s2 = newSymbolSet();
    PROTECT(s2);
    freeVarsMinExp(charCond, s2, NULL);
    assert(getSymbolSet(s2, p));
    assert(getSymbolSet(s2, q));
    assert(countSymbolSet(s2) == 2);
    UNPROTECT(save);
}

static void test_match_sequence_makevec_and_callcc(void) {
    int save = PROTECT(NULL);
    HashSymbol *i = newSymbol("i");
    HashSymbol *a = newSymbol("a");
    HashSymbol *b = newSymbol("b");
    HashSymbol *s1Sym = newSymbol("s1");
    HashSymbol *s2Sym = newSymbol("s2");
    HashSymbol *m1 = newSymbol("m1");
    HashSymbol *m2 = newSymbol("m2");
    HashSymbol *k = newSymbol("k");

    MinMatchList *cases = MatchCase(0, V("a"), MatchCase(1, V("b"), NULL));
    SymbolSet *matchSet = newSymbolSet();
    PROTECT(matchSet);
    freeVarsMinExp(Match(V("i"), cases), matchSet, NULL);
    assert(getSymbolSet(matchSet, i));
    assert(getSymbolSet(matchSet, a));
    assert(getSymbolSet(matchSet, b));
    assert(countSymbolSet(matchSet) == 3);

    SymbolSet *seqSet = newSymbolSet();
    PROTECT(seqSet);
    freeVarsMinExp(Sequence(V("s1"), ExprList(V("s2"), NULL)), seqSet, NULL);
    assert(getSymbolSet(seqSet, s1Sym));
    assert(getSymbolSet(seqSet, s2Sym));
    assert(countSymbolSet(seqSet) == 2);

    SymbolSet *vecSet = newSymbolSet();
    PROTECT(vecSet);
    freeVarsMinExp(MakeVec(V("m1"), ExprList(V("m2"), NULL)), vecSet, NULL);
    assert(getSymbolSet(vecSet, m1));
    assert(getSymbolSet(vecSet, m2));
    assert(countSymbolSet(vecSet) == 2);

    SymbolSet *ccSet = newSymbolSet();
    PROTECT(ccSet);
    freeVarsMinExp(CallCC(V("k")), ccSet, NULL);
    assertSetContainsOnly(ccSet, k, NULL, NULL);
    UNPROTECT(save);
}

static void test_non_variable_forms_do_not_add_free_vars(void) {
    int save = PROTECT(NULL);

    SymbolSet *s1 = newSymbolSet();
    PROTECT(s1);
    freeVarsMinExp(Std(7), s1, NULL);
    assert(countSymbolSet(s1) == 0);

    SymbolSet *s2 = newSymbolSet();
    PROTECT(s2);
    freeVarsMinExp(Chr('x'), s2, NULL);
    assert(countSymbolSet(s2) == 0);

    SymbolSet *s3 = newSymbolSet();
    PROTECT(s3);
    freeVarsMinExp(Big(11), s3, NULL);
    assert(countSymbolSet(s3) == 0);

    SymbolSet *s4 = newSymbolSet();
    PROTECT(s4);
    freeVarsMinExp(NameSpaces(), s4, NULL);
    assert(countSymbolSet(s4) == 0);
    UNPROTECT(save);
}

int main(int argc __attribute__((unused)),
         char *argv[] __attribute__((unused))) {
    initAll();
#ifdef DEBUG_STRESS_GC
    forceGcFlag = 1;
#endif

    runTest("test_var_unbound_is_free", test_var_unbound_is_free);
    runTest("test_var_bound_in_context_is_not_free",
            test_var_bound_in_context_is_not_free);
    runTest("test_add_collects_both_sides", test_add_collects_both_sides);
    runTest("test_lambda_binds_parameter", test_lambda_binds_parameter);
    runTest("test_letrec_binds_binding_names_in_values_and_body",
            test_letrec_binds_binding_names_in_values_and_body);
    runTest("test_letrec_binding_value_respects_lambda_bound_vars",
            test_letrec_binding_value_respects_lambda_bound_vars);
    runTest("test_apply_collects_function_and_args",
            test_apply_collects_function_and_args);
    runTest("test_lookup_iff_amb_collect_free_vars",
            test_lookup_iff_amb_collect_free_vars);
    runTest("test_cond_integer_and_character_cases",
            test_cond_integer_and_character_cases);
    runTest("test_match_sequence_makevec_and_callcc",
            test_match_sequence_makevec_and_callcc);
    runTest("test_non_variable_forms_do_not_add_free_vars",
            test_non_variable_forms_do_not_add_free_vars);

    return 0;
}

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

#include "arithmetic_next.h"
#include "bigint.h"
#include "common.h"
#include "init.h"
#include "minlam_pp.h"
#include "minlam_simplify.h"
#include "symbol.h"
#include "term_helper.h"

#ifdef DEBUG_STRESS_GC
extern int forceGcFlag;
#endif

typedef void (*TestFn)(void);

static void runTest(char *name, TestFn testFn) {
    printf("%s\n", name);
    testFn();
}

static MinExp *smallIntExp(int n) {
    MaybeBigInt *mbi = fakeBigInt(n, false);
    int save = PROTECT(mbi);
    MinExp *exp = newMinExp_BigInteger(NULLPI, mbi);
    UNPROTECT(save);
    return exp;
}

static MinExp *prim2(MinPrimOp op, MinExp *left, MinExp *right) {
    int save = PROTECT(left);
    PROTECT(right);
    MinExp *result = makeMinExp_Prim(NULLPI, op, left, right);
    UNPROTECT(save);
    return result;
}

static MinExp *prim2SmallInts(MinPrimOp op, int left, int right) {
    MinExp *leftExp = smallIntExp(left);
    int save = PROTECT(leftExp);
    MinExp *rightExp = smallIntExp(right);
    PROTECT(rightExp);
    MinExp *result = prim2(op, leftExp, rightExp);
    UNPROTECT(save);
    return result;
}

static MinExp *N(int n) {
    MinExp *result = smallIntExp(n);
    PROTECT(result);
    return result;
}

static MinExp *V(char *name) {
    MinExp *result = newMinExp_Var(NULLPI, newSymbol(name));
    PROTECT(result);
    return result;
}

static MinExp *Vx(void) { return V("x"); }

static MinExp *Vy(void) { return V("y"); }

static MinExp *Add(MinExp *left, MinExp *right) {
    MinExp *result = prim2(MINPRIMOP_TYPE_ADD, left, right);
    PROTECT(result);
    return result;
}

static MinExp *Sub(MinExp *left, MinExp *right) {
    MinExp *result = prim2(MINPRIMOP_TYPE_SUB, left, right);
    PROTECT(result);
    return result;
}

static MinExp *Mul(MinExp *left, MinExp *right) {
    MinExp *result = prim2(MINPRIMOP_TYPE_MUL, left, right);
    PROTECT(result);
    return result;
}

static MinExp *Div(MinExp *left, MinExp *right) {
    MinExp *result = prim2(MINPRIMOP_TYPE_DIV, left, right);
    PROTECT(result);
    return result;
}

static MinExp *Mod(MinExp *left, MinExp *right) {
    MinExp *result = prim2(MINPRIMOP_TYPE_MOD, left, right);
    PROTECT(result);
    return result;
}

static MinExp *Pow(MinExp *left, MinExp *right) {
    MinExp *result = prim2(MINPRIMOP_TYPE_POW, left, right);
    PROTECT(result);
    return result;
}

static MinExp *Gcd(MinExp *left, MinExp *right) {
    MinExp *result = prim2(MINPRIMOP_TYPE_GCD, left, right);
    PROTECT(result);
    return result;
}

static MinExp *Lcm(MinExp *left, MinExp *right) {
    MinExp *result = prim2(MINPRIMOP_TYPE_LCM, left, right);
    PROTECT(result);
    return result;
}

static MinExp *Canon(MinExp *value) {
    MinExp *zero = N(0);
    int save = PROTECT(zero);
    MinExp *result = prim2(MINPRIMOP_TYPE_CANON, value, zero);
    REPLACE_PROTECT(save, result);
    return result;
}

static void assertSimplifiesToInt(MinExp *expr, int expected) {
    int save = PROTECT(expr);
    MinExp *simplified = simplifyMinExp(expr);
    PROTECT(simplified);
    Term *asTerm = minExpToTerm(simplified);
    PROTECT(asTerm);
    assert(asTerm->type == TERM_TYPE_NUM);
    Value result = getTerm_Num(asTerm)->value;
    assert(ncmp(result, value_Stdint(expected)) == CMP_EQ);
    UNPROTECT(save);
}

static void assertSimplifiesToPrimVars(MinExp *expr, MinPrimOp op,
                                       HashSymbol *left, HashSymbol *right) {
    int save = PROTECT(expr);
    MinExp *simplified = simplifyMinExp(expr);
    PROTECT(simplified);
    assert(isMinExp_Prim(simplified));
    MinPrimApp *prim = getMinExp_Prim(simplified);
    assert(prim->type == op);
    assert(isMinExp_Var(prim->exp1));
    assert(isMinExp_Var(prim->exp2));
    assert(getMinExp_Var(prim->exp1) == left);
    assert(getMinExp_Var(prim->exp2) == right);
    UNPROTECT(save);
}

// Compare in Term space for semantic equality; MinExp node shape can differ
// for equivalent expressions, so eqMinExp is too representation-sensitive.
static void assertSimplifiesToExpr(MinExp *expr, MinExp *expected) {
    int save = PROTECT(expr);
    PROTECT(expected);
    MinExp *simplified = simplifyMinExp(expr);
    PROTECT(simplified);
    Term *simplifiedTerm = minExpToTerm(simplified);
    PROTECT(simplifiedTerm);
    Term *expectedTerm = minExpToTerm(expected);
    PROTECT(expectedTerm);
    bool matches = eqTerm(simplifiedTerm, expectedTerm);
    if (!matches) {
        eprintf("assertSimplifiesToExpr mismatch\n");
        eprintf("  expr:       ");
        ppMinExp(expr);
        eprintf("\n  simplified: ");
        ppMinExp(simplified);
        eprintf("\n  expected:   ");
        ppMinExp(expected);
        eprintf("\n");
    }
    assert(matches);
    UNPROTECT(save);
}

static void assertMinExpIsInt(MinExp *exp, int expected) {
    int save = PROTECT(exp);
    Term *asTerm = minExpToTerm(exp);
    PROTECT(asTerm);
    assert(asTerm->type == TERM_TYPE_NUM);
    bool matches =
        ncmp(getTerm_Num(asTerm)->value, value_Stdint(expected)) == CMP_EQ;
    assert(matches);
    UNPROTECT(save);
}

static void test_const_add(void) {
    int save = PROTECT(NULL);
    assertSimplifiesToInt(Add(N(2), N(3)), 5);
    UNPROTECT(save);
}

static void test_mul_zero(void) {
    int save = PROTECT(NULL);
    assertSimplifiesToInt(Mul(Vx(), N(0)), 0);
    UNPROTECT(save);
}

static void test_sub_self(void) {
    int save = PROTECT(NULL);
    assertSimplifiesToInt(Sub(Vx(), Vx()), 0);
    UNPROTECT(save);
}

static void test_div_self(void) {
    int save = PROTECT(NULL);
    assertSimplifiesToInt(Div(Vx(), Vx()), 1);
    UNPROTECT(save);
}

static void test_add_zero_right(void) {
    int save = PROTECT(NULL);
    assertSimplifiesToExpr(Add(Vx(), N(0)), Vx());
    UNPROTECT(save);
}

static void test_pow_one(void) {
    int save = PROTECT(NULL);
    assertSimplifiesToExpr(Pow(Vx(), N(1)), Vx());
    UNPROTECT(save);
}

static void test_div_one_right(void) {
    int save = PROTECT(NULL);
    assertSimplifiesToExpr(Div(Vx(), N(1)), Vx());
    UNPROTECT(save);
}

static void test_div_zero_left(void) {
    int save = PROTECT(NULL);
    assertSimplifiesToInt(Div(N(0), Vx()), 0);
    UNPROTECT(save);
}

static void test_mod_self(void) {
    int save = PROTECT(NULL);
    assertSimplifiesToInt(Mod(Vx(), Vx()), 0);
    UNPROTECT(save);
}

static void test_mod_zero_left(void) {
    int save = PROTECT(NULL);
    assertSimplifiesToInt(Mod(N(0), Vx()), 0);
    UNPROTECT(save);
}

static void test_pow_zero_exponent(void) {
    int save = PROTECT(NULL);
    assertSimplifiesToInt(Pow(Vx(), N(0)), 1);
    UNPROTECT(save);
}

static void test_pow_zero_base(void) {
    int save = PROTECT(NULL);
    assertSimplifiesToInt(Pow(N(0), Vx()), 0);
    UNPROTECT(save);
}

static void test_pow_one_base(void) {
    int save = PROTECT(NULL);
    assertSimplifiesToInt(Pow(N(1), Vx()), 1);
    UNPROTECT(save);
}

static void test_gcd_folds_after_argument_simplification(void) {
    int save = PROTECT(NULL);
    MinExp *expr = Gcd(Add(N(2), N(4)), Mul(N(3), N(2)));
    assertSimplifiesToInt(expr, 6);
    UNPROTECT(save);
}

static void test_lcm_folds_after_argument_simplification(void) {
    int save = PROTECT(NULL);
    MinExp *expr = Lcm(Add(N(8), N(4)), Sub(N(30), N(24)));
    assertSimplifiesToInt(expr, 12);
    UNPROTECT(save);
}

static void test_gcd_non_numeric_keeps_prim_after_arg_simplification(void) {
    int save = PROTECT(NULL);
    MinExp *expr = Gcd(Add(Vx(), N(0)), Add(Vy(), N(0)));
    assertSimplifiesToPrimVars(expr, MINPRIMOP_TYPE_GCD, newSymbol("x"),
                               newSymbol("y"));
    UNPROTECT(save);
}

static void test_lcm_non_numeric_keeps_prim_after_arg_simplification(void) {
    int save = PROTECT(NULL);
    MinExp *expr = Lcm(Add(Vx(), N(0)), Add(Vy(), N(0)));
    assertSimplifiesToPrimVars(expr, MINPRIMOP_TYPE_LCM, newSymbol("x"),
                               newSymbol("y"));
    UNPROTECT(save);
}

static void test_gcd_self_wraps_in_canon(void) {
    int save = PROTECT(NULL);
    assertSimplifiesToExpr(Gcd(Vx(), Vx()), Canon(Vx()));
    UNPROTECT(save);
}

static void test_gcd_zero_operand_wraps_in_canon(void) {
    int save = PROTECT(NULL);
    assertSimplifiesToExpr(Gcd(Vx(), N(0)), Canon(Vx()));
    assertSimplifiesToExpr(Gcd(N(0), Vy()), Canon(Vy()));
    UNPROTECT(save);
}

static void test_gcd_one_operand_folds_to_one(void) {
    int save = PROTECT(NULL);
    assertSimplifiesToInt(Gcd(Vx(), N(1)), 1);
    assertSimplifiesToInt(Gcd(N(1), Vy()), 1);
    UNPROTECT(save);
}

static void test_lcm_zero_operand_folds_to_zero(void) {
    int save = PROTECT(NULL);
    assertSimplifiesToInt(Lcm(Vx(), N(0)), 0);
    assertSimplifiesToInt(Lcm(N(0), Vy()), 0);
    UNPROTECT(save);
}

static void test_lcm_self_wraps_in_canon(void) {
    int save = PROTECT(NULL);
    assertSimplifiesToExpr(Lcm(Vx(), Vx()), Canon(Vx()));
    UNPROTECT(save);
}

static void test_lcm_one_operand_wraps_in_canon(void) {
    int save = PROTECT(NULL);
    assertSimplifiesToExpr(Lcm(Vx(), N(1)), Canon(Vx()));
    assertSimplifiesToExpr(Lcm(N(1), Vy()), Canon(Vy()));
    UNPROTECT(save);
}

static void test_canon_of_canon_collapses(void) {
    int save = PROTECT(NULL);
    assertSimplifiesToExpr(Canon(Canon(Vx())), Canon(Vx()));
    UNPROTECT(save);
}

static void test_nested_recursive_simplification(void) {
    int save = PROTECT(NULL);
    assertSimplifiesToExpr(Add(Mul(Vx(), N(1)), Mul(N(0), Vy())), Vx());
    UNPROTECT(save);
}

static void test_chained_sub_add_zero_to_zero(void) {
    int save = PROTECT(NULL);
    assertSimplifiesToInt(Add(Sub(Vx(), Vx()), N(0)), 0);
    UNPROTECT(save);
}

static void test_const_nested_arithmetic(void) {
    int save = PROTECT(NULL);
    assertSimplifiesToInt(Mul(Add(N(2), N(3)), Sub(N(4), N(1))), 15);
    UNPROTECT(save);
}

static void test_nested_div_mod_identity_to_zero(void) {
    int save = PROTECT(NULL);
    assertSimplifiesToInt(Mod(Div(Vx(), N(1)), Vx()), 0);
    UNPROTECT(save);
}

static void test_nested_pow_constants_to_one(void) {
    int save = PROTECT(NULL);
    assertSimplifiesToInt(Add(Pow(N(1), Vx()), Pow(N(0), Vy())), 1);
    UNPROTECT(save);
}

static void test_deep_nested_add_chain_with_var(void) {
    int save = PROTECT(NULL);
    assertSimplifiesToExpr(Add(N(1), Add(N(2), Add(N(3), Add(N(4), Vx())))),
                           Add(N(10), Vx()));
    UNPROTECT(save);
}

static void test_deep_nested_alternating_add_sub(void) {
    int save = PROTECT(NULL);
    assertSimplifiesToExpr(
        Add(N(1), Sub(N(2), Add(N(3), Sub(N(4), Add(N(5), Vx()))))),
        Add(N(1), Vx()));
    UNPROTECT(save);
}

static void test_deep_nested_division_chain_over_mul(void) {
    int save = PROTECT(NULL);
    assertSimplifiesToExpr(Div(Div(Div(Mul(N(24), Vx()), N(2)), N(3)), N(4)),
                           Vx());
    UNPROTECT(save);
}

static void test_no_simplify_add_vars(void) {
    int save = PROTECT(NULL);
    assertSimplifiesToPrimVars(Add(Vx(), Vy()), MINPRIMOP_TYPE_ADD,
                               newSymbol("x"), newSymbol("y"));
    UNPROTECT(save);
}

static void test_no_simplify_div_vars(void) {
    int save = PROTECT(NULL);
    assertSimplifiesToPrimVars(Div(Vx(), Vy()), MINPRIMOP_TYPE_DIV,
                               newSymbol("x"), newSymbol("y"));
    UNPROTECT(save);
}

static void test_no_simplify_mod_vars(void) {
    int save = PROTECT(NULL);
    assertSimplifiesToPrimVars(Mod(Vx(), Vy()), MINPRIMOP_TYPE_MOD,
                               newSymbol("x"), newSymbol("y"));
    UNPROTECT(save);
}

static void test_no_simplify_pow_vars(void) {
    int save = PROTECT(NULL);
    assertSimplifiesToPrimVars(Pow(Vx(), Vy()), MINPRIMOP_TYPE_POW,
                               newSymbol("x"), newSymbol("y"));
    UNPROTECT(save);
}

static void test_mixed_const_fold_without_distribution(void) {
    int save = PROTECT(NULL);
    HashSymbol *sx = newSymbol("x");
    HashSymbol *sy = newSymbol("y");
    MinExp *expr = Mul(Add(Vx(), Vy()), Add(N(1), N(2)));

    MinExp *simplified = simplifyMinExp(expr);
    PROTECT(simplified);
    assert(isMinExp_Prim(simplified));
    MinPrimApp *top = getMinExp_Prim(simplified);
    assert(top->type == MINPRIMOP_TYPE_MUL);

    assert(isMinExp_Prim(top->exp1));
    MinPrimApp *lhs = getMinExp_Prim(top->exp1);
    assert(lhs->type == MINPRIMOP_TYPE_ADD);
    assert(isMinExp_Var(lhs->exp1));
    assert(isMinExp_Var(lhs->exp2));
    assert(getMinExp_Var(lhs->exp1) == sx);
    assert(getMinExp_Var(lhs->exp2) == sy);

    assertMinExpIsInt(top->exp2, 3);

    UNPROTECT(save);
}

static void test_add_cancel_negation(void) {
    int save = PROTECT(NULL);
    assertSimplifiesToInt(Add(Vx(), Sub(N(0), Vx())), 0);
    UNPROTECT(save);
}

static void test_add_duplicate_to_mul_two(void) {
    int save = PROTECT(NULL);
    assertSimplifiesToExpr(Add(Vx(), Vx()), Mul(N(2), Vx()));
    UNPROTECT(save);
}

static void test_sub_double_negation(void) {
    int save = PROTECT(NULL);
    assertSimplifiesToExpr(Sub(N(0), Sub(N(0), Vx())), Vx());
    UNPROTECT(save);
}

static void test_mul_self_to_pow_two(void) {
    int save = PROTECT(NULL);
    assertSimplifiesToExpr(Mul(Vx(), Vx()), Pow(Vx(), N(2)));
    UNPROTECT(save);
}

static void test_mul_cancel_division(void) {
    int save = PROTECT(NULL);
    assertSimplifiesToExpr(Mul(Vx(), Div(Vy(), Vx())), Vy());
    UNPROTECT(save);
}

static void test_mul_const_into_mul_const_x(void) {
    int save = PROTECT(NULL);
    assertSimplifiesToExpr(Mul(N(2), Mul(N(3), Vx())), Mul(N(6), Vx()));
    UNPROTECT(save);
}

static void test_mul_const_into_add_const_x(void) {
    int save = PROTECT(NULL);
    assertSimplifiesToExpr(Mul(N(2), Add(N(3), Vx())),
                           Add(N(6), Mul(N(2), Vx())));
    UNPROTECT(save);
}

static void test_mul_const_into_sub_const_x(void) {
    int save = PROTECT(NULL);
    assertSimplifiesToExpr(Mul(N(2), Sub(N(3), Vx())),
                           Sub(N(6), Mul(N(2), Vx())));
    UNPROTECT(save);
}

static void test_mul_const_into_sub_x_const(void) {
    int save = PROTECT(NULL);
    assertSimplifiesToExpr(Mul(N(2), Sub(Vx(), N(3))),
                           Sub(Mul(N(2), Vx()), N(6)));
    UNPROTECT(save);
}

static void test_mul_const_with_div_const_over_x(void) {
    int save = PROTECT(NULL);
    assertSimplifiesToExpr(Mul(N(2), Div(N(3), Vx())), Div(N(6), Vx()));
    UNPROTECT(save);
}

static void test_mul_const_with_div_x_over_const(void) {
    int save = PROTECT(NULL);
    assertSimplifiesToExpr(Mul(N(8), Div(Vx(), N(2))), Mul(N(4), Vx()));
    UNPROTECT(save);
}

static void test_div_nested_denominator_product(void) {
    int save = PROTECT(NULL);
    assertSimplifiesToExpr(Div(Div(Vx(), N(2)), N(3)), Div(Vx(), N(6)));
    UNPROTECT(save);
}

static void test_div_const_over_div_const_x(void) {
    int save = PROTECT(NULL);
    assertSimplifiesToExpr(Div(N(8), Div(N(2), Vx())), Mul(N(4), Vx()));
    UNPROTECT(save);
}

static void test_div_const_over_div_x_const(void) {
    int save = PROTECT(NULL);
    assertSimplifiesToExpr(Div(N(8), Div(Vx(), N(2))), Div(N(16), Vx()));
    UNPROTECT(save);
}

static void test_div_mul_const_x_by_const(void) {
    int save = PROTECT(NULL);
    assertSimplifiesToExpr(Div(Mul(N(8), Vx()), N(2)), Mul(N(4), Vx()));
    UNPROTECT(save);
}

static void test_div_add_const_x_by_const(void) {
    int save = PROTECT(NULL);
    assertSimplifiesToExpr(Div(Add(N(8), Vx()), N(2)),
                           Add(N(4), Div(Vx(), N(2))));
    UNPROTECT(save);
}

static void test_div_sub_x_const_by_const(void) {
    int save = PROTECT(NULL);
    assertSimplifiesToExpr(Div(Sub(Vx(), N(8)), N(2)),
                           Sub(Div(Vx(), N(2)), N(4)));
    UNPROTECT(save);
}

static void test_div_pow_by_base_decrements_exponent(void) {
    int save = PROTECT(NULL);
    assertSimplifiesToExpr(Div(Pow(Vx(), N(3)), Vx()), Pow(Vx(), N(2)));
    UNPROTECT(save);
}

static void test_div_pow_by_pow_same_base_subtracts_exponents(void) {
    int save = PROTECT(NULL);
    assertSimplifiesToExpr(Div(Pow(Vx(), N(5)), Pow(Vx(), N(2))),
                           Pow(Vx(), N(3)));
    UNPROTECT(save);
}

static void test_mul_divx_a_mul_divy_b_to_div_xy_ab(void) {
    int save = PROTECT(NULL);
    assertSimplifiesToExpr(Mul(Div(Vx(), N(2)), Div(Vy(), N(3))),
                           Div(Mul(Vx(), Vy()), N(6)));
    UNPROTECT(save);
}

static void test_mul_diva_x_mul_divb_y_to_div_ab_xy(void) {
    int save = PROTECT(NULL);
    assertSimplifiesToExpr(Mul(Div(N(8), Vx()), Div(N(3), Vy())),
                           Div(N(24), Mul(Vx(), Vy())));
    UNPROTECT(save);
}

static void test_div_divx_a_by_b_to_div_x_ab(void) {
    int save = PROTECT(NULL);
    assertSimplifiesToExpr(Div(Div(Vx(), N(8)), N(3)), Div(Vx(), N(24)));
    UNPROTECT(save);
}

static void test_div_diva_x_by_b_to_div_ab_x(void) {
    int save = PROTECT(NULL);
    assertSimplifiesToExpr(Div(Div(N(12), Vx()), N(3)), Div(N(4), Vx()));
    UNPROTECT(save);
}

static void test_div_divx_a_by_divy_b_to_mul_divxy_ba(void) {
    int save = PROTECT(NULL);
    assertSimplifiesToExpr(Div(Div(Vx(), N(2)), Div(Vy(), N(6))),
                           Mul(Div(Vx(), Vy()), N(3)));
    UNPROTECT(save);
}

static void test_div_diva_x_by_divb_y_to_mul_ab_divyx(void) {
    int save = PROTECT(NULL);
    assertSimplifiesToExpr(Div(Div(N(12), Vx()), Div(N(3), Vy())),
                           Mul(N(4), Div(Vy(), Vx())));
    UNPROTECT(save);
}

static void test_add_div_same_denominator_combines(void) {
    int save = PROTECT(NULL);
    assertSimplifiesToExpr(Add(Div(Vx(), N(2)), Div(Vy(), N(2))),
                           Div(Add(Vx(), Vy()), N(2)));
    UNPROTECT(save);
}

static void test_sub_div_same_denominator_combines(void) {
    int save = PROTECT(NULL);
    assertSimplifiesToExpr(Sub(Div(Vx(), N(2)), Div(Vy(), N(2))),
                           Div(Sub(Vx(), Vy()), N(2)));
    UNPROTECT(save);
}

static void test_add_div_same_symbolic_denominator_combines(void) {
    int save = PROTECT(NULL);
    assertSimplifiesToExpr(Add(Div(Vx(), V("d")), Div(Vy(), V("d"))),
                           Div(Add(Vx(), Vy()), V("d")));
    UNPROTECT(save);
}

static void test_div_mul_common_factor_cancels(void) {
    int save = PROTECT(NULL);
    assertSimplifiesToExpr(Div(Mul(V("k"), Vx()), Mul(V("k"), Vy())),
                           Div(Vx(), Vy()));
    UNPROTECT(save);
}

static void test_mod_by_one(void) {
    int save = PROTECT(NULL);
    assertSimplifiesToInt(Mod(Vx(), N(1)), 0);
    UNPROTECT(save);
}

static void test_mod_nested_same_divisor_reduces(void) {
    int save = PROTECT(NULL);
    assertSimplifiesToExpr(Mod(Mod(N(17), Vx()), Vx()), Mod(N(17), Vx()));
    UNPROTECT(save);
}

static void test_mod_nested_different_divisor_no_change(void) {
    int save = PROTECT(NULL);
    HashSymbol *sx = newSymbol("x");
    HashSymbol *sy = newSymbol("y");

    MinExp *expr = Mod(Mod(N(17), Vx()), Vy());

    MinExp *simplified = simplifyMinExp(expr);
    PROTECT(simplified);
    assert(isMinExp_Prim(simplified));
    MinPrimApp *top = getMinExp_Prim(simplified);
    assert(top->type == MINPRIMOP_TYPE_MOD);
    assert(isMinExp_Prim(top->exp1));
    MinPrimApp *lhs = getMinExp_Prim(top->exp1);
    assert(lhs->type == MINPRIMOP_TYPE_MOD);
    assertMinExpIsInt(lhs->exp1, 17);
    assert(isMinExp_Var(lhs->exp2));
    assert(getMinExp_Var(lhs->exp2) == sx);
    assert(isMinExp_Var(top->exp2));
    assert(getMinExp_Var(top->exp2) == sy);

    UNPROTECT(save);
}

static void test_pow_nested_exponents_multiply(void) {
    int save = PROTECT(NULL);
    assertSimplifiesToExpr(Pow(Pow(Vx(), N(2)), N(3)), Pow(Vx(), N(6)));
    UNPROTECT(save);
}

static void test_mul_pow_pow_same_base(void) {
    int save = PROTECT(NULL);
    assertSimplifiesToExpr(Mul(Pow(Vx(), N(2)), Pow(Vx(), N(3))),
                           Pow(Vx(), N(5)));
    UNPROTECT(save);
}

static void test_add_const_into_nested_add(void) {
    int save = PROTECT(NULL);
    assertSimplifiesToExpr(Add(N(2), Add(N(3), Vx())), Add(N(5), Vx()));
    UNPROTECT(save);
}

static void test_add_const_over_sub_const_x(void) {
    int save = PROTECT(NULL);
    assertSimplifiesToExpr(Add(N(2), Sub(N(3), Vx())), Sub(N(5), Vx()));
    UNPROTECT(save);
}

static void test_add_const_over_sub_x_const(void) {
    int save = PROTECT(NULL);
    assertSimplifiesToExpr(Add(N(5), Sub(Vx(), N(3))), Add(N(2), Vx()));
    UNPROTECT(save);
}

static void test_sub_const_over_add_const_x(void) {
    int save = PROTECT(NULL);
    assertSimplifiesToExpr(Sub(N(10), Add(N(3), Vx())), Sub(N(7), Vx()));
    UNPROTECT(save);
}

static void test_sub_add_const_x_minus_const(void) {
    int save = PROTECT(NULL);
    assertSimplifiesToExpr(Sub(Add(N(10), Vx()), N(3)), Add(N(7), Vx()));
    UNPROTECT(save);
}

static void test_sub_sub_x_const_minus_const(void) {
    int save = PROTECT(NULL);
    assertSimplifiesToExpr(Sub(Sub(Vx(), N(2)), N(3)), Sub(Vx(), N(5)));
    UNPROTECT(save);
}

static void test_sub_const_over_sub_x_const(void) {
    int save = PROTECT(NULL);
    assertSimplifiesToExpr(Sub(N(10), Sub(Vx(), N(3))), Sub(N(13), Vx()));
    UNPROTECT(save);
}

static void test_add_addnum_addnum_normalizes(void) {
    int save = PROTECT(NULL);
    assertSimplifiesToExpr(Add(Add(N(2), Vx()), Add(N(3), Vy())),
                           Add(N(5), Add(Vx(), Vy())));
    UNPROTECT(save);
}

static void test_add_addnum_subnum_normalizes(void) {
    int save = PROTECT(NULL);
    assertSimplifiesToExpr(Add(Add(N(2), Vx()), Sub(N(3), Vy())),
                           Add(N(5), Sub(Vx(), Vy())));
    UNPROTECT(save);
}

static void test_add_subother_subother_normalizes(void) {
    int save = PROTECT(NULL);
    assertSimplifiesToExpr(Add(Sub(Vx(), N(4)), Sub(Vy(), N(6))),
                           Sub(Add(Vx(), Vy()), N(10)));
    UNPROTECT(save);
}

static void test_sub_addnum_addnum_normalizes(void) {
    int save = PROTECT(NULL);
    assertSimplifiesToExpr(Sub(Add(N(9), Vx()), Add(N(4), Vy())),
                           Add(N(5), Sub(Vx(), Vy())));
    UNPROTECT(save);
}

static void test_sub_subother_subother_normalizes(void) {
    int save = PROTECT(NULL);
    assertSimplifiesToExpr(Sub(Sub(Vx(), N(2)), Sub(Vy(), N(7))),
                           Add(N(5), Sub(Vx(), Vy())));
    UNPROTECT(save);
}

static void test_sub_subnum_addnum_normalizes(void) {
    int save = PROTECT(NULL);
    assertSimplifiesToExpr(Sub(Sub(N(10), Vx()), Add(N(3), Vy())),
                           Sub(N(7), Add(Vx(), Vy())));
    UNPROTECT(save);
}

static void test_sub_subother_addnum_normalizes(void) {
    int save = PROTECT(NULL);
    assertSimplifiesToExpr(Sub(Sub(Vx(), N(2)), Add(N(5), Vy())),
                           Sub(Sub(Vx(), N(7)), Vy()));
    UNPROTECT(save);
}

static void test_sub_addnum_subnum_normalizes(void) {
    int save = PROTECT(NULL);
    assertSimplifiesToExpr(Sub(Add(N(9), Vx()), Sub(N(4), Vy())),
                           Add(N(5), Add(Vx(), Vy())));
    UNPROTECT(save);
}

static void test_sub_addnum_subothernum_normalizes(void) {
    int save = PROTECT(NULL);
    assertSimplifiesToExpr(Sub(Add(N(9), Vx()), Sub(Vy(), N(4))),
                           Add(N(13), Sub(Vx(), Vy())));
    UNPROTECT(save);
}

static void test_sub_subnum_subothernum_normalizes(void) {
    int save = PROTECT(NULL);
    assertSimplifiesToExpr(Sub(Sub(N(10), Vx()), Sub(Vy(), N(3))),
                           Sub(N(13), Add(Vx(), Vy())));
    UNPROTECT(save);
}

static void test_sub_subothernum_subnum_normalizes(void) {
    int save = PROTECT(NULL);
    assertSimplifiesToExpr(Sub(Sub(Vx(), N(2)), Sub(N(7), Vy())),
                           Add(Sub(Vx(), N(9)), Vy()));
    UNPROTECT(save);
}

static void test_dsl_combinatorial_add_sub_matrix(void) {
    int constants[] = {2, 5};
    size_t count = sizeof(constants) / sizeof(constants[0]);

    for (size_t i = 0; i < count; i++) {
        for (size_t j = 0; j < count; j++) {
            int save = PROTECT(NULL);
            int a = constants[i];
            int b = constants[j];
            int delta = a - b;

            assertSimplifiesToExpr(Sub(Sub(N(a), Vx()), Add(N(b), Vy())),
                                   Sub(N(delta), Add(Vx(), Vy())));

            MinExp *expected2 = Add(N(delta), Add(Vx(), Vy()));
            if (delta == 0) {
                expected2 = Add(Vx(), Vy());
            }
            assertSimplifiesToExpr(Sub(Add(N(a), Vx()), Sub(N(b), Vy())),
                                   expected2);

            assertSimplifiesToExpr(Add(Sub(Vx(), N(a)), Sub(Vy(), N(b))),
                                   Sub(Add(Vx(), Vy()), N(a + b)));

            UNPROTECT(save);
        }
    }
}

int main(int argc __attribute__((unused)),
         char *argv[] __attribute__((unused))) {
    initAll();
#ifdef DEBUG_STRESS_GC
    forceGcFlag = 1;
#endif

    runTest("test_const_add", test_const_add);
    runTest("test_mul_zero", test_mul_zero);
    runTest("test_sub_self", test_sub_self);
    runTest("test_div_self", test_div_self);
    runTest("test_add_zero_right", test_add_zero_right);
    runTest("test_pow_one", test_pow_one);
    runTest("test_div_one_right", test_div_one_right);
    runTest("test_div_zero_left", test_div_zero_left);
    runTest("test_mod_self", test_mod_self);
    runTest("test_mod_zero_left", test_mod_zero_left);
    runTest("test_pow_zero_exponent", test_pow_zero_exponent);
    runTest("test_pow_zero_base", test_pow_zero_base);
    runTest("test_pow_one_base", test_pow_one_base);
    runTest("test_gcd_folds_after_argument_simplification",
            test_gcd_folds_after_argument_simplification);
    runTest("test_lcm_folds_after_argument_simplification",
            test_lcm_folds_after_argument_simplification);
    runTest("test_gcd_non_numeric_keeps_prim_after_arg_simplification",
            test_gcd_non_numeric_keeps_prim_after_arg_simplification);
    runTest("test_lcm_non_numeric_keeps_prim_after_arg_simplification",
            test_lcm_non_numeric_keeps_prim_after_arg_simplification);
    runTest("test_gcd_self_wraps_in_canon", test_gcd_self_wraps_in_canon);
    runTest("test_gcd_zero_operand_wraps_in_canon",
            test_gcd_zero_operand_wraps_in_canon);
    runTest("test_gcd_one_operand_folds_to_one",
            test_gcd_one_operand_folds_to_one);
    runTest("test_lcm_zero_operand_folds_to_zero",
            test_lcm_zero_operand_folds_to_zero);
    runTest("test_lcm_self_wraps_in_canon", test_lcm_self_wraps_in_canon);
    runTest("test_lcm_one_operand_wraps_in_canon",
            test_lcm_one_operand_wraps_in_canon);
    runTest("test_canon_of_canon_collapses", test_canon_of_canon_collapses);
    runTest("test_nested_recursive_simplification",
            test_nested_recursive_simplification);
    runTest("test_chained_sub_add_zero_to_zero",
            test_chained_sub_add_zero_to_zero);
    runTest("test_const_nested_arithmetic", test_const_nested_arithmetic);
    runTest("test_nested_div_mod_identity_to_zero",
            test_nested_div_mod_identity_to_zero);
    runTest("test_nested_pow_constants_to_one",
            test_nested_pow_constants_to_one);
    runTest("test_deep_nested_add_chain_with_var",
            test_deep_nested_add_chain_with_var);
    runTest("test_deep_nested_alternating_add_sub",
            test_deep_nested_alternating_add_sub);
    runTest("test_deep_nested_division_chain_over_mul",
            test_deep_nested_division_chain_over_mul);
    runTest("test_no_simplify_add_vars", test_no_simplify_add_vars);
    runTest("test_no_simplify_div_vars", test_no_simplify_div_vars);
    runTest("test_no_simplify_mod_vars", test_no_simplify_mod_vars);
    runTest("test_no_simplify_pow_vars", test_no_simplify_pow_vars);
    runTest("test_mixed_const_fold_without_distribution",
            test_mixed_const_fold_without_distribution);
    runTest("test_add_cancel_negation", test_add_cancel_negation);
    runTest("test_add_duplicate_to_mul_two", test_add_duplicate_to_mul_two);
    runTest("test_sub_double_negation", test_sub_double_negation);
    runTest("test_mul_self_to_pow_two", test_mul_self_to_pow_two);
    runTest("test_mul_cancel_division", test_mul_cancel_division);
    runTest("test_mul_const_into_mul_const_x", test_mul_const_into_mul_const_x);
    runTest("test_mul_const_into_add_const_x", test_mul_const_into_add_const_x);
    runTest("test_mul_const_into_sub_const_x", test_mul_const_into_sub_const_x);
    runTest("test_mul_const_into_sub_x_const", test_mul_const_into_sub_x_const);
    runTest("test_mul_const_with_div_const_over_x",
            test_mul_const_with_div_const_over_x);
    runTest("test_mul_const_with_div_x_over_const",
            test_mul_const_with_div_x_over_const);
    runTest("test_div_nested_denominator_product",
            test_div_nested_denominator_product);
    runTest("test_div_const_over_div_const_x", test_div_const_over_div_const_x);
    runTest("test_div_const_over_div_x_const", test_div_const_over_div_x_const);
    runTest("test_div_mul_const_x_by_const", test_div_mul_const_x_by_const);
    runTest("test_div_add_const_x_by_const", test_div_add_const_x_by_const);
    runTest("test_div_sub_x_const_by_const", test_div_sub_x_const_by_const);
    runTest("test_div_pow_by_base_decrements_exponent",
            test_div_pow_by_base_decrements_exponent);
    runTest("test_div_pow_by_pow_same_base_subtracts_exponents",
            test_div_pow_by_pow_same_base_subtracts_exponents);
    runTest("test_mul_divx_a_mul_divy_b_to_div_xy_ab",
            test_mul_divx_a_mul_divy_b_to_div_xy_ab);
    runTest("test_mul_diva_x_mul_divb_y_to_div_ab_xy",
            test_mul_diva_x_mul_divb_y_to_div_ab_xy);
    runTest("test_div_divx_a_by_b_to_div_x_ab",
            test_div_divx_a_by_b_to_div_x_ab);
    runTest("test_div_diva_x_by_b_to_div_ab_x",
            test_div_diva_x_by_b_to_div_ab_x);
    runTest("test_div_divx_a_by_divy_b_to_mul_divxy_ba",
            test_div_divx_a_by_divy_b_to_mul_divxy_ba);
    runTest("test_div_diva_x_by_divb_y_to_mul_ab_divyx",
            test_div_diva_x_by_divb_y_to_mul_ab_divyx);
    runTest("test_add_div_same_denominator_combines",
            test_add_div_same_denominator_combines);
    runTest("test_sub_div_same_denominator_combines",
            test_sub_div_same_denominator_combines);
    runTest("test_add_div_same_symbolic_denominator_combines",
            test_add_div_same_symbolic_denominator_combines);
    runTest("test_div_mul_common_factor_cancels",
            test_div_mul_common_factor_cancels);
    runTest("test_mod_by_one", test_mod_by_one);
    runTest("test_mod_nested_same_divisor_reduces",
            test_mod_nested_same_divisor_reduces);
    runTest("test_mod_nested_different_divisor_no_change",
            test_mod_nested_different_divisor_no_change);
    runTest("test_pow_nested_exponents_multiply",
            test_pow_nested_exponents_multiply);
    runTest("test_mul_pow_pow_same_base", test_mul_pow_pow_same_base);
    runTest("test_add_const_into_nested_add", test_add_const_into_nested_add);
    runTest("test_add_const_over_sub_const_x", test_add_const_over_sub_const_x);
    runTest("test_add_const_over_sub_x_const", test_add_const_over_sub_x_const);
    runTest("test_sub_const_over_add_const_x", test_sub_const_over_add_const_x);
    runTest("test_sub_add_const_x_minus_const",
            test_sub_add_const_x_minus_const);
    runTest("test_sub_sub_x_const_minus_const",
            test_sub_sub_x_const_minus_const);
    runTest("test_sub_const_over_sub_x_const", test_sub_const_over_sub_x_const);
    runTest("test_add_addnum_addnum_normalizes",
            test_add_addnum_addnum_normalizes);
    runTest("test_add_addnum_subnum_normalizes",
            test_add_addnum_subnum_normalizes);
    runTest("test_add_subother_subother_normalizes",
            test_add_subother_subother_normalizes);
    runTest("test_sub_addnum_addnum_normalizes",
            test_sub_addnum_addnum_normalizes);
    runTest("test_sub_subother_subother_normalizes",
            test_sub_subother_subother_normalizes);
    runTest("test_sub_subnum_addnum_normalizes",
            test_sub_subnum_addnum_normalizes);
    runTest("test_sub_subother_addnum_normalizes",
            test_sub_subother_addnum_normalizes);
    runTest("test_sub_addnum_subnum_normalizes",
            test_sub_addnum_subnum_normalizes);
    runTest("test_sub_addnum_subothernum_normalizes",
            test_sub_addnum_subothernum_normalizes);
    runTest("test_sub_subnum_subothernum_normalizes",
            test_sub_subnum_subothernum_normalizes);
    runTest("test_sub_subothernum_subnum_normalizes",
            test_sub_subothernum_subnum_normalizes);
    runTest("test_dsl_combinatorial_add_sub_matrix",
            test_dsl_combinatorial_add_sub_matrix);

    return 0;
}

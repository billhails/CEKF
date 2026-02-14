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

#include "arithmetic.h"
#include "bigint.h"
#include "common.h"
#include "init.h"
#include "minlam_simplify.h"
#include "symbol.h"
#include "term_helper.h"

extern int forceGcFlag;

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

static void test_const_add(void) {
    MinExp *expr = prim2SmallInts(MINPRIMOP_TYPE_ADD, 2, 3);
    assertSimplifiesToInt(expr, 5);
}

static void test_mul_zero(void) {
    MinExp *x = newMinExp_Var(NULLPI, newSymbol("x"));
    int save = PROTECT(x);
    MinExp *zero = smallIntExp(0);
    PROTECT(zero);
    MinExp *expr = prim2(MINPRIMOP_TYPE_MUL, x, zero);
    PROTECT(expr);
    assertSimplifiesToInt(expr, 0);
    UNPROTECT(save);
}

static void test_sub_self(void) {
    MinExp *x = newMinExp_Var(NULLPI, newSymbol("x"));
    int save = PROTECT(x);
    MinExp *expr = prim2(MINPRIMOP_TYPE_SUB, x, x);
    PROTECT(expr);
    assertSimplifiesToInt(expr, 0);
    UNPROTECT(save);
}

static void test_div_self(void) {
    MinExp *x = newMinExp_Var(NULLPI, newSymbol("x"));
    int save = PROTECT(x);
    MinExp *expr = prim2(MINPRIMOP_TYPE_DIV, x, x);
    PROTECT(expr);
    assertSimplifiesToInt(expr, 1);
    UNPROTECT(save);
}

static void test_add_zero_right(void) {
    MinExp *x = newMinExp_Var(NULLPI, newSymbol("x"));
    int save = PROTECT(x);
    MinExp *zero = smallIntExp(0);
    PROTECT(zero);
    MinExp *expr = prim2(MINPRIMOP_TYPE_ADD, x, zero);
    PROTECT(expr);
    MinExp *simplified = simplifyMinExp(expr);
    PROTECT(simplified);
    assert(eqMinExp(simplified, x));
    UNPROTECT(save);
}

static void test_pow_one(void) {
    MinExp *x = newMinExp_Var(NULLPI, newSymbol("x"));
    int save = PROTECT(x);
    MinExp *one = smallIntExp(1);
    PROTECT(one);
    MinExp *expr = prim2(MINPRIMOP_TYPE_POW, x, one);
    PROTECT(expr);
    MinExp *simplified = simplifyMinExp(expr);
    PROTECT(simplified);
    assert(eqMinExp(simplified, x));
    UNPROTECT(save);
}

static void test_div_one_right(void) {
    MinExp *x = newMinExp_Var(NULLPI, newSymbol("x"));
    int save = PROTECT(x);
    MinExp *one = smallIntExp(1);
    PROTECT(one);
    MinExp *expr = prim2(MINPRIMOP_TYPE_DIV, x, one);
    PROTECT(expr);
    MinExp *simplified = simplifyMinExp(expr);
    PROTECT(simplified);
    assert(eqMinExp(simplified, x));
    UNPROTECT(save);
}

static void test_div_zero_left(void) {
    MinExp *x = newMinExp_Var(NULLPI, newSymbol("x"));
    int save = PROTECT(x);
    MinExp *zero = smallIntExp(0);
    PROTECT(zero);
    MinExp *expr = prim2(MINPRIMOP_TYPE_DIV, zero, x);
    PROTECT(expr);
    assertSimplifiesToInt(expr, 0);
    UNPROTECT(save);
}

static void test_mod_self(void) {
    MinExp *x = newMinExp_Var(NULLPI, newSymbol("x"));
    int save = PROTECT(x);
    MinExp *expr = prim2(MINPRIMOP_TYPE_MOD, x, x);
    PROTECT(expr);
    assertSimplifiesToInt(expr, 0);
    UNPROTECT(save);
}

static void test_mod_zero_left(void) {
    MinExp *x = newMinExp_Var(NULLPI, newSymbol("x"));
    int save = PROTECT(x);
    MinExp *zero = smallIntExp(0);
    PROTECT(zero);
    MinExp *expr = prim2(MINPRIMOP_TYPE_MOD, zero, x);
    PROTECT(expr);
    assertSimplifiesToInt(expr, 0);
    UNPROTECT(save);
}

static void test_pow_zero_exponent(void) {
    MinExp *x = newMinExp_Var(NULLPI, newSymbol("x"));
    int save = PROTECT(x);
    MinExp *zero = smallIntExp(0);
    PROTECT(zero);
    MinExp *expr = prim2(MINPRIMOP_TYPE_POW, x, zero);
    PROTECT(expr);
    assertSimplifiesToInt(expr, 1);
    UNPROTECT(save);
}

static void test_pow_zero_base(void) {
    MinExp *x = newMinExp_Var(NULLPI, newSymbol("x"));
    int save = PROTECT(x);
    MinExp *zero = smallIntExp(0);
    PROTECT(zero);
    MinExp *expr = prim2(MINPRIMOP_TYPE_POW, zero, x);
    PROTECT(expr);
    assertSimplifiesToInt(expr, 0);
    UNPROTECT(save);
}

static void test_pow_one_base(void) {
    MinExp *x = newMinExp_Var(NULLPI, newSymbol("x"));
    int save = PROTECT(x);
    MinExp *one = smallIntExp(1);
    PROTECT(one);
    MinExp *expr = prim2(MINPRIMOP_TYPE_POW, one, x);
    PROTECT(expr);
    assertSimplifiesToInt(expr, 1);
    UNPROTECT(save);
}

static void test_nested_recursive_simplification(void) {
    MinExp *x = newMinExp_Var(NULLPI, newSymbol("x"));
    int save = PROTECT(x);
    MinExp *y = newMinExp_Var(NULLPI, newSymbol("y"));
    PROTECT(y);
    MinExp *one = smallIntExp(1);
    PROTECT(one);
    MinExp *zero = smallIntExp(0);
    PROTECT(zero);

    MinExp *left = prim2(MINPRIMOP_TYPE_MUL, x, one);
    PROTECT(left);
    MinExp *right = prim2(MINPRIMOP_TYPE_MUL, zero, y);
    PROTECT(right);
    MinExp *expr = prim2(MINPRIMOP_TYPE_ADD, left, right);
    PROTECT(expr);

    MinExp *simplified = simplifyMinExp(expr);
    PROTECT(simplified);
    assert(eqMinExp(simplified, x));

    UNPROTECT(save);
}

static void test_chained_sub_add_zero_to_zero(void) {
    MinExp *x = newMinExp_Var(NULLPI, newSymbol("x"));
    int save = PROTECT(x);
    MinExp *zero = smallIntExp(0);
    PROTECT(zero);

    MinExp *sub = prim2(MINPRIMOP_TYPE_SUB, x, x);
    PROTECT(sub);
    MinExp *expr = prim2(MINPRIMOP_TYPE_ADD, sub, zero);
    PROTECT(expr);

    assertSimplifiesToInt(expr, 0);
    UNPROTECT(save);
}

static void test_const_nested_arithmetic(void) {
    MinExp *add = prim2SmallInts(MINPRIMOP_TYPE_ADD, 2, 3);
    int save = PROTECT(add);
    MinExp *sub = prim2SmallInts(MINPRIMOP_TYPE_SUB, 4, 1);
    PROTECT(sub);
    MinExp *expr = prim2(MINPRIMOP_TYPE_MUL, add, sub);
    PROTECT(expr);

    assertSimplifiesToInt(expr, 15);
    UNPROTECT(save);
}

static void test_nested_div_mod_identity_to_zero(void) {
    MinExp *x = newMinExp_Var(NULLPI, newSymbol("x"));
    int save = PROTECT(x);
    MinExp *one = smallIntExp(1);
    PROTECT(one);

    MinExp *div = prim2(MINPRIMOP_TYPE_DIV, x, one);
    PROTECT(div);
    MinExp *expr = prim2(MINPRIMOP_TYPE_MOD, div, x);
    PROTECT(expr);

    assertSimplifiesToInt(expr, 0);
    UNPROTECT(save);
}

static void test_nested_pow_constants_to_one(void) {
    MinExp *x = newMinExp_Var(NULLPI, newSymbol("x"));
    int save = PROTECT(x);
    MinExp *y = newMinExp_Var(NULLPI, newSymbol("y"));
    PROTECT(y);
    MinExp *one = smallIntExp(1);
    PROTECT(one);
    MinExp *zero = smallIntExp(0);
    PROTECT(zero);

    MinExp *left = prim2(MINPRIMOP_TYPE_POW, one, x);
    PROTECT(left);
    MinExp *right = prim2(MINPRIMOP_TYPE_POW, zero, y);
    PROTECT(right);
    MinExp *expr = prim2(MINPRIMOP_TYPE_ADD, left, right);
    PROTECT(expr);

    assertSimplifiesToInt(expr, 1);
    UNPROTECT(save);
}

static void test_no_simplify_add_vars(void) {
    MinExp *x = newMinExp_Var(NULLPI, newSymbol("x"));
    int save = PROTECT(x);
    MinExp *y = newMinExp_Var(NULLPI, newSymbol("y"));
    PROTECT(y);
    HashSymbol *sx = getMinExp_Var(x);
    HashSymbol *sy = getMinExp_Var(y);
    MinExp *expr = prim2(MINPRIMOP_TYPE_ADD, x, y);
    PROTECT(expr);
    assertSimplifiesToPrimVars(expr, MINPRIMOP_TYPE_ADD, sx, sy);
    UNPROTECT(save);
}

static void test_no_simplify_div_vars(void) {
    MinExp *x = newMinExp_Var(NULLPI, newSymbol("x"));
    int save = PROTECT(x);
    MinExp *y = newMinExp_Var(NULLPI, newSymbol("y"));
    PROTECT(y);
    HashSymbol *sx = getMinExp_Var(x);
    HashSymbol *sy = getMinExp_Var(y);
    MinExp *expr = prim2(MINPRIMOP_TYPE_DIV, x, y);
    PROTECT(expr);
    assertSimplifiesToPrimVars(expr, MINPRIMOP_TYPE_DIV, sx, sy);
    UNPROTECT(save);
}

static void test_no_simplify_mod_vars(void) {
    MinExp *x = newMinExp_Var(NULLPI, newSymbol("x"));
    int save = PROTECT(x);
    MinExp *y = newMinExp_Var(NULLPI, newSymbol("y"));
    PROTECT(y);
    HashSymbol *sx = getMinExp_Var(x);
    HashSymbol *sy = getMinExp_Var(y);
    MinExp *expr = prim2(MINPRIMOP_TYPE_MOD, x, y);
    PROTECT(expr);
    assertSimplifiesToPrimVars(expr, MINPRIMOP_TYPE_MOD, sx, sy);
    UNPROTECT(save);
}

static void test_no_simplify_pow_vars(void) {
    MinExp *x = newMinExp_Var(NULLPI, newSymbol("x"));
    int save = PROTECT(x);
    MinExp *y = newMinExp_Var(NULLPI, newSymbol("y"));
    PROTECT(y);
    HashSymbol *sx = getMinExp_Var(x);
    HashSymbol *sy = getMinExp_Var(y);
    MinExp *expr = prim2(MINPRIMOP_TYPE_POW, x, y);
    PROTECT(expr);
    assertSimplifiesToPrimVars(expr, MINPRIMOP_TYPE_POW, sx, sy);
    UNPROTECT(save);
}

static void test_mixed_const_fold_without_distribution(void) {
    MinExp *x = newMinExp_Var(NULLPI, newSymbol("x"));
    int save = PROTECT(x);
    MinExp *y = newMinExp_Var(NULLPI, newSymbol("y"));
    PROTECT(y);
    HashSymbol *sx = getMinExp_Var(x);
    HashSymbol *sy = getMinExp_Var(y);

    MinExp *left = prim2(MINPRIMOP_TYPE_ADD, x, y);
    PROTECT(left);
    MinExp *right = prim2SmallInts(MINPRIMOP_TYPE_ADD, 1, 2);
    PROTECT(right);
    MinExp *expr = prim2(MINPRIMOP_TYPE_MUL, left, right);
    PROTECT(expr);

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

    Term *rhsTerm = minExpToTerm(top->exp2);
    PROTECT(rhsTerm);
    assert(rhsTerm->type == TERM_TYPE_NUM);
    assert(ncmp(getTerm_Num(rhsTerm)->value, value_Stdint(3)) == CMP_EQ);

    UNPROTECT(save);
}

int main(int argc __attribute__((unused)),
         char *argv[] __attribute__((unused))) {
    initAll();
    forceGcFlag = 1;

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
    runTest("test_nested_recursive_simplification",
            test_nested_recursive_simplification);
    runTest("test_chained_sub_add_zero_to_zero",
            test_chained_sub_add_zero_to_zero);
    runTest("test_const_nested_arithmetic", test_const_nested_arithmetic);
    runTest("test_nested_div_mod_identity_to_zero",
            test_nested_div_mod_identity_to_zero);
    runTest("test_nested_pow_constants_to_one",
            test_nested_pow_constants_to_one);
    runTest("test_no_simplify_add_vars", test_no_simplify_add_vars);
    runTest("test_no_simplify_div_vars", test_no_simplify_div_vars);
    runTest("test_no_simplify_mod_vars", test_no_simplify_mod_vars);
    runTest("test_no_simplify_pow_vars", test_no_simplify_pow_vars);
    runTest("test_mixed_const_fold_without_distribution",
            test_mixed_const_fold_without_distribution);

    return 0;
}

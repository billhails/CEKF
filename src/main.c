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
#include <stdbool.h>

#include "common.h"
#include "analysis.h"
#include "exp.h"
#include "memory.h"
#include "step.h"
#include "debug.h"

#ifdef DEBUG_RUN_TESTS

Exp *makeIntExp(int num) {
    return newExp(AEXP_TYPE_INT, AEXP_VAL_INT(num));
}

Exp *makeVarExp(AexpVar *var) {
    return newExp(AEXP_TYPE_VAR, AEXP_VAL_VAR(var));
}

Exp *makeLambda(AexpVar *arg, Exp *body) {
    AexpVarList *args = newAexpVarList(NULL, arg);
    int save = PROTECT(args);
    AexpLam *lam = newAexpLam(args, body);
    PROTECT(lam);
    Exp *exp = newExp(AEXP_TYPE_LAM, AEXP_VAL_LAM(lam));
    UNPROTECT(save);
    return exp;
}

AexpList *makeList(int argc, Exp* argv[]) {
    AexpList *acc = NULL;
    while (--argc >= 0) {
        int save = PROTECT(acc);
        acc = newAexpList(acc, argv[argc]);
        UNPROTECT(save);
    }
    return acc;
}

Exp *makeLetRec(LetRecBindings *bindings, Exp *body) {
    CexpLetRec *letRec = newCexpLetRec(bindings, body);
    int save = PROTECT(letRec);
    Exp *exp = newExp(CEXP_TYPE_LETREC, CEXP_VAL_LETREC(letRec));
    UNPROTECT(save);
    return exp;
}

Exp *makePrim(AexpPrimOp op, Exp *exp1, Exp *exp2) {
    AexpPrimApp *prim = newAexpPrimApp(op, exp1, exp2);
    int save = PROTECT(prim);
    Exp *add = newExp(AEXP_TYPE_PRIM, AEXP_VAL_PRIM(prim));
    UNPROTECT(save);
    return add;
}

Exp *makeLet(AexpVar *var, Exp *val, Exp *body) {
    ExpLet *expLet = newExpLet(var, val, body);
    int save = PROTECT(expLet);
    Exp *let = newExp(EXP_TYPE_LET, EXP_VAL_LET(expLet));
    UNPROTECT(save);
    return let;
}

Exp *makeIf(Exp *cond, Exp *cons, Exp *alt) {
    CexpCond *cec = newCexpCond(cond, cons, alt);
    int save = PROTECT(cec);
    Exp *ifExp = newExp(CEXP_TYPE_COND, CEXP_VAL_COND(cec));
    UNPROTECT(save);
    return ifExp;
}

Exp *makeBack() {
    return newExp(CEXP_TYPE_BACK, CEXP_VAL_NONE());
}

Exp *makeApply(Exp *function, Exp *arg) {
    AexpList *args = newAexpList(NULL, arg);
    int save = PROTECT(args);
    CexpApply *apply = newCexpApply(function, args);
    PROTECT(apply);
    Exp *exp = newExp(CEXP_TYPE_APPLY, CEXP_VAL_APPLY(apply));
    UNPROTECT(save);
    return exp;
}

Exp *makeAmb(Exp *exp1, Exp *exp2) {
    CexpAmb *cexpAmb = newCexpAmb(exp1, exp2);
    int save = PROTECT(cexpAmb);
    Exp *amb = newExp(CEXP_TYPE_AMB, CEXP_VAL_AMB(cexpAmb));
    UNPROTECT(save);
    return amb;
}

Exp *makeBool(bool val) {
    ExpType type = val ? AEXP_TYPE_TRUE : AEXP_TYPE_FALSE;
    return newExp(type, AEXP_VAL_NONE());
}

Exp *makeTestExp1() {
    // (let (x 10) (+ x 10))

    Exp *ten = makeIntExp(10);
    int save = PROTECT(ten);
    AexpVar *var = newAexpVar("x");
    PROTECT(var);
    Exp *x = makeVarExp(var);
    PROTECT(x);
    Exp *addTen = makePrim(AEXP_PRIM_ADD, x, ten);
    PROTECT(addTen);
    Exp *let = makeLet(var, ten, addTen);
    UNPROTECT(save);
    return let;
}

Exp *makeTestExp2() {
    // (let (x (amb #f #t)) (if x 5 (back)))

    Exp *expFive = makeIntExp(5);
    int save = PROTECT(expFive);
    AexpVar *var = newAexpVar("x");
    PROTECT(var);
    Exp *x = makeVarExp(var);
    PROTECT(x);
    Exp *expBack = makeBack();
    PROTECT(expBack);
    Exp *expIf = makeIf(x, expFive, expBack);
    PROTECT(expIf);
    Exp *expFalse = makeBool(false);
    PROTECT(expFalse);
    Exp *expTrue = makeBool(true);
    PROTECT(expTrue);
    Exp *expAmb = makeAmb(expFalse, expTrue);
    PROTECT(expAmb);
    Exp *let = makeLet(var, expAmb, expIf);
    UNPROTECT(save);
    return let;
}

Exp *makeTestExp3(int depth) {
    Exp *args[2];
    /*
    (letrec ((fib
             (lambda (n)
                     (if (< n 2)
                         1
                         (+ (fib (- n 1))
                            (fib (- n 2)))))))
            (fib 5))

    ANF:
    (letrec ((fib
              (lambda (n)
                      (if (< n 2)
                          1
                          (let (fib1 (fib (- n 1)))
                               (let (fib2 (fib (- n 2)))
                                    (+ fib1 fib2)))))))
            (fib 5))
    */

    AexpVar *fibVar = newAexpVar("fib");
    int save = PROTECT(fibVar);

    AexpVar *n = newAexpVar("n");
    PROTECT(n);

    Exp *one = makeIntExp(1);
    PROTECT(one);

    Exp *two = makeIntExp(2);
    PROTECT(two);

    AexpVar *fib1 = newAexpVar("fib1");
    PROTECT(fib1);

    AexpVar *fib2 = newAexpVar("fib2");
    PROTECT(fib2);

    Exp *fib = makeVarExp(fibVar);
    PROTECT(fib);
    
    Exp *nExp = makeVarExp(n);
    PROTECT(nExp);
    Exp *nMinusTwo = makePrim(AEXP_PRIM_SUB, nExp, two);
    PROTECT(nMinusTwo);

    Exp *fibNminusTwo = makeApply(fib, nMinusTwo);
    PROTECT(fibNminusTwo);

    Exp *fib1Exp = makeVarExp(fib1);
    PROTECT(fib1Exp);

    Exp *fib2Exp = makeVarExp(fib2);
    PROTECT(fib2Exp);

    Exp *addFib1fib2 = makePrim(AEXP_PRIM_ADD, fib1Exp, fib2Exp);
    PROTECT(addFib1fib2);

    Exp *fibInnerLet = makeLet(fib2, fibNminusTwo, addFib1fib2);
    PROTECT(fibInnerLet);
    
    fib = makeVarExp(fibVar);
    PROTECT(fib);
    
    nExp = makeVarExp(n);
    PROTECT(nExp);
    Exp *nMinusOne = makePrim(AEXP_PRIM_SUB, nExp, one);
    PROTECT(nMinusOne);
    
    Exp *fibNminusOne = makeApply(fib, nMinusOne);
    PROTECT(fibNminusOne);

    Exp *fibOuterLet = makeLet(fib1, fibNminusOne, fibInnerLet);
    PROTECT(fibOuterLet);

    nExp = makeVarExp(n);
    PROTECT(nExp);

    Exp *nLtTwo = makePrim(AEXP_PRIM_LT, nExp, two);
    PROTECT(nLtTwo);

    Exp *fibBody = makeIf(nLtTwo, one, fibOuterLet);
    PROTECT(fibBody);
    
    Exp *fibLambda = makeLambda(n, fibBody);
    PROTECT(fibLambda);

    LetRecBindings *bindings = newLetRecBindings(NULL, fibVar, fibLambda);
    PROTECT(bindings);

    Exp *arg = makeIntExp(depth);
    PROTECT(arg);

    fib = makeVarExp(fibVar);
    PROTECT(fib);
    
    Exp *body = makeApply(fib, arg);
    PROTECT(body);

    Exp *letRec = makeLetRec(bindings, body);

    UNPROTECT(save);

    return letRec;
}

int main(int argc, char *argv[]) {
    // run(makeTestExp1());
    // run(makeTestExp2());
    Exp *exp = makeTestExp3(35);
    PROTECT(exp);
    analizeExp(exp, NULL, 0);
    printExp(exp);
    printf("\n");
    run(exp);
}

#else

int main(int argc, char *argv[]) {
}

#endif

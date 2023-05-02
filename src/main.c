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
    AexpLam *lam = newAexpLam(args, body);
    Exp *exp = newExp(AEXP_TYPE_LAM, AEXP_VAL_LAM(lam));
    return exp;
}

AexpList *makeList(int argc, Exp* argv[]) {
    AexpList *acc = NULL;
    while (--argc >= 0) {
        acc = newAexpList(acc, argv[argc]);
    }
    return acc;
}

Exp *makeLetRec(LetRecBindings *bindings, Exp *body) {
    CexpLetRec *letRec = newCexpLetRec(bindings, body);
    Exp *exp = newExp(CEXP_TYPE_LETREC, CEXP_VAL_LETREC(letRec));
    return exp;
}

Exp *makePrim(AexpPrimOp op, Exp *exp1, Exp *exp2) {
    AexpPrimApp *prim = newAexpPrimApp(op, exp1, exp2);
    Exp *add = newExp(AEXP_TYPE_PRIM, AEXP_VAL_PRIM(prim));
    return add;
}

Exp *makeLet(AexpVar *var, Exp *val, Exp *body) {
    ExpLet *expLet = newExpLet(var, val, body);
    Exp *let = newExp(EXP_TYPE_LET, EXP_VAL_LET(expLet));
    return let;
}

Exp *makeIf(Exp *cond, Exp *cons, Exp *alt) {
    CexpCond *cec = newCexpCond(cond, cons, alt);
    Exp *ifExp = newExp(CEXP_TYPE_COND, CEXP_VAL_COND(cec));
    return ifExp;
}

Exp *makeBack() {
    return newExp(CEXP_TYPE_BACK, CEXP_VAL_NONE());
}

Exp *makeApply(Exp *function, Exp *arg) {
    AexpList *args = newAexpList(NULL, arg);
    CexpApply *apply = newCexpApply(function, args);
    Exp *exp = newExp(CEXP_TYPE_APPLY, CEXP_VAL_APPLY(apply));
    return exp;
}

Exp *makeAmb(Exp *exp1, Exp *exp2) {
    CexpAmb *cexpAmb = newCexpAmb(exp1, exp2);
    Exp *amb = newExp(CEXP_TYPE_AMB, CEXP_VAL_AMB(cexpAmb));
    return amb;
}

Exp *makeBool(bool val) {
    ExpType type = val ? AEXP_TYPE_TRUE : AEXP_TYPE_FALSE;
    return newExp(type, AEXP_VAL_NONE());
}

Exp *makeTestExp1() {
    // (let (x 10) (+ x 10))

    Exp *ten = makeIntExp(10);
    AexpVar *var = newAexpVar("x");
    Exp *x = makeVarExp(var);
    Exp *addTen = makePrim(AEXP_PRIM_ADD, x, ten);
    Exp *let = makeLet(var, ten, addTen);
    return let;
}

Exp *makeTestExp2() {
    // (let (x (amb #f #t)) (if x 5 (back)))

    Exp *expFive = makeIntExp(5);
    AexpVar *var = newAexpVar("x");
    Exp *x = makeVarExp(var);
    Exp *expBack = makeBack();
    Exp *expIf = makeIf(x, expFive, expBack);
    Exp *expFalse = makeBool(false);
    Exp *expTrue = makeBool(true);
    Exp *expAmb = makeAmb(expFalse, expTrue);
    Exp *let = makeLet(var, expAmb, expIf);
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
    AexpVar *n = newAexpVar("n");
    Exp *one = makeIntExp(1);
    Exp *two = makeIntExp(2);
    AexpVar *fib1 = newAexpVar("fib1");
    AexpVar *fib2 = newAexpVar("fib2");
    Exp *fib = makeVarExp(fibVar);
    Exp *nExp = makeVarExp(n);
    Exp *nMinusTwo = makePrim(AEXP_PRIM_SUB, nExp, two);
    Exp *fibNminusTwo = makeApply(fib, nMinusTwo);
    Exp *fib1Exp = makeVarExp(fib1);
    Exp *fib2Exp = makeVarExp(fib2);
    Exp *addFib1fib2 = makePrim(AEXP_PRIM_ADD, fib1Exp, fib2Exp);
    Exp *fibInnerLet = makeLet(fib2, fibNminusTwo, addFib1fib2);
    fib = makeVarExp(fibVar);
    nExp = makeVarExp(n);
    Exp *nMinusOne = makePrim(AEXP_PRIM_SUB, nExp, one);
    Exp *fibNminusOne = makeApply(fib, nMinusOne);
    Exp *fibOuterLet = makeLet(fib1, fibNminusOne, fibInnerLet);
    nExp = makeVarExp(n);
    Exp *nLtTwo = makePrim(AEXP_PRIM_LT, nExp, two);
    Exp *fibBody = makeIf(nLtTwo, one, fibOuterLet);
    Exp *fibLambda = makeLambda(n, fibBody);
    LetRecBindings *bindings = newLetRecBindings(NULL, fibVar, fibLambda);
    Exp *arg = makeIntExp(depth);
    fib = makeVarExp(fibVar);
    Exp *body = makeApply(fib, arg);
    Exp *letRec = makeLetRec(bindings, body);
    return letRec;
}

int main(int argc, char *argv[]) {
    // run(makeTestExp1());
    // run(makeTestExp2());
    disableGC();
    /* fib.scm */
    /*
    Exp *exp = newExp(CEXP_TYPE_LETREC,CEXP_VAL_LETREC(newCexpLetRec(newLetRecBindings(NULL,newAexpVar("fib"),newExp(AEXP_TYPE_LAM,AEXP_VAL_LAM(newAexpLam(newAexpVarList(NULL,newAexpVar("n")),newExp(CEXP_TYPE_COND,CEXP_VAL_COND(newCexpCond(newExp(AEXP_TYPE_PRIM,AEXP_VAL_PRIM(newAexpPrimApp(AEXP_PRIM_LT,newExp(AEXP_TYPE_VAR,AEXP_VAL_VAR(newAexpVar("n"))),newExp(AEXP_TYPE_INT,AEXP_VAL_INT(2))))),newExp(AEXP_TYPE_INT,AEXP_VAL_INT(1)),newExp(EXP_TYPE_LET,EXP_VAL_LET(newExpLet(newAexpVar("fib1"),newExp(CEXP_TYPE_APPLY,CEXP_VAL_APPLY(newCexpApply(newExp(AEXP_TYPE_VAR,AEXP_VAL_VAR(newAexpVar("fib"))),newAexpList(NULL,newExp(AEXP_TYPE_PRIM,AEXP_VAL_PRIM(newAexpPrimApp(AEXP_PRIM_SUB,newExp(AEXP_TYPE_VAR,AEXP_VAL_VAR(newAexpVar("n"))),newExp(AEXP_TYPE_INT,AEXP_VAL_INT(1))))))))),newExp(EXP_TYPE_LET,EXP_VAL_LET(newExpLet(newAexpVar("fib2"),newExp(CEXP_TYPE_APPLY,CEXP_VAL_APPLY(newCexpApply(newExp(AEXP_TYPE_VAR,AEXP_VAL_VAR(newAexpVar("fib"))),newAexpList(NULL,newExp(AEXP_TYPE_PRIM,AEXP_VAL_PRIM(newAexpPrimApp(AEXP_PRIM_SUB,newExp(AEXP_TYPE_VAR,AEXP_VAL_VAR(newAexpVar("n"))),newExp(AEXP_TYPE_INT,AEXP_VAL_INT(2))))))))),newExp(AEXP_TYPE_PRIM,AEXP_VAL_PRIM(newAexpPrimApp(AEXP_PRIM_ADD,newExp(AEXP_TYPE_VAR,AEXP_VAL_VAR(newAexpVar("fib1"))),newExp(AEXP_TYPE_VAR,AEXP_VAL_VAR(newAexpVar("fib2"))))))))))))))))))),newExp(CEXP_TYPE_APPLY,CEXP_VAL_APPLY(newCexpApply(newExp(AEXP_TYPE_VAR,AEXP_VAL_VAR(newAexpVar("fib"))),newAexpList(NULL,newExp(AEXP_TYPE_INT,AEXP_VAL_INT(20)))))))));
    */
    /* back.scm */
    /*
    Exp *exp = newExp(EXP_TYPE_LET,EXP_VAL_LET(newExpLet(newAexpVar("x"),newExp(CEXP_TYPE_AMB,CEXP_VAL_AMB(newCexpAmb(newExp(AEXP_TYPE_FALSE,AEXP_VAL_NONE()),newExp(AEXP_TYPE_TRUE,AEXP_VAL_NONE())))),newExp(CEXP_TYPE_COND,CEXP_VAL_COND(newCexpCond(newExp(AEXP_TYPE_VAR,AEXP_VAL_VAR(newAexpVar("x"))),newExp(AEXP_TYPE_INT,AEXP_VAL_INT(5)),newExp(CEXP_TYPE_BACK,CEXP_VAL_NONE())))))));
    */
    /* callcc.scm */
    Exp *exp = newExp(CEXP_TYPE_CALLCC,CEXP_VAL_CALLCC(newExp(AEXP_TYPE_LAM,AEXP_VAL_LAM(newAexpLam(newAexpVarList(NULL,newAexpVar("ret")),newExp(CEXP_TYPE_COND,CEXP_VAL_COND(newCexpCond(newExp(CEXP_TYPE_APPLY,CEXP_VAL_APPLY(newCexpApply(newExp(AEXP_TYPE_VAR,AEXP_VAL_VAR(newAexpVar("ret"))),newAexpList(NULL,newExp(AEXP_TYPE_INT,AEXP_VAL_INT(5)))))),newExp(AEXP_TYPE_INT,AEXP_VAL_INT(6)),newExp(AEXP_TYPE_INT,AEXP_VAL_INT(7))))))))));
    PROTECT(exp);
    enableGC();
    analizeExp(exp, NULL, 0);
    printExp(exp);
    printf("\n");
    run(exp);
}

#else

int main(int argc, char *argv[]) {
}

#endif

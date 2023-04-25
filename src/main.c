#include <stdbool.h>

#include "exp.h"
#include "step.h"

Aexp *makeIntAexp(int num) {
    return newAexp(AEXP_TYPE_INT, AEXP_VAL_INT(num));
}

Exp *makeIntExp(int num) {
    Aexp *intAexp = makeIntAexp(num);
    return newExp(EXP_TYPE_AEXP, EXP_VAL_AEXP(intAexp));
}

Aexp *makeVar(AexpVar *var) {
    return newAexp(AEXP_TYPE_VAR, AEXP_VAL_VAR(var));
}

AexpList *makeList(int argc, Aexp* argv[]) {
    AexpList *acc = NULL;
    while (--argc >= 0) {
        acc = newAexpList(acc, argv[argc]);
    }
    return acc;
}

Exp *makeAdd(int argc, Aexp *argv[]) {
    AexpList *args = makeList(argc, argv);
    AexpPrimApp *prim = newAexpPrimApp(AEXP_PRIM_ADD, args);
    Aexp *aexp = newAexp(AEXP_TYPE_PRIM, AEXP_VAL_PRIM(prim));
    return newExp(EXP_TYPE_AEXP, EXP_VAL_AEXP(aexp));
}

Exp *makeLet(AexpVar *var, Exp *val, Exp *body) {
    ExpLet *expLet = newExpLet(var, val, body);
    return newExp(EXP_TYPE_LET, EXP_VAL_LET(expLet));
}

Exp *makeIf(Aexp *cond, Exp *cons, Exp *alt) {
    CexpConditional *cec = newCexpConditional(cond, cons, alt);
    Cexp *cexp = newCexp(CEXP_TYPE_CONDITIONAL, CEXP_VAL_CONDITIONAL(cec));
    return newExp(EXP_TYPE_CEXP, EXP_VAL_CEXP(cexp));
}

Exp *makeBack() {
    Cexp *cexpBack = newCexp(CEXP_TYPE_BACK, CEXP_VAL_NONE());
    return newExp(EXP_TYPE_CEXP, EXP_VAL_CEXP(cexpBack));
}

Exp *makeAmb(Exp *exp1, Exp *exp2) {
    CexpAmb *cexpAmb = newCexpAmb(exp1, exp2);
    Cexp *cexp = newCexp(CEXP_TYPE_AMB, CEXP_VAL_AMB(cexpAmb));
    return newExp(EXP_TYPE_CEXP, EXP_VAL_CEXP(cexp));
}

Exp *makeBool(bool val) {
    AexpType type = val ? AEXP_TYPE_TRUE : AEXP_TYPE_FALSE;
    Aexp *aexp = newAexp(type, AEXP_VAL_NONE());
    return newExp(EXP_TYPE_AEXP, EXP_VAL_AEXP(aexp));
}

Exp *makeTestExp1() {
    // (let (x 10) (+ x 10))

    Aexp *ten = makeIntAexp(10);
    AexpVar *var = newAexpVar("x");
    Aexp *x = makeVar(var);
    Aexp *args[2];
    args[0] = x;
    args[1] = ten;
    Exp *addTen = makeAdd(2, args);
    Exp *expTen = newExp(EXP_TYPE_AEXP, EXP_VAL_AEXP(ten));
    return makeLet(var, expTen, addTen);
}

Exp *makeTestExp2() {
    // (let (x (amb #f #t)) (if x 5 (back)))

    Exp *expFive = makeIntExp(5);
    AexpVar *var = newAexpVar("x");
    Aexp *x = makeVar(var);
    Exp *expBack = makeBack();
    Exp *expIf = makeIf(x, expFive, expBack);
    Exp *expFalse = makeBool(false);
    Exp *expTrue = makeBool(true);
    Exp *expAmb = makeAmb(expFalse, expTrue);
    return makeLet(var, expAmb, expIf);
}

int main(int argc, char *argv[]) {
    Exp *exp = makeTestExp2();
    run(exp);
}

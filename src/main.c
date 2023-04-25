#include <stdbool.h>

#include "exp.h"
#include "step.h"

Exp *makeIntExp(int num) {
    return newExp(AEXP_TYPE_INT, AEXP_VAL_INT(num));
}

Exp *makeVar(AexpVar *var) {
    return newExp(AEXP_TYPE_VAR, AEXP_VAL_VAR(var));
}

AexpList *makeList(int argc, Exp* argv[]) {
    AexpList *acc = NULL;
    while (--argc >= 0) {
        acc = newAexpList(acc, argv[argc]);
    }
    return acc;
}

Exp *makeAdd(int argc, Exp *argv[]) {
    AexpList *args = makeList(argc, argv);
    AexpPrimApp *prim = newAexpPrimApp(AEXP_PRIM_ADD, args);
    return newExp(AEXP_TYPE_PRIM, AEXP_VAL_PRIM(prim));
}

Exp *makeLet(AexpVar *var, Exp *val, Exp *body) {
    ExpLet *expLet = newExpLet(var, val, body);
    return newExp(EXP_TYPE_LET, EXP_VAL_LET(expLet));
}

Exp *makeIf(Exp *cond, Exp *cons, Exp *alt) {
    CexpConditional *cec = newCexpConditional(cond, cons, alt);
    return newExp(CEXP_TYPE_CONDITIONAL, CEXP_VAL_CONDITIONAL(cec));
}

Exp *makeBack() {
    return newExp(CEXP_TYPE_BACK, CEXP_VAL_NONE());
}

Exp *makeAmb(Exp *exp1, Exp *exp2) {
    CexpAmb *cexpAmb = newCexpAmb(exp1, exp2);
    return newExp(CEXP_TYPE_AMB, CEXP_VAL_AMB(cexpAmb));
}

Exp *makeBool(bool val) {
    ExpType type = val ? AEXP_TYPE_TRUE : AEXP_TYPE_FALSE;
    return newExp(type, AEXP_VAL_NONE());
}

Exp *makeTestExp1() {
    // (let (x 10) (+ x 10))

    Exp *ten = makeIntExp(10);
    AexpVar *var = newAexpVar("x");
    Exp *x = makeVar(var);
    Exp *args[2];
    args[0] = x;
    args[1] = ten;
    Exp *addTen = makeAdd(2, args);
    return makeLet(var, ten, addTen);
}

Exp *makeTestExp2() {
    // (let (x (amb #f #t)) (if x 5 (back)))

    Exp *expFive = makeIntExp(5);
    AexpVar *var = newAexpVar("x");
    Exp *x = makeVar(var);
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

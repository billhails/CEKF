#include <stdbool.h>

#include "exp.h"
#include "memory.h"
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
        int save = PROTECT(acc);
        acc = newAexpList(acc, argv[argc]);
        UNPROTECT(save);
    }
    return acc;
}

Exp *makeAdd(int argc, Exp *argv[]) {
    AexpList *args = makeList(argc, argv);
    int save = PROTECT(args);
    AexpPrimApp *prim = newAexpPrimApp(AEXP_PRIM_ADD, args);
    PROTECT(prim);
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
    Exp *x = makeVar(var);
    PROTECT(x);
    Exp *args[2];
    args[0] = x;
    args[1] = ten;
    Exp *addTen = makeAdd(2, args);
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
    Exp *x = makeVar(var);
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

int main(int argc, char *argv[]) {
    Exp *exp = makeTestExp2();
    run(exp);
}

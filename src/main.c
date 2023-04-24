#include "exp.h"
#include "step.h"

Exp *makeTestExp() {
    Aexp *aexp = newAexp(AEXP_TYPE_INT, AEXP_VAL_INT(10));
    return newExp(EXP_TYPE_AEXP, EXP_VAL_AEXP(aexp));
}

int main(int argc, char *argv[]) {
    Exp *exp = makeTestExp();
    run(exp);
}

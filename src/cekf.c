#include "cekf.h"
#include "memory.h"

/*
 * memory allocation functions for the CEKF machine
 */

Value *newValue(ValueType type, ValueVal val) {
    Value *x = NEW(Value);
    x->type = type;
    x->val = val;
    return x;
}

ValueList *newValueList(ValueList *next, Value *value) {
    ValueList *x = NEW(ValueList);
    x->next = next;
    x->value = value;
    return x;
}

Clo *newClo(AexpLam *lam, Env *rho) {
    Clo *x = NEW(Clo);
    x->lam = lam;
    x->rho = rho;
    return x;
}

Env *newEnv(Env *next, AexpVar *var, Value *val) {
    Env *x = NEW(Env);
    x->next = next;
    x->var = var;
    x->val = val;
    return x;
}

Kont *newKont(KontType type, KontVal val) {
    Kont *x = NEW(Kont);
    x->type = type;
    x->val = val;
    return x;
}

LetK *newLetK(AexpVar *var, Exp *body, Env *rho, Kont *k) {
    LetK *x = NEW(LetK);
    x->var = var;
    x->body = body;
    x->rho = rho;
    x->k = k;
    return x;
}

Fail *newFail(FailType type, FailVal val) {
    Fail *x = NEW(Fail);
    x->type = type;
    x->val = val;
    return x;
}

Back *newBack(Exp *exp, Env *rho, Kont *k, Fail *f) {
    Back *x = NEW(Back);
    x->exp = exp;
    x->rho = rho;
    x->k = k;
    x->f = f;
    return x;
}

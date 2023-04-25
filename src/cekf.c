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

Kont *newKont(AexpVar *var, Exp *body, Env *rho, Kont *next) {
    Kont *x = NEW(Kont);
    x->var = var;
    x->body = body;
    x->rho = rho;
    x->next = next;
    return x;
}

Fail *newFail(Exp *exp, Env *rho, Kont *k, Fail *next) {
    Fail *x = NEW(Fail);
    x->exp = exp;
    x->rho = rho;
    x->k = k;
    x->next = next;
    return x;
}

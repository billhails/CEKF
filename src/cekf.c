#include "cekf.h"
#include "memory.h"

/*
 * memory allocation functions for the CEKF machine
 */

Value *newValue(ValueType type, ValueVal val) {
    Value *x = NEW(Value, OBJTYPE_VALUE);
    x->type = type;
    x->val = val;
    return x;
}

ValueList *newValueList(ValueList *next, Value *value) {
    ValueList *x = NEW(ValueList, OBJTYPE_VALUELIST);
    x->next = next;
    x->value = value;
    return x;
}

Clo *newClo(AexpLam *lam, Env *rho) {
    Clo *x = NEW(Clo, OBJTYPE_CLO);
    x->lam = lam;
    x->rho = rho;
    return x;
}

Env *newEnv(Env *next, AexpVar *var, Value *val) {
    Env *x = NEW(Env, OBJTYPE_ENV);
    x->next = next;
    x->var = var;
    x->val = val;
    return x;
}

Kont *newKont(AexpVar *var, Exp *body, Env *rho, Kont *next) {
    Kont *x = NEW(Kont, OBJTYPE_KONT);
    x->var = var;
    x->body = body;
    x->rho = rho;
    x->next = next;
    return x;
}

Fail *newFail(Exp *exp, Env *rho, Kont *k, Fail *next) {
    Fail *x = NEW(Fail, OBJTYPE_FAIL);
    x->exp = exp;
    x->rho = rho;
    x->k = k;
    x->next = next;
    return x;
}

void markValue(Value *x) {
    if (x == NULL) return;
    if (MARKED(x)) return;
    MARK(x);
    switch (x->type) {
        case VALUE_TYPE_VOID:
        case VALUE_TYPE_INTEGER:
        case VALUE_TYPE_TRUE:
        case VALUE_TYPE_FALSE:
            break;
        case VALUE_TYPE_CLO:
            markClo(x->val.clo);
            break;
        case VALUE_TYPE_CONT:
            markKont(x->val.k);
            break;
    }
}

void markValueList(ValueList *x) {
    if (x == NULL) return;
    if (MARKED(x)) return;
    MARK(x);
    markValueList(x->next);
    markValue(x->value);
}

void markClo(Clo *x) {
    if (x == NULL) return;
    if (MARKED(x)) return;
    MARK(x);
    markAexpLam(x->lam);
    markEnv(x->rho);
}

void markEnv(Env *x) {
    if (x == NULL) return;
    if (MARKED(x)) return;
    MARK(x);
    markEnv(x->next);
    markAexpVar(x->var);
    markValue(x->val);
}

void markKont(Kont *x) {
    if (x == NULL) return;
    if (MARKED(x)) return;
    MARK(x);
    markAexpVar(x->var);
    markExp(x->body);
    markEnv(x->rho);
    markKont(x->next);
}

void markFail(Fail *x) {
    if (x == NULL) return;
    if (MARKED(x)) return;
    MARK(x);
    markExp(x->exp);
    markEnv(x->rho);
    markKont(x->k);
    markFail(x->next);
}

void markCekfObj(Header *h) {
    switch (h->type) {
        case OBJTYPE_CLO:
            markClo((Clo *)h);
            break;
        case OBJTYPE_ENV:
            markEnv((Env *)h);
            break;
        case OBJTYPE_FAIL:
            markFail((Fail *)h);
            break;
        case OBJTYPE_KONT:
            markKont((Kont *)h);
            break;
        case OBJTYPE_VALUE:
            markValue((Value *)h);
            break;
        case OBJTYPE_VALUELIST:
            markValueList((ValueList *)h);
            break;
    }
}

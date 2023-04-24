#include "exp.h"
#include "memory.h"

AexpLam *newAexpLam(AexpVarList *args, Exp *exp) {
    AexpLam *x = NEW(AexpLam);
    x->args = args;
    x->exp = exp;
    return x;
}

AexpVarList *newAexpVarList(AexpVarList *next, AexpVar *var) {
    AexpVarList *x = NEW(AexpVarList);
    x->next = next;
    x->var = var;
    return x;
}

AexpVar *newAexpVar(char *name) {
    AexpVar *x = NEW(AexpVar);
    x->name = name;
    return x;
}

Aexp *newAexp(AexpType type, AexpVal val) {
    Aexp *x = NEW(Aexp);
    x->type = type;
    x->val = val;
    return x;
}

AexpPrimApp *newAexpPrimApp(AexpPrimOp op, AexpList *args) {
    AexpPrimApp *x = NEW(AexpPrimApp);
    x->op = op;
    x->args = args;
    return x;
}

AexpList *newAexpList(AexpList *next, Aexp *exp) {
    AexpList *x = NEW(AexpList);
    x->next = next;
    x->exp = exp;
    return x;
}

Cexp *newCexp(CexpType type, CexpVal val) {
    Cexp *x = NEW(Cexp);
    x->type = type;
    x->val = val;
    return x;
}

CexpApply *newCexpApply(Aexp *function, AexpList *args) {
    CexpApply *x = NEW(CexpApply);
    x->function = function;
    x->args = args;
    return x;
}

CexpConditional *newCexpConditional(Aexp *condition, Exp *consequent, Exp *alternative) {
    CexpConditional *x = NEW(CexpConditional);
    x->condition = condition;
    x->consequent = consequent;
    x->alternative = alternative;
    return x;
}

CexpLetRec *newCexpLetRec(LetRecBindings *bindings, Exp *body) {
    CexpLetRec *x = NEW(CexpLetRec);
    x->bindings = bindings;
    x->body = body;
    return x;
}

LetRecBindings *newLetRecBindings(LetRecBindings *next, AexpVar *var, Aexp *val) {
    LetRecBindings *x = NEW(LetRecBindings);
    x->next = next;
    x->var = var;
    x->val = val;
    return x;
}

CexpAmb *newCexpAmb(Exp *exp1, Exp *exp2) {
    CexpAmb *x = NEW(CexpAmb);
    x->exp1 = exp1;
    x->exp2 = exp2;
    return x;
}

Exp *newExp(ExpType type, ExpVal val) {
    Exp *x = NEW(Exp);
    x->type = type;
    x->val = val;
    return x;
}

ExpLet *newExpLet(AexpVar *var, Exp *val, Exp *body) {
    ExpLet *x = NEW(ExpLet);
    x->var = var;
    x->val = val;
    x->body = body;
    return x;
}


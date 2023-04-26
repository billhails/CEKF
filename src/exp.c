#include "exp.h"
#include "memory.h"

AexpLam *newAexpLam(AexpVarList *args, Exp *exp) {
    AexpLam *x = NEW(AexpLam, OBJTYPE_LAM);
    x->args = args;
    x->exp = exp;
    return x;
}

AexpVarList *newAexpVarList(AexpVarList *next, AexpVar *var) {
    AexpVarList *x = NEW(AexpVarList, OBJTYPE_VARLIST);
    x->next = next;
    x->var = var;
    return x;
}

AexpVar *newAexpVar(char *name) {
    AexpVar *x = NEW(AexpVar, OBJTYPE_VAR);
    x->name = name;
    return x;
}

AexpPrimApp *newAexpPrimApp(AexpPrimOp op, AexpList *args) {
    AexpPrimApp *x = NEW(AexpPrimApp, OBJTYPE_PRIMAPP);
    x->op = op;
    x->args = args;
    return x;
}

AexpList *newAexpList(AexpList *next, Exp *exp) {
    AexpList *x = NEW(AexpList, OBJTYPE_EXPLIST);
    x->next = next;
    x->exp = exp;
    return x;
}

CexpApply *newCexpApply(Exp *function, AexpList *args) {
    CexpApply *x = NEW(CexpApply, OBJTYPE_APPLY);
    x->function = function;
    x->args = args;
    return x;
}

CexpCond *newCexpCond(Exp *condition, Exp *consequent, Exp *alternative) {
    CexpCond *x = NEW(CexpCond, OBJTYPE_COND);
    x->condition = condition;
    x->consequent = consequent;
    x->alternative = alternative;
    return x;
}

CexpLetRec *newCexpLetRec(LetRecBindings *bindings, Exp *body) {
    CexpLetRec *x = NEW(CexpLetRec, OBJTYPE_LETREC);
    x->bindings = bindings;
    x->body = body;
    return x;
}

LetRecBindings *newLetRecBindings(LetRecBindings *next, AexpVar *var, Exp *val) {
    LetRecBindings *x = NEW(LetRecBindings, OBJTYPE_BINDINGS);
    x->next = next;
    x->var = var;
    x->val = val;
    return x;
}

CexpAmb *newCexpAmb(Exp *exp1, Exp *exp2) {
    CexpAmb *x = NEW(CexpAmb, OBJTYPE_AMB);
    x->exp1 = exp1;
    x->exp2 = exp2;
    return x;
}

Exp *newExp(ExpType type, ExpVal val) {
    Exp *x = NEW(Exp, OBJTYPE_EXP);
    x->type = type;
    x->val = val;
    return x;
}

ExpLet *newExpLet(AexpVar *var, Exp *val, Exp *body) {
    ExpLet *x = NEW(ExpLet, OBJTYPE_LET);
    x->var = var;
    x->val = val;
    x->body = body;
    return x;
}

void markAexpLam(AexpLam *x) {
    if (x == NULL) return;
    if (MARKED(x)) return;
    MARK(x);
    markAexpVarList(x->args);
    markExp(x->exp);
}

void markAexpVarList(AexpVarList *x) {
    if (x == NULL) return;
    if (MARKED(x)) return;
    MARK(x);
    markAexpVarList(x->next);
    markAexpVar(x->var);
}

void markAexpVar(AexpVar *x) {
    if (x == NULL) return;
    if (MARKED(x)) return;
    MARK(x);
}

void markAexpPrimApp(AexpPrimApp *x) {
    if (x == NULL) return;
    if (MARKED(x)) return;
    MARK(x);
    markAexpList(x->args);
}

void markAexpList(AexpList *x) {
    if (x == NULL) return;
    if (MARKED(x)) return;
    MARK(x);
    markAexpList(x->next);
    markExp(x->exp);
}

void markCexpApply(CexpApply *x) {
    if (x == NULL) return;
    if (MARKED(x)) return;
    MARK(x);
    markExp(x->function);
    markAexpList(x->args);
}

void markCexpCond(CexpCond *x) {
    if (x == NULL) return;
    if (MARKED(x)) return;
    MARK(x);
    markExp(x->condition);
    markExp(x->consequent);
    markExp(x->alternative);
}

void markCexpLetRec(CexpLetRec *x) {
    if (x == NULL) return;
    if (MARKED(x)) return;
    MARK(x);
    markLetRecBindings(x->bindings);
    markExp(x->body);
}

void markLetRecBindings(LetRecBindings *x) {
    if (x == NULL) return;
    if (MARKED(x)) return;
    MARK(x);
    markLetRecBindings(x->next);
    markAexpVar(x->var);
    markExp(x->val);
}

void markCexpAmb(CexpAmb *x) {
    if (x == NULL) return;
    if (MARKED(x)) return;
    MARK(x);
    markExp(x->exp1);
    markExp(x->exp2);
}

void markExpLet(ExpLet *x) {
    if (x == NULL) return;
    if (MARKED(x)) return;
    MARK(x);
    markAexpVar(x->var);
    markExp(x->val);
    markExp(x->body);
}

void markExp(Exp *x) {
    if (x == NULL) return;
    if (MARKED(x)) return;
    MARK(x);
    switch (x->type) {
        case AEXP_TYPE_LAM:
            markAexpLam(x->val.aexp.lam);
            break;
        case AEXP_TYPE_VAR:
            markAexpVar(x->val.aexp.var);
            break;
        case AEXP_TYPE_TRUE:
        case AEXP_TYPE_FALSE:
        case AEXP_TYPE_INT:
            break;
        case AEXP_TYPE_PRIM:
            markAexpPrimApp(x->val.aexp.prim);
            break;
        case CEXP_TYPE_APPLY:
            markCexpApply(x->val.cexp.apply);
            break;
        case CEXP_TYPE_COND:
            markCexpCond(x->val.cexp.cond);
            break;
        case CEXP_TYPE_CALLCC:
            markExp(x->val.cexp.callCC);
            break;
        case CEXP_TYPE_LETREC:
            markCexpLetRec(x->val.cexp.letRec);
            break;
        case CEXP_TYPE_AMB:
            markCexpAmb(x->val.cexp.amb);
            break;
        case CEXP_TYPE_BACK:
            break;
        case EXP_TYPE_LET:
            markExpLet(x->val.let);
            break;
        case EXP_TYPE_DONE:
            break;
    }
}

void markExpObj(Header *h) {
    switch (h->type) {
        case OBJTYPE_AMB:
            markCexpAmb((CexpAmb *) h);
            break;
        case OBJTYPE_APPLY:
            markCexpApply((CexpApply *) h);
            break;
        case OBJTYPE_BINDINGS:
            markLetRecBindings((LetRecBindings *) h);
            break;
        case OBJTYPE_COND:
            markCexpCond((CexpCond *) h);
            break;
        case OBJTYPE_EXP:
            markExp((Exp *) h);
            break;
        case OBJTYPE_EXPLIST:
            markAexpList((AexpList *) h);
            break;
        case OBJTYPE_LAM:
            markAexpLam((AexpLam *) h);
            break;
        case OBJTYPE_LET:
            markExpLet((ExpLet *) h);
            break;
        case OBJTYPE_LETREC:
            markCexpLetRec((CexpLetRec *) h);
            break;
        case OBJTYPE_PRIMAPP:
            markAexpPrimApp((AexpPrimApp *) h);
            break;
        case OBJTYPE_VAR:
            markAexpVar((AexpVar *) h);
            break;
        case OBJTYPE_VARLIST:
            markAexpVarList((AexpVarList *) h);
            break;
    }
}

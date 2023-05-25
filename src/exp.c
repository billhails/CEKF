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

#include "exp.h"
#include "hash.h"
#include "memory.h"
#include "symbol.h"

static int countAexpVarList(AexpVarList *list) {
    int count = 0;
    while (list != NULL) {
        count++;
        list = list->next;
    }
    return count;
}

static int countAexpList(AexpList *list) {
    int count = 0;
    while (list != NULL) {
        count++;
        list = list->next;
    }
    return count;
}

static int countLetRecBindings(LetRecBindings *list) {
    int count = 0;
    while (list != NULL) {
        count++;
        list = list->next;
    }
    return count;
}

AexpLam *newAexpLam(AexpVarList *args, Exp *exp) {
    AexpLam *x = NEW(AexpLam, OBJTYPE_LAM);
    x->nargs = countAexpVarList(args);
    x->letRecOffset = 0;
    x->args = args;
    x->exp = exp;
    return x;
}

AexpVarList *newAexpVarList(AexpVarList *next, HashSymbol *var) {
    AexpVarList *x = NEW(AexpVarList, OBJTYPE_VARLIST);
    x->next = next;
    x->var = var;
    return x;
}

HashSymbol *newAexpVar(char *name) {
    return newSymbol(name, 0);
}

AexpAnnotatedVar *newAexpAnnotatedVar(AexpAnnotatedVarType type, int frame, int offset, HashSymbol *var) {
    AexpAnnotatedVar *x = NEW(AexpAnnotatedVar, OBJTYPE_ANNOTATEDVAR);
    x->type = type;
    x->frame = frame;
    x->offset = offset;
    x->var = var;
    return x;
}

AexpPrimApp *newAexpPrimApp(AexpPrimOp op, Aexp *exp1, Aexp *exp2) {
    AexpPrimApp *x = NEW(AexpPrimApp, OBJTYPE_PRIMAPP);
    x->op = op;
    x->exp1 = exp1;
    x->exp2 = exp2;
    return x;
}

AexpUnaryApp *newAexpUnaryApp(AexpUnaryOp op, Aexp *exp) {
    AexpUnaryApp *x = NEW(AexpUnaryApp, OBJTYPE_UNARYAPP);
    x->op = op;
    x->exp = exp;
    return x;
}

AexpList *newAexpList(AexpList *next, Aexp *exp) {
    AexpList *x = NEW(AexpList, OBJTYPE_EXPLIST);
    x->next = next;
    x->exp = exp;
    return x;
}

CexpApply *newCexpApply(Aexp *function, AexpList *args) {
    CexpApply *x = NEW(CexpApply, OBJTYPE_APPLY);
    x->function = function;
    x->nargs = countAexpList(args);
    x->args = args;
    return x;
}

CexpCond *newCexpCond(Aexp *condition, Exp *consequent, Exp *alternative) {
    CexpCond *x = NEW(CexpCond, OBJTYPE_COND);
    x->condition = condition;
    x->consequent = consequent;
    x->alternative = alternative;
    return x;
}

CexpLetRec *newCexpLetRec(LetRecBindings *bindings, Exp *body) {
    CexpLetRec *x = NEW(CexpLetRec, OBJTYPE_LETREC);
    x->nbindings = countLetRecBindings(bindings);
    x->bindings = bindings;
    x->body = body;
    return x;
}

LetRecBindings *newLetRecBindings(LetRecBindings *next, HashSymbol *var, Aexp *val) {
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

CexpBool *newCexpBool(CexpBoolType type, Exp *exp1, Exp *exp2) {
    CexpBool *x = NEW(CexpBool, OBJTYPE_BOOL);
    x->type = type;
    x->exp1 = exp1;
    x->exp2 = exp2;
    return x;
}

Aexp *newAexp(AexpType type, AexpVal val) {
    Aexp *x = NEW(Aexp, OBJTYPE_AEXP);
    x->type = type;
    x->val = val;
    return x;
}

Cexp *newCexp(CexpType type, CexpVal val) {
    Cexp *x = NEW(Cexp, OBJTYPE_CEXP);
    x->type = type;
    x->val = val;
    return x;
}

Exp *newExp(ExpType type, ExpVal val) {
    Exp *x = NEW(Exp, OBJTYPE_EXP);
    x->type = type;
    x->val = val;
    return x;
}

ExpLet *newExpLet(HashSymbol *var, Exp *val, Exp *body) {
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
    markHashSymbol(x->var);
}

void markAexpVar(HashSymbol *x) {
    if (x == NULL) return;
    if (MARKED(x)) return;
    MARK(x);
}

void markAexpAnnotatedVar(AexpAnnotatedVar *x) {
    if (x == NULL) return;
    if (MARKED(x)) return;
    MARK(x);
    markAexpVar(x->var);
}

void markAexpPrimApp(AexpPrimApp *x) {
    if (x == NULL) return;
    if (MARKED(x)) return;
    MARK(x);
    markAexp(x->exp1);
    markAexp(x->exp2);
}

void markAexpUnaryApp(AexpUnaryApp *x) {
    if (x == NULL) return;
    if (MARKED(x)) return;
    MARK(x);
    markAexp(x->exp);
}

void markAexpList(AexpList *x) {
    if (x == NULL) return;
    if (MARKED(x)) return;
    MARK(x);
    markAexpList(x->next);
    markAexp(x->exp);
}

void markCexpApply(CexpApply *x) {
    if (x == NULL) return;
    if (MARKED(x)) return;
    MARK(x);
    markAexp(x->function);
    markAexpList(x->args);
}

void markCexpCond(CexpCond *x) {
    if (x == NULL) return;
    if (MARKED(x)) return;
    MARK(x);
    markAexp(x->condition);
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
    markAexp(x->val);
}

void markCexpAmb(CexpAmb *x) {
    if (x == NULL) return;
    if (MARKED(x)) return;
    MARK(x);
    markExp(x->exp1);
    markExp(x->exp2);
}

void markCexpBool(CexpBool *x) {
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

void markAexp(Aexp *x) {
    if (x == NULL) return;
    if (MARKED(x)) return;
    MARK(x);
    switch (x->type) {
        case AEXP_TYPE_LAM:
            markAexpLam(x->val.lam);
            break;
        case AEXP_TYPE_VAR:
            markAexpVar(x->val.var);
            break;
        case AEXP_TYPE_ANNOTATEDVAR:
            markAexpAnnotatedVar(x->val.annotatedVar);
            break;
        case AEXP_TYPE_TRUE:
        case AEXP_TYPE_FALSE:
        case AEXP_TYPE_INT:
        case AEXP_TYPE_VOID:
            break;
        case AEXP_TYPE_PRIM:
            markAexpPrimApp(x->val.prim);
            break;
        case AEXP_TYPE_UNARY:
            markAexpUnaryApp(x->val.unary);
            break;
        case AEXP_TYPE_LIST:
            markAexpList(x->val.list);
            break;
        default:
            cant_happen("unrecognised aexp type in markAexp");
    }
}

void markCexp(Cexp *x) {
    if (x == NULL) return;
    if (MARKED(x)) return;
    MARK(x);
    switch (x->type) {
        case CEXP_TYPE_APPLY:
            markCexpApply(x->val.apply);
            break;
        case CEXP_TYPE_COND:
            markCexpCond(x->val.cond);
            break;
        case CEXP_TYPE_CALLCC:
            markAexp(x->val.callCC);
            break;
        case CEXP_TYPE_LETREC:
            markCexpLetRec(x->val.letRec);
            break;
        case CEXP_TYPE_AMB:
            markCexpAmb(x->val.amb);
            break;
        case CEXP_TYPE_BOOL:
            markCexpBool(x->val.boolean);
            break;
        case CEXP_TYPE_BACK:
            break;
        default:
            cant_happen("unrecognised cexp type in markCexp");
    }
}

void markExp(Exp *x) {
    if (x == NULL) return;
    if (MARKED(x)) return;
    MARK(x);
    switch (x->type) {
        case EXP_TYPE_AEXP:
            markAexp(x->val.aexp);
            break;
        case EXP_TYPE_CEXP:
            markCexp(x->val.cexp);
            break;
        case EXP_TYPE_LET:
            markExpLet(x->val.let);
            break;
        case EXP_TYPE_DONE:
            break;
        default:
            cant_happen("unrecognised exp type in markExp");
    }
}

void freeExpObj(Header *h) {
    if (h == NULL) return;
    switch (h->type) {
        case OBJTYPE_BOOL:
            FREE(h, CexpAmb);
            break;
        case OBJTYPE_AMB:
            FREE(h, CexpAmb);
            break;
        case OBJTYPE_APPLY:
            FREE(h, CexpApply);
            break;
        case OBJTYPE_BINDINGS:
            FREE(h, LetRecBindings);
            break;
        case OBJTYPE_COND:
            FREE(h, CexpCond);
            break;
        case OBJTYPE_AEXP:
            FREE(h, Aexp);
            break;
        case OBJTYPE_CEXP:
            FREE(h, Cexp);
            break;
        case OBJTYPE_EXP:
            FREE(h, Exp);
            break;
        case OBJTYPE_EXPLIST:
            FREE(h, AexpList);
            break;
        case OBJTYPE_LAM:
            FREE(h, AexpLam);
            break;
        case OBJTYPE_LET:
            FREE(h, ExpLet);
            break;
        case OBJTYPE_LETREC:
            FREE(h, CexpLetRec);
            break;
        case OBJTYPE_PRIMAPP:
            FREE(h, AexpPrimApp);
            break;
        case OBJTYPE_UNARYAPP:
            FREE(h, AexpUnaryApp);
            break;
        case OBJTYPE_ANNOTATEDVAR:
            FREE(h, AexpAnnotatedVar);
            break;
        case OBJTYPE_VARLIST:
            FREE(h, AexpVarList);
            break;
        default:
            cant_happen("unrecognised header type in freeExpObj");
    }
}

void markExpObj(Header *h) {
    if (h == NULL) return;
    switch (h->type) {
        case OBJTYPE_AMB:
            markCexpAmb((CexpAmb *) h);
            break;
        case OBJTYPE_BOOL:
            markCexpBool((CexpBool *) h);
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
        case OBJTYPE_AEXP:
            markAexp((Aexp *) h);
            break;
        case OBJTYPE_CEXP:
            markCexp((Cexp *) h);
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
        case OBJTYPE_ANNOTATEDVAR:
            markAexpAnnotatedVar((AexpAnnotatedVar *) h);
            break;
        case OBJTYPE_VARLIST:
            markAexpVarList((AexpVarList *) h);
            break;
        default:
            cant_happen("unrecognised header type in markExpObj");
    }
}

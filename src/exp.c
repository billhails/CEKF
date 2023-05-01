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

static HashTable varTable;

AexpVar *newAexpVar(char *name) {
    AexpVar *x;
    x = hashGetVar(&varTable, name);
    if (x != NULL) {
        return x;
    }
    x = NEW(AexpVar, OBJTYPE_VAR);
    x->name = name;
    x->hash = hashString(name);
    int save = PROTECT(x);
    hashSet(&varTable, x, vVoid);
    UNPROTECT(save);
    return x;
}

AexpAnnotatedVar *newAexpAnnotatedVar(int frame, int offset, AexpVar *var) {
    AexpAnnotatedVar *x = NEW(AexpAnnotatedVar, OBJTYPE_ANNOTATEDVAR);
    x->frame = frame;
    x->offset = offset;
    x->var = var;
    return x;
}

AexpPrimApp *newAexpPrimApp(AexpPrimOp op, Exp *exp1, Exp *exp2) {
    AexpPrimApp *x = NEW(AexpPrimApp, OBJTYPE_PRIMAPP);
    x->op = op;
    x->exp1 = exp1;
    x->exp2 = exp2;
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
    x->nargs = countAexpList(args);
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
    x->nbindings = countLetRecBindings(bindings);
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
    markExp(x->exp1);
    markExp(x->exp2);
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
        case AEXP_TYPE_ANNOTATEDVAR:
            markAexpAnnotatedVar(x->val.aexp.annotatedVar);
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

void freeExpObj(Header *h) {
    if (h == NULL) return;
    Exp *x = (Exp *)h;
    switch (x->type) {
        case AEXP_TYPE_LAM:
        case AEXP_TYPE_VAR:
        case AEXP_TYPE_ANNOTATEDVAR:
        case AEXP_TYPE_TRUE:
        case AEXP_TYPE_FALSE:
        case AEXP_TYPE_INT:
        case AEXP_TYPE_PRIM:
        case CEXP_TYPE_APPLY:
        case CEXP_TYPE_COND:
        case CEXP_TYPE_CALLCC:
        case CEXP_TYPE_LETREC:
        case CEXP_TYPE_AMB:
        case CEXP_TYPE_BACK:
        case EXP_TYPE_LET:
        case EXP_TYPE_DONE:
            FREE(x, Exp);
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
        case OBJTYPE_ANNOTATEDVAR:
            markAexpAnnotatedVar((AexpAnnotatedVar *) h);
            break;
        case OBJTYPE_VARLIST:
            markAexpVarList((AexpVarList *) h);
            break;
    }
}

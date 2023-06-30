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

// generated from src/lambda.yaml by makeAST.py



#include "lambda.h"

struct LamLam * newLamLam(int nargs, struct LamVarList * args, struct LamExp * exp) {
    struct LamLam * x = NEW(LamLam, OBJTYPE_LAMLAM);
    x->nargs = nargs;
    x->args = args;
    x->exp = exp;
    return x;
}

struct LamVarList * newLamVarList(HashSymbol * var, struct LamVarList * next) {
    struct LamVarList * x = NEW(LamVarList, OBJTYPE_LAMVARLIST);
    x->var = var;
    x->next = next;
    return x;
}

struct LamPrimApp * newLamPrimApp(enum LamPrimOp  type, struct LamExp * exp1, struct LamExp * exp2) {
    struct LamPrimApp * x = NEW(LamPrimApp, OBJTYPE_LAMPRIMAPP);
    x->type = type;
    x->exp1 = exp1;
    x->exp2 = exp2;
    return x;
}

struct LamUnaryApp * newLamUnaryApp(enum LamUnaryOp  type, struct LamExp * exp) {
    struct LamUnaryApp * x = NEW(LamUnaryApp, OBJTYPE_LAMUNARYAPP);
    x->type = type;
    x->exp = exp;
    return x;
}

struct LamList * newLamList(struct LamExp * exp, struct LamList * next) {
    struct LamList * x = NEW(LamList, OBJTYPE_LAMLIST);
    x->exp = exp;
    x->next = next;
    return x;
}

struct LamApply * newLamApply(struct LamExp * function, int nargs, struct LamList * args) {
    struct LamApply * x = NEW(LamApply, OBJTYPE_LAMAPPLY);
    x->function = function;
    x->nargs = nargs;
    x->args = args;
    return x;
}

struct LamMakeVec * newLamMakeVec(int nargs, struct LamList * args) {
    struct LamMakeVec * x = NEW(LamMakeVec, OBJTYPE_LAMMAKEVEC);
    x->nargs = nargs;
    x->args = args;
    return x;
}

struct LamCond * newLamCond(struct LamExp * condition, struct LamExp * consequent, struct LamExp * alternative) {
    struct LamCond * x = NEW(LamCond, OBJTYPE_LAMCOND);
    x->condition = condition;
    x->consequent = consequent;
    x->alternative = alternative;
    return x;
}

struct LamMatch * newLamMatch(struct LamExp * index, struct LamMatchList * cases) {
    struct LamMatch * x = NEW(LamMatch, OBJTYPE_LAMMATCH);
    x->index = index;
    x->cases = cases;
    return x;
}

struct LamMatchList * newLamMatchList(struct LamList * matches, struct LamExp * body, struct LamMatchList * next) {
    struct LamMatchList * x = NEW(LamMatchList, OBJTYPE_LAMMATCHLIST);
    x->matches = matches;
    x->body = body;
    x->next = next;
    return x;
}

struct LamLetRec * newLamLetRec(int nbindings, struct LamLetRecBindings * bindings, struct LamList * body) {
    struct LamLetRec * x = NEW(LamLetRec, OBJTYPE_LAMLETREC);
    x->nbindings = nbindings;
    x->bindings = bindings;
    x->body = body;
    return x;
}

struct LamLetRecBindings * newLamLetRecBindings(HashSymbol * var, struct LamExp * val, struct LamLetRecBindings * next) {
    struct LamLetRecBindings * x = NEW(LamLetRecBindings, OBJTYPE_LAMLETRECBINDINGS);
    x->var = var;
    x->val = val;
    x->next = next;
    return x;
}

struct LamContext * newLamContext(HashTable * frame, struct LamContext * parent) {
    struct LamContext * x = NEW(LamContext, OBJTYPE_LAMCONTEXT);
    x->frame = frame;
    x->parent = parent;
    return x;
}

struct LamTypeConstructorInfo * newLamTypeConstructorInfo(int nargs, int index) {
    struct LamTypeConstructorInfo * x = NEW(LamTypeConstructorInfo, OBJTYPE_LAMTYPECONSTRUCTORINFO);
    x->nargs = nargs;
    x->index = index;
    return x;
}

struct LamExp * newLamExp(enum LamExpType  type, union LamExpVal  val) {
    struct LamExp * x = NEW(LamExp, OBJTYPE_LAMEXP);
    x->type = type;
    x->val = val;
    return x;
}


/************************************/

void markLamLam(struct LamLam * x) {
    if (x == NULL) return;
    if (MARKED(x)) return;
    MARK(x);
    markLamVarList(x->args);
    markLamExp(x->exp);
}

void markLamVarList(struct LamVarList * x) {
    if (x == NULL) return;
    if (MARKED(x)) return;
    MARK(x);
    markHashSymbol(x->var);
    markLamVarList(x->next);
}

void markLamPrimApp(struct LamPrimApp * x) {
    if (x == NULL) return;
    if (MARKED(x)) return;
    MARK(x);
    markLamExp(x->exp1);
    markLamExp(x->exp2);
}

void markLamUnaryApp(struct LamUnaryApp * x) {
    if (x == NULL) return;
    if (MARKED(x)) return;
    MARK(x);
    markLamExp(x->exp);
}

void markLamList(struct LamList * x) {
    if (x == NULL) return;
    if (MARKED(x)) return;
    MARK(x);
    markLamExp(x->exp);
    markLamList(x->next);
}

void markLamApply(struct LamApply * x) {
    if (x == NULL) return;
    if (MARKED(x)) return;
    MARK(x);
    markLamExp(x->function);
    markLamList(x->args);
}

void markLamMakeVec(struct LamMakeVec * x) {
    if (x == NULL) return;
    if (MARKED(x)) return;
    MARK(x);
    markLamList(x->args);
}

void markLamCond(struct LamCond * x) {
    if (x == NULL) return;
    if (MARKED(x)) return;
    MARK(x);
    markLamExp(x->condition);
    markLamExp(x->consequent);
    markLamExp(x->alternative);
}

void markLamMatch(struct LamMatch * x) {
    if (x == NULL) return;
    if (MARKED(x)) return;
    MARK(x);
    markLamExp(x->index);
    markLamMatchList(x->cases);
}

void markLamMatchList(struct LamMatchList * x) {
    if (x == NULL) return;
    if (MARKED(x)) return;
    MARK(x);
    markLamList(x->matches);
    markLamExp(x->body);
    markLamMatchList(x->next);
}

void markLamLetRec(struct LamLetRec * x) {
    if (x == NULL) return;
    if (MARKED(x)) return;
    MARK(x);
    markLamLetRecBindings(x->bindings);
    markLamList(x->body);
}

void markLamLetRecBindings(struct LamLetRecBindings * x) {
    if (x == NULL) return;
    if (MARKED(x)) return;
    MARK(x);
    markHashSymbol(x->var);
    markLamExp(x->val);
    markLamLetRecBindings(x->next);
}

void markLamContext(struct LamContext * x) {
    if (x == NULL) return;
    if (MARKED(x)) return;
    MARK(x);
    markHashTable(x->frame);
    markLamContext(x->parent);
}

void markLamTypeConstructorInfo(struct LamTypeConstructorInfo * x) {
    if (x == NULL) return;
    if (MARKED(x)) return;
    MARK(x);
}

void markLamExp(struct LamExp * x) {
    if (x == NULL) return;
    if (MARKED(x)) return;
    MARK(x);
    switch(x->type) {
        case LAMEXP_TYPE_LAM:
            markLamLam(x->val.lam);
            break;
        case LAMEXP_TYPE_VAR:
            markHashSymbol(x->val.var);
            break;
        case LAMEXP_TYPE_INTEGER:
            break;
        case LAMEXP_TYPE_PRIM:
            markLamPrimApp(x->val.prim);
            break;
        case LAMEXP_TYPE_UNARY:
            markLamUnaryApp(x->val.unary);
            break;
        case LAMEXP_TYPE_LIST:
            markLamList(x->val.list);
            break;
        case LAMEXP_TYPE_MAKEVEC:
            markLamMakeVec(x->val.makeVec);
            break;
        case LAMEXP_TYPE_APPLY:
            markLamApply(x->val.apply);
            break;
        case LAMEXP_TYPE_COND:
            markLamCond(x->val.cond);
            break;
        case LAMEXP_TYPE_CALLCC:
            markLamExp(x->val.callcc);
            break;
        case LAMEXP_TYPE_LETREC:
            markLamLetRec(x->val.letrec);
            break;
        case LAMEXP_TYPE_MATCH:
            markLamMatch(x->val.match);
            break;
        case LAMEXP_TYPE_CHARACTER:
            break;
        case LAMEXP_TYPE_STRING:
            break;
        case LAMEXP_TYPE_BACK:
            break;
        case LAMEXP_TYPE_T:
            break;
        case LAMEXP_TYPE_F:
            break;
        case LAMEXP_TYPE_NIL:
            break;
        default:
            cant_happen("unrecognised type %d in markLamExp", x->type);
    }
}


void markLambdaObj(struct Header *h) {
    switch(h->type) {
        case OBJTYPE_LAMLAM:
            markLamLam((LamLam *)h);
            break;
        case OBJTYPE_LAMVARLIST:
            markLamVarList((LamVarList *)h);
            break;
        case OBJTYPE_LAMPRIMAPP:
            markLamPrimApp((LamPrimApp *)h);
            break;
        case OBJTYPE_LAMUNARYAPP:
            markLamUnaryApp((LamUnaryApp *)h);
            break;
        case OBJTYPE_LAMLIST:
            markLamList((LamList *)h);
            break;
        case OBJTYPE_LAMAPPLY:
            markLamApply((LamApply *)h);
            break;
        case OBJTYPE_LAMMAKEVEC:
            markLamMakeVec((LamMakeVec *)h);
            break;
        case OBJTYPE_LAMCOND:
            markLamCond((LamCond *)h);
            break;
        case OBJTYPE_LAMMATCH:
            markLamMatch((LamMatch *)h);
            break;
        case OBJTYPE_LAMMATCHLIST:
            markLamMatchList((LamMatchList *)h);
            break;
        case OBJTYPE_LAMLETREC:
            markLamLetRec((LamLetRec *)h);
            break;
        case OBJTYPE_LAMLETRECBINDINGS:
            markLamLetRecBindings((LamLetRecBindings *)h);
            break;
        case OBJTYPE_LAMCONTEXT:
            markLamContext((LamContext *)h);
            break;
        case OBJTYPE_LAMTYPECONSTRUCTORINFO:
            markLamTypeConstructorInfo((LamTypeConstructorInfo *)h);
            break;
        case OBJTYPE_LAMEXP:
            markLamExp((LamExp *)h);
            break;
    }
}

/************************************/

void freeLamLam(struct LamLam * x) {
    FREE(x, LamLam);
}

void freeLamVarList(struct LamVarList * x) {
    FREE(x, LamVarList);
}

void freeLamPrimApp(struct LamPrimApp * x) {
    FREE(x, LamPrimApp);
}

void freeLamUnaryApp(struct LamUnaryApp * x) {
    FREE(x, LamUnaryApp);
}

void freeLamList(struct LamList * x) {
    FREE(x, LamList);
}

void freeLamApply(struct LamApply * x) {
    FREE(x, LamApply);
}

void freeLamMakeVec(struct LamMakeVec * x) {
    FREE(x, LamMakeVec);
}

void freeLamCond(struct LamCond * x) {
    FREE(x, LamCond);
}

void freeLamMatch(struct LamMatch * x) {
    FREE(x, LamMatch);
}

void freeLamMatchList(struct LamMatchList * x) {
    FREE(x, LamMatchList);
}

void freeLamLetRec(struct LamLetRec * x) {
    FREE(x, LamLetRec);
}

void freeLamLetRecBindings(struct LamLetRecBindings * x) {
    FREE(x, LamLetRecBindings);
}

void freeLamContext(struct LamContext * x) {
    FREE(x, LamContext);
}

void freeLamTypeConstructorInfo(struct LamTypeConstructorInfo * x) {
    FREE(x, LamTypeConstructorInfo);
}

void freeLamExp(struct LamExp * x) {
    FREE(x, LamExp);
}


void freeLambdaObj(struct Header *h) {
    switch(h->type) {
        case OBJTYPE_LAMLAM:
            freeLamLam((LamLam *)h);
            break;
        case OBJTYPE_LAMVARLIST:
            freeLamVarList((LamVarList *)h);
            break;
        case OBJTYPE_LAMPRIMAPP:
            freeLamPrimApp((LamPrimApp *)h);
            break;
        case OBJTYPE_LAMUNARYAPP:
            freeLamUnaryApp((LamUnaryApp *)h);
            break;
        case OBJTYPE_LAMLIST:
            freeLamList((LamList *)h);
            break;
        case OBJTYPE_LAMAPPLY:
            freeLamApply((LamApply *)h);
            break;
        case OBJTYPE_LAMMAKEVEC:
            freeLamMakeVec((LamMakeVec *)h);
            break;
        case OBJTYPE_LAMCOND:
            freeLamCond((LamCond *)h);
            break;
        case OBJTYPE_LAMMATCH:
            freeLamMatch((LamMatch *)h);
            break;
        case OBJTYPE_LAMMATCHLIST:
            freeLamMatchList((LamMatchList *)h);
            break;
        case OBJTYPE_LAMLETREC:
            freeLamLetRec((LamLetRec *)h);
            break;
        case OBJTYPE_LAMLETRECBINDINGS:
            freeLamLetRecBindings((LamLetRecBindings *)h);
            break;
        case OBJTYPE_LAMCONTEXT:
            freeLamContext((LamContext *)h);
            break;
        case OBJTYPE_LAMTYPECONSTRUCTORINFO:
            freeLamTypeConstructorInfo((LamTypeConstructorInfo *)h);
            break;
        case OBJTYPE_LAMEXP:
            freeLamExp((LamExp *)h);
            break;
    }
}

char *typenameLambdaObj(int type) {
    switch(type) {
        case OBJTYPE_LAMLAM:
            return "LamLam";
        case OBJTYPE_LAMVARLIST:
            return "LamVarList";
        case OBJTYPE_LAMPRIMAPP:
            return "LamPrimApp";
        case OBJTYPE_LAMUNARYAPP:
            return "LamUnaryApp";
        case OBJTYPE_LAMLIST:
            return "LamList";
        case OBJTYPE_LAMAPPLY:
            return "LamApply";
        case OBJTYPE_LAMMAKEVEC:
            return "LamMakeVec";
        case OBJTYPE_LAMCOND:
            return "LamCond";
        case OBJTYPE_LAMMATCH:
            return "LamMatch";
        case OBJTYPE_LAMMATCHLIST:
            return "LamMatchList";
        case OBJTYPE_LAMLETREC:
            return "LamLetRec";
        case OBJTYPE_LAMLETRECBINDINGS:
            return "LamLetRecBindings";
        case OBJTYPE_LAMCONTEXT:
            return "LamContext";
        case OBJTYPE_LAMTYPECONSTRUCTORINFO:
            return "LamTypeConstructorInfo";
        case OBJTYPE_LAMEXP:
            return "LamExp";
    }
}


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
 *
 * Plain lambda structures generated by lambda conversion.
 *
 * generated from src/lambda.yaml by makeAST.py
 */

#include "lambda.h"
#include <stdio.h>
#include <strings.h>
#include "common.h"

struct LamLam * newLamLam(int nargs, struct LamVarList * args, struct LamExp * exp) {
    struct LamLam * x = NEW(LamLam, OBJTYPE_LAMLAM);
#ifdef DEBUG_ALLOC
    fprintf(stderr, "new LamLam %p\n", x);
#endif
    x->nargs = nargs;
    x->args = args;
    x->exp = exp;
    return x;
}

struct LamVarList * newLamVarList(HashSymbol * var, struct LamVarList * next) {
    struct LamVarList * x = NEW(LamVarList, OBJTYPE_LAMVARLIST);
#ifdef DEBUG_ALLOC
    fprintf(stderr, "new LamVarList %p\n", x);
#endif
    x->var = var;
    x->next = next;
    return x;
}

struct LamPrimApp * newLamPrimApp(enum LamPrimOp  type, struct LamExp * exp1, struct LamExp * exp2) {
    struct LamPrimApp * x = NEW(LamPrimApp, OBJTYPE_LAMPRIMAPP);
#ifdef DEBUG_ALLOC
    fprintf(stderr, "new LamPrimApp %p\n", x);
#endif
    x->type = type;
    x->exp1 = exp1;
    x->exp2 = exp2;
    return x;
}

struct LamUnaryApp * newLamUnaryApp(enum LamUnaryOp  type, struct LamExp * exp) {
    struct LamUnaryApp * x = NEW(LamUnaryApp, OBJTYPE_LAMUNARYAPP);
#ifdef DEBUG_ALLOC
    fprintf(stderr, "new LamUnaryApp %p\n", x);
#endif
    x->type = type;
    x->exp = exp;
    return x;
}

struct LamSequence * newLamSequence(struct LamExp * exp, struct LamSequence * next) {
    struct LamSequence * x = NEW(LamSequence, OBJTYPE_LAMSEQUENCE);
#ifdef DEBUG_ALLOC
    fprintf(stderr, "new LamSequence %p\n", x);
#endif
    x->exp = exp;
    x->next = next;
    return x;
}

struct LamList * newLamList(struct LamExp * exp, struct LamList * next) {
    struct LamList * x = NEW(LamList, OBJTYPE_LAMLIST);
#ifdef DEBUG_ALLOC
    fprintf(stderr, "new LamList %p\n", x);
#endif
    x->exp = exp;
    x->next = next;
    return x;
}

struct LamApply * newLamApply(struct LamExp * function, int nargs, struct LamList * args) {
    struct LamApply * x = NEW(LamApply, OBJTYPE_LAMAPPLY);
#ifdef DEBUG_ALLOC
    fprintf(stderr, "new LamApply %p\n", x);
#endif
    x->function = function;
    x->nargs = nargs;
    x->args = args;
    return x;
}

struct LamMakeVec * newLamMakeVec(int nargs, struct LamList * args) {
    struct LamMakeVec * x = NEW(LamMakeVec, OBJTYPE_LAMMAKEVEC);
#ifdef DEBUG_ALLOC
    fprintf(stderr, "new LamMakeVec %p\n", x);
#endif
    x->nargs = nargs;
    x->args = args;
    return x;
}

struct LamIff * newLamIff(struct LamExp * condition, struct LamExp * consequent, struct LamExp * alternative) {
    struct LamIff * x = NEW(LamIff, OBJTYPE_LAMIFF);
#ifdef DEBUG_ALLOC
    fprintf(stderr, "new LamIff %p\n", x);
#endif
    x->condition = condition;
    x->consequent = consequent;
    x->alternative = alternative;
    return x;
}

struct LamCond * newLamCond(struct LamExp * value, struct LamCondCases * cases) {
    struct LamCond * x = NEW(LamCond, OBJTYPE_LAMCOND);
#ifdef DEBUG_ALLOC
    fprintf(stderr, "new LamCond %p\n", x);
#endif
    x->value = value;
    x->cases = cases;
    return x;
}

struct LamCondCases * newLamCondCases(struct LamExp * constant, struct LamExp * body, struct LamCondCases * next) {
    struct LamCondCases * x = NEW(LamCondCases, OBJTYPE_LAMCONDCASES);
#ifdef DEBUG_ALLOC
    fprintf(stderr, "new LamCondCases %p\n", x);
#endif
    x->constant = constant;
    x->body = body;
    x->next = next;
    return x;
}

struct LamMatch * newLamMatch(struct LamExp * index, struct LamMatchList * cases) {
    struct LamMatch * x = NEW(LamMatch, OBJTYPE_LAMMATCH);
#ifdef DEBUG_ALLOC
    fprintf(stderr, "new LamMatch %p\n", x);
#endif
    x->index = index;
    x->cases = cases;
    return x;
}

struct LamMatchList * newLamMatchList(struct LamIntList * matches, struct LamExp * body, struct LamMatchList * next) {
    struct LamMatchList * x = NEW(LamMatchList, OBJTYPE_LAMMATCHLIST);
#ifdef DEBUG_ALLOC
    fprintf(stderr, "new LamMatchList %p\n", x);
#endif
    x->matches = matches;
    x->body = body;
    x->next = next;
    return x;
}

struct LamIntList * newLamIntList(int item, struct LamIntList * next) {
    struct LamIntList * x = NEW(LamIntList, OBJTYPE_LAMINTLIST);
#ifdef DEBUG_ALLOC
    fprintf(stderr, "new LamIntList %p\n", x);
#endif
    x->item = item;
    x->next = next;
    return x;
}

struct LamLet * newLamLet(HashSymbol * var, struct LamExp * value, struct LamExp * body) {
    struct LamLet * x = NEW(LamLet, OBJTYPE_LAMLET);
#ifdef DEBUG_ALLOC
    fprintf(stderr, "new LamLet %p\n", x);
#endif
    x->var = var;
    x->value = value;
    x->body = body;
    return x;
}

struct LamLetRec * newLamLetRec(int nbindings, struct LamLetRecBindings * bindings, struct LamExp * body) {
    struct LamLetRec * x = NEW(LamLetRec, OBJTYPE_LAMLETREC);
#ifdef DEBUG_ALLOC
    fprintf(stderr, "new LamLetRec %p\n", x);
#endif
    x->nbindings = nbindings;
    x->bindings = bindings;
    x->body = body;
    return x;
}

struct LamLetRecBindings * newLamLetRecBindings(HashSymbol * var, struct LamExp * val, struct LamLetRecBindings * next) {
    struct LamLetRecBindings * x = NEW(LamLetRecBindings, OBJTYPE_LAMLETRECBINDINGS);
#ifdef DEBUG_ALLOC
    fprintf(stderr, "new LamLetRecBindings %p\n", x);
#endif
    x->var = var;
    x->val = val;
    x->next = next;
    return x;
}

struct LamContext * newLamContext(HashTable * frame, struct LamContext * parent) {
    struct LamContext * x = NEW(LamContext, OBJTYPE_LAMCONTEXT);
#ifdef DEBUG_ALLOC
    fprintf(stderr, "new LamContext %p\n", x);
#endif
    x->frame = frame;
    x->parent = parent;
    return x;
}

struct LamAnd * newLamAnd(struct LamExp * left, struct LamExp * right) {
    struct LamAnd * x = NEW(LamAnd, OBJTYPE_LAMAND);
#ifdef DEBUG_ALLOC
    fprintf(stderr, "new LamAnd %p\n", x);
#endif
    x->left = left;
    x->right = right;
    return x;
}

struct LamOr * newLamOr(struct LamExp * left, struct LamExp * right) {
    struct LamOr * x = NEW(LamOr, OBJTYPE_LAMOR);
#ifdef DEBUG_ALLOC
    fprintf(stderr, "new LamOr %p\n", x);
#endif
    x->left = left;
    x->right = right;
    return x;
}

struct LamAmb * newLamAmb(struct LamExp * left, struct LamExp * right) {
    struct LamAmb * x = NEW(LamAmb, OBJTYPE_LAMAMB);
#ifdef DEBUG_ALLOC
    fprintf(stderr, "new LamAmb %p\n", x);
#endif
    x->left = left;
    x->right = right;
    return x;
}

struct LamTypeConstructorInfo * newLamTypeConstructorInfo(bool vec, int arity, int size, int index) {
    struct LamTypeConstructorInfo * x = NEW(LamTypeConstructorInfo, OBJTYPE_LAMTYPECONSTRUCTORINFO);
#ifdef DEBUG_ALLOC
    fprintf(stderr, "new LamTypeConstructorInfo %p\n", x);
#endif
    x->vec = vec;
    x->arity = arity;
    x->size = size;
    x->index = index;
    return x;
}

struct LamExp * newLamExp(enum LamExpType  type, union LamExpVal  val) {
    struct LamExp * x = NEW(LamExp, OBJTYPE_LAMEXP);
#ifdef DEBUG_ALLOC
    fprintf(stderr, "new LamExp %p\n", x);
#endif
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

void markLamSequence(struct LamSequence * x) {
    if (x == NULL) return;
    if (MARKED(x)) return;
    MARK(x);
    markLamExp(x->exp);
    markLamSequence(x->next);
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

void markLamIff(struct LamIff * x) {
    if (x == NULL) return;
    if (MARKED(x)) return;
    MARK(x);
    markLamExp(x->condition);
    markLamExp(x->consequent);
    markLamExp(x->alternative);
}

void markLamCond(struct LamCond * x) {
    if (x == NULL) return;
    if (MARKED(x)) return;
    MARK(x);
    markLamExp(x->value);
    markLamCondCases(x->cases);
}

void markLamCondCases(struct LamCondCases * x) {
    if (x == NULL) return;
    if (MARKED(x)) return;
    MARK(x);
    markLamExp(x->constant);
    markLamExp(x->body);
    markLamCondCases(x->next);
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
    markLamIntList(x->matches);
    markLamExp(x->body);
    markLamMatchList(x->next);
}

void markLamIntList(struct LamIntList * x) {
    if (x == NULL) return;
    if (MARKED(x)) return;
    MARK(x);
    markLamIntList(x->next);
}

void markLamLet(struct LamLet * x) {
    if (x == NULL) return;
    if (MARKED(x)) return;
    MARK(x);
    markHashSymbol(x->var);
    markLamExp(x->value);
    markLamExp(x->body);
}

void markLamLetRec(struct LamLetRec * x) {
    if (x == NULL) return;
    if (MARKED(x)) return;
    MARK(x);
    markLamLetRecBindings(x->bindings);
    markLamExp(x->body);
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

void markLamAnd(struct LamAnd * x) {
    if (x == NULL) return;
    if (MARKED(x)) return;
    MARK(x);
    markLamExp(x->left);
    markLamExp(x->right);
}

void markLamOr(struct LamOr * x) {
    if (x == NULL) return;
    if (MARKED(x)) return;
    MARK(x);
    markLamExp(x->left);
    markLamExp(x->right);
}

void markLamAmb(struct LamAmb * x) {
    if (x == NULL) return;
    if (MARKED(x)) return;
    MARK(x);
    markLamExp(x->left);
    markLamExp(x->right);
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
            markLamSequence(x->val.list);
            break;
        case LAMEXP_TYPE_MAKEVEC:
            markLamMakeVec(x->val.makeVec);
            break;
        case LAMEXP_TYPE_APPLY:
            markLamApply(x->val.apply);
            break;
        case LAMEXP_TYPE_IFF:
            markLamIff(x->val.iff);
            break;
        case LAMEXP_TYPE_CALLCC:
            markLamExp(x->val.callcc);
            break;
        case LAMEXP_TYPE_LETREC:
            markLamLetRec(x->val.letrec);
            break;
        case LAMEXP_TYPE_LET:
            markLamLet(x->val.let);
            break;
        case LAMEXP_TYPE_MATCH:
            markLamMatch(x->val.match);
            break;
        case LAMEXP_TYPE_COND:
            markLamCond(x->val.cond);
            break;
        case LAMEXP_TYPE_AND:
            markLamAnd(x->val.and);
            break;
        case LAMEXP_TYPE_OR:
            markLamOr(x->val.or);
            break;
        case LAMEXP_TYPE_AMB:
            markLamAmb(x->val.amb);
            break;
        case LAMEXP_TYPE_CHARACTER:
            break;
        case LAMEXP_TYPE_BACK:
            break;
        case LAMEXP_TYPE_ERROR:
            break;
        case LAMEXP_TYPE_COND_DEFAULT:
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
        case OBJTYPE_LAMSEQUENCE:
            markLamSequence((LamSequence *)h);
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
        case OBJTYPE_LAMIFF:
            markLamIff((LamIff *)h);
            break;
        case OBJTYPE_LAMCOND:
            markLamCond((LamCond *)h);
            break;
        case OBJTYPE_LAMCONDCASES:
            markLamCondCases((LamCondCases *)h);
            break;
        case OBJTYPE_LAMMATCH:
            markLamMatch((LamMatch *)h);
            break;
        case OBJTYPE_LAMMATCHLIST:
            markLamMatchList((LamMatchList *)h);
            break;
        case OBJTYPE_LAMINTLIST:
            markLamIntList((LamIntList *)h);
            break;
        case OBJTYPE_LAMLET:
            markLamLet((LamLet *)h);
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
        case OBJTYPE_LAMAND:
            markLamAnd((LamAnd *)h);
            break;
        case OBJTYPE_LAMOR:
            markLamOr((LamOr *)h);
            break;
        case OBJTYPE_LAMAMB:
            markLamAmb((LamAmb *)h);
            break;
        case OBJTYPE_LAMTYPECONSTRUCTORINFO:
            markLamTypeConstructorInfo((LamTypeConstructorInfo *)h);
            break;
        case OBJTYPE_LAMEXP:
            markLamExp((LamExp *)h);
            break;
        default:
            cant_happen("unrecognized type in markLambdaObj\n");
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

void freeLamSequence(struct LamSequence * x) {
    FREE(x, LamSequence);
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

void freeLamIff(struct LamIff * x) {
    FREE(x, LamIff);
}

void freeLamCond(struct LamCond * x) {
    FREE(x, LamCond);
}

void freeLamCondCases(struct LamCondCases * x) {
    FREE(x, LamCondCases);
}

void freeLamMatch(struct LamMatch * x) {
    FREE(x, LamMatch);
}

void freeLamMatchList(struct LamMatchList * x) {
    FREE(x, LamMatchList);
}

void freeLamIntList(struct LamIntList * x) {
    FREE(x, LamIntList);
}

void freeLamLet(struct LamLet * x) {
    FREE(x, LamLet);
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

void freeLamAnd(struct LamAnd * x) {
    FREE(x, LamAnd);
}

void freeLamOr(struct LamOr * x) {
    FREE(x, LamOr);
}

void freeLamAmb(struct LamAmb * x) {
    FREE(x, LamAmb);
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
        case OBJTYPE_LAMSEQUENCE:
            freeLamSequence((LamSequence *)h);
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
        case OBJTYPE_LAMIFF:
            freeLamIff((LamIff *)h);
            break;
        case OBJTYPE_LAMCOND:
            freeLamCond((LamCond *)h);
            break;
        case OBJTYPE_LAMCONDCASES:
            freeLamCondCases((LamCondCases *)h);
            break;
        case OBJTYPE_LAMMATCH:
            freeLamMatch((LamMatch *)h);
            break;
        case OBJTYPE_LAMMATCHLIST:
            freeLamMatchList((LamMatchList *)h);
            break;
        case OBJTYPE_LAMINTLIST:
            freeLamIntList((LamIntList *)h);
            break;
        case OBJTYPE_LAMLET:
            freeLamLet((LamLet *)h);
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
        case OBJTYPE_LAMAND:
            freeLamAnd((LamAnd *)h);
            break;
        case OBJTYPE_LAMOR:
            freeLamOr((LamOr *)h);
            break;
        case OBJTYPE_LAMAMB:
            freeLamAmb((LamAmb *)h);
            break;
        case OBJTYPE_LAMTYPECONSTRUCTORINFO:
            freeLamTypeConstructorInfo((LamTypeConstructorInfo *)h);
            break;
        case OBJTYPE_LAMEXP:
            freeLamExp((LamExp *)h);
            break;
        default:
            cant_happen("unrecognized type in freeLambdaObj\n");
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
        case OBJTYPE_LAMSEQUENCE:
            return "LamSequence";
        case OBJTYPE_LAMLIST:
            return "LamList";
        case OBJTYPE_LAMAPPLY:
            return "LamApply";
        case OBJTYPE_LAMMAKEVEC:
            return "LamMakeVec";
        case OBJTYPE_LAMIFF:
            return "LamIff";
        case OBJTYPE_LAMCOND:
            return "LamCond";
        case OBJTYPE_LAMCONDCASES:
            return "LamCondCases";
        case OBJTYPE_LAMMATCH:
            return "LamMatch";
        case OBJTYPE_LAMMATCHLIST:
            return "LamMatchList";
        case OBJTYPE_LAMINTLIST:
            return "LamIntList";
        case OBJTYPE_LAMLET:
            return "LamLet";
        case OBJTYPE_LAMLETREC:
            return "LamLetRec";
        case OBJTYPE_LAMLETRECBINDINGS:
            return "LamLetRecBindings";
        case OBJTYPE_LAMCONTEXT:
            return "LamContext";
        case OBJTYPE_LAMAND:
            return "LamAnd";
        case OBJTYPE_LAMOR:
            return "LamOr";
        case OBJTYPE_LAMAMB:
            return "LamAmb";
        case OBJTYPE_LAMTYPECONSTRUCTORINFO:
            return "LamTypeConstructorInfo";
        case OBJTYPE_LAMEXP:
            return "LamExp";
        default:
            cant_happen("unrecognized type in typenameLambdaObj\n");
    }
}


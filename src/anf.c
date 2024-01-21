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
 * ANF structures to be converted to bytecode.
 *
 * Generated from src/anf.yaml by tools/makeAST.py
 */

#include "anf.h"
#include <stdio.h>
#include <strings.h>
#include "common.h"
#ifdef DEBUG_ALLOC
#include "debugging_on.h"
#else
#include "debugging_off.h"
#endif

/*
 * constructor functions
 */

struct AexpLam * newAexpLam(int nargs, int letRecOffset, struct AexpVarList * args, struct Exp * exp) {
    struct AexpLam * x = NEW(AexpLam, OBJTYPE_AEXPLAM);
    DEBUG("new AexpLam %pn", x);
    x->nargs = nargs;
    x->letRecOffset = letRecOffset;
    x->args = args;
    x->exp = exp;
    return x;
}

struct AexpVarList * newAexpVarList(HashSymbol * var, struct AexpVarList * next) {
    struct AexpVarList * x = NEW(AexpVarList, OBJTYPE_AEXPVARLIST);
    DEBUG("new AexpVarList %pn", x);
    x->var = var;
    x->next = next;
    return x;
}

struct AexpAnnotatedVar * newAexpAnnotatedVar(enum AexpAnnotatedVarType  type, int frame, int offset, HashSymbol * var) {
    struct AexpAnnotatedVar * x = NEW(AexpAnnotatedVar, OBJTYPE_AEXPANNOTATEDVAR);
    DEBUG("new AexpAnnotatedVar %pn", x);
    x->type = type;
    x->frame = frame;
    x->offset = offset;
    x->var = var;
    return x;
}

struct AexpPrimApp * newAexpPrimApp(enum AexpPrimOp  type, struct Aexp * exp1, struct Aexp * exp2) {
    struct AexpPrimApp * x = NEW(AexpPrimApp, OBJTYPE_AEXPPRIMAPP);
    DEBUG("new AexpPrimApp %pn", x);
    x->type = type;
    x->exp1 = exp1;
    x->exp2 = exp2;
    return x;
}

struct AexpUnaryApp * newAexpUnaryApp(enum AexpUnaryOp  type, struct Aexp * exp) {
    struct AexpUnaryApp * x = NEW(AexpUnaryApp, OBJTYPE_AEXPUNARYAPP);
    DEBUG("new AexpUnaryApp %pn", x);
    x->type = type;
    x->exp = exp;
    return x;
}

struct AexpList * newAexpList(struct Aexp * exp, struct AexpList * next) {
    struct AexpList * x = NEW(AexpList, OBJTYPE_AEXPLIST);
    DEBUG("new AexpList %pn", x);
    x->exp = exp;
    x->next = next;
    return x;
}

struct AexpIntList * newAexpIntList(int integer, struct AexpIntList * next) {
    struct AexpIntList * x = NEW(AexpIntList, OBJTYPE_AEXPINTLIST);
    DEBUG("new AexpIntList %pn", x);
    x->integer = integer;
    x->next = next;
    return x;
}

struct CexpApply * newCexpApply(struct Aexp * function, int nargs, struct AexpList * args) {
    struct CexpApply * x = NEW(CexpApply, OBJTYPE_CEXPAPPLY);
    DEBUG("new CexpApply %pn", x);
    x->function = function;
    x->nargs = nargs;
    x->args = args;
    return x;
}

struct AexpMakeVec * newAexpMakeVec(int nargs, struct AexpList * args) {
    struct AexpMakeVec * x = NEW(AexpMakeVec, OBJTYPE_AEXPMAKEVEC);
    DEBUG("new AexpMakeVec %pn", x);
    x->nargs = nargs;
    x->args = args;
    return x;
}

struct CexpIf * newCexpIf(struct Aexp * condition, struct Exp * consequent, struct Exp * alternative) {
    struct CexpIf * x = NEW(CexpIf, OBJTYPE_CEXPIF);
    DEBUG("new CexpIf %pn", x);
    x->condition = condition;
    x->consequent = consequent;
    x->alternative = alternative;
    return x;
}

struct CexpCond * newCexpCond(struct Aexp * condition, struct CexpCondCases * cases) {
    struct CexpCond * x = NEW(CexpCond, OBJTYPE_CEXPCOND);
    DEBUG("new CexpCond %pn", x);
    x->condition = condition;
    x->cases = cases;
    return x;
}

struct CexpIntCondCases * newCexpIntCondCases(BigInt * option, struct Exp * body, struct CexpIntCondCases * next) {
    struct CexpIntCondCases * x = NEW(CexpIntCondCases, OBJTYPE_CEXPINTCONDCASES);
    DEBUG("new CexpIntCondCases %pn", x);
    x->option = option;
    x->body = body;
    x->next = next;
    return x;
}

struct CexpCharCondCases * newCexpCharCondCases(char option, struct Exp * body, struct CexpCharCondCases * next) {
    struct CexpCharCondCases * x = NEW(CexpCharCondCases, OBJTYPE_CEXPCHARCONDCASES);
    DEBUG("new CexpCharCondCases %pn", x);
    x->option = option;
    x->body = body;
    x->next = next;
    return x;
}

struct CexpMatch * newCexpMatch(struct Aexp * condition, struct MatchList * clauses) {
    struct CexpMatch * x = NEW(CexpMatch, OBJTYPE_CEXPMATCH);
    DEBUG("new CexpMatch %pn", x);
    x->condition = condition;
    x->clauses = clauses;
    return x;
}

struct MatchList * newMatchList(struct AexpIntList * matches, struct Exp * body, struct MatchList * next) {
    struct MatchList * x = NEW(MatchList, OBJTYPE_MATCHLIST);
    DEBUG("new MatchList %pn", x);
    x->matches = matches;
    x->body = body;
    x->next = next;
    return x;
}

struct CexpLetRec * newCexpLetRec(int nbindings, struct LetRecBindings * bindings, struct Exp * body) {
    struct CexpLetRec * x = NEW(CexpLetRec, OBJTYPE_CEXPLETREC);
    DEBUG("new CexpLetRec %pn", x);
    x->nbindings = nbindings;
    x->bindings = bindings;
    x->body = body;
    return x;
}

struct LetRecBindings * newLetRecBindings(HashSymbol * var, struct Aexp * val, struct LetRecBindings * next) {
    struct LetRecBindings * x = NEW(LetRecBindings, OBJTYPE_LETRECBINDINGS);
    DEBUG("new LetRecBindings %pn", x);
    x->var = var;
    x->val = val;
    x->next = next;
    return x;
}

struct CexpAmb * newCexpAmb(struct Exp * exp1, struct Exp * exp2) {
    struct CexpAmb * x = NEW(CexpAmb, OBJTYPE_CEXPAMB);
    DEBUG("new CexpAmb %pn", x);
    x->exp1 = exp1;
    x->exp2 = exp2;
    return x;
}

struct CexpCut * newCexpCut(struct Exp * exp) {
    struct CexpCut * x = NEW(CexpCut, OBJTYPE_CEXPCUT);
    DEBUG("new CexpCut %pn", x);
    x->exp = exp;
    return x;
}

struct CexpBool * newCexpBool(enum CexpBoolType  type, struct Exp * exp1, struct Exp * exp2) {
    struct CexpBool * x = NEW(CexpBool, OBJTYPE_CEXPBOOL);
    DEBUG("new CexpBool %pn", x);
    x->type = type;
    x->exp1 = exp1;
    x->exp2 = exp2;
    return x;
}

struct ExpLet * newExpLet(HashSymbol * var, struct Exp * val, struct Exp * body) {
    struct ExpLet * x = NEW(ExpLet, OBJTYPE_EXPLET);
    DEBUG("new ExpLet %pn", x);
    x->var = var;
    x->val = val;
    x->body = body;
    return x;
}

struct CexpCondCases * newCexpCondCases(enum CexpCondCasesType  type, union CexpCondCasesVal  val) {
    struct CexpCondCases * x = NEW(CexpCondCases, OBJTYPE_CEXPCONDCASES);
    DEBUG("new CexpCondCases %pn", x);
    x->type = type;
    x->val = val;
    return x;
}

struct Aexp * newAexp(enum AexpType  type, union AexpVal  val) {
    struct Aexp * x = NEW(Aexp, OBJTYPE_AEXP);
    DEBUG("new Aexp %pn", x);
    x->type = type;
    x->val = val;
    return x;
}

struct Cexp * newCexp(enum CexpType  type, union CexpVal  val) {
    struct Cexp * x = NEW(Cexp, OBJTYPE_CEXP);
    DEBUG("new Cexp %pn", x);
    x->type = type;
    x->val = val;
    return x;
}

struct Exp * newExp(enum ExpType  type, union ExpVal  val) {
    struct Exp * x = NEW(Exp, OBJTYPE_EXP);
    DEBUG("new Exp %pn", x);
    x->type = type;
    x->val = val;
    return x;
}


/*
 * copy functions
 */

struct AexpLam * copyAexpLam(struct AexpLam * o) {
    if (o == NULL) return NULL;
    struct AexpLam * x = NEW(AexpLam, OBJTYPE_AEXPLAM);
    DEBUG("copy AexpLam %pn", x);
    Header _h = x->header;
    bzero(x, sizeof(struct AexpLam));
    x->header = _h;
    int save = PROTECT(x);
    x->nargs = o->nargs;
    x->letRecOffset = o->letRecOffset;
    x->args = copyAexpVarList(o->args);
    x->exp = copyExp(o->exp);
    UNPROTECT(save);
    return x;
}

struct AexpVarList * copyAexpVarList(struct AexpVarList * o) {
    if (o == NULL) return NULL;
    struct AexpVarList * x = NEW(AexpVarList, OBJTYPE_AEXPVARLIST);
    DEBUG("copy AexpVarList %pn", x);
    Header _h = x->header;
    bzero(x, sizeof(struct AexpVarList));
    x->header = _h;
    int save = PROTECT(x);
    x->var = o->var;
    x->next = copyAexpVarList(o->next);
    UNPROTECT(save);
    return x;
}

struct AexpAnnotatedVar * copyAexpAnnotatedVar(struct AexpAnnotatedVar * o) {
    if (o == NULL) return NULL;
    struct AexpAnnotatedVar * x = NEW(AexpAnnotatedVar, OBJTYPE_AEXPANNOTATEDVAR);
    DEBUG("copy AexpAnnotatedVar %pn", x);
    Header _h = x->header;
    bzero(x, sizeof(struct AexpAnnotatedVar));
    x->header = _h;
    int save = PROTECT(x);
    x->type = o->type;
    x->frame = o->frame;
    x->offset = o->offset;
    x->var = o->var;
    UNPROTECT(save);
    return x;
}

struct AexpPrimApp * copyAexpPrimApp(struct AexpPrimApp * o) {
    if (o == NULL) return NULL;
    struct AexpPrimApp * x = NEW(AexpPrimApp, OBJTYPE_AEXPPRIMAPP);
    DEBUG("copy AexpPrimApp %pn", x);
    Header _h = x->header;
    bzero(x, sizeof(struct AexpPrimApp));
    x->header = _h;
    int save = PROTECT(x);
    x->type = o->type;
    x->exp1 = copyAexp(o->exp1);
    x->exp2 = copyAexp(o->exp2);
    UNPROTECT(save);
    return x;
}

struct AexpUnaryApp * copyAexpUnaryApp(struct AexpUnaryApp * o) {
    if (o == NULL) return NULL;
    struct AexpUnaryApp * x = NEW(AexpUnaryApp, OBJTYPE_AEXPUNARYAPP);
    DEBUG("copy AexpUnaryApp %pn", x);
    Header _h = x->header;
    bzero(x, sizeof(struct AexpUnaryApp));
    x->header = _h;
    int save = PROTECT(x);
    x->type = o->type;
    x->exp = copyAexp(o->exp);
    UNPROTECT(save);
    return x;
}

struct AexpList * copyAexpList(struct AexpList * o) {
    if (o == NULL) return NULL;
    struct AexpList * x = NEW(AexpList, OBJTYPE_AEXPLIST);
    DEBUG("copy AexpList %pn", x);
    Header _h = x->header;
    bzero(x, sizeof(struct AexpList));
    x->header = _h;
    int save = PROTECT(x);
    x->exp = copyAexp(o->exp);
    x->next = copyAexpList(o->next);
    UNPROTECT(save);
    return x;
}

struct AexpIntList * copyAexpIntList(struct AexpIntList * o) {
    if (o == NULL) return NULL;
    struct AexpIntList * x = NEW(AexpIntList, OBJTYPE_AEXPINTLIST);
    DEBUG("copy AexpIntList %pn", x);
    Header _h = x->header;
    bzero(x, sizeof(struct AexpIntList));
    x->header = _h;
    int save = PROTECT(x);
    x->integer = o->integer;
    x->next = copyAexpIntList(o->next);
    UNPROTECT(save);
    return x;
}

struct CexpApply * copyCexpApply(struct CexpApply * o) {
    if (o == NULL) return NULL;
    struct CexpApply * x = NEW(CexpApply, OBJTYPE_CEXPAPPLY);
    DEBUG("copy CexpApply %pn", x);
    Header _h = x->header;
    bzero(x, sizeof(struct CexpApply));
    x->header = _h;
    int save = PROTECT(x);
    x->function = copyAexp(o->function);
    x->nargs = o->nargs;
    x->args = copyAexpList(o->args);
    UNPROTECT(save);
    return x;
}

struct AexpMakeVec * copyAexpMakeVec(struct AexpMakeVec * o) {
    if (o == NULL) return NULL;
    struct AexpMakeVec * x = NEW(AexpMakeVec, OBJTYPE_AEXPMAKEVEC);
    DEBUG("copy AexpMakeVec %pn", x);
    Header _h = x->header;
    bzero(x, sizeof(struct AexpMakeVec));
    x->header = _h;
    int save = PROTECT(x);
    x->nargs = o->nargs;
    x->args = copyAexpList(o->args);
    UNPROTECT(save);
    return x;
}

struct CexpIf * copyCexpIf(struct CexpIf * o) {
    if (o == NULL) return NULL;
    struct CexpIf * x = NEW(CexpIf, OBJTYPE_CEXPIF);
    DEBUG("copy CexpIf %pn", x);
    Header _h = x->header;
    bzero(x, sizeof(struct CexpIf));
    x->header = _h;
    int save = PROTECT(x);
    x->condition = copyAexp(o->condition);
    x->consequent = copyExp(o->consequent);
    x->alternative = copyExp(o->alternative);
    UNPROTECT(save);
    return x;
}

struct CexpCond * copyCexpCond(struct CexpCond * o) {
    if (o == NULL) return NULL;
    struct CexpCond * x = NEW(CexpCond, OBJTYPE_CEXPCOND);
    DEBUG("copy CexpCond %pn", x);
    Header _h = x->header;
    bzero(x, sizeof(struct CexpCond));
    x->header = _h;
    int save = PROTECT(x);
    x->condition = copyAexp(o->condition);
    x->cases = copyCexpCondCases(o->cases);
    UNPROTECT(save);
    return x;
}

struct CexpIntCondCases * copyCexpIntCondCases(struct CexpIntCondCases * o) {
    if (o == NULL) return NULL;
    struct CexpIntCondCases * x = NEW(CexpIntCondCases, OBJTYPE_CEXPINTCONDCASES);
    DEBUG("copy CexpIntCondCases %pn", x);
    Header _h = x->header;
    bzero(x, sizeof(struct CexpIntCondCases));
    x->header = _h;
    int save = PROTECT(x);
    x->option = o->option;
    x->body = copyExp(o->body);
    x->next = copyCexpIntCondCases(o->next);
    UNPROTECT(save);
    return x;
}

struct CexpCharCondCases * copyCexpCharCondCases(struct CexpCharCondCases * o) {
    if (o == NULL) return NULL;
    struct CexpCharCondCases * x = NEW(CexpCharCondCases, OBJTYPE_CEXPCHARCONDCASES);
    DEBUG("copy CexpCharCondCases %pn", x);
    Header _h = x->header;
    bzero(x, sizeof(struct CexpCharCondCases));
    x->header = _h;
    int save = PROTECT(x);
    x->option = o->option;
    x->body = copyExp(o->body);
    x->next = copyCexpCharCondCases(o->next);
    UNPROTECT(save);
    return x;
}

struct CexpMatch * copyCexpMatch(struct CexpMatch * o) {
    if (o == NULL) return NULL;
    struct CexpMatch * x = NEW(CexpMatch, OBJTYPE_CEXPMATCH);
    DEBUG("copy CexpMatch %pn", x);
    Header _h = x->header;
    bzero(x, sizeof(struct CexpMatch));
    x->header = _h;
    int save = PROTECT(x);
    x->condition = copyAexp(o->condition);
    x->clauses = copyMatchList(o->clauses);
    UNPROTECT(save);
    return x;
}

struct MatchList * copyMatchList(struct MatchList * o) {
    if (o == NULL) return NULL;
    struct MatchList * x = NEW(MatchList, OBJTYPE_MATCHLIST);
    DEBUG("copy MatchList %pn", x);
    Header _h = x->header;
    bzero(x, sizeof(struct MatchList));
    x->header = _h;
    int save = PROTECT(x);
    x->matches = copyAexpIntList(o->matches);
    x->body = copyExp(o->body);
    x->next = copyMatchList(o->next);
    UNPROTECT(save);
    return x;
}

struct CexpLetRec * copyCexpLetRec(struct CexpLetRec * o) {
    if (o == NULL) return NULL;
    struct CexpLetRec * x = NEW(CexpLetRec, OBJTYPE_CEXPLETREC);
    DEBUG("copy CexpLetRec %pn", x);
    Header _h = x->header;
    bzero(x, sizeof(struct CexpLetRec));
    x->header = _h;
    int save = PROTECT(x);
    x->nbindings = o->nbindings;
    x->bindings = copyLetRecBindings(o->bindings);
    x->body = copyExp(o->body);
    UNPROTECT(save);
    return x;
}

struct LetRecBindings * copyLetRecBindings(struct LetRecBindings * o) {
    if (o == NULL) return NULL;
    struct LetRecBindings * x = NEW(LetRecBindings, OBJTYPE_LETRECBINDINGS);
    DEBUG("copy LetRecBindings %pn", x);
    Header _h = x->header;
    bzero(x, sizeof(struct LetRecBindings));
    x->header = _h;
    int save = PROTECT(x);
    x->var = o->var;
    x->val = copyAexp(o->val);
    x->next = copyLetRecBindings(o->next);
    UNPROTECT(save);
    return x;
}

struct CexpAmb * copyCexpAmb(struct CexpAmb * o) {
    if (o == NULL) return NULL;
    struct CexpAmb * x = NEW(CexpAmb, OBJTYPE_CEXPAMB);
    DEBUG("copy CexpAmb %pn", x);
    Header _h = x->header;
    bzero(x, sizeof(struct CexpAmb));
    x->header = _h;
    int save = PROTECT(x);
    x->exp1 = copyExp(o->exp1);
    x->exp2 = copyExp(o->exp2);
    UNPROTECT(save);
    return x;
}

struct CexpCut * copyCexpCut(struct CexpCut * o) {
    if (o == NULL) return NULL;
    struct CexpCut * x = NEW(CexpCut, OBJTYPE_CEXPCUT);
    DEBUG("copy CexpCut %pn", x);
    Header _h = x->header;
    bzero(x, sizeof(struct CexpCut));
    x->header = _h;
    int save = PROTECT(x);
    x->exp = copyExp(o->exp);
    UNPROTECT(save);
    return x;
}

struct CexpBool * copyCexpBool(struct CexpBool * o) {
    if (o == NULL) return NULL;
    struct CexpBool * x = NEW(CexpBool, OBJTYPE_CEXPBOOL);
    DEBUG("copy CexpBool %pn", x);
    Header _h = x->header;
    bzero(x, sizeof(struct CexpBool));
    x->header = _h;
    int save = PROTECT(x);
    x->type = o->type;
    x->exp1 = copyExp(o->exp1);
    x->exp2 = copyExp(o->exp2);
    UNPROTECT(save);
    return x;
}

struct ExpLet * copyExpLet(struct ExpLet * o) {
    if (o == NULL) return NULL;
    struct ExpLet * x = NEW(ExpLet, OBJTYPE_EXPLET);
    DEBUG("copy ExpLet %pn", x);
    Header _h = x->header;
    bzero(x, sizeof(struct ExpLet));
    x->header = _h;
    int save = PROTECT(x);
    x->var = o->var;
    x->val = copyExp(o->val);
    x->body = copyExp(o->body);
    UNPROTECT(save);
    return x;
}

struct CexpCondCases * copyCexpCondCases(struct CexpCondCases * o) {
    if (o == NULL) return NULL;
    struct CexpCondCases * x = NEW(CexpCondCases, OBJTYPE_CEXPCONDCASES);
    DEBUG("copy CexpCondCases %pn", x);
    Header _h = x->header;
    bzero(x, sizeof(struct CexpCondCases));
    x->header = _h;
    int save = PROTECT(x);
    switch(o->type) {
        case CEXPCONDCASES_TYPE_CHARCASES:
            x->val.charCases = copyCexpCharCondCases(o->val.charCases);
            break;
        case CEXPCONDCASES_TYPE_INTCASES:
            x->val.intCases = copyCexpIntCondCases(o->val.intCases);
            break;
        default:
            cant_happen("unrecognised type %d in copyCexpCondCases", o->type);
    }
    x->type = o->type;
    UNPROTECT(save);
    return x;
}

struct Aexp * copyAexp(struct Aexp * o) {
    if (o == NULL) return NULL;
    struct Aexp * x = NEW(Aexp, OBJTYPE_AEXP);
    DEBUG("copy Aexp %pn", x);
    Header _h = x->header;
    bzero(x, sizeof(struct Aexp));
    x->header = _h;
    int save = PROTECT(x);
    switch(o->type) {
        case AEXP_TYPE_T:
            x->val.t = o->val.t;
            break;
        case AEXP_TYPE_F:
            x->val.f = o->val.f;
            break;
        case AEXP_TYPE_V:
            x->val.v = o->val.v;
            break;
        case AEXP_TYPE_LAM:
            x->val.lam = copyAexpLam(o->val.lam);
            break;
        case AEXP_TYPE_VAR:
            x->val.var = o->val.var;
            break;
        case AEXP_TYPE_ANNOTATEDVAR:
            x->val.annotatedVar = copyAexpAnnotatedVar(o->val.annotatedVar);
            break;
        case AEXP_TYPE_BIGINTEGER:
            x->val.biginteger = o->val.biginteger;
            break;
        case AEXP_TYPE_LITTLEINTEGER:
            x->val.littleinteger = o->val.littleinteger;
            break;
        case AEXP_TYPE_CHARACTER:
            x->val.character = o->val.character;
            break;
        case AEXP_TYPE_PRIM:
            x->val.prim = copyAexpPrimApp(o->val.prim);
            break;
        case AEXP_TYPE_UNARY:
            x->val.unary = copyAexpUnaryApp(o->val.unary);
            break;
        case AEXP_TYPE_LIST:
            x->val.list = copyAexpList(o->val.list);
            break;
        case AEXP_TYPE_MAKEVEC:
            x->val.makeVec = copyAexpMakeVec(o->val.makeVec);
            break;
        default:
            cant_happen("unrecognised type %d in copyAexp", o->type);
    }
    x->type = o->type;
    UNPROTECT(save);
    return x;
}

struct Cexp * copyCexp(struct Cexp * o) {
    if (o == NULL) return NULL;
    struct Cexp * x = NEW(Cexp, OBJTYPE_CEXP);
    DEBUG("copy Cexp %pn", x);
    Header _h = x->header;
    bzero(x, sizeof(struct Cexp));
    x->header = _h;
    int save = PROTECT(x);
    switch(o->type) {
        case CEXP_TYPE_BACK:
            x->val.back = o->val.back;
            break;
        case CEXP_TYPE_ERROR:
            x->val.error = o->val.error;
            break;
        case CEXP_TYPE_APPLY:
            x->val.apply = copyCexpApply(o->val.apply);
            break;
        case CEXP_TYPE_IFF:
            x->val.iff = copyCexpIf(o->val.iff);
            break;
        case CEXP_TYPE_COND:
            x->val.cond = copyCexpCond(o->val.cond);
            break;
        case CEXP_TYPE_CALLCC:
            x->val.callCC = copyAexp(o->val.callCC);
            break;
        case CEXP_TYPE_LETREC:
            x->val.letRec = copyCexpLetRec(o->val.letRec);
            break;
        case CEXP_TYPE_AMB:
            x->val.amb = copyCexpAmb(o->val.amb);
            break;
        case CEXP_TYPE_CUT:
            x->val.cut = copyCexpCut(o->val.cut);
            break;
        case CEXP_TYPE_BOOLEAN:
            x->val.boolean = copyCexpBool(o->val.boolean);
            break;
        case CEXP_TYPE_MATCH:
            x->val.match = copyCexpMatch(o->val.match);
            break;
        default:
            cant_happen("unrecognised type %d in copyCexp", o->type);
    }
    x->type = o->type;
    UNPROTECT(save);
    return x;
}

struct Exp * copyExp(struct Exp * o) {
    if (o == NULL) return NULL;
    struct Exp * x = NEW(Exp, OBJTYPE_EXP);
    DEBUG("copy Exp %pn", x);
    Header _h = x->header;
    bzero(x, sizeof(struct Exp));
    x->header = _h;
    int save = PROTECT(x);
    switch(o->type) {
        case EXP_TYPE_DONE:
            x->val.done = o->val.done;
            break;
        case EXP_TYPE_AEXP:
            x->val.aexp = copyAexp(o->val.aexp);
            break;
        case EXP_TYPE_CEXP:
            x->val.cexp = copyCexp(o->val.cexp);
            break;
        case EXP_TYPE_LET:
            x->val.let = copyExpLet(o->val.let);
            break;
        default:
            cant_happen("unrecognised type %d in copyExp", o->type);
    }
    x->type = o->type;
    UNPROTECT(save);
    return x;
}


/*
 * push functions
 */


/*
 * mark functions
 */

void markAexpLam(struct AexpLam * x) {
    if (x == NULL) return;
    if (MARKED(x)) return;
    MARK(x);
    markAexpVarList(x->args);
    markExp(x->exp);
}

void markAexpVarList(struct AexpVarList * x) {
    if (x == NULL) return;
    if (MARKED(x)) return;
    MARK(x);
    markAexpVarList(x->next);
}

void markAexpAnnotatedVar(struct AexpAnnotatedVar * x) {
    if (x == NULL) return;
    if (MARKED(x)) return;
    MARK(x);
}

void markAexpPrimApp(struct AexpPrimApp * x) {
    if (x == NULL) return;
    if (MARKED(x)) return;
    MARK(x);
    markAexp(x->exp1);
    markAexp(x->exp2);
}

void markAexpUnaryApp(struct AexpUnaryApp * x) {
    if (x == NULL) return;
    if (MARKED(x)) return;
    MARK(x);
    markAexp(x->exp);
}

void markAexpList(struct AexpList * x) {
    if (x == NULL) return;
    if (MARKED(x)) return;
    MARK(x);
    markAexp(x->exp);
    markAexpList(x->next);
}

void markAexpIntList(struct AexpIntList * x) {
    if (x == NULL) return;
    if (MARKED(x)) return;
    MARK(x);
    markAexpIntList(x->next);
}

void markCexpApply(struct CexpApply * x) {
    if (x == NULL) return;
    if (MARKED(x)) return;
    MARK(x);
    markAexp(x->function);
    markAexpList(x->args);
}

void markAexpMakeVec(struct AexpMakeVec * x) {
    if (x == NULL) return;
    if (MARKED(x)) return;
    MARK(x);
    markAexpList(x->args);
}

void markCexpIf(struct CexpIf * x) {
    if (x == NULL) return;
    if (MARKED(x)) return;
    MARK(x);
    markAexp(x->condition);
    markExp(x->consequent);
    markExp(x->alternative);
}

void markCexpCond(struct CexpCond * x) {
    if (x == NULL) return;
    if (MARKED(x)) return;
    MARK(x);
    markAexp(x->condition);
    markCexpCondCases(x->cases);
}

void markCexpIntCondCases(struct CexpIntCondCases * x) {
    if (x == NULL) return;
    if (MARKED(x)) return;
    MARK(x);
    markBigInt(x->option);
    markExp(x->body);
    markCexpIntCondCases(x->next);
}

void markCexpCharCondCases(struct CexpCharCondCases * x) {
    if (x == NULL) return;
    if (MARKED(x)) return;
    MARK(x);
    markExp(x->body);
    markCexpCharCondCases(x->next);
}

void markCexpMatch(struct CexpMatch * x) {
    if (x == NULL) return;
    if (MARKED(x)) return;
    MARK(x);
    markAexp(x->condition);
    markMatchList(x->clauses);
}

void markMatchList(struct MatchList * x) {
    if (x == NULL) return;
    if (MARKED(x)) return;
    MARK(x);
    markAexpIntList(x->matches);
    markExp(x->body);
    markMatchList(x->next);
}

void markCexpLetRec(struct CexpLetRec * x) {
    if (x == NULL) return;
    if (MARKED(x)) return;
    MARK(x);
    markLetRecBindings(x->bindings);
    markExp(x->body);
}

void markLetRecBindings(struct LetRecBindings * x) {
    if (x == NULL) return;
    if (MARKED(x)) return;
    MARK(x);
    markAexp(x->val);
    markLetRecBindings(x->next);
}

void markCexpAmb(struct CexpAmb * x) {
    if (x == NULL) return;
    if (MARKED(x)) return;
    MARK(x);
    markExp(x->exp1);
    markExp(x->exp2);
}

void markCexpCut(struct CexpCut * x) {
    if (x == NULL) return;
    if (MARKED(x)) return;
    MARK(x);
    markExp(x->exp);
}

void markCexpBool(struct CexpBool * x) {
    if (x == NULL) return;
    if (MARKED(x)) return;
    MARK(x);
    markExp(x->exp1);
    markExp(x->exp2);
}

void markExpLet(struct ExpLet * x) {
    if (x == NULL) return;
    if (MARKED(x)) return;
    MARK(x);
    markExp(x->val);
    markExp(x->body);
}

void markCexpCondCases(struct CexpCondCases * x) {
    if (x == NULL) return;
    if (MARKED(x)) return;
    MARK(x);
    switch(x->type) {
        case CEXPCONDCASES_TYPE_CHARCASES:
            markCexpCharCondCases(x->val.charCases);
            break;
        case CEXPCONDCASES_TYPE_INTCASES:
            markCexpIntCondCases(x->val.intCases);
            break;
        default:
            cant_happen("unrecognised type %d in markCexpCondCases", x->type);
    }
}

void markAexp(struct Aexp * x) {
    if (x == NULL) return;
    if (MARKED(x)) return;
    MARK(x);
    switch(x->type) {
        case AEXP_TYPE_T:
            break;
        case AEXP_TYPE_F:
            break;
        case AEXP_TYPE_V:
            break;
        case AEXP_TYPE_LAM:
            markAexpLam(x->val.lam);
            break;
        case AEXP_TYPE_VAR:
            break;
        case AEXP_TYPE_ANNOTATEDVAR:
            markAexpAnnotatedVar(x->val.annotatedVar);
            break;
        case AEXP_TYPE_BIGINTEGER:
            markBigInt(x->val.biginteger);
            break;
        case AEXP_TYPE_LITTLEINTEGER:
            break;
        case AEXP_TYPE_CHARACTER:
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
        case AEXP_TYPE_MAKEVEC:
            markAexpMakeVec(x->val.makeVec);
            break;
        default:
            cant_happen("unrecognised type %d in markAexp", x->type);
    }
}

void markCexp(struct Cexp * x) {
    if (x == NULL) return;
    if (MARKED(x)) return;
    MARK(x);
    switch(x->type) {
        case CEXP_TYPE_BACK:
            break;
        case CEXP_TYPE_ERROR:
            break;
        case CEXP_TYPE_APPLY:
            markCexpApply(x->val.apply);
            break;
        case CEXP_TYPE_IFF:
            markCexpIf(x->val.iff);
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
        case CEXP_TYPE_CUT:
            markCexpCut(x->val.cut);
            break;
        case CEXP_TYPE_BOOLEAN:
            markCexpBool(x->val.boolean);
            break;
        case CEXP_TYPE_MATCH:
            markCexpMatch(x->val.match);
            break;
        default:
            cant_happen("unrecognised type %d in markCexp", x->type);
    }
}

void markExp(struct Exp * x) {
    if (x == NULL) return;
    if (MARKED(x)) return;
    MARK(x);
    switch(x->type) {
        case EXP_TYPE_DONE:
            break;
        case EXP_TYPE_AEXP:
            markAexp(x->val.aexp);
            break;
        case EXP_TYPE_CEXP:
            markCexp(x->val.cexp);
            break;
        case EXP_TYPE_LET:
            markExpLet(x->val.let);
            break;
        default:
            cant_happen("unrecognised type %d in markExp", x->type);
    }
}


/*
 * generic mark function
 */

void markAnfObj(struct Header *h) {
    switch(h->type) {
        case OBJTYPE_AEXPLAM:
            markAexpLam((AexpLam *)h);
            break;
        case OBJTYPE_AEXPVARLIST:
            markAexpVarList((AexpVarList *)h);
            break;
        case OBJTYPE_AEXPANNOTATEDVAR:
            markAexpAnnotatedVar((AexpAnnotatedVar *)h);
            break;
        case OBJTYPE_AEXPPRIMAPP:
            markAexpPrimApp((AexpPrimApp *)h);
            break;
        case OBJTYPE_AEXPUNARYAPP:
            markAexpUnaryApp((AexpUnaryApp *)h);
            break;
        case OBJTYPE_AEXPLIST:
            markAexpList((AexpList *)h);
            break;
        case OBJTYPE_AEXPINTLIST:
            markAexpIntList((AexpIntList *)h);
            break;
        case OBJTYPE_CEXPAPPLY:
            markCexpApply((CexpApply *)h);
            break;
        case OBJTYPE_AEXPMAKEVEC:
            markAexpMakeVec((AexpMakeVec *)h);
            break;
        case OBJTYPE_CEXPIF:
            markCexpIf((CexpIf *)h);
            break;
        case OBJTYPE_CEXPCOND:
            markCexpCond((CexpCond *)h);
            break;
        case OBJTYPE_CEXPINTCONDCASES:
            markCexpIntCondCases((CexpIntCondCases *)h);
            break;
        case OBJTYPE_CEXPCHARCONDCASES:
            markCexpCharCondCases((CexpCharCondCases *)h);
            break;
        case OBJTYPE_CEXPMATCH:
            markCexpMatch((CexpMatch *)h);
            break;
        case OBJTYPE_MATCHLIST:
            markMatchList((MatchList *)h);
            break;
        case OBJTYPE_CEXPLETREC:
            markCexpLetRec((CexpLetRec *)h);
            break;
        case OBJTYPE_LETRECBINDINGS:
            markLetRecBindings((LetRecBindings *)h);
            break;
        case OBJTYPE_CEXPAMB:
            markCexpAmb((CexpAmb *)h);
            break;
        case OBJTYPE_CEXPCUT:
            markCexpCut((CexpCut *)h);
            break;
        case OBJTYPE_CEXPBOOL:
            markCexpBool((CexpBool *)h);
            break;
        case OBJTYPE_EXPLET:
            markExpLet((ExpLet *)h);
            break;
        case OBJTYPE_CEXPCONDCASES:
            markCexpCondCases((CexpCondCases *)h);
            break;
        case OBJTYPE_AEXP:
            markAexp((Aexp *)h);
            break;
        case OBJTYPE_CEXP:
            markCexp((Cexp *)h);
            break;
        case OBJTYPE_EXP:
            markExp((Exp *)h);
            break;
        default:
            cant_happen("unrecognised type %d in markAnfObj\n", h->type);
    }
}

/*
 * free functions
 */

void freeAexpLam(struct AexpLam * x) {
    FREE(x, AexpLam);
}

void freeAexpVarList(struct AexpVarList * x) {
    FREE(x, AexpVarList);
}

void freeAexpAnnotatedVar(struct AexpAnnotatedVar * x) {
    FREE(x, AexpAnnotatedVar);
}

void freeAexpPrimApp(struct AexpPrimApp * x) {
    FREE(x, AexpPrimApp);
}

void freeAexpUnaryApp(struct AexpUnaryApp * x) {
    FREE(x, AexpUnaryApp);
}

void freeAexpList(struct AexpList * x) {
    FREE(x, AexpList);
}

void freeAexpIntList(struct AexpIntList * x) {
    FREE(x, AexpIntList);
}

void freeCexpApply(struct CexpApply * x) {
    FREE(x, CexpApply);
}

void freeAexpMakeVec(struct AexpMakeVec * x) {
    FREE(x, AexpMakeVec);
}

void freeCexpIf(struct CexpIf * x) {
    FREE(x, CexpIf);
}

void freeCexpCond(struct CexpCond * x) {
    FREE(x, CexpCond);
}

void freeCexpIntCondCases(struct CexpIntCondCases * x) {
    FREE(x, CexpIntCondCases);
}

void freeCexpCharCondCases(struct CexpCharCondCases * x) {
    FREE(x, CexpCharCondCases);
}

void freeCexpMatch(struct CexpMatch * x) {
    FREE(x, CexpMatch);
}

void freeMatchList(struct MatchList * x) {
    FREE(x, MatchList);
}

void freeCexpLetRec(struct CexpLetRec * x) {
    FREE(x, CexpLetRec);
}

void freeLetRecBindings(struct LetRecBindings * x) {
    FREE(x, LetRecBindings);
}

void freeCexpAmb(struct CexpAmb * x) {
    FREE(x, CexpAmb);
}

void freeCexpCut(struct CexpCut * x) {
    FREE(x, CexpCut);
}

void freeCexpBool(struct CexpBool * x) {
    FREE(x, CexpBool);
}

void freeExpLet(struct ExpLet * x) {
    FREE(x, ExpLet);
}

void freeCexpCondCases(struct CexpCondCases * x) {
    FREE(x, CexpCondCases);
}

void freeAexp(struct Aexp * x) {
    FREE(x, Aexp);
}

void freeCexp(struct Cexp * x) {
    FREE(x, Cexp);
}

void freeExp(struct Exp * x) {
    FREE(x, Exp);
}


/*
 * generic free function
 */

void freeAnfObj(struct Header *h) {
    switch(h->type) {
        case OBJTYPE_AEXPLAM:
            freeAexpLam((AexpLam *)h);
            break;
        case OBJTYPE_AEXPVARLIST:
            freeAexpVarList((AexpVarList *)h);
            break;
        case OBJTYPE_AEXPANNOTATEDVAR:
            freeAexpAnnotatedVar((AexpAnnotatedVar *)h);
            break;
        case OBJTYPE_AEXPPRIMAPP:
            freeAexpPrimApp((AexpPrimApp *)h);
            break;
        case OBJTYPE_AEXPUNARYAPP:
            freeAexpUnaryApp((AexpUnaryApp *)h);
            break;
        case OBJTYPE_AEXPLIST:
            freeAexpList((AexpList *)h);
            break;
        case OBJTYPE_AEXPINTLIST:
            freeAexpIntList((AexpIntList *)h);
            break;
        case OBJTYPE_CEXPAPPLY:
            freeCexpApply((CexpApply *)h);
            break;
        case OBJTYPE_AEXPMAKEVEC:
            freeAexpMakeVec((AexpMakeVec *)h);
            break;
        case OBJTYPE_CEXPIF:
            freeCexpIf((CexpIf *)h);
            break;
        case OBJTYPE_CEXPCOND:
            freeCexpCond((CexpCond *)h);
            break;
        case OBJTYPE_CEXPINTCONDCASES:
            freeCexpIntCondCases((CexpIntCondCases *)h);
            break;
        case OBJTYPE_CEXPCHARCONDCASES:
            freeCexpCharCondCases((CexpCharCondCases *)h);
            break;
        case OBJTYPE_CEXPMATCH:
            freeCexpMatch((CexpMatch *)h);
            break;
        case OBJTYPE_MATCHLIST:
            freeMatchList((MatchList *)h);
            break;
        case OBJTYPE_CEXPLETREC:
            freeCexpLetRec((CexpLetRec *)h);
            break;
        case OBJTYPE_LETRECBINDINGS:
            freeLetRecBindings((LetRecBindings *)h);
            break;
        case OBJTYPE_CEXPAMB:
            freeCexpAmb((CexpAmb *)h);
            break;
        case OBJTYPE_CEXPCUT:
            freeCexpCut((CexpCut *)h);
            break;
        case OBJTYPE_CEXPBOOL:
            freeCexpBool((CexpBool *)h);
            break;
        case OBJTYPE_EXPLET:
            freeExpLet((ExpLet *)h);
            break;
        case OBJTYPE_CEXPCONDCASES:
            freeCexpCondCases((CexpCondCases *)h);
            break;
        case OBJTYPE_AEXP:
            freeAexp((Aexp *)h);
            break;
        case OBJTYPE_CEXP:
            freeCexp((Cexp *)h);
            break;
        case OBJTYPE_EXP:
            freeExp((Exp *)h);
            break;
        default:
            cant_happen("unrecognised type %d in freeAnfObj\n", h->type);
    }
}

/*
 * type identifier function
 */

char *typenameAnfObj(int type) {
    switch(type) {
        case OBJTYPE_AEXPLAM:
            return "AexpLam";
        case OBJTYPE_AEXPVARLIST:
            return "AexpVarList";
        case OBJTYPE_AEXPANNOTATEDVAR:
            return "AexpAnnotatedVar";
        case OBJTYPE_AEXPPRIMAPP:
            return "AexpPrimApp";
        case OBJTYPE_AEXPUNARYAPP:
            return "AexpUnaryApp";
        case OBJTYPE_AEXPLIST:
            return "AexpList";
        case OBJTYPE_AEXPINTLIST:
            return "AexpIntList";
        case OBJTYPE_CEXPAPPLY:
            return "CexpApply";
        case OBJTYPE_AEXPMAKEVEC:
            return "AexpMakeVec";
        case OBJTYPE_CEXPIF:
            return "CexpIf";
        case OBJTYPE_CEXPCOND:
            return "CexpCond";
        case OBJTYPE_CEXPINTCONDCASES:
            return "CexpIntCondCases";
        case OBJTYPE_CEXPCHARCONDCASES:
            return "CexpCharCondCases";
        case OBJTYPE_CEXPMATCH:
            return "CexpMatch";
        case OBJTYPE_MATCHLIST:
            return "MatchList";
        case OBJTYPE_CEXPLETREC:
            return "CexpLetRec";
        case OBJTYPE_LETRECBINDINGS:
            return "LetRecBindings";
        case OBJTYPE_CEXPAMB:
            return "CexpAmb";
        case OBJTYPE_CEXPCUT:
            return "CexpCut";
        case OBJTYPE_CEXPBOOL:
            return "CexpBool";
        case OBJTYPE_EXPLET:
            return "ExpLet";
        case OBJTYPE_CEXPCONDCASES:
            return "CexpCondCases";
        case OBJTYPE_AEXP:
            return "Aexp";
        case OBJTYPE_CEXP:
            return "Cexp";
        case OBJTYPE_EXP:
            return "Exp";
        default:
            cant_happen("unrecognised type %d in typenameAnfObj\n", type);
    }
}

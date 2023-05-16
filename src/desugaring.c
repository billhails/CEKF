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

#include "common.h"
#include "desugaring.h"

#ifdef DEBUG_DESUGARING
#include "debug.h"
#endif

static AexpLam *desugarAexpLam(AexpLam *x);
static AexpPrimApp *desugarAexpPrimApp(AexpPrimApp *x);
static AexpUnaryApp *desugarAexpUnaryApp(AexpUnaryApp *x);
static AexpVar *desugarAexpVar(AexpVar *x);
static AexpList *desugarAexpList(AexpList *x);
static CexpApply *desugarCexpApply(CexpApply *x);
static CexpCond *desugarCexpCond(CexpCond *x);
static CexpLetRec *desugarCexpLetRec(CexpLetRec *x);
static CexpAmb *desugarCexpAmb(CexpAmb *x);
static ExpLet *desugarCexpBool(CexpBool *x);
static ExpLet *desugarExpLet(ExpLet *x);
static Aexp *desugarAexp(Aexp *x);
static Cexp *desugarCexp(Cexp *x);
static Aexp *desugarAexpMakeList(Aexp *x);

#ifdef DEBUG_DESUGARING
#define DEBUG_DESUGAR(type, val) do { printf("desugar" #type ": "); print ## type (val); printf("\n"); } while(0)
#else
#define DEBUG_DESUGAR(type, val) do {} while(0)
#endif

static AexpLam *desugarAexpLam(AexpLam *x) {
    DEBUG_DESUGAR(AexpLam, x);
    x->exp = desugarExp(x->exp);
    return x;
}

static AexpPrimApp *desugarAexpPrimApp(AexpPrimApp *x) {
    DEBUG_DESUGAR(AexpPrimApp, x);
    x->exp1 = desugarAexp(x->exp1);
    x->exp2 = desugarAexp(x->exp2);
    return x;
}

static AexpUnaryApp *desugarAexpUnaryApp(AexpUnaryApp *x) {
    DEBUG_DESUGAR(AexpUnaryApp, x);
    x->exp = desugarAexp(x->exp);
    return x;
}

static AexpVar *desugarAexpVar(AexpVar *x) {
    DEBUG_DESUGAR(AexpVar, x);
    return x;
}

static AexpList *desugarAexpList(AexpList *x) {
    DEBUG_DESUGAR(AexpList, x);
    AexpList *y = x;
    while(x != NULL) {
        x->exp = desugarAexp(x->exp);
        x = x->next;
    }
    return y;
}

static CexpApply *desugarCexpApply(CexpApply *x) {
    DEBUG_DESUGAR(CexpApply, x);
    x->function = desugarAexp(x->function);
    x->args = desugarAexpList(x->args);
    return x;
}

static CexpCond *desugarCexpCond(CexpCond *x) {
    DEBUG_DESUGAR(CexpCond, x);
    x->condition = desugarAexp(x->condition);
    x->consequent = desugarExp(x->consequent);
    x->alternative = desugarExp(x->alternative);
    return x;
}

static CexpLetRec *desugarCexpLetRec(CexpLetRec *x) {
    DEBUG_DESUGAR(CexpLetRec, x);
    LetRecBindings *bindings = x->bindings;
    while (bindings != NULL) {
        bindings->var = desugarAexpVar(bindings->var);
        bindings = bindings->next;
    }
    bindings = x->bindings;
    while (bindings != NULL) {
        bindings->val = desugarAexp(bindings->val);
        bindings = bindings->next;
    }
    x->body = desugarExp(x->body);
    return x;
}

static CexpAmb *desugarCexpAmb(CexpAmb *x) {
    DEBUG_DESUGAR(CexpAmb, x);
    x->exp1 = desugarExp(x->exp1);
    x->exp2 = desugarExp(x->exp2);
    return x;
}

static ExpLet *andToExpLet(CexpBool * x) {
  // (and <exp1> <ep2>) => (let (<sym> <exp1>) (if <sym> <exp2> <sym>))
  Exp *exp1 = desugarExp(x->exp1);
  Exp *exp2 = desugarExp(x->exp2);
  AexpVar *sym = genSym("and_");
  return
    newExpLet(sym, exp1,
              newExp(EXP_TYPE_CEXP,
                     EXP_VAL_CEXP(newCexp
                                  (CEXP_TYPE_COND,
                                   CEXP_VAL_COND(newCexpCond
                                                 (newAexp
                                                  (AEXP_TYPE_VAR,
                                                   AEXP_VAL_VAR(sym)),
                                                  exp2,
                                                  newExp(EXP_TYPE_AEXP,
                                                         EXP_VAL_AEXP(newAexp
                                                                      (AEXP_TYPE_VAR,
                                                                       AEXP_VAL_VAR
                                                                       (sym))))))))));
}

static ExpLet *orToExpLet(CexpBool * x) {
  // (or <exp1> <ep2>) => (let (<sym> <exp1>) (if <sym> <sym> <exp2>))
  Exp *exp1 = desugarExp(x->exp1);
  Exp *exp2 = desugarExp(x->exp2);
  AexpVar *sym = genSym("or_");
  return
    newExpLet(sym,
              exp1,
              newExp(EXP_TYPE_CEXP,
                     EXP_VAL_CEXP(newCexp
                                  (CEXP_TYPE_COND,
                                   CEXP_VAL_COND
                                   (newCexpCond
                                    (newAexp
                                     (AEXP_TYPE_VAR,
                                      AEXP_VAL_VAR(sym)),
                                     newExp(EXP_TYPE_AEXP,
                                            EXP_VAL_AEXP
                                            (newAexp
                                             (AEXP_TYPE_VAR,
                                              AEXP_VAL_VAR
                                              (sym)))), exp2))))));
}

static ExpLet *desugarCexpBool(CexpBool *x) {
    DEBUG_DESUGAR(CexpBool, x);
    switch (x->type) {
        case BOOL_TYPE_AND:
            return andToExpLet(x);
        case BOOL_TYPE_OR:
            return orToExpLet(x);
        default:
            cant_happen("unrecognised type %d in desugarCexpBool");
    }
}

static ExpLet *desugarExpLet(ExpLet *x) {
    DEBUG_DESUGAR(ExpLet, x);
    x->val = desugarExp(x->val);
    x->var = desugarAexpVar(x->var);
    x->body = desugarExp(x->body);
    return x;
}

static Aexp * listToCons(AexpList * x) {
    if (x == NULL) {
        return newAexp(AEXP_TYPE_VOID, AEXP_VAL_VOID());
    }
    return newAexp(AEXP_TYPE_PRIM, AEXP_VAL_PRIM(newAexpPrimApp(AEXP_PRIM_CONS, desugarAexp(x->exp), listToCons(x->next))));
}

static Aexp *desugarAexp(Aexp *x) {
    DEBUG_DESUGAR(Aexp, x);
    switch (x->type) {
        case AEXP_TYPE_LAM:
            x->val.lam = desugarAexpLam(x->val.lam);
            break;
        case AEXP_TYPE_VAR:
            x->val.var = desugarAexpVar(x->val.var);
            break;
        case AEXP_TYPE_ANNOTATEDVAR:
            cant_happen("desugarAexp called on annotated var");
            break;
        case AEXP_TYPE_TRUE:
        case AEXP_TYPE_FALSE:
        case AEXP_TYPE_INT:
        case AEXP_TYPE_VOID:
            break;
        case AEXP_TYPE_PRIM:
            x->val.prim = desugarAexpPrimApp(x->val.prim);
            break;
        case AEXP_TYPE_UNARY:
            x->val.unary = desugarAexpUnaryApp(x->val.unary);
            break;
        case AEXP_TYPE_LIST:
            return listToCons(x->val.list);
        default:
            cant_happen("unrecognized type in desugarAexp");
    }
    return x;
}

static Cexp *desugarCexp(Cexp *x) {
    DEBUG_DESUGAR(Cexp, x);
    switch (x->type) {
        case CEXP_TYPE_APPLY:
            x->val.apply = desugarCexpApply(x->val.apply);
            break;
        case CEXP_TYPE_COND:
            x->val.cond = desugarCexpCond(x->val.cond);
            break;
        case CEXP_TYPE_CALLCC:
            x->val.callCC = desugarAexp(x->val.callCC);
            break;
        case CEXP_TYPE_LETREC:
            x->val.letRec = desugarCexpLetRec(x->val.letRec);
            break;
        case CEXP_TYPE_AMB:
            x->val.amb = desugarCexpAmb(x->val.amb);
            break;
        case CEXP_TYPE_BOOL:
            cant_happen("desugarCexp given CEXP_TYPE_BOOL");
            break;
        case CEXP_TYPE_BACK:
            break;
        default:
            cant_happen("unrecognized type in desugarCexp");
    }
    return x;
}

Exp *desugarExp(Exp *x) {
    DEBUG_DESUGAR(Exp, x);
    switch (x->type) {
        case EXP_TYPE_AEXP:
            x->val.aexp = desugarAexp(x->val.aexp);
            break;
        case EXP_TYPE_CEXP:
            if (x->val.cexp->type == CEXP_TYPE_BOOL) {
                x->val = EXP_VAL_LET(desugarCexpBool(x->val.cexp->val.boolean));
                x->type = EXP_TYPE_LET;
            } else {
                x->val.cexp = desugarCexp(x->val.cexp);
            }
            break;
        case EXP_TYPE_LET:
            x->val.let = desugarExpLet(x->val.let);
            break;
        case EXP_TYPE_DONE:
            break;
        default:
            cant_happen("unrecognized type in desugarAexp");
    }
    return x;
}

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

 // additional desugaring not handled by the parser.
 // we should consider handling all desugaring in the parser and remove this

#include <stdio.h>

#include "common.h"
#include "desugaring.h"
#include "symbol.h"
#include "anf_pp.h"

#ifdef DEBUG_DESUGARING
#  include "debug.h"
#endif

static AexpLam *desugarAexpLam(AexpLam *x);
static AexpPrimApp *desugarAexpPrimApp(AexpPrimApp *x);
static AexpUnaryApp *desugarAexpUnaryApp(AexpUnaryApp *x);
static HashSymbol *desugarAexpVar(HashSymbol *x);
static AexpList *desugarAexpList(AexpList *x);
static CexpApply *desugarCexpApply(CexpApply *x);
static CexpIf *desugarCexpIf(CexpIf *x);
static CexpCond *desugarCexpCond(CexpCond *x);
static CexpLetRec *desugarCexpLetRec(CexpLetRec *x);
static CexpAmb *desugarCexpAmb(CexpAmb *x);
static CexpCut *desugarCexpCut(CexpCut *x);
static ExpLet *desugarExpLet(ExpLet *x);
static Aexp *desugarAexp(Aexp *x);
static Cexp *desugarCexp(Cexp *x);

#ifdef DEBUG_DESUGARING
#  define DEBUG_DESUGAR(type, val) do { printf("desugar" #type ": "); pp ## type (val); printf("\n"); } while(0)
#else
#  define DEBUG_DESUGAR(type, val) do {} while(0)
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

static HashSymbol *desugarAexpVar(HashSymbol *x) {
    DEBUG_DESUGAR(AexpVar, x);
    return x;
}

static AexpList *desugarAexpList(AexpList *x) {
    DEBUG_DESUGAR(AexpList, x);
    AexpList *y = x;
    while (x != NULL) {
        x->exp = desugarAexp(x->exp);
        x = x->next;
    }
    return y;
}

static AexpMakeVec *desugarAexpMakeVec(AexpMakeVec *x) {
    DEBUG_DESUGAR(AexpMakeVec, x);
    x->args = desugarAexpList(x->args);
    return x;
}

static AexpNamespaceArray *desugarAexpNamespaceArray(AexpNamespaceArray *x) {
    DEBUG_DESUGAR(AexpNamespaceArray, x);
    for (Index i = 0; i < x->size; i++) {
        x->entries[i]->body = desugarExp(x->entries[i]->body);
    }
    return x;
}

static AexpNamespaces *desugarAexpNamespaces(AexpNamespaces *x) {
    DEBUG_DESUGAR(AexpNamespaces, x);
    x->namespaces = desugarAexpNamespaceArray(x->namespaces);
    x->body = desugarExp(x->body);
    return x;
}

static CexpApply *desugarCexpApply(CexpApply *x) {
    DEBUG_DESUGAR(CexpApply, x);
    x->function = desugarAexp(x->function);
    x->args = desugarAexpList(x->args);
    return x;
}

static CexpIf *desugarCexpIf(CexpIf *x) {
    DEBUG_DESUGAR(CexpIf, x);
    x->condition = desugarAexp(x->condition);
    x->consequent = desugarExp(x->consequent);
    x->alternative = desugarExp(x->alternative);
    return x;
}

static CexpIntCondCases *desugarCexpIntCondCases(CexpIntCondCases *x) {
    if (x == NULL) {
        return NULL;
    }
    x->body = desugarExp(x->body);
    x->next = desugarCexpIntCondCases(x->next);
    return x;
}

static CexpCharCondCases *desugarCexpCharCondCases(CexpCharCondCases *x) {
    if (x == NULL) {
        return NULL;
    }
    x->body = desugarExp(x->body);
    x->next = desugarCexpCharCondCases(x->next);
    return x;
}

static CexpCondCases *desugarCexpCondCases(CexpCondCases *x) {
    switch (x->type) {
        case CEXPCONDCASES_TYPE_INTCASES:
            x->val.intCases = desugarCexpIntCondCases(x->val.intCases);
            break;
        case CEXPCONDCASES_TYPE_CHARCASES:
            x->val.charCases = desugarCexpCharCondCases(x->val.charCases);
            break;
        default:
            cant_happen("unrecognized type %d in desugarCexpCondCases",
                        x->type);
    }
    return x;
}

static CexpCond *desugarCexpCond(CexpCond *x) {
    x->condition = desugarAexp(x->condition);
    x->cases = desugarCexpCondCases(x->cases);
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

static CexpCut *desugarCexpCut(CexpCut *x) {
    DEBUG_DESUGAR(CexpCut, x);
    x->exp = desugarExp(x->exp);
    return x;
}

static Exp *aexpAndToExp(Aexp *exp1, Exp *exp2) {
    return newExp_Cexp(newCexp_Iff(newCexpIf(exp1, exp2, newExp_Aexp(newAexp_F()))));
}

static Exp *expAndToExp(Exp *exp1, Exp *exp2) {
    HashSymbol *sym = genSym("and_");
    return newExp_Let(
        newExpLet(
            sym,
            exp1,
            newExp_Cexp(
                newCexp_Iff(
                    newCexpIf(
                        newAexp_Var(sym),
                        exp2,
                        newExp_Aexp(newAexp_F()))))));
}

static Exp *andToExp(CexpBool *x) {
    // (and <exp1> <exp2>) => (let (<sym> <exp1>) (if <sym> <exp2> #f))
    // (and <aexp> <exp>) => (if <aexp> <exp> #f)
    Exp *exp1 = desugarExp(x->exp1);
    Exp *exp2 = desugarExp(x->exp2);
    if (exp1->type == EXP_TYPE_AEXP) {
        return aexpAndToExp(exp1->val.aexp, exp2);
    } else {
        return expAndToExp(exp1, exp2);
    }
}

static Exp *aexpOrToExp(Aexp *exp1, Exp *exp2) {
    return newExp_Cexp(
        newCexp_Iff(
            newCexpIf(
                exp1,
                newExp_Aexp(newAexp_T()),
                exp2)));
}

static Exp *expOrToExp(Exp *exp1, Exp *exp2) {
    HashSymbol *sym = genSym("or_");
    return newExp_Let(
        newExpLet(
            sym,
            exp1,
            newExp_Cexp(
                newCexp_Iff(
                    newCexpIf(
                        newAexp_Var(sym),
                        newExp_Aexp(newAexp_T()),
                        exp2)))));
}

static Exp *orToExp(CexpBool *x) {
    // (or <exp1> <exp2>) => (let (<sym> <exp1>) (if <sym> #t <exp2>))
    // (or <aexp> <exp>) => (if <aexp> #t <exp2>)
    Exp *exp1 = desugarExp(x->exp1);
    Exp *exp2 = desugarExp(x->exp2);
    if (exp1->type == EXP_TYPE_AEXP) {
        return aexpOrToExp(exp1->val.aexp, exp2);
    } else {
        return expOrToExp(exp1, exp2);
    }
}

static Exp *boolToExp(CexpBool *x) {
    switch (x->type) {
        case CEXPBOOLTYPE_TYPE_AND:
            return andToExp(x);
        case CEXPBOOLTYPE_TYPE_OR:
            return orToExp(x);
        default:
            cant_happen("unrecognised type %d in boolToExp", x->type);
    }
}

static ExpLet *desugarExpLet(ExpLet *x) {
    DEBUG_DESUGAR(ExpLet, x);
    x->val = desugarExp(x->val);
    x->var = desugarAexpVar(x->var);
    x->body = desugarExp(x->body);
    return x;
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
        case AEXP_TYPE_T:
        case AEXP_TYPE_F:
        case AEXP_TYPE_LITTLEINTEGER:
        case AEXP_TYPE_BIGINTEGER:
        case AEXP_TYPE_CHARACTER:
        case AEXP_TYPE_V:
            break;
        case AEXP_TYPE_PRIM:
            x->val.prim = desugarAexpPrimApp(x->val.prim);
            break;
        case AEXP_TYPE_UNARY:
            x->val.unary = desugarAexpUnaryApp(x->val.unary);
            break;
        case AEXP_TYPE_MAKEVEC:
            x->val.makeVec = desugarAexpMakeVec(x->val.makeVec);
            break;
        case AEXP_TYPE_NAMESPACES:
            x->val.namespaces = desugarAexpNamespaces(x->val.namespaces);
            break;
        default:
            cant_happen("unrecognized type %s in desugarAexp", aexpTypeName(x->type));
    }
    return x;
}

static MatchList *desugarMatchList(MatchList *x) {
    DEBUG_DESUGAR(MatchList, x);
    if (x == NULL)
        return NULL;
    x->body = desugarExp(x->body);
    x->next = desugarMatchList(x->next);
    return x;
}

static CexpMatch *desugarCexpMatch(CexpMatch *x) {
    DEBUG_DESUGAR(CexpMatch, x);
    x->condition = desugarAexp(x->condition);
    x->clauses = desugarMatchList(x->clauses);
    return x;
}

static ExpLookup *desugarExpLookup(ExpLookup *x) {
    x->body = desugarExp(x->body);
    return x;
}

static Cexp *desugarCexp(Cexp *x) {
    DEBUG_DESUGAR(Cexp, x);
    switch (x->type) {
        case CEXP_TYPE_APPLY:
            x->val.apply = desugarCexpApply(x->val.apply);
            break;
        case CEXP_TYPE_IFF:
            x->val.iff = desugarCexpIf(x->val.iff);
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
        case CEXP_TYPE_CUT:
            x->val.cut = desugarCexpCut(x->val.cut);
            break;
        case CEXP_TYPE_MATCH:
            x->val.match = desugarCexpMatch(x->val.match);
            break;
        case CEXP_TYPE_BOOLEAN:
            cant_happen("desugarCexp given CEXP_TYPE_BOOL");
            break;
        case CEXP_TYPE_ERROR:
        case CEXP_TYPE_BACK:
            break;
        default:
            cant_happen("unrecognized type %s in desugarCexp", cexpTypeName(x->type));
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
            if (x->val.cexp->type == CEXP_TYPE_BOOLEAN) {
                return boolToExp(x->val.cexp->val.boolean);
            } else {
                x->val.cexp = desugarCexp(x->val.cexp);
            }
            break;
        case EXP_TYPE_LET:
            x->val.let = desugarExpLet(x->val.let);
            break;
        case EXP_TYPE_LOOKUP:
            x->val.lookup = desugarExpLookup(x->val.lookup);
        case EXP_TYPE_DONE:
        case EXP_TYPE_ENV:
            break;
        default:
            cant_happen("unrecognized type %s in desugarExp", expTypeName(x->type));
    }
    return x;
}

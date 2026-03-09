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
 * bespoke pretty-printer for minlam.yaml structs
 *
 */

#include "minlam_pp.h"
#include <stdio.h>
#include <unistd.h>
void ppMinTag(MinExp *tag);

// indented pretty-printer internals
static void iMinExp(MinExp *exp, int d);
static void iMinLam(MinLam *lam, int d);
static void iMinAmb(MinAmb *amb, int d);
static void iMinPrimApp(MinPrimApp *primApp, int d);
static void iMinApply(MinApply *apply, int d);
static void iMinIff(MinIff *iff, int d);
static void iMinLetRec(MinLetRec *letRec, int d);
static void iMinBindings(MinBindings *bindings, int d);
static void iMinMatch(MinMatch *match, int d);
static void iMinCond(MinCond *cond, int d);
static void iMinMakeVec(MinExprList *makeVec, int d);
static void iMinSequence(MinExprList *sequence, int d);
static void iMinCallCC(MinExp *exp, int d);
static void iMinAnnotatedVar(MinAnnotatedVar *var);

static void indent(int d) {
    for (int i = 0; i < d; i++) {
        eprintf("  ");
    }
}

static void newlineIndent(int d) {
    eprintf("\n");
    indent(d);
}

// returns true if the expression is "simple" (fits on one line)
static bool isSimple(MinExp *exp) {
    if (exp == NULL)
        return true;
    switch (exp->type) {
    case MINEXP_TYPE_VAR:
    case MINEXP_TYPE_BIGINTEGER:
    case MINEXP_TYPE_STDINT:
    case MINEXP_TYPE_CHARACTER:
    case MINEXP_TYPE_BACK:
    case MINEXP_TYPE_DONE:
    case MINEXP_TYPE_AVAR:
        return true;
    case MINEXP_TYPE_PRIM: {
        MinPrimApp *p = getMinExp_Prim(exp);
        return isSimple(p->exp1) && isSimple(p->exp2);
    }
    default:
        return false;
    }
}

static bool allSimple(MinExprList *list) {
    while (list != NULL) {
        if (!isSimple(list->exp))
            return false;
        list = list->next;
    }
    return true;
}

void ppMinExpD(MinExp *exp, int depth) { iMinExp(exp, depth); }

static void iMinLam(MinLam *lam, int d) {
    if (lam == NULL) {
        eprintf("<NULL lambda>");
        return;
    }
    eprintf("(λ ");
    ppMinVarList(lam->args);
    newlineIndent(d + 1);
    iMinExp(lam->exp, d + 1);
    eprintf(")");
}

static void iMinAmb(MinAmb *amb, int d) {
    if (amb == NULL) {
        eprintf("<NULL amb>");
        return;
    }
    eprintf("(amb");
    newlineIndent(d + 1);
    iMinExp(amb->left, d + 1);
    newlineIndent(d + 1);
    iMinExp(amb->right, d + 1);
    eprintf(")");
}

void ppMinLam(MinLam *lam) { iMinLam(lam, 0); }

void ppMinAmb(MinAmb *amb) { iMinAmb(amb, 0); }

static void _ppMinVarList(SymbolList *varList) {
    if (varList == NULL)
        return;
    ppHashSymbol(varList->symbol);
    if (varList->next != NULL) {
        eprintf(" ");
        _ppMinVarList(varList->next);
    }
}

void ppMinVarList(SymbolList *varList) {
    eprintf("(");
    _ppMinVarList(varList);
    eprintf(")");
}

static void iMinExp(MinExp *exp, int d) {
    if (exp == NULL) {
        eprintf("<NULL exp>");
        return;
    }
    switch (exp->type) {
    case MINEXP_TYPE_LAM:
        iMinLam(getMinExp_Lam(exp), d);
        break;
    case MINEXP_TYPE_VAR:
        ppHashSymbol(getMinExp_Var(exp));
        break;
    case MINEXP_TYPE_AVAR:
        iMinAnnotatedVar(getMinExp_Avar(exp));
        break;
    case MINEXP_TYPE_BIGINTEGER:
        fprintMaybeBigInt(errout, getMinExp_BigInteger(exp));
        break;
    case MINEXP_TYPE_STDINT:
        eprintf("%d", getMinExp_Stdint(exp));
        break;
    case MINEXP_TYPE_PRIM:
        iMinPrimApp(getMinExp_Prim(exp), d);
        break;
    case MINEXP_TYPE_SEQUENCE:
        iMinSequence(getMinExp_Sequence(exp), d);
        break;
    case MINEXP_TYPE_MAKEVEC:
        iMinMakeVec(getMinExp_MakeVec(exp), d);
        break;
    case MINEXP_TYPE_APPLY:
        iMinApply(getMinExp_Apply(exp), d);
        break;
    case MINEXP_TYPE_IFF:
        iMinIff(getMinExp_Iff(exp), d);
        break;
    case MINEXP_TYPE_CALLCC:
        iMinCallCC(getMinExp_CallCC(exp), d);
        break;
    case MINEXP_TYPE_LETREC:
        iMinLetRec(getMinExp_LetRec(exp), d);
        break;
    case MINEXP_TYPE_MATCH:
        iMinMatch(getMinExp_Match(exp), d);
        break;
    case MINEXP_TYPE_CHARACTER:
        if (getMinExp_Character(exp) == L'\n')
            eprintf("\"\\n\"");
        else if (getMinExp_Character(exp) == L'\t')
            eprintf("\"\\t\"");
        else if (getMinExp_Character(exp) == L'\"')
            eprintf("\"\\\"\"");
        else if (getMinExp_Character(exp) == L'\\')
            eprintf("\"\\\\\"");
        else
            eprintf("\"%lc\"", getMinExp_Character(exp));
        break;
    case MINEXP_TYPE_BACK:
        eprintf("(back)");
        break;
    case MINEXP_TYPE_DONE:
        eprintf("(done)");
        break;
    case MINEXP_TYPE_COND:
        iMinCond(getMinExp_Cond(exp), d);
        break;
    case MINEXP_TYPE_AMB:
        iMinAmb(getMinExp_Amb(exp), d);
        break;
    default:
        eprintf("<unrecognised expression type %s>", minExpTypeName(exp->type));
    }
}

void ppMinExp(MinExp *exp) { iMinExp(exp, 0); }

static void iMinPrimApp(MinPrimApp *primApp, int d) {
    if (primApp == NULL) {
        eprintf("<NULL primApp>");
        return;
    }
    eprintf("(");
    ppMinPrimOp(primApp->type);
    eprintf(" ");
    iMinExp(primApp->exp1, d);
    eprintf(" ");
    iMinExp(primApp->exp2, d);
    eprintf(")");
}

void ppMinPrimApp(MinPrimApp *primApp) { iMinPrimApp(primApp, 0); }

void ppMinPrimOp(MinPrimOp type) {
    switch (type) {
    case MINPRIMOP_TYPE_ADD:
        eprintf("+");
        break;
    case MINPRIMOP_TYPE_SUB:
        eprintf("-");
        break;
    case MINPRIMOP_TYPE_MUL:
        eprintf("*");
        break;
    case MINPRIMOP_TYPE_DIV:
        eprintf("/");
        break;
    case MINPRIMOP_TYPE_GCD:
        eprintf("gcd");
        break;
    case MINPRIMOP_TYPE_LCM:
        eprintf("lcm");
        break;
    case MINPRIMOP_TYPE_CANON:
        eprintf("canon");
        break;
    case MINPRIMOP_TYPE_EQ:
        eprintf("==");
        break;
    case MINPRIMOP_TYPE_NE:
        eprintf("!=");
        break;
    case MINPRIMOP_TYPE_GT:
        eprintf(">");
        break;
    case MINPRIMOP_TYPE_LT:
        eprintf("<");
        break;
    case MINPRIMOP_TYPE_GE:
        eprintf(">=");
        break;
    case MINPRIMOP_TYPE_LE:
        eprintf("<=");
        break;
    case MINPRIMOP_TYPE_VEC:
        eprintf("vec");
        break;
    case MINPRIMOP_TYPE_MOD:
        eprintf("%%");
        break;
    case MINPRIMOP_TYPE_POW:
        eprintf("**");
        break;
    case MINPRIMOP_TYPE_CMP:
        eprintf("<=>");
        break;
    default:
        eprintf("<unrecognised prim op %s>", minPrimOpName(type));
    }
}

// indented sequence helper
static void _iMinSequence(MinExprList *sequence, int d, bool first) {
    if (sequence == NULL)
        return;
    if (!first) {
        if (isSimple(sequence->exp)) {
            eprintf(" ");
        } else {
            newlineIndent(d);
        }
    }
    iMinExp(sequence->exp, d);
    _iMinSequence(sequence->next, d, false);
}

// indented args helper
static void _iMinArgs(MinExprList *list, int d) {
    if (list == NULL)
        return;
    if (isSimple(list->exp)) {
        eprintf(" ");
    } else {
        newlineIndent(d);
    }
    iMinExp(list->exp, d);
    _iMinArgs(list->next, d);
}

static void iMinSequence(MinExprList *sequence, int d) {
    eprintf("(begin");
    _iMinSequence(sequence, d + 1, false);
    eprintf(")");
}

void ppMinSequence(MinExprList *sequence) { iMinSequence(sequence, 0); }

static void iMinMakeVec(MinExprList *makeVec, int d) {
    eprintf("(make-vec");
    _iMinArgs(makeVec, d + 1);
    eprintf(")");
}

void ppMinMakeVec(MinExprList *makeVec) { iMinMakeVec(makeVec, 0); }

void ppMinMakeTuple(MinExprList *args) {
    eprintf("(make-tuple");
    _iMinArgs(args, 1);
    eprintf(")");
}

static void iMinApply(MinApply *apply, int d) {
    if (apply == NULL) {
        eprintf("<NULL apply>");
        return;
    }
    if (apply->isBuiltin) {
        eprintf("[builtin ");
    } else {
        eprintf("(");
    }
    iMinExp(apply->function, d + 1);
    if (allSimple(apply->args)) {
        _iMinArgs(apply->args, d + 1);
    } else {
        _iMinArgs(apply->args, d + 1);
    }
    if (apply->isBuiltin) {
        eprintf("]");
    } else {
        eprintf(")");
    }
}

void ppMinApply(MinApply *apply) { iMinApply(apply, 0); }

static void iMinIff(MinIff *iff, int d) {
    if (iff == NULL) {
        eprintf("<NULL if>");
        return;
    }
    eprintf("(if ");
    iMinExp(iff->condition, d + 1);
    newlineIndent(d + 1);
    iMinExp(iff->consequent, d + 1);
    newlineIndent(d + 1);
    iMinExp(iff->alternative, d + 1);
    eprintf(")");
}

void ppMinIff(MinIff *iff) { iMinIff(iff, 0); }

static void _iMinIntCondCases(MinIntCondCases *cases, int d) {
    newlineIndent(d);
    eprintf("(");
    fprintMaybeBigInt(errout, cases->constant);
    eprintf(" ");
    iMinExp(cases->body, d + 1);
    eprintf(")");
    if (cases->next != NULL) {
        _iMinIntCondCases(cases->next, d);
    }
}

static void _iMinCharCondCases(MinCharCondCases *cases, int d) {
    newlineIndent(d);
    eprintf("(\"%c\" ", cases->constant);
    iMinExp(cases->body, d + 1);
    eprintf(")");
    if (cases->next != NULL) {
        _iMinCharCondCases(cases->next, d);
    }
}

static void _iMinCondCases(MinCondCases *cases, int d) {
    switch (cases->type) {
    case MINCONDCASES_TYPE_INTEGERS:
        _iMinIntCondCases(getMinCondCases_Integers(cases), d);
        break;
    case MINCONDCASES_TYPE_CHARACTERS:
        _iMinCharCondCases(getMinCondCases_Characters(cases), d);
        break;
    default:
        eprintf("<unrecognised case type %s>",
                minCondCasesTypeName(cases->type));
    }
}

static void iMinCond(MinCond *cond, int d) {
    if (cond == NULL) {
        eprintf("<NULL cond>");
        return;
    }
    eprintf("(cond ");
    iMinExp(cond->value, d + 1);
    if (cond->cases != NULL) {
        _iMinCondCases(cond->cases, d + 1);
    }
    eprintf(")");
}

void ppMinCond(MinCond *cond) { iMinCond(cond, 0); }

static void iMinCallCC(MinExp *exp, int d) {
    if (exp == NULL) {
        eprintf("<NULL call/cc>");
        return;
    }
    eprintf("(call/cc");
    newlineIndent(d + 1);
    iMinExp(exp, d + 1);
    eprintf(")");
}

void ppMinCallCC(MinExp *exp) { iMinCallCC(exp, 0); }

static void _iMinBindings(MinBindings *bindings, int d) {
    if (bindings == NULL)
        return;
    newlineIndent(d);
    eprintf("(");
    ppHashSymbol(bindings->var);
    if (isSimple(bindings->val)) {
        eprintf(" ");
        iMinExp(bindings->val, d + 1);
    } else {
        newlineIndent(d + 1);
        iMinExp(bindings->val, d + 1);
    }
    eprintf(")");
    _iMinBindings(bindings->next, d);
}

static void iMinBindings(MinBindings *bindings, int d) {
    eprintf("(");
    _iMinBindings(bindings, d);
    eprintf(")");
}

static void iMinLetRec(MinLetRec *letRec, int d) {
    if (letRec == NULL) {
        eprintf("<NULL letRec>");
        return;
    }
    eprintf("(letrec");
    newlineIndent(d + 1);
    iMinBindings(letRec->bindings, d + 2);
    if (letRec->body != NULL) {
        newlineIndent(d + 1);
        iMinExp(letRec->body, d + 1);
    }
    eprintf(")");
}

void ppMinLetRec(MinLetRec *letRec) { iMinLetRec(letRec, 0); }

static void _iMinMatchList(MinMatchList *cases, int d) {
    if (cases == NULL)
        return;
    newlineIndent(d);
    eprintf("(");
    ppMinIntList(cases->matches);
    if (cases->body) {
        if (isSimple(cases->body)) {
            eprintf(" ");
            iMinExp(cases->body, d + 1);
        } else {
            newlineIndent(d + 1);
            iMinExp(cases->body, d + 1);
        }
    }
    eprintf(")");
    _iMinMatchList(cases->next, d);
}

static void iMinMatch(MinMatch *match, int d) {
    if (match == NULL) {
        eprintf("<NULL match>");
        return;
    }
    eprintf("(match ");
    iMinExp(match->index, d + 1);
    if (match->cases != NULL) {
        _iMinMatchList(match->cases, d + 1);
    }
    eprintf(")");
}

void ppMinMatch(MinMatch *match) { iMinMatch(match, 0); }

static void iMinAnnotatedVar(MinAnnotatedVar *var) {
    if (var == NULL) {
        eprintf("<NULL annotated var>");
        return;
    }
    ppHashSymbol(var->var);
    eprintf("<%d>", var->position);
}

void ppMinAnnotatedVar(MinAnnotatedVar *var) { iMinAnnotatedVar(var); }

static void _ppMinBindings(MinBindings *bindings) {
    if (bindings == NULL)
        return;
    eprintf("(");
    ppHashSymbol(bindings->var);
    eprintf(" ");
    ppMinExp(bindings->val);
    eprintf(")");
    if (bindings->next) {
        eprintf(" ");
        _ppMinBindings(bindings->next);
    }
}

void ppMinBindings(MinBindings *bindings) {
    eprintf("(");
    _ppMinBindings(bindings);
    eprintf(")");
}

static void _ppMinIntList(MinIntList *list) {
    if (list == NULL)
        return;
    eprintf("%d", list->item);
    if (list->next != NULL) {
        eprintf(" ");
        _ppMinIntList(list->next);
    }
}

void ppMinIntList(MinIntList *list) {
    eprintf("(");
    _ppMinIntList(list);
    eprintf(")");
}

void ppMinTag(MinExp *tag) {
    eprintf("(tag ");
    ppMinExp(tag);
    eprintf(")");
}
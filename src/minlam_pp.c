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
void ppMinTag(FILE *out, MinExp *tag);

// indented pretty-printer internals
static void iMinExp(FILE *out, MinExp *exp, int d);
static void iMinLam(FILE *out, MinLam *lam, int d);
static void iMinAmb(FILE *out, MinAmb *amb, int d);
static void iMinPrimApp(FILE *out, MinPrimApp *primApp, int d);
static void iMinApply(FILE *out, MinApply *apply, int d);
static void iMinIff(FILE *out, MinIff *iff, int d);
static void iMinLetRec(FILE *out, MinLetRec *letRec, int d);
static void iMinBindings(FILE *out, MinBindings *bindings, int d);
static void iMinMatch(FILE *out, MinMatch *match, int d);
static void iMinCond(FILE *out, MinCond *cond, int d);
static void iMinMakeVec(FILE *out, MinExprList *makeVec, int d);
static void iMinSequence(FILE *out, MinExprList *sequence, int d);
static void iMinCallCC(FILE *out, MinExp *exp, int d);
static void iMinAnnotatedVar(FILE *out, MinAnnotatedVar *var);

static void indent(FILE *out, int d) {
    for (int i = 0; i < d; i++) {
        fprintf(out, "  ");
    }
}

static void newlineIndent(FILE *out, int d) {
    fprintf(out, "\n");
    indent(out, d);
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

static void ppMinHashSymbol(FILE *out, HashSymbol *symbol) {
    fprintf(out, "%s", symbol->name);
}

void ppMinExpD(FILE *out, MinExp *exp, int depth) { iMinExp(out, exp, depth); }

static void iMinLam(FILE *out, MinLam *lam, int d) {
    if (lam == NULL) {
        fprintf(out, "<NULL lambda>");
        return;
    }
    fprintf(out, "(λ ");
    ppMinVarList(out, lam->args);
    newlineIndent(out, d + 1);
    iMinExp(out, lam->exp, d + 1);
    fprintf(out, ")");
}

static void iMinAmb(FILE *out, MinAmb *amb, int d) {
    if (amb == NULL) {
        fprintf(out, "<NULL amb>");
        return;
    }
    fprintf(out, "(amb");
    newlineIndent(out, d + 1);
    iMinExp(out, amb->left, d + 1);
    newlineIndent(out, d + 1);
    iMinExp(out, amb->right, d + 1);
    fprintf(out, ")");
}

void ppMinLam(FILE *out, MinLam *lam) { iMinLam(out, lam, 0); }

void ppMinAmb(FILE *out, MinAmb *amb) { iMinAmb(out, amb, 0); }

static void _ppMinVarList(FILE *out, SymbolList *varList) {
    if (varList == NULL)
        return;
    ppMinHashSymbol(out, varList->symbol);
    if (varList->next != NULL) {
        fprintf(out, " ");
        _ppMinVarList(out, varList->next);
    }
}

void ppMinVarList(FILE *out, SymbolList *varList) {
    fprintf(out, "(");
    _ppMinVarList(out, varList);
    fprintf(out, ")");
}

static void iMinExp(FILE *out, MinExp *exp, int d) {
    if (exp == NULL) {
        fprintf(out, "<NULL exp>");
        return;
    }
    switch (exp->type) {
    case MINEXP_TYPE_LAM:
        iMinLam(out, getMinExp_Lam(exp), d);
        break;
    case MINEXP_TYPE_VAR:
        ppMinHashSymbol(out, getMinExp_Var(exp));
        break;
    case MINEXP_TYPE_AVAR:
        iMinAnnotatedVar(out, getMinExp_Avar(exp));
        break;
    case MINEXP_TYPE_BIGINTEGER:
        fprintMaybeBigInt2(out, getMinExp_BigInteger(exp));
        break;
    case MINEXP_TYPE_STDINT:
        fprintf(out, "%d", getMinExp_Stdint(exp));
        break;
    case MINEXP_TYPE_PRIM:
        iMinPrimApp(out, getMinExp_Prim(exp), d);
        break;
    case MINEXP_TYPE_SEQUENCE:
        iMinSequence(out, getMinExp_Sequence(exp), d);
        break;
    case MINEXP_TYPE_MAKEVEC:
        iMinMakeVec(out, getMinExp_MakeVec(exp), d);
        break;
    case MINEXP_TYPE_APPLY:
        iMinApply(out, getMinExp_Apply(exp), d);
        break;
    case MINEXP_TYPE_IFF:
        iMinIff(out, getMinExp_Iff(exp), d);
        break;
    case MINEXP_TYPE_CALLCC:
        iMinCallCC(out, getMinExp_CallCC(exp), d);
        break;
    case MINEXP_TYPE_LETREC:
        iMinLetRec(out, getMinExp_LetRec(exp), d);
        break;
    case MINEXP_TYPE_MATCH:
        iMinMatch(out, getMinExp_Match(exp), d);
        break;
    case MINEXP_TYPE_CHARACTER:
        if (getMinExp_Character(exp) == L'\n')
            fprintf(out, "\"\\n\"");
        else if (getMinExp_Character(exp) == L'\t')
            fprintf(out, "\"\\t\"");
        else if (getMinExp_Character(exp) == L'\"')
            fprintf(out, "\"\\\"\"");
        else if (getMinExp_Character(exp) == L'\\')
            fprintf(out, "\"\\\\\"");
        else
            fprintf(out, "\"%lc\"", getMinExp_Character(exp));
        break;
    case MINEXP_TYPE_BACK:
        fprintf(out, "(back)");
        break;
    case MINEXP_TYPE_DONE:
        fprintf(out, "(done)");
        break;
    case MINEXP_TYPE_COND:
        iMinCond(out, getMinExp_Cond(exp), d);
        break;
    case MINEXP_TYPE_AMB:
        iMinAmb(out, getMinExp_Amb(exp), d);
        break;
    default:
        fprintf(out, "<unrecognised expression type %s>",
                minExpTypeName(exp->type));
    }
}

void ppMinExp(FILE *out, MinExp *exp) { iMinExp(out, exp, 0); }

static void iMinPrimApp(FILE *out, MinPrimApp *primApp, int d) {
    if (primApp == NULL) {
        fprintf(out, "<NULL primApp>");
        return;
    }
    fprintf(out, "(");
    ppMinPrimOp(out, primApp->type);
    fprintf(out, " ");
    iMinExp(out, primApp->exp1, d);
    fprintf(out, " ");
    iMinExp(out, primApp->exp2, d);
    fprintf(out, ")");
}

void ppMinPrimApp(FILE *out, MinPrimApp *primApp) {
    iMinPrimApp(out, primApp, 0);
}

void ppMinPrimOp(FILE *out, MinPrimOp type) {
    switch (type) {
    case MINPRIMOP_TYPE_ADD:
        fprintf(out, "+");
        break;
    case MINPRIMOP_TYPE_SUB:
        fprintf(out, "-");
        break;
    case MINPRIMOP_TYPE_MUL:
        fprintf(out, "*");
        break;
    case MINPRIMOP_TYPE_DIV:
        fprintf(out, "/");
        break;
    case MINPRIMOP_TYPE_GCD:
        fprintf(out, "gcd");
        break;
    case MINPRIMOP_TYPE_LCM:
        fprintf(out, "lcm");
        break;
    case MINPRIMOP_TYPE_CANON:
        fprintf(out, "canon");
        break;
    case MINPRIMOP_TYPE_EQ:
        fprintf(out, "==");
        break;
    case MINPRIMOP_TYPE_NE:
        fprintf(out, "!=");
        break;
    case MINPRIMOP_TYPE_GT:
        fprintf(out, ">");
        break;
    case MINPRIMOP_TYPE_LT:
        fprintf(out, "<");
        break;
    case MINPRIMOP_TYPE_GE:
        fprintf(out, ">=");
        break;
    case MINPRIMOP_TYPE_LE:
        fprintf(out, "<=");
        break;
    case MINPRIMOP_TYPE_VEC:
        fprintf(out, "vec");
        break;
    case MINPRIMOP_TYPE_MOD:
        fprintf(out, "%%");
        break;
    case MINPRIMOP_TYPE_POW:
        fprintf(out, "**");
        break;
    case MINPRIMOP_TYPE_CMP:
        fprintf(out, "<=>");
        break;
    default:
        fprintf(out, "<unrecognised prim op %s>", minPrimOpName(type));
    }
}

// indented sequence helper
static void _iMinSequence(FILE *out, MinExprList *sequence, int d, bool first) {
    if (sequence == NULL)
        return;
    if (!first) {
        if (isSimple(sequence->exp)) {
            fprintf(out, " ");
        } else {
            newlineIndent(out, d);
        }
    }
    iMinExp(out, sequence->exp, d);
    _iMinSequence(out, sequence->next, d, false);
}

// indented args helper
static void _iMinArgs(FILE *out, MinExprList *list, int d) {
    if (list == NULL)
        return;
    if (isSimple(list->exp)) {
        fprintf(out, " ");
    } else {
        newlineIndent(out, d);
    }
    iMinExp(out, list->exp, d);
    _iMinArgs(out, list->next, d);
}

static void iMinSequence(FILE *out, MinExprList *sequence, int d) {
    fprintf(out, "(begin");
    _iMinSequence(out, sequence, d + 1, false);
    fprintf(out, ")");
}

void ppMinSequence(FILE *out, MinExprList *sequence) {
    iMinSequence(out, sequence, 0);
}

static void iMinMakeVec(FILE *out, MinExprList *makeVec, int d) {
    fprintf(out, "(make-vec");
    _iMinArgs(out, makeVec, d + 1);
    fprintf(out, ")");
}

void ppMinMakeVec(FILE *out, MinExprList *makeVec) {
    iMinMakeVec(out, makeVec, 0);
}

void ppMinMakeTuple(FILE *out, MinExprList *args) {
    fprintf(out, "(make-tuple");
    _iMinArgs(out, args, 1);
    fprintf(out, ")");
}

static void iMinApply(FILE *out, MinApply *apply, int d) {
    if (apply == NULL) {
        fprintf(out, "<NULL apply>");
        return;
    }
    if (apply->isBuiltin) {
        fprintf(out, "[builtin ");
    } else {
        if (apply->cc) {
            fprintf(out, "(A/C");
        } else {
            fprintf(out, "(");
        }
    }
    iMinExp(out, apply->function, d + 1);
    if (allSimple(apply->args)) {
        _iMinArgs(out, apply->args, d + 1);
    } else {
        _iMinArgs(out, apply->args, d + 1);
    }
    if (apply->isBuiltin) {
        fprintf(out, "]");
    } else {
        fprintf(out, ")");
    }
}

void ppMinApply(FILE *out, MinApply *apply) { iMinApply(out, apply, 0); }

static void iMinIff(FILE *out, MinIff *iff, int d) {
    if (iff == NULL) {
        fprintf(out, "<NULL if>");
        return;
    }
    fprintf(out, "(if ");
    iMinExp(out, iff->condition, d + 1);
    newlineIndent(out, d + 1);
    iMinExp(out, iff->consequent, d + 1);
    newlineIndent(out, d + 1);
    iMinExp(out, iff->alternative, d + 1);
    fprintf(out, ")");
}

void ppMinIff(FILE *out, MinIff *iff) { iMinIff(out, iff, 0); }

static void _iMinIntCondCases(FILE *out, MinIntCondCases *cases, int d) {
    newlineIndent(out, d);
    fprintf(out, "(");
    fprintMaybeBigInt2(out, cases->constant);
    fprintf(out, " ");
    iMinExp(out, cases->body, d + 1);
    fprintf(out, ")");
    if (cases->next != NULL) {
        _iMinIntCondCases(out, cases->next, d);
    }
}

static void _iMinCharCondCases(FILE *out, MinCharCondCases *cases, int d) {
    newlineIndent(out, d);
    if (cases->isDefault) {
        fprintf(out, "(_ ");
    } else {
        fprintf(out, "(\"%c\" ", cases->constant);
    }
    iMinExp(out, cases->body, d + 1);
    fprintf(out, ")");
    if (cases->next != NULL) {
        _iMinCharCondCases(out, cases->next, d);
    }
}

static void _iMinCondCases(FILE *out, MinCondCases *cases, int d) {
    switch (cases->type) {
    case MINCONDCASES_TYPE_INTEGERS:
        _iMinIntCondCases(out, getMinCondCases_Integers(cases), d);
        break;
    case MINCONDCASES_TYPE_CHARACTERS:
        _iMinCharCondCases(out, getMinCondCases_Characters(cases), d);
        break;
    default:
        fprintf(out, "<unrecognised case type %s>",
                minCondCasesTypeName(cases->type));
    }
}

static void iMinCond(FILE *out, MinCond *cond, int d) {
    if (cond == NULL) {
        fprintf(out, "<NULL cond>");
        return;
    }
    fprintf(out, "(cond ");
    iMinExp(out, cond->value, d + 1);
    if (cond->cases != NULL) {
        _iMinCondCases(out, cond->cases, d + 1);
    }
    fprintf(out, ")");
}

void ppMinCond(FILE *out, MinCond *cond) { iMinCond(out, cond, 0); }

static void iMinCallCC(FILE *out, MinExp *exp, int d) {
    if (exp == NULL) {
        fprintf(out, "<NULL call/cc>");
        return;
    }
    fprintf(out, "(call/cc");
    newlineIndent(out, d + 1);
    iMinExp(out, exp, d + 1);
    fprintf(out, ")");
}

void ppMinCallCC(FILE *out, MinExp *exp) { iMinCallCC(out, exp, 0); }

static void _iMinBindings(FILE *out, MinBindings *bindings, int d) {
    if (bindings == NULL)
        return;
    newlineIndent(out, d);
    fprintf(out, "(");
    ppMinHashSymbol(out, bindings->var);
    if (isSimple(bindings->val)) {
        fprintf(out, " ");
        iMinExp(out, bindings->val, d + 1);
    } else {
        newlineIndent(out, d + 1);
        iMinExp(out, bindings->val, d + 1);
    }
    fprintf(out, ")");
    _iMinBindings(out, bindings->next, d);
}

static void iMinBindings(FILE *out, MinBindings *bindings, int d) {
    fprintf(out, "(");
    _iMinBindings(out, bindings, d);
    fprintf(out, ")");
}

static void iMinLetRec(FILE *out, MinLetRec *letRec, int d) {
    if (letRec == NULL) {
        fprintf(out, "<NULL letRec>");
        return;
    }
    fprintf(out, "(letrec");
    newlineIndent(out, d + 1);
    iMinBindings(out, letRec->bindings, d + 2);
    if (letRec->body != NULL) {
        newlineIndent(out, d + 1);
        iMinExp(out, letRec->body, d + 1);
    }
    fprintf(out, ")");
}

void ppMinLetRec(FILE *out, MinLetRec *letRec) { iMinLetRec(out, letRec, 0); }

static void _iMinMatchList(FILE *out, MinMatchList *cases, int d) {
    if (cases == NULL)
        return;
    newlineIndent(out, d);
    fprintf(out, "(");
    ppMinIntList(out, cases->matches);
    if (cases->body) {
        if (isSimple(cases->body)) {
            fprintf(out, " ");
            iMinExp(out, cases->body, d + 1);
        } else {
            newlineIndent(out, d + 1);
            iMinExp(out, cases->body, d + 1);
        }
    }
    fprintf(out, ")");
    _iMinMatchList(out, cases->next, d);
}

static void iMinMatch(FILE *out, MinMatch *match, int d) {
    if (match == NULL) {
        fprintf(out, "<NULL match>");
        return;
    }
    fprintf(out, "(match ");
    iMinExp(out, match->index, d + 1);
    if (match->cases != NULL) {
        _iMinMatchList(out, match->cases, d + 1);
    }
    fprintf(out, ")");
}

void ppMinMatch(FILE *out, MinMatch *match) { iMinMatch(out, match, 0); }

static void iMinAnnotatedVar(FILE *out, MinAnnotatedVar *var) {
    if (var == NULL) {
        fprintf(out, "<NULL annotated var>");
        return;
    }
    ppMinHashSymbol(out, var->var);
    fprintf(out, "<%d>", var->position);
}

void ppMinAnnotatedVar(FILE *out, MinAnnotatedVar *var) {
    iMinAnnotatedVar(out, var);
}

static void _ppMinBindings(FILE *out, MinBindings *bindings) {
    if (bindings == NULL)
        return;
    fprintf(out, "(");
    ppMinHashSymbol(out, bindings->var);
    fprintf(out, " ");
    ppMinExp(out, bindings->val);
    fprintf(out, ")");
    if (bindings->next) {
        fprintf(out, " ");
        _ppMinBindings(out, bindings->next);
    }
}

void ppMinBindings(FILE *out, MinBindings *bindings) {
    fprintf(out, "(");
    _ppMinBindings(out, bindings);
    fprintf(out, ")");
}

static void _ppMinIntList(FILE *out, MinIntList *list) {
    if (list == NULL)
        return;
    fprintf(out, "%d", list->item);
    if (list->next != NULL) {
        fprintf(out, " ");
        _ppMinIntList(out, list->next);
    }
}

void ppMinIntList(FILE *out, MinIntList *list) {
    fprintf(out, "(");
    _ppMinIntList(out, list);
    fprintf(out, ")");
}

void ppMinTag(FILE *out, MinExp *tag) {
    fprintf(out, "(tag ");
    ppMinExp(out, tag);
    fprintf(out, ")");
}
/*
 * CEKF - VM supporting amb
 * Copyright (C) 2022-2025  Bill Hails
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

// Bespoke pretty-printer for anf

#include <stdio.h>
#include <stdlib.h>

#include "anf_pp.h"
#include "common.h"
#include "hash.h"
#include "utils.h"

void ppAnfEnv(FILE *out, AnfEnv *env) {
    fprintf(out, "[\n");
    if (env == NULL) {
        fprintf(out, "]\n");
        return;
    }
    Index i = 0;
    HashSymbol *key;
    int value;
    while ((key = iterateIntMap(env->table, &i, &value)) != NULL) {
        fprintf(out, "%s: %d\n", key->name, value);
    }
    ppAnfEnv(out, env->next);
    fprintf(out, "]\n");
}

void ppAexpLam(FILE *out, AexpLam *x) {
    fprintf(out, "(lambda ");
    ppAexpVarList(out, x->args);
    fprintf(out, " ");
    ppAnfExp(out, x->exp);
    fprintf(out, ")");
}

void ppAexpVarList(FILE *out, AexpVarList *x) {
    fprintf(out, "(");
    while (x != NULL) {
        ppAexpVar(out, x->var);
        if (x->next != NULL) {
            fprintf(out, " ");
        }
        x = x->next;
    }
    fprintf(out, ")");
}

static void ppChar(FILE *out, char c) {
    switch (c) {
    case '\n':
        fprintf(out, "\"\\n\"");
        break;
    case '\t':
        fprintf(out, "\"\\t\"");
        break;
    case '\"':
        fprintf(out, "\"\\\"\"");
        break;
    default:
        fprintf(out, "\"%c\"", c);
    }
}

void ppAexpVar(FILE *out, HashSymbol *x) { fprintf(out, "%s", x->name); }

void ppAexpAnnotatedVar(FILE *out, AexpAnnotatedVar *x) {
    // ppAexpVar(out, x->var);
    if (x->type == AEXPANNOTATEDVARTYPE_TYPE_STACK)
        fprintf(out, "LVAR:%s:%d", x->var->name, x->offset);
    else
        fprintf(out, "VAR:%s:%d:%d", x->var->name, x->frame, x->offset);
}

void ppAexpPrimApp(FILE *out, AexpPrimApp *x) {
    fprintf(out, "(");
    switch (x->type) {
    case AEXPPRIMOP_TYPE_ADD:
        fprintf(out, "add ");
        break;
    case AEXPPRIMOP_TYPE_SUB:
        fprintf(out, "sub ");
        break;
    case AEXPPRIMOP_TYPE_MUL:
        fprintf(out, "mul ");
        break;
    case AEXPPRIMOP_TYPE_DIV:
        fprintf(out, "div ");
        break;
    case AEXPPRIMOP_TYPE_GCD:
        fprintf(out, "gcd ");
        break;
    case AEXPPRIMOP_TYPE_LCM:
        fprintf(out, "lcm ");
        break;
    case AEXPPRIMOP_TYPE_CANON:
        fprintf(out, "canon ");
        break;
    case AEXPPRIMOP_TYPE_EQ:
        fprintf(out, "eq ");
        break;
    case AEXPPRIMOP_TYPE_NE:
        fprintf(out, "ne ");
        break;
    case AEXPPRIMOP_TYPE_GT:
        fprintf(out, "gt ");
        break;
    case AEXPPRIMOP_TYPE_LT:
        fprintf(out, "lt ");
        break;
    case AEXPPRIMOP_TYPE_GE:
        fprintf(out, "ge ");
        break;
    case AEXPPRIMOP_TYPE_LE:
        fprintf(out, "le ");
        break;
    case AEXPPRIMOP_TYPE_VEC:
        fprintf(out, "vec ");
        break;
    case AEXPPRIMOP_TYPE_MOD:
        fprintf(out, "mod ");
        break;
    case AEXPPRIMOP_TYPE_CMP:
        fprintf(out, "cmp ");
        break;
    case AEXPPRIMOP_TYPE_POW:
        fprintf(out, "pow ");
        break;
    default:
        cant_happen("unrecognized op %s", aexpPrimOpName(x->type));
    }
    ppAexp(out, x->exp1);
    if (x->exp2 != NULL) {
        fprintf(out, " ");
        ppAexp(out, x->exp2);
    }
    fprintf(out, ")");
}

static void ppAexpListContents(FILE *out, AexpList *x) {
    while (x != NULL) {
        ppAexp(out, x->exp);
        if (x->next) {
            fprintf(out, " ");
        }
        x = x->next;
    }
}

void ppAexpList(FILE *out, AexpList *x) {
    fprintf(out, "(");
    ppAexpListContents(out, x);
    fprintf(out, ")");
}

static void ppAexpIntListContents(FILE *out, AexpIntList *x) {
    while (x != NULL) {
        fprintf(out, "%d", x->integer);
        if (x->next) {
            fprintf(out, " ");
        }
        x = x->next;
    }
}

void ppAexpIntList(FILE *out, AexpIntList *x) {
    fprintf(out, "(");
    ppAexpIntListContents(out, x);
    fprintf(out, ")");
}

void ppAexpMakeList(FILE *out, AexpList *x) {
    fprintf(out, "(list ");
    ppAexpListContents(out, x);
    fprintf(out, ")");
}

void ppAexpMakeVec(FILE *out, AexpMakeVec *x) {
    fprintf(out, "(make-vec ");
    ppAexpListContents(out, x->args);
    fprintf(out, ")");
}

void ppBareAexpList(FILE *out, AexpList *x) {
    while (x != NULL) {
        ppAexp(out, x->exp);
        if (x->next) {
            fprintf(out, " ");
        }
        x = x->next;
    }
}

void ppCexpApply(FILE *out, CexpApply *x) {
    fprintf(out, "(");
    ppAexp(out, x->function);
    fprintf(out, " ");
    ppBareAexpList(out, x->args);
    fprintf(out, ")");
}

void ppCexpIf(FILE *out, CexpIf *x) {
    fprintf(out, "(if ");
    ppAexp(out, x->condition);
    fprintf(out, " ");
    ppAnfExp(out, x->consequent);
    fprintf(out, " ");
    ppAnfExp(out, x->alternative);
    fprintf(out, ")");
}

void ppCexpCond(FILE *out, CexpCond *x) {
    fprintf(out, "(cond ");
    ppAexp(out, x->condition);
    fprintf(out, " ");
    ppCexpCondCases(out, x->cases);
    fprintf(out, ")");
}

void ppCexpIntCondCases(FILE *out, CexpIntCondCases *x) {
    while (x != NULL) {
        fprintf(out, "(");
        fprintMaybeBigInt2(out, x->option);
        fprintf(out, " ");
        ppAnfExp(out, x->body);
        fprintf(out, ")");
        if (x->next) {
            fprintf(out, " ");
        }
        x = x->next;
    }
}

void ppCexpCharCondCases(FILE *out, CexpCharCondCases *x) {
    while (x != NULL) {
        fprintf(out, "(");
        ppChar(out, x->option);
        fprintf(out, " ");
        ppAnfExp(out, x->body);
        fprintf(out, ")");
        if (x->next) {
            fprintf(out, " ");
        }
        x = x->next;
    }
}

void ppCexpCondCases(FILE *out, CexpCondCases *x) {
    switch (x->type) {
    case CEXPCONDCASES_TYPE_INTCASES:
        ppCexpIntCondCases(out, x->val.intCases);
        break;
    case CEXPCONDCASES_TYPE_CHARCASES:
        ppCexpCharCondCases(out, x->val.charCases);
        break;
    default:
        cant_happen("unrecognised type %d in ppCexpCondCases", x->type);
    }
}

void ppCexpLetRec(FILE *out, CexpLetRec *x) {
    fprintf(out, "(letrec ");
    ppAnfLetRecBindings(out, x->bindings);
    fprintf(out, " ");
    ppAnfExp(out, x->body);
    fprintf(out, ")");
}

void ppAnfLetRecBindings(FILE *out, AnfLetRecBindings *x) {
    fprintf(out, "(");
    while (x != NULL) {
        fprintf(out, "(");
        ppAexpVar(out, x->var);
        fprintf(out, " ");
        ppAexp(out, x->val);
        fprintf(out, ")");
        if (x->next != NULL) {
            fprintf(out, " ");
        }
        x = x->next;
    }
    fprintf(out, ")");
}

void ppCexpAmb(FILE *out, CexpAmb *x) {
    fprintf(out, "(amb ");
    ppAnfExp(out, x->exp1);
    fprintf(out, " ");
    ppAnfExp(out, x->exp2);
    fprintf(out, ")");
}

void ppCexpCut(FILE *out, CexpCut *x) {
    fprintf(out, "(cut ");
    ppAnfExp(out, x->exp);
    fprintf(out, ")");
}

void ppAnfMatchList(FILE *out, AnfMatchList *x) {
    if (x == NULL)
        return;
    fprintf(out, "(");
    ppAexpIntList(out, x->matches);
    fprintf(out, " ");
    ppAnfExp(out, x->body);
    fprintf(out, ")");
    if (x->next != NULL) {
        fprintf(out, " ");
        ppAnfMatchList(out, x->next);
    }
}

void ppCexpMatch(FILE *out, CexpMatch *x) {
    fprintf(out, "(match ");
    ppAexp(out, x->condition);
    fprintf(out, " ");
    ppAnfMatchList(out, x->clauses);
    fprintf(out, ")");
}

void ppAexp(FILE *out, Aexp *x) {
    switch (x->type) {
    case AEXP_TYPE_LAM:
        ppAexpLam(out, x->val.lam);
        break;
    case AEXP_TYPE_VAR:
        ppAexpVar(out, x->val.var);
        break;
    case AEXP_TYPE_ANNOTATEDVAR:
        ppAexpAnnotatedVar(out, x->val.annotatedVar);
        break;
    case AEXP_TYPE_BIGINTEGER:
        fprintMaybeBigInt2(out, x->val.bigInteger);
        break;
    case AEXP_TYPE_LITTLEINTEGER:
        fprintf(out, "%d", x->val.littleInteger);
        break;
    case AEXP_TYPE_CHARACTER:
        ppChar(out, x->val.character);
        break;
    case AEXP_TYPE_PRIM:
        ppAexpPrimApp(out, x->val.prim);
        break;
    case AEXP_TYPE_MAKEVEC:
        ppAexpMakeVec(out, x->val.makeVec);
        break;
    default:
        cant_happen("unrecognised aexp %s", aexpTypeName(x->type));
    }
}

void ppCexp(FILE *out, Cexp *x) {
    switch (x->type) {
    case CEXP_TYPE_APPLY:
        ppCexpApply(out, x->val.apply);
        break;
    case CEXP_TYPE_IFF:
        ppCexpIf(out, x->val.iff);
        break;
    case CEXP_TYPE_COND:
        ppCexpCond(out, x->val.cond);
        break;
    case CEXP_TYPE_CALLCC:
        fprintf(out, "(call/cc ");
        ppAexp(out, x->val.callCC);
        fprintf(out, ")");
        break;
    case CEXP_TYPE_LETREC:
        ppCexpLetRec(out, x->val.letRec);
        break;
    case CEXP_TYPE_AMB:
        ppCexpAmb(out, x->val.amb);
        break;
    case CEXP_TYPE_CUT:
        ppCexpCut(out, x->val.cut);
        break;
    case CEXP_TYPE_MATCH:
        ppCexpMatch(out, x->val.match);
        break;
    case CEXP_TYPE_BACK:
        fprintf(out, "(back)");
        break;
    case CEXP_TYPE_ERROR:
        fprintf(out, "(error)");
        break;
    default:
        cant_happen("unrecognised cexp %d in ppCexp", x->type);
    }
}

void ppAnfExp(FILE *out, AnfExp *x) {
    switch (x->type) {
    case ANFEXP_TYPE_AEXP:
        ppAexp(out, x->val.aexp);
        break;
    case ANFEXP_TYPE_CEXP:
        ppCexp(out, x->val.cexp);
        break;
    case ANFEXP_TYPE_LET:
        ppAnfExpLet(out, x->val.let);
        break;
    case ANFEXP_TYPE_DONE:
        fprintf(out, "<DONE>");
        break;
    default:
        fprintf(out, "<unrecognised exp %s>", anfExpTypeName(x->type));
        exit(1);
    }
}

void ppAnfExpLet(FILE *out, AnfExpLet *x) {
    fprintf(out, "(let (");
    ppAexpVar(out, x->var);
    fprintf(out, " ");
    ppAnfExp(out, x->val);
    fprintf(out, ") ");
    ppAnfExp(out, x->body);
    fprintf(out, ")");
}

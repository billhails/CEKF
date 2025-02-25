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

// Bespoke pretty-printer for anf

#include <stdio.h>
#include <stdlib.h>

#include "common.h"
#include "anf_pp.h"
#include "hash.h"

void ppCTEnv(CTEnv *env) {
    eprintf("[\n");
    if (env == NULL) {
        eprintf("]\n");
        return;
    }
    Index i = 0;
    HashSymbol *key;
    int value;
    while ((key = iterateCTIntTable(env->table, &i, &value)) != NULL) {
        eprintf("%s: %d\n", key->name, value);
    }
    ppCTEnv(env->next);
    eprintf("]\n");
}

void ppAexpLam(AexpLam *x) {
    eprintf("(lambda ");
    ppAexpVarList(x->args);
    eprintf(" ");
    ppExp(x->exp);
    eprintf(")");
}

void ppAexpVarList(AexpVarList *x) {
    eprintf("(");
    while (x != NULL) {
        ppAexpVar(x->var);
        if (x->next != NULL) {
            eprintf(" ");
        }
        x = x->next;
    }
    eprintf(")");
}

static void ppChar(char c) {
    switch(c) {
        case '\n':
            eprintf("\"\\n\"");
            break;
        case '\t':
            eprintf("\"\\t\"");
            break;
        case '\"':
            eprintf("\"\\\"\"");
            break;
        default:
            eprintf("\"%c\"", c);
    }
}

void ppAexpVar(HashSymbol *x) {
    eprintf("%s", x->name);
}

void ppAexpAnnotatedVar(AexpAnnotatedVar *x) {
    // ppAexpVar(x->var);
    if (x->type == AEXPANNOTATEDVARTYPE_TYPE_STACK)
        eprintf("LVAR:%s:%d", x->var->name, x->offset);
    else
        eprintf("VAR:%s:%d:%d", x->var->name, x->frame, x->offset);
}

void ppAexpPrimApp(AexpPrimApp *x) {
    eprintf("(");
    switch (x->type) {
        case AEXPPRIMOP_TYPE_ADD:
            eprintf("add ");
            break;
        case AEXPPRIMOP_TYPE_SUB:
            eprintf("sub ");
            break;
        case AEXPPRIMOP_TYPE_MUL:
            eprintf("mul ");
            break;
        case AEXPPRIMOP_TYPE_DIV:
            eprintf("div ");
            break;
        case AEXPPRIMOP_TYPE_EQ:
            eprintf("eq ");
            break;
        case AEXPPRIMOP_TYPE_NE:
            eprintf("ne ");
            break;
        case AEXPPRIMOP_TYPE_GT:
            eprintf("gt ");
            break;
        case AEXPPRIMOP_TYPE_LT:
            eprintf("lt ");
            break;
        case AEXPPRIMOP_TYPE_GE:
            eprintf("ge ");
            break;
        case AEXPPRIMOP_TYPE_LE:
            eprintf("le ");
            break;
        case AEXPPRIMOP_TYPE_VEC:
            eprintf("vec ");
            break;
        case AEXPPRIMOP_TYPE_MOD:
            eprintf("mod ");
            break;
        case AEXPPRIMOP_TYPE_CMP:
            eprintf("cmp ");
            break;
        case AEXPPRIMOP_TYPE_POW:
            eprintf("pow ");
            break;
        default:
            cant_happen("unrecognized op %s", aexpPrimOpName(x->type));
    }
    ppAexp(x->exp1);
    if (x->exp2 != NULL) {
        eprintf(" ");
        ppAexp(x->exp2);
    }
    eprintf(")");
}

static void ppAexpListContents(AexpList *x) {
    while (x != NULL) {
        ppAexp(x->exp);
        if (x->next) {
            eprintf(" ");
        }
        x = x->next;
    }
}

void ppAexpList(AexpList *x) {
    eprintf("(");
    ppAexpListContents(x);
    eprintf(")");
}

static void ppAexpIntListContents(AexpIntList *x) {
    while (x != NULL) {
        eprintf("%d", x->integer);
        if (x->next) {
            eprintf(" ");
        }
        x = x->next;
    }
}

void ppAexpIntList(AexpIntList *x) {
    eprintf("(");
    ppAexpIntListContents(x);
    eprintf(")");
}

void ppAexpMakeList(AexpList *x) {
    eprintf("(list ");
    ppAexpListContents(x);
    eprintf(")");
}

void ppAexpMakeVec(AexpMakeVec *x) {
    eprintf("(make-vec ");
    ppAexpListContents(x->args);
    eprintf(")");
}

void ppAexpNamespaceArray(AexpNamespaceArray *x) {
    for (Index i = 0; i < x->size; i++) {
        eprintf("[");
        ppExp(x->entries[i]->body);
        eprintf("]");
        if (i + 1 < x->size) {
            eprintf(" ");
        }
    }
}

void ppAexpNamespaces(AexpNamespaces *x) {
    eprintf("(namespaces ");
    ppAexpNamespaceArray(x->namespaces);
    eprintf(" ");
    ppExp(x->body);
    eprintf(")");
}

void ppBareAexpList(AexpList *x) {
    while (x != NULL) {
        ppAexp(x->exp);
        if (x->next) {
            eprintf(" ");
        }
        x = x->next;
    }
}

void ppCexpApply(CexpApply *x) {
    eprintf("(");
    ppAexp(x->function);
    eprintf(" ");
    ppBareAexpList(x->args);
    eprintf(")");
}

void ppCexpIf(CexpIf *x) {
    eprintf("(if ");
    ppAexp(x->condition);
    eprintf(" ");
    ppExp(x->consequent);
    eprintf(" ");
    ppExp(x->alternative);
    eprintf(")");
}

void ppCexpCond(CexpCond *x) {
    eprintf("(cond ");
    ppAexp(x->condition);
    eprintf(" ");
    ppCexpCondCases(x->cases);
    eprintf(")");
}

void ppCexpIntCondCases(CexpIntCondCases *x) {
    while (x != NULL) {
        eprintf("(");
        fprintMaybeBigInt(errout, x->option);
        eprintf(" ");
        ppExp(x->body);
        eprintf(")");
        if (x->next) {
            eprintf(" ");
        }
        x = x->next;
    }
}

void ppCexpCharCondCases(CexpCharCondCases *x) {
    while (x != NULL) {
        eprintf("(");
        ppChar(x->option);
        eprintf(" ");
        ppExp(x->body);
        eprintf(")");
        if (x->next) {
            eprintf(" ");
        }
        x = x->next;
    }
}

void ppCexpCondCases(CexpCondCases *x) {
    switch (x->type) {
        case CEXPCONDCASES_TYPE_INTCASES:
            ppCexpIntCondCases(x->val.intCases);
            break;
        case CEXPCONDCASES_TYPE_CHARCASES:
            ppCexpCharCondCases(x->val.charCases);
            break;
        default:
            cant_happen("unrecognised type %d in ppCexpCondCases", x->type);
    }
}

void ppCexpLetRec(CexpLetRec *x) {
    eprintf("(letrec ");
    ppLetRecBindings(x->bindings);
    eprintf(" ");
    ppExp(x->body);
    eprintf(")");
}

void ppLetRecBindings(LetRecBindings *x) {
    eprintf("(");
    while (x != NULL) {
        eprintf("(");
        ppAexpVar(x->var);
        eprintf(" ");
        ppAexp(x->val);
        eprintf(")");
        if (x->next != NULL) {
            eprintf(" ");
        }
        x = x->next;
    }
    eprintf(")");
}

void ppCexpAmb(CexpAmb *x) {
    eprintf("(amb ");
    ppExp(x->exp1);
    eprintf(" ");
    ppExp(x->exp2);
    eprintf(")");
}

void ppCexpCut(CexpCut *x) {
    eprintf("(cut ");
    ppExp(x->exp);
    eprintf(")");
}

void ppMatchList(MatchList *x) {
    if (x == NULL)
        return;
    eprintf("(");
    ppAexpIntList(x->matches);
    eprintf(" ");
    ppExp(x->body);
    eprintf(")");
    if (x->next != NULL) {
        eprintf(" ");
        ppMatchList(x->next);
    }
}

void ppCexpMatch(CexpMatch *x) {
    eprintf("(match ");
    ppAexp(x->condition);
    eprintf(" ");
    ppMatchList(x->clauses);
    eprintf(")");
}

void ppExpLookup(ExpLookup *x) {
    eprintf("(lookup %d ", x->namespace);
    ppExp(x->body);
    eprintf(")");
}

void ppAexp(Aexp *x) {
    switch (x->type) {
        case AEXP_TYPE_LAM:
            ppAexpLam(x->val.lam);
            break;
        case AEXP_TYPE_VAR:
            ppAexpVar(x->val.var);
            break;
        case AEXP_TYPE_ANNOTATEDVAR:
            ppAexpAnnotatedVar(x->val.annotatedVar);
            break;
        case AEXP_TYPE_BIGINTEGER:
            fprintMaybeBigInt(errout, x->val.biginteger);
            break;
        case AEXP_TYPE_LITTLEINTEGER:
            eprintf("%d", x->val.littleinteger);
            break;
        case AEXP_TYPE_CHARACTER:
            ppChar(x->val.character);
            break;
        case AEXP_TYPE_PRIM:
            ppAexpPrimApp(x->val.prim);
            break;
        case AEXP_TYPE_MAKEVEC:
            ppAexpMakeVec(x->val.makeVec);
            break;
        case AEXP_TYPE_NAMESPACES:
            ppAexpNamespaces(x->val.namespaces);
            break;
        default:
            cant_happen("unrecognised aexp %s", aexpTypeName(x->type));
    }
}

void ppCexp(Cexp *x) {
    switch (x->type) {
        case CEXP_TYPE_APPLY:
            ppCexpApply(x->val.apply);
            break;
        case CEXP_TYPE_IFF:
            ppCexpIf(x->val.iff);
            break;
        case CEXP_TYPE_COND:
            ppCexpCond(x->val.cond);
            break;
        case CEXP_TYPE_CALLCC:
            eprintf("(call/cc ");
            ppAexp(x->val.callCC);
            eprintf(")");
            break;
        case CEXP_TYPE_LETREC:
            ppCexpLetRec(x->val.letRec);
            break;
        case CEXP_TYPE_AMB:
            ppCexpAmb(x->val.amb);
            break;
        case CEXP_TYPE_CUT:
            ppCexpCut(x->val.cut);
            break;
        case CEXP_TYPE_MATCH:
            ppCexpMatch(x->val.match);
            break;
        case CEXP_TYPE_BACK:
            eprintf("(back)");
            break;
        case CEXP_TYPE_ERROR:
            eprintf("(error)");
            break;
        default:
            cant_happen("unrecognised cexp %d in ppCexp", x->type);
    }
}

void ppExp(Exp *x) {
    switch (x->type) {
        case EXP_TYPE_AEXP:
            ppAexp(x->val.aexp);
            break;
        case EXP_TYPE_CEXP:
            ppCexp(x->val.cexp);
            break;
        case EXP_TYPE_LET:
            ppExpLet(x->val.let);
            break;
        case EXP_TYPE_DONE:
            eprintf("<DONE>");
            break;
        case EXP_TYPE_ENV:
            eprintf("ENV");
            break;
        case EXP_TYPE_LOOKUP:
            ppExpLookup(x->val.lookup);
            break;
        default:
            eprintf("<unrecognised exp %s>", expTypeName(x->type));
            exit(1);
    }
}

void ppExpLet(ExpLet *x) {
    eprintf("(let (");
    ppAexpVar(x->var);
    eprintf(" ");
    ppExp(x->val);
    eprintf(") ");
    ppExp(x->body);
    eprintf(")");
}

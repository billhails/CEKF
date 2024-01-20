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

#include <stdio.h>

#include "anf_debug.h"
#include "bigint.h"
#include "ast_helper.h"
#include "tc.h"
#include "tc_debug.h"

/*
 * helper functions
 */

static void pad(int depth) { eprintf("%*s", depth * PAD_WIDTH, ""); }

/*
 * print functions
 */

void printAexpLam(struct AexpLam * x, int depth) {
    pad(depth);
    if (x == NULL) { eprintf("AexpLam (NULL)"); return; }
    eprintf("AexpLam[\n");
    pad(depth + 1);
eprintf("int %d", x->nargs);
    eprintf("\n");
    pad(depth + 1);
eprintf("int %d", x->letRecOffset);
    eprintf("\n");
    printAexpVarList(x->args, depth + 1);
    eprintf("\n");
    printExp(x->exp, depth + 1);
    eprintf("\n");
    pad(depth);
    eprintf("]");
}

void printAexpVarList(struct AexpVarList * x, int depth) {
    pad(depth);
    if (x == NULL) { eprintf("AexpVarList (NULL)"); return; }
    eprintf("AexpVarList[\n");
    printAstSymbol(x->var, depth + 1);
    eprintf("\n");
    printAexpVarList(x->next, depth + 1);
    eprintf("\n");
    pad(depth);
    eprintf("]");
}

void printAexpAnnotatedVar(struct AexpAnnotatedVar * x, int depth) {
    pad(depth);
    if (x == NULL) { eprintf("AexpAnnotatedVar (NULL)"); return; }
    eprintf("AexpAnnotatedVar[\n");
    switch (x->type) {
        case AEXPANNOTATEDVARTYPE_TYPE_STACK:
            pad(depth + 1);
            eprintf("AEXPANNOTATEDVARTYPE_TYPE_STACK");
            break;
        case AEXPANNOTATEDVARTYPE_TYPE_ENV:
            pad(depth + 1);
            eprintf("AEXPANNOTATEDVARTYPE_TYPE_ENV");
            break;
    }
    eprintf("\n");
    pad(depth + 1);
eprintf("int %d", x->frame);
    eprintf("\n");
    pad(depth + 1);
eprintf("int %d", x->offset);
    eprintf("\n");
    printAstSymbol(x->var, depth + 1);
    eprintf("\n");
    pad(depth);
    eprintf("]");
}

void printAexpPrimApp(struct AexpPrimApp * x, int depth) {
    pad(depth);
    if (x == NULL) { eprintf("AexpPrimApp (NULL)"); return; }
    eprintf("AexpPrimApp[\n");
    switch (x->type) {
        case AEXPPRIMOP_TYPE_ADD:
            pad(depth + 1);
            eprintf("AEXPPRIMOP_TYPE_ADD");
            break;
        case AEXPPRIMOP_TYPE_SUB:
            pad(depth + 1);
            eprintf("AEXPPRIMOP_TYPE_SUB");
            break;
        case AEXPPRIMOP_TYPE_MUL:
            pad(depth + 1);
            eprintf("AEXPPRIMOP_TYPE_MUL");
            break;
        case AEXPPRIMOP_TYPE_DIV:
            pad(depth + 1);
            eprintf("AEXPPRIMOP_TYPE_DIV");
            break;
        case AEXPPRIMOP_TYPE_POW:
            pad(depth + 1);
            eprintf("AEXPPRIMOP_TYPE_POW");
            break;
        case AEXPPRIMOP_TYPE_EQ:
            pad(depth + 1);
            eprintf("AEXPPRIMOP_TYPE_EQ");
            break;
        case AEXPPRIMOP_TYPE_NE:
            pad(depth + 1);
            eprintf("AEXPPRIMOP_TYPE_NE");
            break;
        case AEXPPRIMOP_TYPE_LT:
            pad(depth + 1);
            eprintf("AEXPPRIMOP_TYPE_LT");
            break;
        case AEXPPRIMOP_TYPE_GT:
            pad(depth + 1);
            eprintf("AEXPPRIMOP_TYPE_GT");
            break;
        case AEXPPRIMOP_TYPE_LE:
            pad(depth + 1);
            eprintf("AEXPPRIMOP_TYPE_LE");
            break;
        case AEXPPRIMOP_TYPE_GE:
            pad(depth + 1);
            eprintf("AEXPPRIMOP_TYPE_GE");
            break;
        case AEXPPRIMOP_TYPE_CONS:
            pad(depth + 1);
            eprintf("AEXPPRIMOP_TYPE_CONS");
            break;
        case AEXPPRIMOP_TYPE_VEC:
            pad(depth + 1);
            eprintf("AEXPPRIMOP_TYPE_VEC");
            break;
        case AEXPPRIMOP_TYPE_XOR:
            pad(depth + 1);
            eprintf("AEXPPRIMOP_TYPE_XOR");
            break;
        case AEXPPRIMOP_TYPE_MOD:
            pad(depth + 1);
            eprintf("AEXPPRIMOP_TYPE_MOD");
            break;
        case AEXPPRIMOP_TYPE_CMP:
            pad(depth + 1);
            eprintf("AEXPPRIMOP_TYPE_CMP");
            break;
    }
    eprintf("\n");
    printAexp(x->exp1, depth + 1);
    eprintf("\n");
    printAexp(x->exp2, depth + 1);
    eprintf("\n");
    pad(depth);
    eprintf("]");
}

void printAexpUnaryApp(struct AexpUnaryApp * x, int depth) {
    pad(depth);
    if (x == NULL) { eprintf("AexpUnaryApp (NULL)"); return; }
    eprintf("AexpUnaryApp[\n");
    switch (x->type) {
        case AEXPUNARYOP_TYPE_CAR:
            pad(depth + 1);
            eprintf("AEXPUNARYOP_TYPE_CAR");
            break;
        case AEXPUNARYOP_TYPE_CDR:
            pad(depth + 1);
            eprintf("AEXPUNARYOP_TYPE_CDR");
            break;
        case AEXPUNARYOP_TYPE_NOT:
            pad(depth + 1);
            eprintf("AEXPUNARYOP_TYPE_NOT");
            break;
        case AEXPUNARYOP_TYPE_PUTC:
            pad(depth + 1);
            eprintf("AEXPUNARYOP_TYPE_PUTC");
            break;
        case AEXPUNARYOP_TYPE_PUTN:
            pad(depth + 1);
            eprintf("AEXPUNARYOP_TYPE_PUTN");
            break;
        case AEXPUNARYOP_TYPE_PUTV:
            pad(depth + 1);
            eprintf("AEXPUNARYOP_TYPE_PUTV");
            break;
    }
    eprintf("\n");
    printAexp(x->exp, depth + 1);
    eprintf("\n");
    pad(depth);
    eprintf("]");
}

void printAexpList(struct AexpList * x, int depth) {
    pad(depth);
    if (x == NULL) { eprintf("AexpList (NULL)"); return; }
    eprintf("AexpList[\n");
    printAexp(x->exp, depth + 1);
    eprintf("\n");
    printAexpList(x->next, depth + 1);
    eprintf("\n");
    pad(depth);
    eprintf("]");
}

void printAexpIntList(struct AexpIntList * x, int depth) {
    pad(depth);
    if (x == NULL) { eprintf("AexpIntList (NULL)"); return; }
    eprintf("AexpIntList[\n");
    pad(depth + 1);
eprintf("int %d", x->integer);
    eprintf("\n");
    printAexpIntList(x->next, depth + 1);
    eprintf("\n");
    pad(depth);
    eprintf("]");
}

void printCexpApply(struct CexpApply * x, int depth) {
    pad(depth);
    if (x == NULL) { eprintf("CexpApply (NULL)"); return; }
    eprintf("CexpApply[\n");
    printAexp(x->function, depth + 1);
    eprintf("\n");
    pad(depth + 1);
eprintf("int %d", x->nargs);
    eprintf("\n");
    printAexpList(x->args, depth + 1);
    eprintf("\n");
    pad(depth);
    eprintf("]");
}

void printAexpMakeVec(struct AexpMakeVec * x, int depth) {
    pad(depth);
    if (x == NULL) { eprintf("AexpMakeVec (NULL)"); return; }
    eprintf("AexpMakeVec[\n");
    pad(depth + 1);
eprintf("int %d", x->nargs);
    eprintf("\n");
    printAexpList(x->args, depth + 1);
    eprintf("\n");
    pad(depth);
    eprintf("]");
}

void printCexpIf(struct CexpIf * x, int depth) {
    pad(depth);
    if (x == NULL) { eprintf("CexpIf (NULL)"); return; }
    eprintf("CexpIf[\n");
    printAexp(x->condition, depth + 1);
    eprintf("\n");
    printExp(x->consequent, depth + 1);
    eprintf("\n");
    printExp(x->alternative, depth + 1);
    eprintf("\n");
    pad(depth);
    eprintf("]");
}

void printCexpCond(struct CexpCond * x, int depth) {
    pad(depth);
    if (x == NULL) { eprintf("CexpCond (NULL)"); return; }
    eprintf("CexpCond[\n");
    printAexp(x->condition, depth + 1);
    eprintf("\n");
    printCexpCondCases(x->cases, depth + 1);
    eprintf("\n");
    pad(depth);
    eprintf("]");
}

void printCexpIntCondCases(struct CexpIntCondCases * x, int depth) {
    pad(depth);
    if (x == NULL) { eprintf("CexpIntCondCases (NULL)"); return; }
    eprintf("CexpIntCondCases[\n");
    printBigInt(x->option, depth + 1);
    eprintf("\n");
    printExp(x->body, depth + 1);
    eprintf("\n");
    printCexpIntCondCases(x->next, depth + 1);
    eprintf("\n");
    pad(depth);
    eprintf("]");
}

void printCexpCharCondCases(struct CexpCharCondCases * x, int depth) {
    pad(depth);
    if (x == NULL) { eprintf("CexpCharCondCases (NULL)"); return; }
    eprintf("CexpCharCondCases[\n");
    pad(depth + 1);
eprintf("char %c", x->option);
    eprintf("\n");
    printExp(x->body, depth + 1);
    eprintf("\n");
    printCexpCharCondCases(x->next, depth + 1);
    eprintf("\n");
    pad(depth);
    eprintf("]");
}

void printCexpMatch(struct CexpMatch * x, int depth) {
    pad(depth);
    if (x == NULL) { eprintf("CexpMatch (NULL)"); return; }
    eprintf("CexpMatch[\n");
    printAexp(x->condition, depth + 1);
    eprintf("\n");
    printMatchList(x->clauses, depth + 1);
    eprintf("\n");
    pad(depth);
    eprintf("]");
}

void printMatchList(struct MatchList * x, int depth) {
    pad(depth);
    if (x == NULL) { eprintf("MatchList (NULL)"); return; }
    eprintf("MatchList[\n");
    printAexpIntList(x->matches, depth + 1);
    eprintf("\n");
    printExp(x->body, depth + 1);
    eprintf("\n");
    printMatchList(x->next, depth + 1);
    eprintf("\n");
    pad(depth);
    eprintf("]");
}

void printCexpLetRec(struct CexpLetRec * x, int depth) {
    pad(depth);
    if (x == NULL) { eprintf("CexpLetRec (NULL)"); return; }
    eprintf("CexpLetRec[\n");
    pad(depth + 1);
eprintf("int %d", x->nbindings);
    eprintf("\n");
    printLetRecBindings(x->bindings, depth + 1);
    eprintf("\n");
    printExp(x->body, depth + 1);
    eprintf("\n");
    pad(depth);
    eprintf("]");
}

void printLetRecBindings(struct LetRecBindings * x, int depth) {
    pad(depth);
    if (x == NULL) { eprintf("LetRecBindings (NULL)"); return; }
    eprintf("LetRecBindings[\n");
    printAstSymbol(x->var, depth + 1);
    eprintf("\n");
    printAexp(x->val, depth + 1);
    eprintf("\n");
    printLetRecBindings(x->next, depth + 1);
    eprintf("\n");
    pad(depth);
    eprintf("]");
}

void printCexpAmb(struct CexpAmb * x, int depth) {
    pad(depth);
    if (x == NULL) { eprintf("CexpAmb (NULL)"); return; }
    eprintf("CexpAmb[\n");
    printExp(x->exp1, depth + 1);
    eprintf("\n");
    printExp(x->exp2, depth + 1);
    eprintf("\n");
    pad(depth);
    eprintf("]");
}

void printCexpCut(struct CexpCut * x, int depth) {
    pad(depth);
    if (x == NULL) { eprintf("CexpCut (NULL)"); return; }
    eprintf("CexpCut[\n");
    printExp(x->exp, depth + 1);
    eprintf("\n");
    pad(depth);
    eprintf("]");
}

void printCexpBool(struct CexpBool * x, int depth) {
    pad(depth);
    if (x == NULL) { eprintf("CexpBool (NULL)"); return; }
    eprintf("CexpBool[\n");
    switch (x->type) {
        case CEXPBOOLTYPE_TYPE_AND:
            pad(depth + 1);
            eprintf("CEXPBOOLTYPE_TYPE_AND");
            break;
        case CEXPBOOLTYPE_TYPE_OR:
            pad(depth + 1);
            eprintf("CEXPBOOLTYPE_TYPE_OR");
            break;
    }
    eprintf("\n");
    printExp(x->exp1, depth + 1);
    eprintf("\n");
    printExp(x->exp2, depth + 1);
    eprintf("\n");
    pad(depth);
    eprintf("]");
}

void printExpLet(struct ExpLet * x, int depth) {
    pad(depth);
    if (x == NULL) { eprintf("ExpLet (NULL)"); return; }
    eprintf("ExpLet[\n");
    printAstSymbol(x->var, depth + 1);
    eprintf("\n");
    printExp(x->val, depth + 1);
    eprintf("\n");
    printExp(x->body, depth + 1);
    eprintf("\n");
    pad(depth);
    eprintf("]");
}

void printCexpCondCases(struct CexpCondCases * x, int depth) {
    pad(depth);
    if (x == NULL) { eprintf("CexpCondCases (NULL)"); return; }
    eprintf("CexpCondCases[\n");
    switch(x->type) {
        case CEXPCONDCASES_TYPE_CHARCASES:
            pad(depth + 1);
            eprintf("CEXPCONDCASES_TYPE_CHARCASES\n");
            printCexpCharCondCases(x->val.charCases, depth + 1);
            break;
        case CEXPCONDCASES_TYPE_INTCASES:
            pad(depth + 1);
            eprintf("CEXPCONDCASES_TYPE_INTCASES\n");
            printCexpIntCondCases(x->val.intCases, depth + 1);
            break;
        default:
            cant_happen("unrecognised type %d in printCexpCondCases", x->type);
    }
    eprintf("\n");
    pad(depth);
    eprintf("]");
}

void printAexp(struct Aexp * x, int depth) {
    pad(depth);
    if (x == NULL) { eprintf("Aexp (NULL)"); return; }
    eprintf("Aexp[\n");
    switch(x->type) {
        case AEXP_TYPE_T:
            pad(depth + 1);
            eprintf("AEXP_TYPE_T\n");
            pad(depth + 1);
eprintf("void * %p", x->val.t);
            break;
        case AEXP_TYPE_F:
            pad(depth + 1);
            eprintf("AEXP_TYPE_F\n");
            pad(depth + 1);
eprintf("void * %p", x->val.f);
            break;
        case AEXP_TYPE_V:
            pad(depth + 1);
            eprintf("AEXP_TYPE_V\n");
            pad(depth + 1);
eprintf("void * %p", x->val.v);
            break;
        case AEXP_TYPE_LAM:
            pad(depth + 1);
            eprintf("AEXP_TYPE_LAM\n");
            printAexpLam(x->val.lam, depth + 1);
            break;
        case AEXP_TYPE_VAR:
            pad(depth + 1);
            eprintf("AEXP_TYPE_VAR\n");
            printAstSymbol(x->val.var, depth + 1);
            break;
        case AEXP_TYPE_ANNOTATEDVAR:
            pad(depth + 1);
            eprintf("AEXP_TYPE_ANNOTATEDVAR\n");
            printAexpAnnotatedVar(x->val.annotatedVar, depth + 1);
            break;
        case AEXP_TYPE_BIGINTEGER:
            pad(depth + 1);
            eprintf("AEXP_TYPE_BIGINTEGER\n");
            printBigInt(x->val.biginteger, depth + 1);
            break;
        case AEXP_TYPE_LITTLEINTEGER:
            pad(depth + 1);
            eprintf("AEXP_TYPE_LITTLEINTEGER\n");
            pad(depth + 1);
eprintf("int %d", x->val.littleinteger);
            break;
        case AEXP_TYPE_CHARACTER:
            pad(depth + 1);
            eprintf("AEXP_TYPE_CHARACTER\n");
            pad(depth + 1);
eprintf("char %c", x->val.character);
            break;
        case AEXP_TYPE_PRIM:
            pad(depth + 1);
            eprintf("AEXP_TYPE_PRIM\n");
            printAexpPrimApp(x->val.prim, depth + 1);
            break;
        case AEXP_TYPE_UNARY:
            pad(depth + 1);
            eprintf("AEXP_TYPE_UNARY\n");
            printAexpUnaryApp(x->val.unary, depth + 1);
            break;
        case AEXP_TYPE_LIST:
            pad(depth + 1);
            eprintf("AEXP_TYPE_LIST\n");
            printAexpList(x->val.list, depth + 1);
            break;
        case AEXP_TYPE_MAKEVEC:
            pad(depth + 1);
            eprintf("AEXP_TYPE_MAKEVEC\n");
            printAexpMakeVec(x->val.makeVec, depth + 1);
            break;
        default:
            cant_happen("unrecognised type %d in printAexp", x->type);
    }
    eprintf("\n");
    pad(depth);
    eprintf("]");
}

void printCexp(struct Cexp * x, int depth) {
    pad(depth);
    if (x == NULL) { eprintf("Cexp (NULL)"); return; }
    eprintf("Cexp[\n");
    switch(x->type) {
        case CEXP_TYPE_BACK:
            pad(depth + 1);
            eprintf("CEXP_TYPE_BACK\n");
            pad(depth + 1);
eprintf("void * %p", x->val.back);
            break;
        case CEXP_TYPE_ERROR:
            pad(depth + 1);
            eprintf("CEXP_TYPE_ERROR\n");
            pad(depth + 1);
eprintf("void * %p", x->val.error);
            break;
        case CEXP_TYPE_APPLY:
            pad(depth + 1);
            eprintf("CEXP_TYPE_APPLY\n");
            printCexpApply(x->val.apply, depth + 1);
            break;
        case CEXP_TYPE_IFF:
            pad(depth + 1);
            eprintf("CEXP_TYPE_IFF\n");
            printCexpIf(x->val.iff, depth + 1);
            break;
        case CEXP_TYPE_COND:
            pad(depth + 1);
            eprintf("CEXP_TYPE_COND\n");
            printCexpCond(x->val.cond, depth + 1);
            break;
        case CEXP_TYPE_CALLCC:
            pad(depth + 1);
            eprintf("CEXP_TYPE_CALLCC\n");
            printAexp(x->val.callCC, depth + 1);
            break;
        case CEXP_TYPE_PRINT:
            pad(depth + 1);
            eprintf("CEXP_TYPE_PRINT\n");
            printAexp(x->val.print, depth + 1);
            break;
        case CEXP_TYPE_LETREC:
            pad(depth + 1);
            eprintf("CEXP_TYPE_LETREC\n");
            printCexpLetRec(x->val.letRec, depth + 1);
            break;
        case CEXP_TYPE_AMB:
            pad(depth + 1);
            eprintf("CEXP_TYPE_AMB\n");
            printCexpAmb(x->val.amb, depth + 1);
            break;
        case CEXP_TYPE_CUT:
            pad(depth + 1);
            eprintf("CEXP_TYPE_CUT\n");
            printCexpCut(x->val.cut, depth + 1);
            break;
        case CEXP_TYPE_BOOLEAN:
            pad(depth + 1);
            eprintf("CEXP_TYPE_BOOLEAN\n");
            printCexpBool(x->val.boolean, depth + 1);
            break;
        case CEXP_TYPE_MATCH:
            pad(depth + 1);
            eprintf("CEXP_TYPE_MATCH\n");
            printCexpMatch(x->val.match, depth + 1);
            break;
        default:
            cant_happen("unrecognised type %d in printCexp", x->type);
    }
    eprintf("\n");
    pad(depth);
    eprintf("]");
}

void printExp(struct Exp * x, int depth) {
    pad(depth);
    if (x == NULL) { eprintf("Exp (NULL)"); return; }
    eprintf("Exp[\n");
    switch(x->type) {
        case EXP_TYPE_DONE:
            pad(depth + 1);
            eprintf("EXP_TYPE_DONE\n");
            pad(depth + 1);
eprintf("void * %p", x->val.done);
            break;
        case EXP_TYPE_AEXP:
            pad(depth + 1);
            eprintf("EXP_TYPE_AEXP\n");
            printAexp(x->val.aexp, depth + 1);
            break;
        case EXP_TYPE_CEXP:
            pad(depth + 1);
            eprintf("EXP_TYPE_CEXP\n");
            printCexp(x->val.cexp, depth + 1);
            break;
        case EXP_TYPE_LET:
            pad(depth + 1);
            eprintf("EXP_TYPE_LET\n");
            printExpLet(x->val.let, depth + 1);
            break;
        default:
            cant_happen("unrecognised type %d in printExp", x->type);
    }
    eprintf("\n");
    pad(depth);
    eprintf("]");
}


/*
 * compare functions
 */

bool eqAexpLam(struct AexpLam * a, struct AexpLam * b) {
    if (a == b) return true;
    if (a == NULL || b == NULL) return false;
    if (a->nargs != b->nargs) return false;
    if (a->letRecOffset != b->letRecOffset) return false;
    if (!eqAexpVarList(a->args, b->args)) return false;
    if (!eqExp(a->exp, b->exp)) return false;
    return true;
}

bool eqAexpVarList(struct AexpVarList * a, struct AexpVarList * b) {
    if (a == b) return true;
    if (a == NULL || b == NULL) return false;
    if (a->var != b->var) return false;
    if (!eqAexpVarList(a->next, b->next)) return false;
    return true;
}

bool eqAexpAnnotatedVar(struct AexpAnnotatedVar * a, struct AexpAnnotatedVar * b) {
    if (a == b) return true;
    if (a == NULL || b == NULL) return false;
    switch (a->type) {
        case AEXPANNOTATEDVARTYPE_TYPE_STACK:
            if (a != b) return false;
            break;
        case AEXPANNOTATEDVARTYPE_TYPE_ENV:
            if (a != b) return false;
            break;
    }
    if (a->frame != b->frame) return false;
    if (a->offset != b->offset) return false;
    if (a->var != b->var) return false;
    return true;
}

bool eqAexpPrimApp(struct AexpPrimApp * a, struct AexpPrimApp * b) {
    if (a == b) return true;
    if (a == NULL || b == NULL) return false;
    switch (a->type) {
        case AEXPPRIMOP_TYPE_ADD:
            if (a != b) return false;
            break;
        case AEXPPRIMOP_TYPE_SUB:
            if (a != b) return false;
            break;
        case AEXPPRIMOP_TYPE_MUL:
            if (a != b) return false;
            break;
        case AEXPPRIMOP_TYPE_DIV:
            if (a != b) return false;
            break;
        case AEXPPRIMOP_TYPE_POW:
            if (a != b) return false;
            break;
        case AEXPPRIMOP_TYPE_EQ:
            if (a != b) return false;
            break;
        case AEXPPRIMOP_TYPE_NE:
            if (a != b) return false;
            break;
        case AEXPPRIMOP_TYPE_LT:
            if (a != b) return false;
            break;
        case AEXPPRIMOP_TYPE_GT:
            if (a != b) return false;
            break;
        case AEXPPRIMOP_TYPE_LE:
            if (a != b) return false;
            break;
        case AEXPPRIMOP_TYPE_GE:
            if (a != b) return false;
            break;
        case AEXPPRIMOP_TYPE_CONS:
            if (a != b) return false;
            break;
        case AEXPPRIMOP_TYPE_VEC:
            if (a != b) return false;
            break;
        case AEXPPRIMOP_TYPE_XOR:
            if (a != b) return false;
            break;
        case AEXPPRIMOP_TYPE_MOD:
            if (a != b) return false;
            break;
        case AEXPPRIMOP_TYPE_CMP:
            if (a != b) return false;
            break;
    }
    if (!eqAexp(a->exp1, b->exp1)) return false;
    if (!eqAexp(a->exp2, b->exp2)) return false;
    return true;
}

bool eqAexpUnaryApp(struct AexpUnaryApp * a, struct AexpUnaryApp * b) {
    if (a == b) return true;
    if (a == NULL || b == NULL) return false;
    switch (a->type) {
        case AEXPUNARYOP_TYPE_CAR:
            if (a != b) return false;
            break;
        case AEXPUNARYOP_TYPE_CDR:
            if (a != b) return false;
            break;
        case AEXPUNARYOP_TYPE_NOT:
            if (a != b) return false;
            break;
        case AEXPUNARYOP_TYPE_PUTC:
            if (a != b) return false;
            break;
        case AEXPUNARYOP_TYPE_PUTN:
            if (a != b) return false;
            break;
        case AEXPUNARYOP_TYPE_PUTV:
            if (a != b) return false;
            break;
    }
    if (!eqAexp(a->exp, b->exp)) return false;
    return true;
}

bool eqAexpList(struct AexpList * a, struct AexpList * b) {
    if (a == b) return true;
    if (a == NULL || b == NULL) return false;
    if (!eqAexp(a->exp, b->exp)) return false;
    if (!eqAexpList(a->next, b->next)) return false;
    return true;
}

bool eqAexpIntList(struct AexpIntList * a, struct AexpIntList * b) {
    if (a == b) return true;
    if (a == NULL || b == NULL) return false;
    if (a->integer != b->integer) return false;
    if (!eqAexpIntList(a->next, b->next)) return false;
    return true;
}

bool eqCexpApply(struct CexpApply * a, struct CexpApply * b) {
    if (a == b) return true;
    if (a == NULL || b == NULL) return false;
    if (!eqAexp(a->function, b->function)) return false;
    if (a->nargs != b->nargs) return false;
    if (!eqAexpList(a->args, b->args)) return false;
    return true;
}

bool eqAexpMakeVec(struct AexpMakeVec * a, struct AexpMakeVec * b) {
    if (a == b) return true;
    if (a == NULL || b == NULL) return false;
    if (a->nargs != b->nargs) return false;
    if (!eqAexpList(a->args, b->args)) return false;
    return true;
}

bool eqCexpIf(struct CexpIf * a, struct CexpIf * b) {
    if (a == b) return true;
    if (a == NULL || b == NULL) return false;
    if (!eqAexp(a->condition, b->condition)) return false;
    if (!eqExp(a->consequent, b->consequent)) return false;
    if (!eqExp(a->alternative, b->alternative)) return false;
    return true;
}

bool eqCexpCond(struct CexpCond * a, struct CexpCond * b) {
    if (a == b) return true;
    if (a == NULL || b == NULL) return false;
    if (!eqAexp(a->condition, b->condition)) return false;
    if (!eqCexpCondCases(a->cases, b->cases)) return false;
    return true;
}

bool eqCexpIntCondCases(struct CexpIntCondCases * a, struct CexpIntCondCases * b) {
    if (a == b) return true;
    if (a == NULL || b == NULL) return false;
    if (a->option != b->option) return false;
    if (!eqExp(a->body, b->body)) return false;
    if (!eqCexpIntCondCases(a->next, b->next)) return false;
    return true;
}

bool eqCexpCharCondCases(struct CexpCharCondCases * a, struct CexpCharCondCases * b) {
    if (a == b) return true;
    if (a == NULL || b == NULL) return false;
    if (a->option != b->option) return false;
    if (!eqExp(a->body, b->body)) return false;
    if (!eqCexpCharCondCases(a->next, b->next)) return false;
    return true;
}

bool eqCexpMatch(struct CexpMatch * a, struct CexpMatch * b) {
    if (a == b) return true;
    if (a == NULL || b == NULL) return false;
    if (!eqAexp(a->condition, b->condition)) return false;
    if (!eqMatchList(a->clauses, b->clauses)) return false;
    return true;
}

bool eqMatchList(struct MatchList * a, struct MatchList * b) {
    if (a == b) return true;
    if (a == NULL || b == NULL) return false;
    if (!eqAexpIntList(a->matches, b->matches)) return false;
    if (!eqExp(a->body, b->body)) return false;
    if (!eqMatchList(a->next, b->next)) return false;
    return true;
}

bool eqCexpLetRec(struct CexpLetRec * a, struct CexpLetRec * b) {
    if (a == b) return true;
    if (a == NULL || b == NULL) return false;
    if (a->nbindings != b->nbindings) return false;
    if (!eqLetRecBindings(a->bindings, b->bindings)) return false;
    if (!eqExp(a->body, b->body)) return false;
    return true;
}

bool eqLetRecBindings(struct LetRecBindings * a, struct LetRecBindings * b) {
    if (a == b) return true;
    if (a == NULL || b == NULL) return false;
    if (a->var != b->var) return false;
    if (!eqAexp(a->val, b->val)) return false;
    if (!eqLetRecBindings(a->next, b->next)) return false;
    return true;
}

bool eqCexpAmb(struct CexpAmb * a, struct CexpAmb * b) {
    if (a == b) return true;
    if (a == NULL || b == NULL) return false;
    if (!eqExp(a->exp1, b->exp1)) return false;
    if (!eqExp(a->exp2, b->exp2)) return false;
    return true;
}

bool eqCexpCut(struct CexpCut * a, struct CexpCut * b) {
    if (a == b) return true;
    if (a == NULL || b == NULL) return false;
    if (!eqExp(a->exp, b->exp)) return false;
    return true;
}

bool eqCexpBool(struct CexpBool * a, struct CexpBool * b) {
    if (a == b) return true;
    if (a == NULL || b == NULL) return false;
    switch (a->type) {
        case CEXPBOOLTYPE_TYPE_AND:
            if (a != b) return false;
            break;
        case CEXPBOOLTYPE_TYPE_OR:
            if (a != b) return false;
            break;
    }
    if (!eqExp(a->exp1, b->exp1)) return false;
    if (!eqExp(a->exp2, b->exp2)) return false;
    return true;
}

bool eqExpLet(struct ExpLet * a, struct ExpLet * b) {
    if (a == b) return true;
    if (a == NULL || b == NULL) return false;
    if (a->var != b->var) return false;
    if (!eqExp(a->val, b->val)) return false;
    if (!eqExp(a->body, b->body)) return false;
    return true;
}

bool eqCexpCondCases(struct CexpCondCases * a, struct CexpCondCases * b) {
    if (a == b) return true;
    if (a == NULL || b == NULL) return false;
    if (a->type != b->type) return false;
    switch(a->type) {
        case CEXPCONDCASES_TYPE_CHARCASES:
            if (!eqCexpCharCondCases(a->val.charCases, b->val.charCases)) return false;
            break;
        case CEXPCONDCASES_TYPE_INTCASES:
            if (!eqCexpIntCondCases(a->val.intCases, b->val.intCases)) return false;
            break;
        default:
            cant_happen("unrecognised type %d in eqCexpCondCases", a->type);
    }
    return true;
}

bool eqAexp(struct Aexp * a, struct Aexp * b) {
    if (a == b) return true;
    if (a == NULL || b == NULL) return false;
    if (a->type != b->type) return false;
    switch(a->type) {
        case AEXP_TYPE_T:
            if (a->val.t != b->val.t) return false;
            break;
        case AEXP_TYPE_F:
            if (a->val.f != b->val.f) return false;
            break;
        case AEXP_TYPE_V:
            if (a->val.v != b->val.v) return false;
            break;
        case AEXP_TYPE_LAM:
            if (!eqAexpLam(a->val.lam, b->val.lam)) return false;
            break;
        case AEXP_TYPE_VAR:
            if (a->val.var != b->val.var) return false;
            break;
        case AEXP_TYPE_ANNOTATEDVAR:
            if (!eqAexpAnnotatedVar(a->val.annotatedVar, b->val.annotatedVar)) return false;
            break;
        case AEXP_TYPE_BIGINTEGER:
            if (a->val.biginteger != b->val.biginteger) return false;
            break;
        case AEXP_TYPE_LITTLEINTEGER:
            if (a->val.littleinteger != b->val.littleinteger) return false;
            break;
        case AEXP_TYPE_CHARACTER:
            if (a->val.character != b->val.character) return false;
            break;
        case AEXP_TYPE_PRIM:
            if (!eqAexpPrimApp(a->val.prim, b->val.prim)) return false;
            break;
        case AEXP_TYPE_UNARY:
            if (!eqAexpUnaryApp(a->val.unary, b->val.unary)) return false;
            break;
        case AEXP_TYPE_LIST:
            if (!eqAexpList(a->val.list, b->val.list)) return false;
            break;
        case AEXP_TYPE_MAKEVEC:
            if (!eqAexpMakeVec(a->val.makeVec, b->val.makeVec)) return false;
            break;
        default:
            cant_happen("unrecognised type %d in eqAexp", a->type);
    }
    return true;
}

bool eqCexp(struct Cexp * a, struct Cexp * b) {
    if (a == b) return true;
    if (a == NULL || b == NULL) return false;
    if (a->type != b->type) return false;
    switch(a->type) {
        case CEXP_TYPE_BACK:
            if (a->val.back != b->val.back) return false;
            break;
        case CEXP_TYPE_ERROR:
            if (a->val.error != b->val.error) return false;
            break;
        case CEXP_TYPE_APPLY:
            if (!eqCexpApply(a->val.apply, b->val.apply)) return false;
            break;
        case CEXP_TYPE_IFF:
            if (!eqCexpIf(a->val.iff, b->val.iff)) return false;
            break;
        case CEXP_TYPE_COND:
            if (!eqCexpCond(a->val.cond, b->val.cond)) return false;
            break;
        case CEXP_TYPE_CALLCC:
            if (!eqAexp(a->val.callCC, b->val.callCC)) return false;
            break;
        case CEXP_TYPE_PRINT:
            if (!eqAexp(a->val.print, b->val.print)) return false;
            break;
        case CEXP_TYPE_LETREC:
            if (!eqCexpLetRec(a->val.letRec, b->val.letRec)) return false;
            break;
        case CEXP_TYPE_AMB:
            if (!eqCexpAmb(a->val.amb, b->val.amb)) return false;
            break;
        case CEXP_TYPE_CUT:
            if (!eqCexpCut(a->val.cut, b->val.cut)) return false;
            break;
        case CEXP_TYPE_BOOLEAN:
            if (!eqCexpBool(a->val.boolean, b->val.boolean)) return false;
            break;
        case CEXP_TYPE_MATCH:
            if (!eqCexpMatch(a->val.match, b->val.match)) return false;
            break;
        default:
            cant_happen("unrecognised type %d in eqCexp", a->type);
    }
    return true;
}

bool eqExp(struct Exp * a, struct Exp * b) {
    if (a == b) return true;
    if (a == NULL || b == NULL) return false;
    if (a->type != b->type) return false;
    switch(a->type) {
        case EXP_TYPE_DONE:
            if (a->val.done != b->val.done) return false;
            break;
        case EXP_TYPE_AEXP:
            if (!eqAexp(a->val.aexp, b->val.aexp)) return false;
            break;
        case EXP_TYPE_CEXP:
            if (!eqCexp(a->val.cexp, b->val.cexp)) return false;
            break;
        case EXP_TYPE_LET:
            if (!eqExpLet(a->val.let, b->val.let)) return false;
            break;
        default:
            cant_happen("unrecognised type %d in eqExp", a->type);
    }
    return true;
}


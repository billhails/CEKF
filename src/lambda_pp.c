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
 * bespoke pretty-printer for lambda.yaml structs
 *
 */

#include <stdio.h>
#include "lambda_pp.h"

void ppLamExpD(LamExp *exp, int depth) {
    while (depth > 0) {
        depth--;
        fprintf(stderr, "    ");
    }
    ppLamExp(exp);
}

void ppLamLam(LamLam *lam) {
    if (lam == NULL) {
        fprintf(stderr, "<NULL lambda>");
        return;
    }
    fprintf(stderr, "(lambda ");
    ppLamVarList(lam->args);
    fprintf(stderr, " ");
    ppLamExp(lam->exp);
    fprintf(stderr, ")");
}

void ppLamAnd(LamAnd *and) {
    if (and == NULL) {
        fprintf(stderr, "<NULL and>");
        return;
    }
    fprintf(stderr, "(and ");
    ppLamExp(and->left);
    fprintf(stderr, " ");
    ppLamExp(and->right);
    fprintf(stderr, ")");
}

void ppLamOr(LamOr *or) {
    if (or == NULL) {
        fprintf(stderr, "<NULL or>");
        return;
    }
    fprintf(stderr, "(or ");
    ppLamExp(or->left);
    fprintf(stderr, " ");
    ppLamExp(or->right);
    fprintf(stderr, ")");
}

void ppLamAmb(LamAmb *amb) {
    if (amb == NULL) {
        fprintf(stderr, "<NULL amb>");
        return;
    }
    fprintf(stderr, "(amb ");
    ppLamExp(amb->left);
    fprintf(stderr, " ");
    ppLamExp(amb->right);
    fprintf(stderr, ")");
}

static void _ppLamVarList(LamVarList *varList) {
    if (varList == NULL) return;
    ppHashSymbol(varList->var);
    if (varList->next != NULL) {
        fprintf(stderr, " ");
        _ppLamVarList(varList->next);
    }
}

void ppLamVarList(LamVarList *varList) {
    fprintf(stderr, "(");
    _ppLamVarList(varList);
    fprintf(stderr, ")");
}

void ppLamExp(LamExp *exp) {
    if (exp == NULL) {
        fprintf(stderr, "<NULL exp>");
        return;
    }
    switch (exp->type) {
        case LAMEXP_TYPE_LAM:
            ppLamLam(exp->val.lam);
            break;
        case LAMEXP_TYPE_VAR:
            ppHashSymbol(exp->val.var);
            break;
        case LAMEXP_TYPE_INTEGER:
            fprintf(stderr, "%d", exp->val.integer);
            break;
        case LAMEXP_TYPE_PRIM:
            ppLamPrimApp(exp->val.prim);
            break;
        case LAMEXP_TYPE_UNARY:
            ppLamUnary(exp->val.unary);
            break;
        case LAMEXP_TYPE_LIST:
            ppLamSequence(exp->val.list);
            break;
        case LAMEXP_TYPE_MAKEVEC:
            ppLamMakeVec(exp->val.makeVec);
            break;
        case LAMEXP_TYPE_APPLY:
            ppLamApply(exp->val.apply);
            break;
        case LAMEXP_TYPE_IFF:
            ppLamIff(exp->val.iff);
            break;
        case LAMEXP_TYPE_CALLCC:
            ppLamCallCC(exp->val.callcc); // LamExp
            break;
        case LAMEXP_TYPE_LETREC:
            ppLamLetRec(exp->val.letrec);
            break;
        case LAMEXP_TYPE_LET:
            ppLamLet(exp->val.let);
            break;
        case LAMEXP_TYPE_MATCH:
            ppLamMatch(exp->val.match);
            break;
        case LAMEXP_TYPE_CHARACTER:
            fprintf(stderr, "'%c'", exp->val.character);
            break;
        case LAMEXP_TYPE_BACK:
            fprintf(stderr, "(back)");
            break;
        case LAMEXP_TYPE_ERROR:
            fprintf(stderr, "(error)");
            break;
        case LAMEXP_TYPE_COND:
            ppLamCond(exp->val.cond);
            break;
        case LAMEXP_TYPE_AND:
            ppLamAnd(exp->val.and);
            break;
        case LAMEXP_TYPE_OR:
            ppLamOr(exp->val.or);
            break;
        case LAMEXP_TYPE_AMB:
            ppLamAmb(exp->val.amb);
            break;
        case LAMEXP_TYPE_COND_DEFAULT:
            fprintf(stderr, "default");
            break;
        default:
            cant_happen("unrecognized type %d in ppLamExp", exp->type);
    }
}

void ppHashSymbol(HashSymbol *symbol) {
    if (symbol == NULL) {
        fprintf(stderr, "<NULL symbol>");
        return;
    }
    fprintf(stderr, "<%s>", symbol->name);
}

void ppLamPrimApp(LamPrimApp *primApp) {
    if (primApp == NULL) {
        fprintf(stderr, "<NULL primApp>");
        return;
    }
    fprintf(stderr, "(");
    ppLamPrimOp(primApp->type);
    fprintf(stderr, " ");
    ppLamExp(primApp->exp1);
    fprintf(stderr, " ");
    ppLamExp(primApp->exp2);
    fprintf(stderr, ")");
}

void ppLamPrimOp(LamPrimOp type) {
    switch (type) {
        case LAMPRIMOP_TYPE_LAM_PRIM_ADD:
            fprintf(stderr, "add");
            break;
        case LAMPRIMOP_TYPE_LAM_PRIM_SUB:
            fprintf(stderr, "sub");
            break;
        case LAMPRIMOP_TYPE_LAM_PRIM_MUL:
            fprintf(stderr, "mul");
            break;
        case LAMPRIMOP_TYPE_LAM_PRIM_DIV:
            fprintf(stderr, "div");
            break;
        case LAMPRIMOP_TYPE_LAM_PRIM_EQ:
            fprintf(stderr, "eq");
            break;
        case LAMPRIMOP_TYPE_LAM_PRIM_NE:
            fprintf(stderr, "ne");
            break;
        case LAMPRIMOP_TYPE_LAM_PRIM_GT:
            fprintf(stderr, "gt");
            break;
        case LAMPRIMOP_TYPE_LAM_PRIM_LT:
            fprintf(stderr, "lt");
            break;
        case LAMPRIMOP_TYPE_LAM_PRIM_GE:
            fprintf(stderr, "ge");
            break;
        case LAMPRIMOP_TYPE_LAM_PRIM_LE:
            fprintf(stderr, "le");
            break;
        case LAMPRIMOP_TYPE_LAM_PRIM_VEC:
            fprintf(stderr, "vec");
            break;
        case LAMPRIMOP_TYPE_LAM_PRIM_XOR:
            fprintf(stderr, "xor");
            break;
        case LAMPRIMOP_TYPE_LAM_PRIM_MOD:
            fprintf(stderr, "mod");
            break;
        default:
            cant_happen("unrecognised type %d in ppLamPrimOp", type);
    }
}

void ppLamUnary(LamUnaryApp *unaryApp) {
    if (unaryApp == NULL) {
        fprintf(stderr, "<NULL unaryApp>");
        return;
    }
    fprintf(stderr, "(");
    ppLamUnaryOp(unaryApp->type);
    fprintf(stderr, " ");
    ppLamExp(unaryApp->exp);
    fprintf(stderr, ")");
}

void ppLamUnaryOp(LamUnaryOp type) {
    switch (type) {
        case LAMUNARYOP_TYPE_LAM_UNARY_NOT:
            fprintf(stderr, "not");
            break;
        case LAMUNARYOP_TYPE_LAM_UNARY_PRINT:
            fprintf(stderr, "print");
            break;
        default:
            cant_happen("unrecognised type %d in ppLamUnaryOp", type);
    }
}

static void _ppLamSequence(LamSequence *sequence) {
    if (sequence == NULL) return;
    ppLamExp(sequence->exp);
    if (sequence->next != NULL) {
        fprintf(stderr, " ");
        _ppLamSequence(sequence->next);
    }
}

static void _ppLamList(LamList *list) {
    if (list == NULL) return;
    ppLamExp(list->exp);
    if (list->next != NULL) {
        fprintf(stderr, " ");
        _ppLamList(list->next);
    }
}

void ppLamSequence(LamSequence *sequence) {
    fprintf(stderr, "(begin ");
    _ppLamSequence(sequence);
    fprintf(stderr, ")");
}

void ppLamMakeVec(LamMakeVec *makeVec) {
    if (makeVec == NULL) {
        fprintf(stderr, "<NULL makeVec>");
        return;
    }
    fprintf(stderr, "(make-vec");
    if (makeVec->args != NULL) {
        fprintf(stderr, " ");
        _ppLamList(makeVec->args);
    }
    fprintf(stderr, ")");
}

void ppLamApply(LamApply *apply) {
    if (apply == NULL) {
        fprintf(stderr, "<NULL apply>");
        return;
    }
    fprintf(stderr, "(");
    ppLamExp(apply->function);
    if (apply->args != NULL) {
        fprintf(stderr, " ");
        _ppLamList(apply->args);
    }
    fprintf(stderr, ")");
}

void ppLamIff(LamIff *iff) {
    if (iff == NULL) {
        fprintf(stderr, "<NULL if>");
        return;
    }
    fprintf(stderr, "(if ");
    ppLamExp(iff->condition);
    fprintf(stderr, " ");
    ppLamExp(iff->consequent);
    fprintf(stderr, " ");
    ppLamExp(iff->alternative);
    fprintf(stderr, ")");
}

static void _ppLamCondCases(LamCondCases *cases) {
    fprintf(stderr, "(");
    ppLamExp(cases->constant);
    fprintf(stderr, " ");
    ppLamExp(cases->body);
    fprintf(stderr, ")");
    if (cases->next != NULL) {
        fprintf(stderr, " ");
        _ppLamCondCases(cases->next);
    }
}

void ppLamCond(LamCond *cond) {
    if (cond == NULL) {
        fprintf(stderr, "<NULL cond>");
        return;
    }
    fprintf(stderr, "(cond ");
    ppLamExp(cond->value);
    if (cond->cases != NULL) {
        fprintf(stderr, " ");
        _ppLamCondCases(cond->cases);
    }
    fprintf(stderr, ")");
}

void ppLamCallCC(LamExp *exp) {
    if (exp == NULL) {
        fprintf(stderr, "<NULL call/cc>");
        return;
    }
    fprintf(stderr, "(call/cc ");
    ppLamExp(exp);
    fprintf(stderr, ")");
}

void ppLamLetRec(LamLetRec *letRec) {
    if (letRec == NULL) {
        fprintf(stderr, "<NULL letRec>");
        return;
    }
    fprintf(stderr, "(letrec ");
    ppLamLetRecBindings(letRec->bindings);
    if (letRec->body != NULL) {
        fprintf(stderr, " ");
        ppLamExp(letRec->body);
    }
    fprintf(stderr, ")");
}

void ppLamLet(LamLet *let) {
    if (let == NULL) {
        fprintf(stderr, "<NULL let>");
        return;
    }
    fprintf(stderr, "(let (");
    ppHashSymbol(let->var);
    fprintf(stderr, " ");
    ppLamExp(let->value);
    fprintf(stderr, ") ");
    ppLamExp(let->body);
    fprintf(stderr, ")");

}

static void _ppLamMatchList(LamMatchList *cases) {
    if (cases == NULL) return;
    fprintf(stderr, "(");
    ppLamIntList(cases->matches);
    if (cases->body) {
        fprintf(stderr, " ");
        ppLamExp(cases->body);
    }
    fprintf(stderr, ")");
    if (cases->next) {
        fprintf(stderr, " ");
        _ppLamMatchList(cases->next);
    }
}

void ppLamMatch(LamMatch *match) {
    if (match == NULL) {
        fprintf(stderr, "<NULL match>");
        return;
    }
    fprintf(stderr, "(match ");
    ppLamExp(match->index);
    if (match->cases != NULL) {
        fprintf(stderr, " ");
        _ppLamMatchList(match->cases);
    }
    fprintf(stderr, ")");
}

static void _ppLamLetRecBindings(LamLetRecBindings *bindings) {
    if (bindings == NULL) return;
    fprintf(stderr, "(");
    ppHashSymbol(bindings->var);
    fprintf(stderr, " ");
    ppLamExp(bindings->val);
    fprintf(stderr, ")");
    if (bindings->next) {
        fprintf(stderr, " ");
       _ppLamLetRecBindings(bindings->next);
    }
}

void ppLamLetRecBindings(LamLetRecBindings *bindings) {
    fprintf(stderr, "(");
    _ppLamLetRecBindings(bindings);
    fprintf(stderr, ")");
}

static void _ppLamIntList(LamIntList *list) {
    if (list == NULL) return;
    fprintf(stderr, "%d", list->item);
    if (list->next != NULL) {
        fprintf(stderr, " ");
        _ppLamIntList(list->next);
    }
}

void ppLamIntList(LamIntList *list) {
    fprintf(stderr, "(");
    _ppLamIntList(list);
    fprintf(stderr, ")");
}

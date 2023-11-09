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
        printf("    ");
    }
    ppLamExp(exp);
}

void ppLamLam(LamLam *lam) {
    printf("(lambda ");
    ppLamVarList(lam->args);
    printf(" ");
    ppLamExp(lam->exp);
    printf(")");
}

static void _ppLamVarList(LamVarList *varList) {
    if (varList == NULL) return;
    ppHashSymbol(varList->var);
    if (varList->next != NULL) {
        printf(" ");
        _ppLamVarList(varList->next);
    }
}

void ppLamVarList(LamVarList *varList) {
    printf("(");
    _ppLamVarList(varList);
    printf(")");
}

void ppLamExp(LamExp *exp) {
    switch (exp->type) {
        case LAMEXP_TYPE_LAM:
            ppLamLam(exp->val.lam);
            break;
        case LAMEXP_TYPE_VAR:
            ppHashSymbol(exp->val.var);
            break;
        case LAMEXP_TYPE_INTEGER:
            printf("%d", exp->val.integer);
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
        case LAMEXP_TYPE_COND:
            ppLamCond(exp->val.cond);
            break;
        case LAMEXP_TYPE_CALLCC:
            ppLamCallCC(exp->val.callcc); // LamExp
            break;
        case LAMEXP_TYPE_LETREC:
            ppLamLetRec(exp->val.letrec);
            break;
        case LAMEXP_TYPE_MATCH:
            ppLamMatch(exp->val.match);
            break;
        case LAMEXP_TYPE_CHARACTER:
            printf("'%c'", exp->val.character);
            break;
        case LAMEXP_TYPE_BACK:
            printf("(back)");
            break;
        default:
            cant_happen("unrecognized type %d in ppLamExp", exp->type);
    }
}

void ppHashSymbol(HashSymbol *symbol) {
    printf("<%s>", symbol->name);
}

void ppLamPrimApp(LamPrimApp *primApp) {
    printf("(");
    ppLamPrimOp(primApp->type);
    printf(" ");
    ppLamExp(primApp->exp1);
    printf(" ");
    ppLamExp(primApp->exp2);
    printf(")");
}

void ppLamPrimOp(LamPrimOp type) {
    switch (type) {
        case LAMPRIMOP_TYPE_LAM_PRIM_ADD:
            printf("+");
            break;
        case LAMPRIMOP_TYPE_LAM_PRIM_SUB:
            printf("-");
            break;
        case LAMPRIMOP_TYPE_LAM_PRIM_MUL:
            printf("*");
            break;
        case LAMPRIMOP_TYPE_LAM_PRIM_DIV:
            printf("/");
            break;
        case LAMPRIMOP_TYPE_LAM_PRIM_EQ:
            printf("eq");
            break;
        case LAMPRIMOP_TYPE_LAM_PRIM_NE:
            printf("ne");
            break;
        case LAMPRIMOP_TYPE_LAM_PRIM_GT:
            printf("gt");
            break;
        case LAMPRIMOP_TYPE_LAM_PRIM_LT:
            printf("lt");
            break;
        case LAMPRIMOP_TYPE_LAM_PRIM_GE:
            printf("ge");
            break;
        case LAMPRIMOP_TYPE_LAM_PRIM_LE:
            printf("le");
            break;
        case LAMPRIMOP_TYPE_LAM_PRIM_VEC:
            printf("vec");
            break;
        case LAMPRIMOP_TYPE_LAM_PRIM_XOR:
            printf("xor");
            break;
        case LAMPRIMOP_TYPE_LAM_PRIM_AND:
            printf("and");
            break;
        case LAMPRIMOP_TYPE_LAM_PRIM_OR:
            printf("or");
            break;
        case LAMPRIMOP_TYPE_LAM_PRIM_AMB:
            printf("amb");
            break;
        default:
            cant_happen("unrecognised type %d in ppLamPrimOp", type);
    }
}

void ppLamUnary(LamUnaryApp *unaryApp) {
    printf("(");
    ppLamUnaryOp(unaryApp->type);
    printf(" ");
    ppLamExp(unaryApp->exp);
    printf(")");
}

void ppLamUnaryOp(LamUnaryOp type) {
    switch (type) {
        case LAMUNARYOP_TYPE_LAM_UNARY_NOT:
            printf("not");
            break;
        case LAMUNARYOP_TYPE_LAM_UNARY_PRINT:
            printf("print");
            break;
        default:
            cant_happen("unrecognised type %d in ppLamUnaryOp", type);
    }
}

static void _ppLamSequence(LamSequence *sequence) {
    if (sequence == NULL) return;
    ppLamExp(sequence->exp);
    if (sequence->next != NULL) {
        printf(" ");
        _ppLamSequence(sequence->next);
    }
}

void ppLamSequence(LamSequence *sequence) {
    printf("(");
    _ppLamSequence(sequence);
    printf(")");
}

void ppLamMakeVec(LamMakeVec *makeVec) {
    printf("(make-vec");
    if (makeVec->args != NULL) {
        printf(" ");
        _ppLamSequence(makeVec->args);
    }
    printf(")");
}

void ppLamApply(LamApply *apply) {
    printf("(");
    ppLamExp(apply->function);
    if (apply->args != NULL) {
        printf(" ");
        _ppLamSequence(apply->args);
    }
    printf(")");
}

void ppLamCond(LamCond *cond) {
    printf("(if ");
    ppLamExp(cond->condition);
    printf(" ");
    ppLamExp(cond->consequent);
    printf(" ");
    ppLamExp(cond->alternative);
    printf(")");
}

void ppLamCallCC(LamExp *exp) {
    printf("(call/cc ");
    ppLamExp(exp);
    printf(")");
}

void ppLamLetRec(LamLetRec *letRec) {
    printf("(letrec ");
    ppLamLetRecBindings(letRec->bindings);
    if (letRec->body != NULL) {
        printf(" ");
        _ppLamSequence(letRec->body);
    }
    printf(")");
}

static void _ppLamMatchList(LamMatchList *cases) {
    if (cases == NULL) return;
    printf("(");
    ppLamIntList(cases->matches);
    if (cases->body) {
        printf(" ");
        ppLamExp(cases->body);
    }
    printf(")");
    if (cases->next) {
        printf(" ");
        _ppLamMatchList(cases->next);
    }
}

void ppLamMatch(LamMatch *match) {
    printf("(match ");
    ppLamExp(match->index);
    if (match->cases != NULL) {
        printf(" ");
        _ppLamMatchList(match->cases);
    }
    printf(")");
}

static void _ppLamLetRecBindings(LamLetRecBindings *bindings) {
    if (bindings == NULL) return;
    printf("(");
    ppHashSymbol(bindings->var);
    printf(" ");
    ppLamExp(bindings->val);
    printf(")");
    if (bindings->next) {
        printf(" ");
       _ppLamLetRecBindings(bindings->next);
    }
}

void ppLamLetRecBindings(LamLetRecBindings *bindings) {
    printf("(");
    _ppLamLetRecBindings(bindings);
    printf(")");
}

static void _ppLamIntList(LamIntList *list) {
    if (list == NULL) return;
    printf("%d", list->item);
    if (list->next != NULL) {
        printf(" ");
        _ppLamIntList(list->next);
    }
}

void ppLamIntList(LamIntList *list) {
    printf("(");
    _ppLamIntList(list);
    printf(")");
}

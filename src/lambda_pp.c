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
        eprintf("    ");
    }
    ppLamExp(exp);
}

void ppLamLam(LamLam *lam) {
    if (lam == NULL) {
        eprintf("<NULL lambda>");
        return;
    }
    eprintf("(lambda ");
    ppLamVarList(lam->args);
    eprintf(" ");
    ppLamExp(lam->exp);
    eprintf(")");
}

void ppLamAnd(LamAnd *and) {
    if (and == NULL) {
        eprintf("<NULL and>");
        return;
    }
    eprintf("(and ");
    ppLamExp(and->left);
    eprintf(" ");
    ppLamExp(and->right);
    eprintf(")");
}

void ppLamOr(LamOr *or) {
    if (or == NULL) {
        eprintf("<NULL or>");
        return;
    }
    eprintf("(or ");
    ppLamExp(or->left);
    eprintf(" ");
    ppLamExp(or->right);
    eprintf(")");
}

void ppLamAmb(LamAmb *amb) {
    if (amb == NULL) {
        eprintf("<NULL amb>");
        return;
    }
    eprintf("(amb ");
    ppLamExp(amb->left);
    eprintf(" ");
    ppLamExp(amb->right);
    eprintf(")");
}

static void _ppLamVarList(LamVarList *varList) {
    if (varList == NULL) return;
    ppHashSymbol(varList->var);
    if (varList->next != NULL) {
        eprintf(" ");
        _ppLamVarList(varList->next);
    }
}

void ppLamVarList(LamVarList *varList) {
    eprintf("(");
    _ppLamVarList(varList);
    eprintf(")");
}

void ppLamExp(LamExp *exp) {
    if (exp == NULL) {
        eprintf("<NULL exp>");
        return;
    }
    switch (exp->type) {
        case LAMEXP_TYPE_LAM:
            ppLamLam(exp->val.lam);
            break;
        case LAMEXP_TYPE_VAR:
            ppHashSymbol(exp->val.var);
            break;
        case LAMEXP_TYPE_BIGINTEGER:
            fprintBigInt(stderr, exp->val.biginteger);
            break;
        case LAMEXP_TYPE_STDINT:
            eprintf("%d", exp->val.stdint);
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
        case LAMEXP_TYPE_CONSTRUCT:
            ppLamConstruct(exp->val.construct);
            break;
        case LAMEXP_TYPE_CONSTANT:
            ppLamConstant(exp->val.constant);
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
        case LAMEXP_TYPE_TYPEDEFS:
            ppLamTypeDefs(exp->val.typedefs);
            break;
        case LAMEXP_TYPE_LET:
            ppLamLet(exp->val.let);
            break;
        case LAMEXP_TYPE_MATCH:
            ppLamMatch(exp->val.match);
            break;
        case LAMEXP_TYPE_CHARACTER:
            eprintf("'%c'", exp->val.character);
            break;
        case LAMEXP_TYPE_BACK:
            eprintf("(back)");
            break;
        case LAMEXP_TYPE_ERROR:
            eprintf("(error)");
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
            eprintf("default");
            break;
        default:
            cant_happen("unrecognized type %d in ppLamExp", exp->type);
    }
}

void ppHashSymbol(HashSymbol *symbol) {
    if (symbol == NULL) {
        eprintf("<NULL symbol>");
        return;
    }
    eprintf("%s", symbol->name);
}

void ppLamPrimApp(LamPrimApp *primApp) {
    if (primApp == NULL) {
        eprintf("<NULL primApp>");
        return;
    }
    eprintf("(");
    ppLamPrimOp(primApp->type);
    eprintf(" ");
    ppLamExp(primApp->exp1);
    eprintf(" ");
    ppLamExp(primApp->exp2);
    eprintf(")");
}

void ppLamPrimOp(LamPrimOp type) {
    switch (type) {
        case LAMPRIMOP_TYPE_ADD:
            eprintf("add");
            break;
        case LAMPRIMOP_TYPE_SUB:
            eprintf("sub");
            break;
        case LAMPRIMOP_TYPE_MUL:
            eprintf("mul");
            break;
        case LAMPRIMOP_TYPE_DIV:
            eprintf("div");
            break;
        case LAMPRIMOP_TYPE_EQ:
            eprintf("eq");
            break;
        case LAMPRIMOP_TYPE_NE:
            eprintf("ne");
            break;
        case LAMPRIMOP_TYPE_GT:
            eprintf("gt");
            break;
        case LAMPRIMOP_TYPE_LT:
            eprintf("lt");
            break;
        case LAMPRIMOP_TYPE_GE:
            eprintf("ge");
            break;
        case LAMPRIMOP_TYPE_LE:
            eprintf("le");
            break;
        case LAMPRIMOP_TYPE_VEC:
            eprintf("vec");
            break;
        case LAMPRIMOP_TYPE_XOR:
            eprintf("xor");
            break;
        case LAMPRIMOP_TYPE_MOD:
            eprintf("mod");
            break;
        default:
            cant_happen("unrecognised type %d in ppLamPrimOp", type);
    }
}

void ppLamUnary(LamUnaryApp *unaryApp) {
    if (unaryApp == NULL) {
        eprintf("<NULL unaryApp>");
        return;
    }
    eprintf("(");
    ppLamUnaryOp(unaryApp->type);
    eprintf(" ");
    ppLamExp(unaryApp->exp);
    eprintf(")");
}

void ppLamUnaryOp(LamUnaryOp type) {
    switch (type) {
        case LAMUNARYOP_TYPE_NOT:
            eprintf("not");
            break;
        case LAMUNARYOP_TYPE_PRINT:
            eprintf("print");
            break;
        default:
            cant_happen("unrecognised type %d in ppLamUnaryOp", type);
    }
}

static void _ppLamSequence(LamSequence *sequence) {
    if (sequence == NULL) return;
    ppLamExp(sequence->exp);
    if (sequence->next != NULL) {
        eprintf(" ");
        _ppLamSequence(sequence->next);
    }
}

static void _ppLamList(LamList *list) {
    if (list == NULL) return;
    eprintf(" ");
    ppLamExp(list->exp);
    _ppLamList(list->next);
}

void ppLamSequence(LamSequence *sequence) {
    eprintf("(begin ");
    _ppLamSequence(sequence);
    eprintf(")");
}

void ppLamMakeVec(LamMakeVec *makeVec) {
    if (makeVec == NULL) {
        eprintf("<NULL makeVec>");
        return;
    }
    eprintf("(make-vec");
    _ppLamList(makeVec->args);
    eprintf(")");
}

void ppLamApply(LamApply *apply) {
    if (apply == NULL) {
        eprintf("<NULL apply>");
        return;
    }
    eprintf("(");
    ppLamExp(apply->function);
    _ppLamList(apply->args);
    eprintf(")");
}

void ppLamIff(LamIff *iff) {
    if (iff == NULL) {
        eprintf("<NULL if>");
        return;
    }
    eprintf("(if ");
    ppLamExp(iff->condition);
    eprintf(" ");
    ppLamExp(iff->consequent);
    eprintf(" ");
    ppLamExp(iff->alternative);
    eprintf(")");
}

static void _ppLamIntCondCases(LamIntCondCases *cases) {
    eprintf("(");
    fprintBigInt(stderr, cases->constant);
    eprintf(" ");
    ppLamExp(cases->body);
    eprintf(")");
    if (cases->next != NULL) {
        eprintf(" ");
        _ppLamIntCondCases(cases->next);
    }
}

static void _ppLamCharCondCases(LamCharCondCases *cases) {
    eprintf("('%c' ", cases->constant);
    ppLamExp(cases->body);
    eprintf(")");
    if (cases->next != NULL) {
        eprintf(" ");
        _ppLamCharCondCases(cases->next);
    }
}

static void _ppLamCondCases(LamCondCases *cases) {
    switch (cases->type) {
        case LAMCONDCASES_TYPE_INTEGERS:
            _ppLamIntCondCases(cases->val.integers);
            break;
        case LAMCONDCASES_TYPE_CHARACTERS:
            _ppLamCharCondCases(cases->val.characters);
            break;
        default:
            cant_happen("unrecognised type %d in _ppLamCondCases", cases->type);
    }
}

void ppLamCond(LamCond *cond) {
    if (cond == NULL) {
        eprintf("<NULL cond>");
        return;
    }
    eprintf("(cond ");
    ppLamExp(cond->value);
    if (cond->cases != NULL) {
        eprintf(" ");
        _ppLamCondCases(cond->cases);
    }
    eprintf(")");
}

void ppLamCallCC(LamExp *exp) {
    if (exp == NULL) {
        eprintf("<NULL call/cc>");
        return;
    }
    eprintf("(call/cc ");
    ppLamExp(exp);
    eprintf(")");
}

void ppLamLetRec(LamLetRec *letRec) {
    if (letRec == NULL) {
        eprintf("<NULL letRec>");
        return;
    }
    eprintf("(letrec ");
    ppLamLetRecBindings(letRec->bindings);
    if (letRec->body != NULL) {
        eprintf(" ");
        ppLamExp(letRec->body);
    }
    eprintf(")");
}

void ppLamTypeDefList(LamTypeDefList *typeDefList);

void ppLamTypeDefs(LamTypeDefs *typeDefs) {
    if (typeDefs == NULL) {
        eprintf("<NULL typedefs>");
        return;
    }
    eprintf("(typedefs ");
    ppLamTypeDefList(typeDefs->typeDefs);
    if (typeDefs->body != NULL) {
        eprintf(" ");
        ppLamExp(typeDefs->body);
    }
    eprintf(")");
}

void ppLamLet(LamLet *let) {
    if (let == NULL) {
        eprintf("<NULL let>");
        return;
    }
    eprintf("(let (");
    ppHashSymbol(let->var);
    eprintf(" ");
    ppLamExp(let->value);
    eprintf(") ");
    ppLamExp(let->body);
    eprintf(")");

}

static void _ppLamMatchList(LamMatchList *cases) {
    if (cases == NULL) return;
    eprintf("(");
    ppLamIntList(cases->matches);
    if (cases->body) {
        eprintf(" ");
        ppLamExp(cases->body);
    }
    eprintf(")");
    if (cases->next) {
        eprintf(" ");
        _ppLamMatchList(cases->next);
    }
}

void ppLamMatch(LamMatch *match) {
    if (match == NULL) {
        eprintf("<NULL match>");
        return;
    }
    eprintf("(match ");
    ppLamExp(match->index);
    if (match->cases != NULL) {
        eprintf(" ");
        _ppLamMatchList(match->cases);
    }
    eprintf(")");
}

static void _ppLamLetRecBindings(LamLetRecBindings *bindings) {
    if (bindings == NULL) return;
    eprintf("(");
    ppHashSymbol(bindings->var);
    eprintf(" ");
    ppLamExp(bindings->val);
    eprintf(")");
    if (bindings->next) {
        eprintf(" ");
       _ppLamLetRecBindings(bindings->next);
    }
}

void ppLamLetRecBindings(LamLetRecBindings *bindings) {
    eprintf("(");
    _ppLamLetRecBindings(bindings);
    eprintf(")");
}

static void _ppLamTypeArgs(LamTypeArgs *args) {
    if (args == NULL) return;
    eprintf(" ");
    ppHashSymbol(args->name);
    _ppLamTypeArgs(args->next);
}

static void _ppLamType(LamType *type) {
    if (type->args == NULL) {
        ppHashSymbol(type->name);
        return;
    }
    eprintf("(");
    ppHashSymbol(type->name);
    _ppLamTypeArgs(type->args);
    eprintf(")");
}

static void _ppLamTypeConstructorArgs(LamTypeConstructorArgs *args);

static void _ppLamTypeFunction(LamTypeFunction *function) {
    eprintf("(");
    ppHashSymbol(function->name);
    _ppLamTypeConstructorArgs(function->args);
    eprintf(")");
}

static void _ppLamTypeConstructorType(LamTypeConstructorType *type) {
    switch (type->type) {
        case LAMTYPECONSTRUCTORTYPE_TYPE_INTEGER:
            eprintf("int");
            break;
        case LAMTYPECONSTRUCTORTYPE_TYPE_CHARACTER:
            eprintf("char");
            break;
        case LAMTYPECONSTRUCTORTYPE_TYPE_VAR:
            ppHashSymbol(type->val.var);
            break;
        case LAMTYPECONSTRUCTORTYPE_TYPE_FUNCTION:
            _ppLamTypeFunction(type->val.function);
            break;
        default:
            cant_happen("unrecognised type %d in _ppLamTypeConstructorType", type->type);
    }
}

static void _ppLamTypeConstructorArgs(LamTypeConstructorArgs *args) {
    if (args == NULL) return;
    eprintf(" ");
    _ppLamTypeConstructorType(args->arg);
    _ppLamTypeConstructorArgs(args->next);
}

static void _ppLamTypeConstructor(LamTypeConstructor *constructor) {
    // deliberately don't print the type
    if (constructor->args == NULL) {
        ppHashSymbol(constructor->name);
        return;
    }
    eprintf("(");
    ppHashSymbol(constructor->name);
    _ppLamTypeConstructorArgs(constructor->args);
    eprintf(")");
}

static void _ppLamTypeConstructorList(LamTypeConstructorList *list) {
    if (list == NULL) return;
    eprintf(" ");
    _ppLamTypeConstructor(list->constructor);
    _ppLamTypeConstructorList(list->next);
}

void ppLamTypeDef(LamTypeDef *typeDef) {
    eprintf("(");
    _ppLamType(typeDef->type);
    _ppLamTypeConstructorList(typeDef->constructors);
    eprintf(")");
}

static void _ppLamTypeDefList(LamTypeDefList *list) {
    if (list == NULL) return;
    ppLamTypeDef(list->typeDef);
    if (list->next) {
        eprintf(" ");
        _ppLamTypeDefList(list->next);
    }
}

void ppLamTypeDefList(LamTypeDefList *typeDefList) {
    eprintf("(");
    _ppLamTypeDefList(typeDefList);
    eprintf(")");
}

static void _ppLamIntList(LamIntList *list) {
    if (list == NULL) return;
    eprintf("%d", list->item);
    if (list->next != NULL) {
        eprintf(" ");
        _ppLamIntList(list->next);
    }
}

void ppLamIntList(LamIntList *list) {
    eprintf("(");
    _ppLamIntList(list);
    eprintf(")");
}

void ppLamConstruct(LamConstruct *construct) {
    eprintf("(construct ");
    ppHashSymbol(construct->name);
    eprintf(" %d", construct->tag);
    _ppLamList(construct->args);
    eprintf(")");
}

void ppLamConstant(LamConstant *constant) {
    eprintf("(constant ");
    ppHashSymbol(constant->name);
    eprintf(" %d)", constant->tag);
}

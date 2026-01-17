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

void ppMinExpD(MinExp *exp, int depth) {
    while (depth > 0) {
        depth--;
        eprintf("  ");
    }
    ppMinExp(exp);
}

void ppMinLam(MinLam *lam) {
    if (lam == NULL) {
        eprintf("<NULL lambda>");
        return;
    }
    eprintf("(λ ");
    ppMinVarList(lam->args);
    eprintf(" ");
    ppMinExp(lam->exp);
    eprintf(")");
}

void ppMinAmb(MinAmb *amb) {
    if (amb == NULL) {
        eprintf("<NULL amb>");
        return;
    }
    eprintf("(amb ");
    ppMinExp(amb->left);
    eprintf(" ");
    ppMinExp(amb->right);
    eprintf(")");
}

static void _ppMinVarList(MinVarList *varList) {
    if (varList == NULL)
        return;
    ppHashSymbol(varList->var);
    if (varList->next != NULL) {
        eprintf(" ");
        _ppMinVarList(varList->next);
    }
}

void ppMinVarList(MinVarList *varList) {
    eprintf("(");
    _ppMinVarList(varList);
    eprintf(")");
}

void ppMinExp(MinExp *exp) {
    // sleep(1);
    if (exp == NULL) {
        eprintf("<NULL exp>");
        return;
    }
    switch (exp->type) {
    case MINEXP_TYPE_LAM:
        ppMinLam(getMinExp_Lam(exp));
        break;
    case MINEXP_TYPE_VAR:
        ppHashSymbol(getMinExp_Var(exp));
        break;
    case MINEXP_TYPE_BIGINTEGER:
        fprintMaybeBigInt(errout, getMinExp_BigInteger(exp));
        break;
    case MINEXP_TYPE_STDINT:
        eprintf("%d", getMinExp_Stdint(exp));
        break;
    case MINEXP_TYPE_PRIM:
        ppMinPrimApp(getMinExp_Prim(exp));
        break;
    case MINEXP_TYPE_SEQUENCE:
        ppMinSequence(getMinExp_Sequence(exp));
        break;
    case MINEXP_TYPE_MAKEVEC:
        ppMinMakeVec(getMinExp_MakeVec(exp));
        break;
    case MINEXP_TYPE_TAG:
        ppMinTag(getMinExp_Tag(exp));
        break;
    case MINEXP_TYPE_APPLY:
        ppMinApply(getMinExp_Apply(exp));
        break;
    case MINEXP_TYPE_IFF:
        ppMinIff(getMinExp_Iff(exp));
        break;
    case MINEXP_TYPE_CALLCC:
        ppMinCallCC(getMinExp_CallCC(exp)); // MinExp
        break;
    case MINEXP_TYPE_TYPEDEFS:
        ppMinTypeDefs(getMinExp_TypeDefs(exp));
        break;
    case MINEXP_TYPE_LETREC:
        ppMinLetRec(getMinExp_LetRec(exp));
        break;
    case MINEXP_TYPE_MATCH:
        ppMinMatch(getMinExp_Match(exp));
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
    case MINEXP_TYPE_ERROR:
        eprintf("(error)");
        break;
    case MINEXP_TYPE_COND:
        ppMinCond(getMinExp_Cond(exp));
        break;
    case MINEXP_TYPE_AMB:
        ppMinAmb(getMinExp_Amb(exp));
        break;
    case MINEXP_TYPE_TUPLEINDEX:
        ppMinTupleIndex(getMinExp_TupleIndex(exp));
        break;
    case MINEXP_TYPE_MAKETUPLE:
        ppMinMakeTuple(getMinExp_MakeTuple(exp));
        break;
    case MINEXP_TYPE_NAMESPACES:
        ppMinNameSpaces(getMinExp_NameSpaces(exp));
        break;
    case MINEXP_TYPE_ENV:
        eprintf("env");
        break;
    case MINEXP_TYPE_LOOKUP:
        ppMinLookUp(getMinExp_LookUp(exp));
        break;
    default:
        cant_happen("unrecognized type %s", minExpTypeName(exp->type));
    }
}

void ppMinLookUp(MinLookUp *lookUp) {
    eprintf("(lookUp %s:%d ",
            lookUp->nsSymbol == NULL ? "" : lookUp->nsSymbol->name,
            lookUp->nsId);
    ppMinExp(lookUp->exp);
    eprintf(")");
}

void ppMinNameSpaces(MinNameSpaceArray *arr) {
    eprintf("(nameSpaces");
    for (Index i = 0; i < arr->size; ++i) {
        eprintf(" [");
        ppMinExp(arr->entries[i]);
        eprintf("]");
    }
    eprintf(")");
}

void ppMinPrimApp(MinPrimApp *primApp) {
    if (primApp == NULL) {
        eprintf("<NULL primApp>");
        return;
    }
    eprintf("(");
    ppMinPrimOp(primApp->type);
    eprintf(" ");
    ppMinExp(primApp->exp1);
    eprintf(" ");
    ppMinExp(primApp->exp2);
    eprintf(")");
}

void ppMinPrimOp(MinPrimOp type) {
    switch (type) {
    case MINPRIMOP_TYPE_ADD:
        eprintf("add");
        break;
    case MINPRIMOP_TYPE_SUB:
        eprintf("sub");
        break;
    case MINPRIMOP_TYPE_MUL:
        eprintf("mul");
        break;
    case MINPRIMOP_TYPE_DIV:
        eprintf("div");
        break;
    case MINPRIMOP_TYPE_EQ:
        eprintf("eq");
        break;
    case MINPRIMOP_TYPE_NE:
        eprintf("ne");
        break;
    case MINPRIMOP_TYPE_GT:
        eprintf("gt");
        break;
    case MINPRIMOP_TYPE_LT:
        eprintf("lt");
        break;
    case MINPRIMOP_TYPE_GE:
        eprintf("ge");
        break;
    case MINPRIMOP_TYPE_LE:
        eprintf("le");
        break;
    case MINPRIMOP_TYPE_VEC:
        eprintf("vec");
        break;
    case MINPRIMOP_TYPE_MOD:
        eprintf("mod");
        break;
    case MINPRIMOP_TYPE_POW:
        eprintf("pow");
        break;
    case MINPRIMOP_TYPE_CMP:
        eprintf("cmp");
        break;
    default:
        cant_happen("unrecognised type %d in ppMinPrimOp", type);
    }
}

static void _ppMinSequence(MinSequence *sequence) {
    if (sequence == NULL)
        return;
    ppMinExp(sequence->exp);
    if (sequence->next != NULL) {
        eprintf(" ");
        _ppMinSequence(sequence->next);
    }
}

static void _ppMinArgs(MinArgs *list) {
    if (list == NULL)
        return;
    eprintf(" ");
    ppMinExp(list->exp);
    _ppMinArgs(list->next);
}

void ppMinMakeTuple(MinArgs *args) {
    eprintf("(make-tuple");
    _ppMinArgs(args);
    eprintf(")");
}

void ppMinSequence(MinSequence *sequence) {
    eprintf("(begin ");
    _ppMinSequence(sequence);
    eprintf(")");
}

void ppMinMakeVec(MinMakeVec *makeVec) {
    if (makeVec == NULL) {
        eprintf("<NULL makeVec>");
        return;
    }
    eprintf("(make-vec");
    _ppMinArgs(makeVec->args);
    eprintf(")");
}

void ppMinApply(MinApply *apply) {
    if (apply == NULL) {
        eprintf("<NULL apply>");
        return;
    }
    eprintf("(");
    ppMinExp(apply->function);
    _ppMinArgs(apply->args);
    eprintf(")");
}

void ppMinIff(MinIff *iff) {
    if (iff == NULL) {
        eprintf("<NULL if>");
        return;
    }
    eprintf("(if ");
    ppMinExp(iff->condition);
    eprintf(" ");
    ppMinExp(iff->consequent);
    eprintf(" ");
    ppMinExp(iff->alternative);
    eprintf(")");
}

static void _ppMinIntCondCases(MinIntCondCases *cases) {
    eprintf("(");
    fprintMaybeBigInt(errout, cases->constant);
    eprintf(" ");
    ppMinExp(cases->body);
    eprintf(")");
    if (cases->next != NULL) {
        eprintf(" ");
        _ppMinIntCondCases(cases->next);
    }
}

static void _ppMinCharCondCases(MinCharCondCases *cases) {
    eprintf("(\"%c\" ", cases->constant);
    ppMinExp(cases->body);
    eprintf(")");
    if (cases->next != NULL) {
        eprintf(" ");
        _ppMinCharCondCases(cases->next);
    }
}

static void _ppMinCondCases(MinCondCases *cases) {
    switch (cases->type) {
    case MINCONDCASES_TYPE_INTEGERS:
        _ppMinIntCondCases(getMinCondCases_Integers(cases));
        break;
    case MINCONDCASES_TYPE_CHARACTERS:
        _ppMinCharCondCases(getMinCondCases_Characters(cases));
        break;
    default:
        cant_happen("unrecognised type %d in _ppMinCondCases", cases->type);
    }
}

void ppMinCond(MinCond *cond) {
    if (cond == NULL) {
        eprintf("<NULL cond>");
        return;
    }
    eprintf("(cond ");
    ppMinExp(cond->value);
    if (cond->cases != NULL) {
        eprintf(" ");
        _ppMinCondCases(cond->cases);
    }
    eprintf(")");
}

void ppMinCallCC(MinExp *exp) {
    if (exp == NULL) {
        eprintf("<NULL call/cc>");
        return;
    }
    eprintf("(call/cc ");
    ppMinExp(exp);
    eprintf(")");
}

void ppMinLetRec(MinLetRec *letRec) {
    if (letRec == NULL) {
        eprintf("<NULL letRec>");
        return;
    }
    eprintf("(letrec ");
    ppMinBindings(letRec->bindings);
    if (letRec->body != NULL) {
        eprintf(" ");
        ppMinExp(letRec->body);
    }
    eprintf(")");
}

void ppMinTypeDefList(MinTypeDefList *typeDefList);

void ppMinTypeDefs(MinTypeDefs *typeDefs) {
    if (typeDefs == NULL) {
        eprintf("<NULL typeDefs>");
        return;
    }
    eprintf("(typedefs ");
    ppMinTypeDefList(typeDefs->typeDefs);
    if (typeDefs->body != NULL) {
        eprintf(" ");
        ppMinExp(typeDefs->body);
    }
    eprintf(")");
}

static void _ppMinMatchList(MinMatchList *cases) {
    if (cases == NULL)
        return;
    eprintf("(");
    ppMinIntList(cases->matches);
    if (cases->body) {
        eprintf(" ");
        ppMinExp(cases->body);
    }
    eprintf(")");
    if (cases->next) {
        eprintf(" ");
        _ppMinMatchList(cases->next);
    }
}

void ppMinMatch(MinMatch *match) {
    if (match == NULL) {
        eprintf("<NULL match>");
        return;
    }
    eprintf("(match ");
    ppMinExp(match->index);
    if (match->cases != NULL) {
        eprintf(" ");
        _ppMinMatchList(match->cases);
    }
    eprintf(")");
}

void ppMinTupleIndex(MinTupleIndex *index) {
    if (index == NULL) {
        eprintf("<NULL tuple index>");
        return;
    }
    eprintf("(index %d ", index->vec);
    ppMinExp(index->exp);
    eprintf(")");
}

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

static void _ppMinTypeSigArgs(MinTypeSigArgs *args) {
    if (args == NULL)
        return;
    eprintf(" ");
    ppHashSymbol(args->name);
    _ppMinTypeSigArgs(args->next);
}

static void _ppMinTypeSig(MinTypeSig *type) {
    if (type->args == NULL) {
        ppHashSymbol(type->name);
        return;
    }
    eprintf("(");
    ppHashSymbol(type->name);
    _ppMinTypeSigArgs(type->args);
    eprintf(")");
}

static void ppLookUpSymbol(MinLookUpSymbol *ls) {
    eprintf("(lookUp %s:%d %s)", ls->nsSymbol->name, ls->nsId,
            ls->symbol->name);
}

static void ppLookUpOrSymbol(MinLookUpOrSymbol *los) {
    switch (los->type) {
    case MINLOOKUPORSYMBOL_TYPE_SYMBOL:
        ppHashSymbol(getMinLookUpOrSymbol_Symbol(los));
        break;
    case MINLOOKUPORSYMBOL_TYPE_LOOKUP:
        ppLookUpSymbol(getMinLookUpOrSymbol_LookUp(los));
        break;
    default:
        cant_happen("unrecognised %s", minLookUpOrSymbolTypeName(los->type));
    }
}

static void _ppMinTypeConstructorArgs(MinTypeConstructorArgs *args);

static void _ppMinTypeFunction(MinTypeFunction *function) {
    eprintf("(");
    ppLookUpOrSymbol(function->name);
    _ppMinTypeConstructorArgs(function->args);
    eprintf(")");
}

static void _ppMinTypeTuple(MinTypeConstructorArgs *args) {
    eprintf("#(");
    _ppMinTypeConstructorArgs(args);
    eprintf(")");
}

static void _ppMinTypeConstructorType(MinTypeConstructorType *type) {
    switch (type->type) {
    case MINTYPECONSTRUCTORTYPE_TYPE_INTEGER:
        eprintf("int");
        break;
    case MINTYPECONSTRUCTORTYPE_TYPE_CHARACTER:
        eprintf("char");
        break;
    case MINTYPECONSTRUCTORTYPE_TYPE_VAR:
        ppHashSymbol(getMinTypeConstructorType_Var(type));
        break;
    case MINTYPECONSTRUCTORTYPE_TYPE_FUNCTION:
        _ppMinTypeFunction(getMinTypeConstructorType_Function(type));
        break;
    case MINTYPECONSTRUCTORTYPE_TYPE_TUPLE:
        _ppMinTypeTuple(getMinTypeConstructorType_Tuple(type));
        break;
    default:
        cant_happen("unrecognised type %s in _ppMinTypeConstructorType",
                    minTypeConstructorTypeTypeName(type->type));
    }
}

static void _ppMinTypeConstructorArgs(MinTypeConstructorArgs *args) {
    if (args == NULL)
        return;
    eprintf(" ");
    _ppMinTypeConstructorType(args->arg);
    _ppMinTypeConstructorArgs(args->next);
}

static void _ppMinTypeConstructor(MinTypeConstructor *constructor) {
    // deliberately don't print the type
    if (constructor->args == NULL) {
        ppHashSymbol(constructor->name);
        return;
    }
    eprintf("(");
    ppHashSymbol(constructor->name);
    _ppMinTypeConstructorArgs(constructor->args);
    eprintf(")");
}

static void _ppMinTypeConstructorList(MinTypeConstructorList *list) {
    if (list == NULL)
        return;
    eprintf(" ");
    _ppMinTypeConstructor(list->constructor);
    _ppMinTypeConstructorList(list->next);
}

void ppMinTypeDef(MinTypeDef *typeDef) {
    eprintf("(");
    _ppMinTypeSig(typeDef->type);
    _ppMinTypeConstructorList(typeDef->constructors);
    eprintf(")");
}

static void _ppMinTypeDefList(MinTypeDefList *list) {
    if (list == NULL)
        return;
    ppMinTypeDef(list->typeDef);
    if (list->next) {
        eprintf(" ");
        _ppMinTypeDefList(list->next);
    }
}

void ppMinTypeDefList(MinTypeDefList *typeDefList) {
    eprintf("(");
    _ppMinTypeDefList(typeDefList);
    eprintf(")");
}

static void _ppMinIntList(MinIntList *list) {
    if (list == NULL)
        return;
    eprintf("%d:%s:%d", list->item, list->name->name, list->nsId);
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
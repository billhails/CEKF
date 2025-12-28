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
#include <unistd.h>
#include "lambda_pp.h"
void ppLamTag(LamExp *tag);

void ppLamExpD(LamExp *exp, int depth) {
    while (depth > 0) {
        depth--;
        eprintf("  ");
    }
    ppLamExp(exp);
}

void ppLamLam(LamLam *lam) {
    if (lam == NULL) {
        eprintf("<NULL lambda>");
        return;
    }
    eprintf("(λ ");
    ppLamVarList(lam->args);
    eprintf(" ");
    ppLamExp(lam->exp);
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
    if (varList == NULL)
        return;
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
    // sleep(1);
    if (exp == NULL) {
        eprintf("<NULL exp>");
        return;
    }
    switch (exp->type) {
        case LAMEXP_TYPE_LAM:
            ppLamLam(getLamExp_Lam(exp));
            break;
        case LAMEXP_TYPE_VAR:
            ppHashSymbol(getLamExp_Var(exp));
            break;
        case LAMEXP_TYPE_BIGINTEGER:
            fprintMaybeBigInt(errout, getLamExp_BigInteger(exp));
            break;
        case LAMEXP_TYPE_STDINT:
            eprintf("%d", getLamExp_Stdint(exp));
            break;
        case LAMEXP_TYPE_PRIM:
            ppLamPrimApp(getLamExp_Prim(exp));
            break;
        case LAMEXP_TYPE_SEQUENCE:
            ppLamSequence(getLamExp_Sequence(exp));
            break;
        case LAMEXP_TYPE_MAKEVEC:
            ppLamMakeVec(getLamExp_MakeVec(exp));
            break;
        case LAMEXP_TYPE_DECONSTRUCT:
            ppLamDeconstruct(getLamExp_Deconstruct(exp));
            break;
        case LAMEXP_TYPE_CONSTRUCT:
            ppLamConstruct(getLamExp_Construct(exp));
            break;
        case LAMEXP_TYPE_TAG:
            ppLamTag(getLamExp_Tag(exp));
            break;
        case LAMEXP_TYPE_CONSTANT:
            ppLamConstant(getLamExp_Constant(exp));
            break;
        case LAMEXP_TYPE_APPLY:
            ppLamApply(getLamExp_Apply(exp));
            break;
        case LAMEXP_TYPE_IFF:
            ppLamIff(getLamExp_Iff(exp));
            break;
        case LAMEXP_TYPE_CALLCC:
            ppLamCallCC(getLamExp_CallCC(exp));       // LamExp
            break;
        case LAMEXP_TYPE_PRINT:
            ppLamPrint(getLamExp_Print(exp));
            break;
        case LAMEXP_TYPE_LETREC:
            ppLamLetRec(getLamExp_LetRec(exp));
            break;
        case LAMEXP_TYPE_TYPEDEFS:
            ppLamTypeDefs(getLamExp_Typedefs(exp));
            break;
        case LAMEXP_TYPE_LET:
            ppLamLet(getLamExp_Let(exp));
            break;
        case LAMEXP_TYPE_MATCH:
            ppLamMatch(getLamExp_Match(exp));
            break;
        case LAMEXP_TYPE_CHARACTER:
            if (getLamExp_Character(exp) == L'\n')
                eprintf("\"\\n\"");
            else if (getLamExp_Character(exp) == L'\t')
                eprintf("\"\\t\"");
            else if (getLamExp_Character(exp) == L'\"')
                eprintf("\"\\\"\"");
            else if (getLamExp_Character(exp) == L'\\')
                eprintf("\"\\\\\"");
            else
                eprintf("\"%lc\"", getLamExp_Character(exp));
            break;
        case LAMEXP_TYPE_BACK:
            eprintf("(back)");
            break;
        case LAMEXP_TYPE_ERROR:
            eprintf("(error)");
            break;
        case LAMEXP_TYPE_COND:
            ppLamCond(getLamExp_Cond(exp));
            break;
        case LAMEXP_TYPE_AMB:
            ppLamAmb(getLamExp_Amb(exp));
            break;
        case LAMEXP_TYPE_TUPLEINDEX:
            ppLamTupleIndex(getLamExp_TupleIndex(exp));
            break;
        case LAMEXP_TYPE_MAKETUPLE:
            ppLamMakeTuple(getLamExp_MakeTuple(exp));
            break;
        case LAMEXP_TYPE_NAMESPACES:
            ppLamNameSpaces(getLamExp_NameSpaces(exp));
            break;
        case LAMEXP_TYPE_ENV:
            eprintf("env");
            break;
        case LAMEXP_TYPE_CONSTRUCTOR:
            eprintf("constructor:%s", getLamExp_Constructor(exp)->name->name);
            break;
        case LAMEXP_TYPE_LOOKUP:
            ppLamLookUp(getLamExp_LookUp(exp));
            break;
        default:
            cant_happen("unrecognized type %s", lamExpTypeName(exp->type));
    }
}

void ppLamLookUp(LamLookUp *lookUp) {
    eprintf("(lookUp %s:%d ", lookUp->nsSymbol == NULL ? "" : lookUp->nsSymbol->name, lookUp->nsid);
    ppLamExp(lookUp->exp);
    eprintf(")");
}

void ppLamNameSpaces(LamNameSpaceArray *arr) {
    eprintf("(nameSpaces");
    for (Index i = 0; i < arr->size; ++i) {
        eprintf(" [");
        ppLamExp(arr->entries[i]);
        eprintf("]");
    }
    eprintf(")");
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
        case LAMPRIMOP_TYPE_MOD:
            eprintf("mod");
            break;
        case LAMPRIMOP_TYPE_POW:
            eprintf("pow");
            break;
        case LAMPRIMOP_TYPE_CMP:
            eprintf("cmp");
            break;
        default:
            cant_happen("unrecognised type %d in ppLamPrimOp", type);
    }
}

static void _ppLamSequence(LamSequence *sequence) {
    if (sequence == NULL)
        return;
    ppLamExp(sequence->exp);
    if (sequence->next != NULL) {
        eprintf(" ");
        _ppLamSequence(sequence->next);
    }
}

static void _ppLamArgs(LamArgs *list) {
    if (list == NULL)
        return;
    eprintf(" ");
    ppLamExp(list->exp);
    _ppLamArgs(list->next);
}

void ppLamMakeTuple(LamArgs *args) {
    eprintf("(make-tuple");
    _ppLamArgs(args);
    eprintf(")");
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
    _ppLamArgs(makeVec->args);
    eprintf(")");
}

void ppLamApply(LamApply *apply) {
    if (apply == NULL) {
        eprintf("<NULL apply>");
        return;
    }
    eprintf("(");
    ppLamExp(apply->function);
    _ppLamArgs(apply->args);
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
    fprintMaybeBigInt(errout, cases->constant);
    eprintf(" ");
    ppLamExp(cases->body);
    eprintf(")");
    if (cases->next != NULL) {
        eprintf(" ");
        _ppLamIntCondCases(cases->next);
    }
}

static void _ppLamCharCondCases(LamCharCondCases *cases) {
    eprintf("(\"%c\" ", cases->constant);
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
            _ppLamIntCondCases(getLamCondCases_Integers(cases));
            break;
        case LAMCONDCASES_TYPE_CHARACTERS:
            _ppLamCharCondCases(getLamCondCases_Characters(cases));
            break;
        default:
            cant_happen("unrecognised type %d in _ppLamCondCases",
                        cases->type);
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

void ppLamPrint(LamPrint *print) {
    if (print == NULL) {
        eprintf("<NULL print>");
        return;
    }
    eprintf("(print ");
    ppLamExp(print->exp);
    eprintf(")");
}

void ppLamLetRec(LamLetRec *letRec) {
    if (letRec == NULL) {
        eprintf("<NULL letRec>");
        return;
    }
    eprintf("(letrec ");
    ppLamBindings(letRec->bindings);
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
    ppLamBindings(let->bindings);
    eprintf(") ");
    ppLamExp(let->body);
    eprintf(")");

}

static void _ppLamMatchList(LamMatchList *cases) {
    if (cases == NULL)
        return;
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

void ppLamTupleIndex(LamTupleIndex *index) {
    if (index == NULL) {
        eprintf("<NULL tuple index>");
        return;
    }
    eprintf("(index %d ", index->vec);
    ppLamExp(index->exp);
    eprintf(")");
}

static void _ppLamBindings(LamBindings *bindings) {
    if (bindings == NULL)
        return;
    eprintf("(");
    ppHashSymbol(bindings->var);
    eprintf(" ");
    ppLamExp(bindings->val);
    eprintf(")");
    if (bindings->next) {
        eprintf(" ");
        _ppLamBindings(bindings->next);
    }
}

void ppLamBindings(LamBindings *bindings) {
    eprintf("(");
    _ppLamBindings(bindings);
    eprintf(")");
}

static void _ppLamTypeSigArgs(LamTypeSigArgs *args) {
    if (args == NULL)
        return;
    eprintf(" ");
    ppHashSymbol(args->name);
    _ppLamTypeSigArgs(args->next);
}

static void _ppLamTypeSig(LamTypeSig *type) {
    if (type->args == NULL) {
        ppHashSymbol(type->name);
        return;
    }
    eprintf("(");
    ppHashSymbol(type->name);
    _ppLamTypeSigArgs(type->args);
    eprintf(")");
}

static void ppLookUpSymbol(LamLookUpSymbol *ls) {
    eprintf("(lookUp %s:%d %s)", ls->nsSymbol->name, ls->nsid, ls->symbol->name);
}

static void ppLookUpOrSymbol(LamLookUpOrSymbol *los) {
    switch (los->type) {
        case LAMLOOKUPORSYMBOL_TYPE_SYMBOL:
            ppHashSymbol(getLamLookUpOrSymbol_Symbol(los));
            break;
        case LAMLOOKUPORSYMBOL_TYPE_LOOKUP:
            ppLookUpSymbol(getLamLookUpOrSymbol_LookUp(los));
            break;
        default:
            cant_happen("unrecognised %s", lamLookUpOrSymbolTypeName(los->type));
    }
}

static void _ppLamTypeConstructorArgs(LamTypeConstructorArgs *args);

static void _ppLamTypeFunction(LamTypeFunction *function) {
    eprintf("(");
    ppLookUpOrSymbol(function->name);
    _ppLamTypeConstructorArgs(function->args);
    eprintf(")");
}

static void _ppLamTypeTuple(LamTypeConstructorArgs *args) {
    eprintf("#(");
    _ppLamTypeConstructorArgs(args);
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
            ppHashSymbol(getLamTypeConstructorType_Var(type));
            break;
        case LAMTYPECONSTRUCTORTYPE_TYPE_FUNCTION:
            _ppLamTypeFunction(getLamTypeConstructorType_Function(type));
            break;
        case LAMTYPECONSTRUCTORTYPE_TYPE_TUPLE:
            _ppLamTypeTuple(getLamTypeConstructorType_Tuple(type));
            break;
        default:
            cant_happen("unrecognised type %s in _ppLamTypeConstructorType",
                        lamTypeConstructorTypeTypeName(type->type));
    }
}

static void _ppLamTypeConstructorArgs(LamTypeConstructorArgs *args) {
    if (args == NULL)
        return;
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
    if (list == NULL)
        return;
    eprintf(" ");
    _ppLamTypeConstructor(list->constructor);
    _ppLamTypeConstructorList(list->next);
}

void ppLamTypeDef(LamTypeDef *typeDef) {
    eprintf("(");
    _ppLamTypeSig(typeDef->type);
    _ppLamTypeConstructorList(typeDef->constructors);
    eprintf(")");
}

static void _ppLamTypeDefList(LamTypeDefList *list) {
    if (list == NULL)
        return;
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
    if (list == NULL)
        return;
    eprintf("%d:%s:%d", list->item, list->name->name, list->nsid);
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
    eprintf("(");
    ppHashSymbol(construct->name);
    _ppLamArgs(construct->args);
    eprintf(")");
}

void ppLamTag(LamExp *tag) {
    eprintf("(tag ");
    ppLamExp(tag);
    eprintf(")");
}

void ppLamConstant(LamConstant *constant) {
    ppHashSymbol(constant->name);
}

void ppLamDeconstruct(LamDeconstruct *deconstruct) {
    eprintf("(deconstruct ");
    ppHashSymbol(deconstruct->name);
    eprintf("[%d] ", deconstruct->vec);
    ppLamExp(deconstruct->exp);
    eprintf(")");
}

static inline void pad(int depth) {
    eprintf("%*s", depth, "");
}

static void _ppLamContext(LamContext *env, int depth, bool done_nameSpaces) {
    if (env == NULL) {
        pad(depth);
        eprintf("<NULL> env\n");
        return;
    }
    pad(depth);
    eprintf("{\n");
    HashSymbol *name;
    Index i = 0;
    LamInfo *value;
    while ((name = iterateLamInfoTable(env->frame, &i, &value)) != NULL) {
        pad(depth);
        if (value->type == LAMINFO_TYPE_NAMESPACEINFO) {
            if (done_nameSpaces) {
                eprintf(" %s => %s\n", name->name, lamInfoTypeName(value->type));
            } else {
                eprintf(" %s => %s [\n", name->name, lamInfoTypeName(value->type));
                _ppLamContext(getLamInfo_NameSpaceInfo(value), depth + 1, true);
                pad(depth);
                eprintf(" ]\n");
            }
        } else if (value->type == LAMINFO_TYPE_NSID) {
            eprintf(" %s => %s [%d]\n", name->name, lamInfoTypeName(value->type), getLamInfo_Nsid(value));
        } else {
            eprintf(" %s => %s\n", name->name, lamInfoTypeName(value->type));
        }
    }
    _ppLamContext(env->parent, depth + 1, done_nameSpaces);
    pad(depth);
    eprintf("}\n");
}

void ppLamContext(LamContext *env) {
    _ppLamContext(env, 0, false);
}

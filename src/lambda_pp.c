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

#include "lambda_pp.h"
#include <stdio.h>
#include <unistd.h>

static void ppLamHashSymbol(FILE *out, HashSymbol *symbol) {
    if (symbol == NULL) {
        fprintf(out, "<NULL symbol>");
        return;
    }
    fprintf(out, "%s", symbol->name);
}

void ppLamTag(FILE *out, LamExp *tag);

void ppLamExpD(FILE *out, LamExp *exp, int depth) {
    while (depth > 0) {
        depth--;
        fprintf(out, "  ");
    }
    ppLamExp(out, exp);
}

void ppLamLam(FILE *out, LamLam *lam) {
    if (lam == NULL) {
        fprintf(out, "<NULL lambda>");
        return;
    }
    fprintf(out, "(λ ");
    ppLamVarList(out, lam->args);
    fprintf(out, " ");
    ppLamExp(out, lam->exp);
    fprintf(out, ")");
}

void ppLamAmb(FILE *out, LamAmb *amb) {
    if (amb == NULL) {
        fprintf(out, "<NULL amb>");
        return;
    }
    fprintf(out, "(amb ");
    ppLamExp(out, amb->left);
    fprintf(out, " ");
    ppLamExp(out, amb->right);
    fprintf(out, ")");
}

static void _ppLamVarList(FILE *out, SymbolList *varList) {
    if (varList == NULL)
        return;
    ppLamHashSymbol(out, varList->symbol);
    if (varList->next != NULL) {
        fprintf(out, " ");
        _ppLamVarList(out, varList->next);
    }
}

void ppLamVarList(FILE *out, SymbolList *varList) {
    fprintf(out, "(");
    _ppLamVarList(out, varList);
    fprintf(out, ")");
}

void ppLamTypeOf(FILE *out, LamTypeOf *typo) {
    fprintf(out, "(typeof ");
    ppLamExp(out, typo->exp);
    fprintf(out, ")");
}

void ppLamExp(FILE *out, LamExp *exp) {
    // sleep(1);
    if (exp == NULL) {
        fprintf(out, "<NULL exp>");
        return;
    }
    switch (exp->type) {
    case LAMEXP_TYPE_LAM:
        ppLamLam(out, getLamExp_Lam(exp));
        break;
    case LAMEXP_TYPE_VAR:
        ppLamHashSymbol(out, getLamExp_Var(exp));
        break;
    case LAMEXP_TYPE_BIGINTEGER:
        fprintMaybeBigInt2(out, getLamExp_BigInteger(exp));
        break;
    case LAMEXP_TYPE_STDINT:
        fprintf(out, "%d", getLamExp_Stdint(exp));
        break;
    case LAMEXP_TYPE_PRIM:
        ppLamPrimApp(out, getLamExp_Prim(exp));
        break;
    case LAMEXP_TYPE_SEQUENCE:
        ppLamSequence(out, getLamExp_Sequence(exp));
        break;
    case LAMEXP_TYPE_MAKEVEC:
        ppLamMakeVec(out, getLamExp_MakeVec(exp));
        break;
    case LAMEXP_TYPE_DECONSTRUCT:
        ppLamDeconstruct(out, getLamExp_Deconstruct(exp));
        break;
    case LAMEXP_TYPE_CONSTRUCT:
        ppLamConstruct(out, getLamExp_Construct(exp));
        break;
    case LAMEXP_TYPE_TAG:
        ppLamTag(out, getLamExp_Tag(exp));
        break;
    case LAMEXP_TYPE_CONSTANT:
        ppLamConstant(out, getLamExp_Constant(exp));
        break;
    case LAMEXP_TYPE_APPLY:
        ppLamApply(out, getLamExp_Apply(exp));
        break;
    case LAMEXP_TYPE_IFF:
        ppLamIff(out, getLamExp_Iff(exp));
        break;
    case LAMEXP_TYPE_CALLCC:
        ppLamCallCC(out, getLamExp_CallCC(exp)); // LamExp
        break;
    case LAMEXP_TYPE_CUT:
        fprintf(out, "(cut ");
        ppLamExp(out, getLamExp_Cut(exp));
        fprintf(out, ")");
        break;
    case LAMEXP_TYPE_TYPEOF:
        ppLamTypeOf(out, getLamExp_TypeOf(exp));
        break;
    case LAMEXP_TYPE_PRINT:
        ppLamPrint(out, getLamExp_Print(exp));
        break;
    case LAMEXP_TYPE_TYPEDEFS:
        ppLamTypeDefs(out, getLamExp_TypeDefs(exp));
        break;
    case LAMEXP_TYPE_LET:
        ppLamLet(out, getLamExp_Let(exp));
        break;
    case LAMEXP_TYPE_LETREC:
        ppLamLetRec(out, getLamExp_LetRec(exp));
        break;
    case LAMEXP_TYPE_LETSTAR:
        ppLamLetStar(out, getLamExp_LetStar(exp));
        break;
    case LAMEXP_TYPE_MATCH:
        ppLamMatch(out, getLamExp_Match(exp));
        break;
    case LAMEXP_TYPE_CHARACTER:
        if (getLamExp_Character(exp) == L'\n')
            fprintf(out, "\"\\n\"");
        else if (getLamExp_Character(exp) == L'\t')
            fprintf(out, "\"\\t\"");
        else if (getLamExp_Character(exp) == L'\"')
            fprintf(out, "\"\\\"\"");
        else if (getLamExp_Character(exp) == L'\\')
            fprintf(out, "\"\\\\\"");
        else
            fprintf(out, "\"%lc\"", getLamExp_Character(exp));
        break;
    case LAMEXP_TYPE_BACK:
        fprintf(out, "(back)");
        break;
    case LAMEXP_TYPE_ERROR:
        fprintf(out, "(error)");
        break;
    case LAMEXP_TYPE_COND:
        ppLamCond(out, getLamExp_Cond(exp));
        break;
    case LAMEXP_TYPE_AMB:
        ppLamAmb(out, getLamExp_Amb(exp));
        break;
    case LAMEXP_TYPE_TUPLEINDEX:
        ppLamTupleIndex(out, getLamExp_TupleIndex(exp));
        break;
    case LAMEXP_TYPE_MAKETUPLE:
        ppLamMakeTuple(out, getLamExp_MakeTuple(exp));
        break;
    case LAMEXP_TYPE_ENV:
        fprintf(out, "env");
        break;
    case LAMEXP_TYPE_CONSTRUCTOR:
        fprintf(out, "constructor:%s", getLamExp_Constructor(exp)->name->name);
        break;
    default:
        cant_happen("unrecognized type %s", lamExpTypeName(exp->type));
    }
}

void ppHashSymbol(FILE *out, HashSymbol *symbol) {
    if (symbol == NULL) {
        fprintf(out, "<NULL symbol>");
        return;
    }
    fprintf(out, "%s", symbol->name);
}

void ppLamPrimApp(FILE *out, LamPrimApp *primApp) {
    if (primApp == NULL) {
        fprintf(out, "<NULL primApp>");
        return;
    }
    fprintf(out, "(");
    ppLamPrimOp(out, primApp->type);
    fprintf(out, " ");
    ppLamExp(out, primApp->exp1);
    fprintf(out, " ");
    ppLamExp(out, primApp->exp2);
    fprintf(out, ")");
}

void ppLamPrimOp(FILE *out, LamPrimOp type) {
    switch (type) {
    case LAMPRIMOP_TYPE_ADD:
        fprintf(out, "add");
        break;
    case LAMPRIMOP_TYPE_SUB:
        fprintf(out, "sub");
        break;
    case LAMPRIMOP_TYPE_MUL:
        fprintf(out, "mul");
        break;
    case LAMPRIMOP_TYPE_DIV:
        fprintf(out, "div");
        break;
    case LAMPRIMOP_TYPE_GCD:
        fprintf(out, "gcd");
        break;
    case LAMPRIMOP_TYPE_LCM:
        fprintf(out, "lcm");
        break;
    case LAMPRIMOP_TYPE_CANON:
        fprintf(out, "canon");
        break;
    case LAMPRIMOP_TYPE_EQ:
        fprintf(out, "eq");
        break;
    case LAMPRIMOP_TYPE_NE:
        fprintf(out, "ne");
        break;
    case LAMPRIMOP_TYPE_GT:
        fprintf(out, "gt");
        break;
    case LAMPRIMOP_TYPE_LT:
        fprintf(out, "lt");
        break;
    case LAMPRIMOP_TYPE_GE:
        fprintf(out, "ge");
        break;
    case LAMPRIMOP_TYPE_LE:
        fprintf(out, "le");
        break;
    case LAMPRIMOP_TYPE_VEC:
        fprintf(out, "vec");
        break;
    case LAMPRIMOP_TYPE_MOD:
        fprintf(out, "mod");
        break;
    case LAMPRIMOP_TYPE_POW:
        fprintf(out, "pow");
        break;
    case LAMPRIMOP_TYPE_CMP:
        fprintf(out, "cmp");
        break;
    default:
        cant_happen("unrecognised type %d in ppLamPrimOp", type);
    }
}

static void _ppLamSequence(FILE *out, LamSequence *sequence) {
    if (sequence == NULL)
        return;
    ppLamExp(out, sequence->exp);
    if (sequence->next != NULL) {
        fprintf(out, " ");
        _ppLamSequence(out, sequence->next);
    }
}

static void _ppLamArgs(FILE *out, LamArgs *list) {
    if (list == NULL)
        return;
    fprintf(out, " ");
    ppLamExp(out, list->exp);
    _ppLamArgs(out, list->next);
}

void ppLamMakeTuple(FILE *out, LamArgs *args) {
    fprintf(out, "(make-tuple");
    _ppLamArgs(out, args);
    fprintf(out, ")");
}

void ppLamSequence(FILE *out, LamSequence *sequence) {
    fprintf(out, "(begin ");
    _ppLamSequence(out, sequence);
    fprintf(out, ")");
}

void ppLamMakeVec(FILE *out, LamMakeVec *makeVec) {
    if (makeVec == NULL) {
        fprintf(out, "<NULL makeVec>");
        return;
    }
    fprintf(out, "(make-vec");
    _ppLamArgs(out, makeVec->args);
    fprintf(out, ")");
}

void ppLamApply(FILE *out, LamApply *apply) {
    if (apply == NULL) {
        fprintf(out, "<NULL apply>");
        return;
    }
    fprintf(out, "(");
    ppLamExp(out, apply->function);
    _ppLamArgs(out, apply->args);
    fprintf(out, ")");
}

void ppLamIff(FILE *out, LamIff *iff) {
    if (iff == NULL) {
        fprintf(out, "<NULL if>");
        return;
    }
    fprintf(out, "(if ");
    ppLamExp(out, iff->condition);
    fprintf(out, " ");
    ppLamExp(out, iff->consequent);
    fprintf(out, " ");
    ppLamExp(out, iff->alternative);
    fprintf(out, ")");
}

static void _ppLamIntCondCases(FILE *out, LamIntCondCases *cases) {
    fprintf(out, "(");
    fprintMaybeBigInt2(out, cases->constant);
    fprintf(out, " ");
    ppLamExp(out, cases->body);
    fprintf(out, ")");
    if (cases->next != NULL) {
        fprintf(out, " ");
        _ppLamIntCondCases(out, cases->next);
    }
}

static void _ppLamCharCondCases(FILE *out, LamCharCondCases *cases) {
    fprintf(out, "(\"%c\" ", cases->constant);
    ppLamExp(out, cases->body);
    fprintf(out, ")");
    if (cases->next != NULL) {
        fprintf(out, " ");
        _ppLamCharCondCases(out, cases->next);
    }
}

static void _ppLamCondCases(FILE *out, LamCondCases *cases) {
    switch (cases->type) {
    case LAMCONDCASES_TYPE_INTEGERS:
        _ppLamIntCondCases(out, getLamCondCases_Integers(cases));
        break;
    case LAMCONDCASES_TYPE_CHARACTERS:
        _ppLamCharCondCases(out, getLamCondCases_Characters(cases));
        break;
    default:
        cant_happen("unrecognised type %d in _ppLamCondCases", cases->type);
    }
}

void ppLamCond(FILE *out, LamCond *cond) {
    if (cond == NULL) {
        fprintf(out, "<NULL cond>");
        return;
    }
    fprintf(out, "(cond ");
    ppLamExp(out, cond->value);
    if (cond->cases != NULL) {
        fprintf(out, " ");
        _ppLamCondCases(out, cond->cases);
    }
    fprintf(out, ")");
}

void ppLamCallCC(FILE *out, LamExp *exp) {
    if (exp == NULL) {
        fprintf(out, "<NULL call/cc>");
        return;
    }
    fprintf(out, "(call/cc ");
    ppLamExp(out, exp);
    fprintf(out, ")");
}

void ppLamPrint(FILE *out, LamPrint *print) {
    if (print == NULL) {
        fprintf(out, "<NULL print>");
        return;
    }
    fprintf(out, "(print ");
    ppLamExp(out, print->exp);
    fprintf(out, ")");
}

void ppLamLetRec(FILE *out, LamLetRec *letRec) {
    if (letRec == NULL) {
        fprintf(out, "<NULL letRec>");
        return;
    }
    fprintf(out, "(letrec ");
    ppLamBindings(out, letRec->bindings);
    if (letRec->body != NULL) {
        fprintf(out, " ");
        ppLamExp(out, letRec->body);
    }
    fprintf(out, ")");
}

void ppLamLetStar(FILE *out, LamLetStar *letStar) {
    if (letStar == NULL) {
        fprintf(out, "<NULL letStar>");
        return;
    }
    fprintf(out, "(let* ");
    ppLamBindings(out, letStar->bindings);
    if (letStar->body != NULL) {
        fprintf(out, " ");
        ppLamExp(out, letStar->body);
    }
    fprintf(out, ")");
}

void ppLamTypeDefList(FILE *out, LamTypeDefList *typeDefList);

void ppLamTypeDefs(FILE *out, LamTypeDefs *typeDefs) {
    if (typeDefs == NULL) {
        fprintf(out, "<NULL typeDefs>");
        return;
    }
    fprintf(out, "(typedefs ");
    ppLamTypeDefList(out, typeDefs->typeDefs);
    if (typeDefs->body != NULL) {
        fprintf(out, " ");
        ppLamExp(out, typeDefs->body);
    }
    fprintf(out, ")");
}

void ppLamLet(FILE *out, LamLet *let) {
    if (let == NULL) {
        fprintf(out, "<NULL let>");
        return;
    }
    fprintf(out, "(let (");
    ppLamBindings(out, let->bindings);
    fprintf(out, ") ");
    ppLamExp(out, let->body);
    fprintf(out, ")");
}

static void _ppLamMatchList(FILE *out, LamMatchList *cases) {
    if (cases == NULL)
        return;
    fprintf(out, "(");
    ppLamIntList(out, cases->matches);
    if (cases->body) {
        fprintf(out, " ");
        ppLamExp(out, cases->body);
    }
    fprintf(out, ")");
    if (cases->next) {
        fprintf(out, " ");
        _ppLamMatchList(out, cases->next);
    }
}

void ppLamMatch(FILE *out, LamMatch *match) {
    if (match == NULL) {
        fprintf(out, "<NULL match>");
        return;
    }
    fprintf(out, "(match ");
    ppLamExp(out, match->index);
    if (match->cases != NULL) {
        fprintf(out, " ");
        _ppLamMatchList(out, match->cases);
    }
    fprintf(out, ")");
}

void ppLamTupleIndex(FILE *out, LamTupleIndex *index) {
    if (index == NULL) {
        fprintf(out, "<NULL tuple index>");
        return;
    }
    fprintf(out, "(index %d ", index->vec);
    ppLamExp(out, index->exp);
    fprintf(out, ")");
}

static void _ppLamBindings(FILE *out, LamBindings *bindings) {
    if (bindings == NULL)
        return;
    fprintf(out, "(");
    ppLamHashSymbol(out, bindings->var);
    fprintf(out, " ");
    ppLamExp(out, bindings->val);
    fprintf(out, ")");
    if (bindings->next) {
        fprintf(out, " ");
        _ppLamBindings(out, bindings->next);
    }
}

void ppLamBindings(FILE *out, LamBindings *bindings) {
    fprintf(out, "(");
    _ppLamBindings(out, bindings);
    fprintf(out, ")");
}

static void _ppLamTypeSigArgs(FILE *out, LamTypeSigArgs *args) {
    if (args == NULL)
        return;
    fprintf(out, " ");
    ppLamHashSymbol(out, args->name);
    _ppLamTypeSigArgs(out, args->next);
}

static void _ppLamTypeSig(FILE *out, LamTypeSig *type) {
    if (type->args == NULL) {
        ppLamHashSymbol(out, type->name);
        return;
    }
    fprintf(out, "(");
    ppLamHashSymbol(out, type->name);
    _ppLamTypeSigArgs(out, type->args);
    fprintf(out, ")");
}

static void _ppLamTypeConstructorArgs(FILE *out, LamTypeConstructorArgs *args);

static void _ppLamTypeFunction(FILE *out, LamTypeFunction *function) {
    fprintf(out, "(");
    ppLamHashSymbol(out, function->name);
    _ppLamTypeConstructorArgs(out, function->args);
    fprintf(out, ")");
}

static void _ppLamTypeTuple(FILE *out, LamTypeConstructorArgs *args) {
    fprintf(out, "#(");
    _ppLamTypeConstructorArgs(out, args);
    fprintf(out, ")");
}

static void _ppLamTypeConstructorType(FILE *out, LamTypeConstructorType *type) {
    switch (type->type) {
    case LAMTYPECONSTRUCTORTYPE_TYPE_INTEGER:
        fprintf(out, "int");
        break;
    case LAMTYPECONSTRUCTORTYPE_TYPE_CHARACTER:
        fprintf(out, "char");
        break;
    case LAMTYPECONSTRUCTORTYPE_TYPE_VAR:
        ppLamHashSymbol(out, getLamTypeConstructorType_Var(type));
        break;
    case LAMTYPECONSTRUCTORTYPE_TYPE_FUNCTION:
        _ppLamTypeFunction(out, getLamTypeConstructorType_Function(type));
        break;
    case LAMTYPECONSTRUCTORTYPE_TYPE_TUPLE:
        _ppLamTypeTuple(out, getLamTypeConstructorType_Tuple(type));
        break;
    default:
        cant_happen("unrecognised type %s in _ppLamTypeConstructorType",
                    lamTypeConstructorTypeTypeName(type->type));
    }
}

static void _ppLamTypeConstructorArgs(FILE *out, LamTypeConstructorArgs *args) {
    if (args == NULL)
        return;
    fprintf(out, " ");
    _ppLamTypeConstructorType(out, args->arg);
    _ppLamTypeConstructorArgs(out, args->next);
}

static void _ppLamTypeConstructor(FILE *out, LamTypeConstructor *constructor) {
    // deliberately don't print the type
    if (constructor->args == NULL) {
        ppLamHashSymbol(out, constructor->name);
        return;
    }
    fprintf(out, "(");
    ppLamHashSymbol(out, constructor->name);
    _ppLamTypeConstructorArgs(out, constructor->args);
    fprintf(out, ")");
}

static void _ppLamTypeConstructorList(FILE *out, LamTypeConstructorList *list) {
    if (list == NULL)
        return;
    fprintf(out, " ");
    _ppLamTypeConstructor(out, list->constructor);
    _ppLamTypeConstructorList(out, list->next);
}

void ppLamTypeDef(FILE *out, LamTypeDef *typeDef) {
    fprintf(out, "(");
    _ppLamTypeSig(out, typeDef->type);
    _ppLamTypeConstructorList(out, typeDef->constructors);
    fprintf(out, ")");
}

static void _ppLamTypeDefList(FILE *out, LamTypeDefList *list) {
    if (list == NULL)
        return;
    ppLamTypeDef(out, list->typeDef);
    if (list->next) {
        fprintf(out, " ");
        _ppLamTypeDefList(out, list->next);
    }
}

void ppLamTypeDefList(FILE *out, LamTypeDefList *typeDefList) {
    fprintf(out, "(");
    _ppLamTypeDefList(out, typeDefList);
    fprintf(out, ")");
}

static void _ppLamIntList(FILE *out, LamIntList *list) {
    if (list == NULL)
        return;
    fprintf(out, "%d:%s", list->item, list->name->name);
    if (list->next != NULL) {
        fprintf(out, " ");
        _ppLamIntList(out, list->next);
    }
}

void ppLamIntList(FILE *out, LamIntList *list) {
    fprintf(out, "(");
    _ppLamIntList(out, list);
    fprintf(out, ")");
}

void ppLamConstruct(FILE *out, LamConstruct *construct) {
    fprintf(out, "(");
    ppLamHashSymbol(out, construct->name);
    _ppLamArgs(out, construct->args);
    fprintf(out, ")");
}

void ppLamTag(FILE *out, LamExp *tag) {
    fprintf(out, "(tag ");
    ppLamExp(out, tag);
    fprintf(out, ")");
}

void ppLamConstant(FILE *out, LamConstant *constant) {
    ppLamHashSymbol(out, constant->name);
}

void ppLamDeconstruct(FILE *out, LamDeconstruct *deconstruct) {
    fprintf(out, "(deconstruct ");
    ppLamHashSymbol(out, deconstruct->name);
    fprintf(out, "[%d] ", deconstruct->vec);
    ppLamExp(out, deconstruct->exp);
    fprintf(out, ")");
}

static inline void pad(FILE *out, int depth) { fprintf(out, "%*s", depth, ""); }

static void _ppLamContext(FILE *out, LamContext *env, int depth) {
    if (env == NULL) {
        pad(out, depth);
        fprintf(out, "<NULL> env\n");
        return;
    }
    pad(out, depth);
    fprintf(out, "{\n");
    HashSymbol *name;
    Index i = 0;
    LamTypeConstructorInfo *value;
    while ((name = iterateLamInfoTable(env->frame, &i, &value)) != NULL) {
        pad(out, depth);
        fprintf(out, " %s => typeConstructorInfo\n", name->name);
    }
    _ppLamContext(out, env->parent, depth + 1);
    pad(out, depth);
    fprintf(out, "}\n");
}

void ppLamContext(FILE *out, LamContext *env) { _ppLamContext(out, env, 0); }

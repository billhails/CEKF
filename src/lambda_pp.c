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
            ppLamLam(exp->val.lam);
            break;
        case LAMEXP_TYPE_VAR:
            ppHashSymbol(exp->val.var);
            break;
        case LAMEXP_TYPE_BIGINTEGER:
            fprintMaybeBigInt(errout, exp->val.biginteger);
            break;
        case LAMEXP_TYPE_STDINT:
            eprintf("%d", exp->val.stdint);
            break;
        case LAMEXP_TYPE_PRIM:
            ppLamPrimApp(exp->val.prim);
            break;
        case LAMEXP_TYPE_SEQUENCE:
            ppLamSequence(exp->val.sequence);
            break;
        case LAMEXP_TYPE_MAKEVEC:
            ppLamMakeVec(exp->val.makeVec);
            break;
        case LAMEXP_TYPE_DECONSTRUCT:
            ppLamDeconstruct(exp->val.deconstruct);
            break;
        case LAMEXP_TYPE_CONSTRUCT:
            ppLamConstruct(exp->val.construct);
            break;
        case LAMEXP_TYPE_TAG:
            ppLamTag(exp->val.tag);
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
            ppLamCallCC(exp->val.callCC);       // LamExp
            break;
        case LAMEXP_TYPE_PRINT:
            ppLamPrint(exp->val.print);
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
            if (exp->val.character == L'\n')
                eprintf("\"\\n\"");
            else if (exp->val.character == L'\t')
                eprintf("\"\\t\"");
            else if (exp->val.character == L'\"')
                eprintf("\"\\\"\"");
            else if (exp->val.character == L'\\')
                eprintf("\"\\\\\"");
            else
                eprintf("\"%lc\"", exp->val.character);
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
        case LAMEXP_TYPE_AMB:
            ppLamAmb(exp->val.amb);
            break;
        case LAMEXP_TYPE_TUPLEINDEX:
            ppLamTupleIndex(exp->val.tupleIndex);
            break;
        case LAMEXP_TYPE_MAKETUPLE:
            ppLamMakeTuple(exp->val.makeTuple);
            break;
        case LAMEXP_TYPE_NAMESPACES:
            ppLamNamespaces(exp->val.namespaces);
            break;
        case LAMEXP_TYPE_ENV:
            eprintf("env");
            break;
        case LAMEXP_TYPE_CONSTRUCTOR:
            eprintf("constructor:%s", exp->val.constructor->name->name);
            break;
        case LAMEXP_TYPE_LOOKUP:
            ppLamLookup(exp->val.lookup);
            break;
        default:
            cant_happen("unrecognized type %s", lamExpTypeName(exp->type));
    }
}

void ppLamLookup(LamLookup *lookup) {
    eprintf("(lookup %s:%d ", lookup->nsSymbol == NULL ? "" : lookup->nsSymbol->name, lookup->nsid);
    ppLamExp(lookup->exp);
    eprintf(")");
}

void ppLamNamespaces(LamNamespaceArray *arr) {
    eprintf("(namespaces");
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
            _ppLamIntCondCases(cases->val.integers);
            break;
        case LAMCONDCASES_TYPE_CHARACTERS:
            _ppLamCharCondCases(cases->val.characters);
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

static void ppLookupSymbol(LamLookupSymbol *ls) {
    eprintf("(lookup %s:%d %s)", ls->nsSymbol->name, ls->nsid, ls->symbol->name);
}

static void ppLookupOrSymbol(LamLookupOrSymbol *los) {
    switch (los->type) {
        case LAMLOOKUPORSYMBOL_TYPE_SYMBOL:
            ppHashSymbol(los->val.symbol);
            break;
        case LAMLOOKUPORSYMBOL_TYPE_LOOKUP:
            ppLookupSymbol(los->val.lookup);
            break;
        default:
            cant_happen("unrecognised %s", lamLookupOrSymbolTypeName(los->type));
    }
}

static void _ppLamTypeConstructorArgs(LamTypeConstructorArgs *args);

static void _ppLamTypeFunction(LamTypeFunction *function) {
    eprintf("(");
    ppLookupOrSymbol(function->name);
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
            ppHashSymbol(type->val.var);
            break;
        case LAMTYPECONSTRUCTORTYPE_TYPE_FUNCTION:
            _ppLamTypeFunction(type->val.function);
            break;
        case LAMTYPECONSTRUCTORTYPE_TYPE_TUPLE:
            _ppLamTypeTuple(type->val.tuple);
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

static void _ppLamContext(LamContext *env, int depth, bool done_namespaces) {
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
            if (done_namespaces) {
                eprintf(" %s => %s\n", name->name, lamInfoTypeName(value->type));
            } else {
                eprintf(" %s => %s [\n", name->name, lamInfoTypeName(value->type));
                _ppLamContext(value->val.namespaceInfo, depth + 1, true);
                pad(depth);
                eprintf(" ]\n");
            }
        } else if (value->type == LAMINFO_TYPE_NSID) {
            eprintf(" %s => %s [%d]\n", name->name, lamInfoTypeName(value->type), value->val.nsid);
        } else {
            eprintf(" %s => %s\n", name->name, lamInfoTypeName(value->type));
        }
    }
    _ppLamContext(env->parent, depth + 1, done_namespaces);
    pad(depth);
    eprintf("}\n");
}

void ppLamContext(LamContext *env) {
    _ppLamContext(env, 0, false);
}

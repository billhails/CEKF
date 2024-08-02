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

#include "inline.h"
#include "common.h"
#include "types.h"

static LamExp *inlineExp(LamExp *x);
static LamTypeDefs *inlineTypeDefs(LamTypeDefs *x);
static LamNamespaceArray *inlineNamespaces(LamNamespaceArray *x);
static LamLam *inlineLam(LamLam *x);
static LamPrimApp *inlinePrim(LamPrimApp *x);
static LamUnaryApp *inlineUnary(LamUnaryApp *x);
static LamSequence *inlineSequence(LamSequence *x);
static LamList *inlineList(LamList *x);
static LamExp *inlineApply(LamApply *x);
static LamExp *inlineConstant(LamTypeConstructorInfo *x);
static LamIff *inlineIff(LamIff *x);
static LamLetRec *inlineLetRec(LamLetRec *x);
static LamLetRecBindings *inlineLetRecBindings(LamLetRecBindings *x);
static LamLet *inlineLet(LamLet *x);
static LamAmb *inlineAmb(LamAmb *x);
static LamPrint *inlinePrint(LamPrint *x);
static LamLookup *inlineLookup(LamLookup *x);
static LamTupleIndex *inlineTupleIndex(LamTupleIndex *x);
static LamMatch *inlineMatch(LamMatch *x);
static LamCond *inlineCond(LamCond *x);
static LamCondCases *inlineCondCases(LamCondCases *x);
static LamCharCondCases *inlineCharCondCases(LamCharCondCases *x);
static LamIntCondCases *inlineIntCondCases(LamIntCondCases *x);
static LamExp *makeConstruct(ParserInfo, HashSymbol *name, int tag, LamList *args);
static LamExp *makeConstant(ParserInfo, HashSymbol *name, int tag);
static LamTypeConstructorInfo *resolveTypeConstructor(LamExp *x);

static LamNamespaceArray *inlineNamespaces(LamNamespaceArray *x) {
    for (Index i = 0; i < x->size; ++i) {
        x->entries[i] = inlineExp(x->entries[i]);
    }
    return x;
}

static LamMatchList *inlineMatchList(LamMatchList *x) {
    if (x != NULL) {
        x->next = inlineMatchList(x->next);
        x->body = inlineExp(x->body);
    }
    return x;
}

static LamMatch *inlineMatch(LamMatch *x) {
    x->index = inlineExp(x->index);
    x->cases = inlineMatchList(x->cases);
    return x;
}

static LamTypeDefs *inlineTypeDefs(LamTypeDefs *x) {
    x->body = inlineExp(x->body);
    return x;
}

static LamLam *inlineLam(LamLam *x) {
    x->exp = inlineExp(x->exp);
    return x;
}

static LamPrimApp *inlinePrim(LamPrimApp *x) {
    x->exp1 = inlineExp(x->exp1);
    x->exp2 = inlineExp(x->exp2);
    return x;
}

static LamUnaryApp *inlineUnary(LamUnaryApp *x) {
    x->exp = inlineExp(x->exp);
    return x;
}

static LamSequence *inlineSequence(LamSequence *x) {
    if (x != NULL) {
        x->next = inlineSequence(x->next);
        x->exp = inlineExp(x->exp);
    }
    return x;
}

static LamList *inlineList(LamList *x) {
    if (x != NULL) {
        x->next = inlineList(x->next);
        x->exp = inlineExp(x->exp);
    }
    return x;
}

static LamExp *makeConstruct(ParserInfo I, HashSymbol *name, int tag, LamList *args) {
    LamConstruct *construct = newLamConstruct(I, name, tag, args);
    int save = PROTECT(construct);
    LamExp *res =
        newLamExp_Construct(I, construct);
    UNPROTECT(save);
    return res;
}

static LamExp *makeConstant(ParserInfo I, HashSymbol *name, int tag) {
    LamConstant *constant = newLamConstant(I, name, tag);
    int save = PROTECT(constant);
    LamExp *res =
        newLamExp_Constant(I, constant);
    UNPROTECT(save);
    return res;
}

static LamExp *inlineConstant(LamTypeConstructorInfo *x) {
    if (x-> arity != 0) {
        cant_happen("missing arguments to constructor %s", x->name->name);
    }
    if (x->needsVec) {
        return makeConstruct(CPI(x), x->name, x->index, NULL);
    } else {
        return makeConstant(CPI(x), x->name, x->index);
    }
}

static LamTypeConstructorInfo *resolveTypeConstructor(LamExp *x) {
    switch (x->type) {
        case LAMEXP_TYPE_CONSTRUCTOR:
            return x->val.constructor;
        case LAMEXP_TYPE_LOOKUP:
            return resolveTypeConstructor(x->val.lookup->exp);
        default:
            return NULL;
    }
}

static LamExp *inlineApply(LamApply *x) {
    x->args = inlineList(x->args);
    LamTypeConstructorInfo *info = resolveTypeConstructor(x->function);
    if (info == NULL) {
        x->function = inlineExp(x->function);
    } else {
        int nargs = countLamList(x->args);
        if (info->needsVec) {
            if (nargs == info->arity) {
                return makeConstruct(CPI(x), info->name, info->index, x->args);
            } else {
                cant_happen("wrong number of arguments to constructor %s, got %d, expected %d",
                            info->name->name, nargs, info->arity);
            }
        } else {
            if (nargs > 0) {
                cant_happen("arguments to constant constructor %s",
                            info->name->name);
            }
            return makeConstant(CPI(x), info->name, info->index);
        }
    }
    return newLamExp_Apply(CPI(x), x);
}

static LamIff *inlineIff(LamIff *x) {
    x->condition = inlineExp(x->condition);
    x->consequent = inlineExp(x->consequent);
    x->alternative = inlineExp(x->alternative);
    return x;
}

static LamLetRec *inlineLetRec(LamLetRec *x) {
    x->bindings = inlineLetRecBindings(x->bindings);
    x->body = inlineExp(x->body);
    return x;
}

static LamLetRecBindings *inlineLetRecBindings(LamLetRecBindings *x) {
    if (x != NULL) {
        x->next = inlineLetRecBindings(x->next);
        x->val = inlineExp(x->val);
    }
    return x;
}

static LamLet *inlineLet(LamLet *x) {
    x->value = inlineExp(x->value);
    x->body = inlineExp(x->body);
    return x;
}

static LamAmb *inlineAmb(LamAmb *x) {
    x->left = inlineExp(x->left);
    x->right = inlineExp(x->right);
    return x;
}

static LamPrint *inlinePrint(LamPrint *x) {
    x->exp = inlineExp(x->exp);
    return x;
}

static LamLookup *inlineLookup(LamLookup *x) {
    x->exp = inlineExp(x->exp);
    return x;
}

static LamTupleIndex *inlineTupleIndex(LamTupleIndex *x) {
    x->exp = inlineExp(x->exp);
    return x;
}

static LamCond *inlineCond(LamCond *x) {
    x->value = inlineExp(x->value);
    x->cases = inlineCondCases(x->cases);
    return x;
}

static LamCondCases *inlineCondCases(LamCondCases *x) {
    if (x != NULL) {
        switch (x->type) {
            case LAMCONDCASES_TYPE_INTEGERS:
                x->val.integers = inlineIntCondCases(x->val.integers);
                break;
            case LAMCONDCASES_TYPE_CHARACTERS:
                x->val.characters = inlineCharCondCases(x->val.characters);
                break;
            default:
                cant_happen("unrecognized %s", lamCondCasesTypeName(x->type));
        }
    }
    return x;
}

static LamCharCondCases *inlineCharCondCases(LamCharCondCases *x) {
    if (x != NULL) {
        x->next = inlineCharCondCases(x->next);
        x->body = inlineExp(x->body);
    }
    return x;
}

static LamIntCondCases *inlineIntCondCases(LamIntCondCases *x) {
    if (x != NULL) {
        x->next = inlineIntCondCases(x->next);
        x->body = inlineExp(x->body);
    }
    return x;
}

static LamExp *inlineExp(LamExp *x) {
    switch (x->type) {
        case LAMEXP_TYPE_VAR:
        case LAMEXP_TYPE_STDINT:
        case LAMEXP_TYPE_BIGINTEGER:
        case LAMEXP_TYPE_CONSTANT:
        case LAMEXP_TYPE_COND_DEFAULT:
        case LAMEXP_TYPE_ENV:
        case LAMEXP_TYPE_ERROR:
        case LAMEXP_TYPE_BACK:
        case LAMEXP_TYPE_CHARACTER:
            break;
        case LAMEXP_TYPE_TYPEDEFS:
            x->val.typedefs = inlineTypeDefs(x->val.typedefs);
            break;
        case LAMEXP_TYPE_NAMESPACES:
            x->val.namespaces = inlineNamespaces(x->val.namespaces);
            break;
        case LAMEXP_TYPE_LAM:
            x->val.lam = inlineLam(x->val.lam);
            break;
        case LAMEXP_TYPE_PRIM:
            x->val.prim = inlinePrim(x->val.prim);
            break;
        case LAMEXP_TYPE_UNARY:
            x->val.unary = inlineUnary(x->val.unary);
            break;
        case LAMEXP_TYPE_LIST:
            x->val.list = inlineSequence(x->val.list);
            break;
        case LAMEXP_TYPE_MAKE_TUPLE:
            x->val.make_tuple = inlineList(x->val.make_tuple);
            break;
        case LAMEXP_TYPE_APPLY:
            x = inlineApply(x->val.apply);
            break;
        case LAMEXP_TYPE_IFF:
            x->val.iff = inlineIff(x->val.iff);
            break;
        case LAMEXP_TYPE_CALLCC:
            x->val.callcc = inlineExp(x->val.callcc);
            break;
        case LAMEXP_TYPE_LETREC:
            x->val.letrec = inlineLetRec(x->val.letrec);
            break;
        case LAMEXP_TYPE_LET:
            x->val.let = inlineLet(x->val.let);
            break;
        case LAMEXP_TYPE_AMB:
            x->val.amb = inlineAmb(x->val.amb);
            break;
        case LAMEXP_TYPE_PRINT:
            x->val.print = inlinePrint(x->val.print);
            break;
        case LAMEXP_TYPE_LOOKUP:
            x->val.lookup = inlineLookup(x->val.lookup);
            break;
        case LAMEXP_TYPE_TUPLE_INDEX:
            x->val.tuple_index = inlineTupleIndex(x->val.tuple_index);
            break;
        case LAMEXP_TYPE_MATCH:
            x->val.match = inlineMatch(x->val.match);
            break;
        case LAMEXP_TYPE_TAG:
            x->val.tag = inlineExp(x->val.tag);
            break;
        case LAMEXP_TYPE_DECONSTRUCT:
            x->val.deconstruct->exp = inlineExp(x->val.deconstruct->exp);
            break;
        case LAMEXP_TYPE_CONSTRUCTOR:
            x = inlineConstant(x->val.constructor);
            break;
        case LAMEXP_TYPE_CONSTRUCT:
            x->val.construct->args = inlineList(x->val.construct->args);
            break;
        case LAMEXP_TYPE_COND:
            x->val.cond = inlineCond(x->val.cond);
            break;
        case LAMEXP_TYPE_TUPLE:
        case LAMEXP_TYPE_MAKEVEC:
            cant_happen("encountered %s", lamExpTypeName(x->type));
        default:
            cant_happen("unrecognised type %s", lamExpTypeName(x->type));
    }
    return x;
}

LamExp *inlineLamExp(LamExp *x) {
    return inlineExp(x);
}

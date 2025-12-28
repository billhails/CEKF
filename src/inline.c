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
static LamNameSpaceArray *inlineNameSpaces(LamNameSpaceArray *x);
static LamLam *inlineLam(LamLam *x);
static LamPrimApp *inlinePrim(LamPrimApp *x);
static LamSequence *inlineSequence(LamSequence *x);
static LamArgs *inlineArgs(LamArgs *x);
static LamExp *inlineApply(LamApply *x);
static LamExp *inlineConstant(LamTypeConstructorInfo *x);
static LamIff *inlineIff(LamIff *x);
static LamLetRec *inlineLetRec(LamLetRec *x);
static LamBindings *inlineBindings(LamBindings *x);
static LamLet *inlineLet(LamLet *x);
static LamLetStar *inlineLetStar(LamLetStar *x);
static LamAmb *inlineAmb(LamAmb *x);
static LamPrint *inlinePrint(LamPrint *x);
static LamLookUp *inlineLookUp(LamLookUp *x);
static LamTupleIndex *inlineTupleIndex(LamTupleIndex *x);
static LamMatch *inlineMatch(LamMatch *x);
static LamCond *inlineCond(LamCond *x);
static LamCondCases *inlineCondCases(LamCondCases *x);
static LamCharCondCases *inlineCharCondCases(LamCharCondCases *x);
static LamIntCondCases *inlineIntCondCases(LamIntCondCases *x);
static LamTypeConstructorInfo *resolveTypeConstructor(LamExp *x);

static LamNameSpaceArray *inlineNameSpaces(LamNameSpaceArray *x) {
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

static LamSequence *inlineSequence(LamSequence *x) {
    if (x != NULL) {
        x->next = inlineSequence(x->next);
        x->exp = inlineExp(x->exp);
    }
    return x;
}

static LamArgs *inlineArgs(LamArgs *x) {
    if (x != NULL) {
        x->next = inlineArgs(x->next);
        x->exp = inlineExp(x->exp);
    }
    return x;
}

static LamExp *inlineConstant(LamTypeConstructorInfo *x) {
    if (x-> arity != 0) {
        cant_happen("missing arguments to constructor %s", x->name->name);
    }
    if (x->needsVec) {
        return makeLamExp_Construct(CPI(x), x->name, x->index, NULL);
    } else {
        return makeLamExp_Constant(CPI(x), x->name, x->index);
    }
}

static LamTypeConstructorInfo *resolveTypeConstructor(LamExp *x) {
    switch (x->type) {
        case LAMEXP_TYPE_CONSTRUCTOR:
            return getLamExp_Constructor(x);
        case LAMEXP_TYPE_LOOKUP:
            return resolveTypeConstructor(getLamExp_LookUp(x)->exp);
        default:
            return NULL;
    }
}

static LamExp *inlineApply(LamApply *x) {
    x->args = inlineArgs(x->args);
    LamTypeConstructorInfo *info = resolveTypeConstructor(x->function);
    if (info == NULL) {
        x->function = inlineExp(x->function);
    } else {
        int nArgs = countLamArgs(x->args);
        if (info->needsVec) {
            if (nArgs == info->arity) {
                return makeLamExp_Construct(CPI(x), info->name, info->index, x->args);
            } else {
                cant_happen("wrong number of arguments to constructor %s, got %d, expected %d",
                            info->name->name, nArgs, info->arity);
            }
        } else {
            if (nArgs > 0) {
                cant_happen("arguments to constant constructor %s",
                            info->name->name);
            }
            return makeLamExp_Constant(CPI(x), info->name, info->index);
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
    x->bindings = inlineBindings(x->bindings);
    x->body = inlineExp(x->body);
    return x;
}

static LamBindings *inlineBindings(LamBindings *x) {
    if (x != NULL) {
        x->next = inlineBindings(x->next);
        x->val = inlineExp(x->val);
    }
    return x;
}

static LamLet *inlineLet(LamLet *x) {
    x->bindings = inlineBindings(x->bindings);
    x->body = inlineExp(x->body);
    return x;
}

static LamLetStar *inlineLetStar(LamLetStar *x) {
    x->bindings = inlineBindings(x->bindings);
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

static LamLookUp *inlineLookUp(LamLookUp *x) {
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
                setLamCondCases_Integers(x, inlineIntCondCases(getLamCondCases_Integers(x)));
                break;
            case LAMCONDCASES_TYPE_CHARACTERS:
                setLamCondCases_Characters(x, inlineCharCondCases(getLamCondCases_Characters(x)));
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
        case LAMEXP_TYPE_ENV:
        case LAMEXP_TYPE_ERROR:
        case LAMEXP_TYPE_BACK:
        case LAMEXP_TYPE_CHARACTER:
            break;
        case LAMEXP_TYPE_TYPEDEFS:
            setLamExp_TypeDefs(x, inlineTypeDefs(getLamExp_TypeDefs(x)));
            break;
        case LAMEXP_TYPE_NAMESPACES:
            setLamExp_NameSpaces(x, inlineNameSpaces(getLamExp_NameSpaces(x)));
            break;
        case LAMEXP_TYPE_LAM:
            setLamExp_Lam(x, inlineLam(getLamExp_Lam(x)));
            break;
        case LAMEXP_TYPE_PRIM:
            setLamExp_Prim(x, inlinePrim(getLamExp_Prim(x)));
            break;
        case LAMEXP_TYPE_SEQUENCE:
            setLamExp_Sequence(x, inlineSequence(getLamExp_Sequence(x)));
            break;
        case LAMEXP_TYPE_MAKETUPLE:
            setLamExp_MakeTuple(x, inlineArgs(getLamExp_MakeTuple(x)));
            break;
        case LAMEXP_TYPE_APPLY:
            x = inlineApply(getLamExp_Apply(x));
            break;
        case LAMEXP_TYPE_IFF:
            setLamExp_Iff(x, inlineIff(getLamExp_Iff(x)));
            break;
        case LAMEXP_TYPE_CALLCC:
            setLamExp_CallCC(x, inlineExp(getLamExp_CallCC(x)));
            break;
        case LAMEXP_TYPE_LETREC:
            setLamExp_LetRec(x, inlineLetRec(getLamExp_LetRec(x)));
            break;
        case LAMEXP_TYPE_LET:
            setLamExp_Let(x, inlineLet(getLamExp_Let(x)));
            break;
        case LAMEXP_TYPE_LETSTAR:
            setLamExp_LetStar(x, inlineLetStar(getLamExp_LetStar(x)));
            break;
        case LAMEXP_TYPE_AMB:
            setLamExp_Amb(x, inlineAmb(getLamExp_Amb(x)));
            break;
        case LAMEXP_TYPE_PRINT:
            setLamExp_Print(x, inlinePrint(getLamExp_Print(x)));
            break;
        case LAMEXP_TYPE_LOOKUP:
            setLamExp_LookUp(x, inlineLookUp(getLamExp_LookUp(x)));
            break;
        case LAMEXP_TYPE_TUPLEINDEX:
            setLamExp_TupleIndex(x, inlineTupleIndex(getLamExp_TupleIndex(x)));
            break;
        case LAMEXP_TYPE_MATCH:
            setLamExp_Match(x, inlineMatch(getLamExp_Match(x)));
            break;
        case LAMEXP_TYPE_TAG:
            setLamExp_Tag(x, inlineExp(getLamExp_Tag(x)));
            break;
        case LAMEXP_TYPE_DECONSTRUCT:
            getLamExp_Deconstruct(x)->exp = inlineExp(getLamExp_Deconstruct(x)->exp);
            break;
        case LAMEXP_TYPE_CONSTRUCTOR:
            x = inlineConstant(getLamExp_Constructor(x));
            break;
        case LAMEXP_TYPE_CONSTRUCT:
            getLamExp_Construct(x)->args = inlineArgs(getLamExp_Construct(x)->args);
            break;
        case LAMEXP_TYPE_COND:
            setLamExp_Cond(x, inlineCond(getLamExp_Cond(x)));
            break;
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

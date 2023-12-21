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
 * Type inference structures used by Algorithm W.
 *
 * generated from src/tin.yaml by makeAST.py
 */

#include "tin.h"
#include <stdio.h>
#include <strings.h>
#include "common.h"

struct TinFunctionApplication * newTinFunctionApplication(HashSymbol * name, int nargs, struct TinMonoTypeList * args) {
    struct TinFunctionApplication * x = NEW(TinFunctionApplication, OBJTYPE_TINFUNCTIONAPPLICATION);
#ifdef DEBUG_ALLOC
    eprintf("new TinFunctionApplication %p\n", x);
#endif
    x->name = name;
    x->nargs = nargs;
    x->args = args;
    return x;
}

struct TinMonoTypeList * newTinMonoTypeList(struct TinMonoType * monoType, struct TinMonoTypeList * next) {
    struct TinMonoTypeList * x = NEW(TinMonoTypeList, OBJTYPE_TINMONOTYPELIST);
#ifdef DEBUG_ALLOC
    eprintf("new TinMonoTypeList %p\n", x);
#endif
    x->monoType = monoType;
    x->next = next;
    return x;
}

struct TinTypeQuantifier * newTinTypeQuantifier(HashSymbol * var, struct TinPolyType * quantifiedType) {
    struct TinTypeQuantifier * x = NEW(TinTypeQuantifier, OBJTYPE_TINTYPEQUANTIFIER);
#ifdef DEBUG_ALLOC
    eprintf("new TinTypeQuantifier %p\n", x);
#endif
    x->var = var;
    x->quantifiedType = quantifiedType;
    return x;
}

struct TinContext * newTinContext(HashTable * varFrame, HashTable * tcFrame, struct TinContext * next) {
    struct TinContext * x = NEW(TinContext, OBJTYPE_TINCONTEXT);
#ifdef DEBUG_ALLOC
    eprintf("new TinContext %p\n", x);
#endif
    x->varFrame = varFrame;
    x->tcFrame = tcFrame;
    x->next = next;
    return x;
}

struct TinSubstitution * newTinSubstitution(HashTable * map) {
    struct TinSubstitution * x = NEW(TinSubstitution, OBJTYPE_TINSUBSTITUTION);
#ifdef DEBUG_ALLOC
    eprintf("new TinSubstitution %p\n", x);
#endif
    x->map = map;
    return x;
}

struct TinArgsResult * newTinArgsResult(struct TinContext * context, struct TinMonoTypeList * vec) {
    struct TinArgsResult * x = NEW(TinArgsResult, OBJTYPE_TINARGSRESULT);
#ifdef DEBUG_ALLOC
    eprintf("new TinArgsResult %p\n", x);
#endif
    x->context = context;
    x->vec = vec;
    return x;
}

struct TinVarResult * newTinVarResult(struct TinSubstitution * substitution, struct TinContext * context, struct TinMonoType * monoType, HashTable * set) {
    struct TinVarResult * x = NEW(TinVarResult, OBJTYPE_TINVARRESULT);
#ifdef DEBUG_ALLOC
    eprintf("new TinVarResult %p\n", x);
#endif
    x->substitution = substitution;
    x->context = context;
    x->monoType = monoType;
    x->set = set;
    return x;
}

struct TinVarsResult * newTinVarsResult(struct TinContext * context, HashTable * set) {
    struct TinVarsResult * x = NEW(TinVarsResult, OBJTYPE_TINVARSRESULT);
#ifdef DEBUG_ALLOC
    eprintf("new TinVarsResult %p\n", x);
#endif
    x->context = context;
    x->set = set;
    return x;
}

struct TinMonoType * newTinMonoType(enum TinMonoTypeType  type, union TinMonoTypeVal  val) {
    struct TinMonoType * x = NEW(TinMonoType, OBJTYPE_TINMONOTYPE);
#ifdef DEBUG_ALLOC
    eprintf("new TinMonoType %p\n", x);
#endif
    x->type = type;
    x->val = val;
    return x;
}

struct TinPolyType * newTinPolyType(enum TinPolyTypeType  type, union TinPolyTypeVal  val) {
    struct TinPolyType * x = NEW(TinPolyType, OBJTYPE_TINPOLYTYPE);
#ifdef DEBUG_ALLOC
    eprintf("new TinPolyType %p\n", x);
#endif
    x->type = type;
    x->val = val;
    return x;
}



/************************************/

void markTinFunctionApplication(struct TinFunctionApplication * x) {
    if (x == NULL) return;
    if (MARKED(x)) return;
    MARK(x);
    markHashSymbol(x->name);
    markTinMonoTypeList(x->args);
}

void markTinMonoTypeList(struct TinMonoTypeList * x) {
    if (x == NULL) return;
    if (MARKED(x)) return;
    MARK(x);
    markTinMonoType(x->monoType);
    markTinMonoTypeList(x->next);
}

void markTinTypeQuantifier(struct TinTypeQuantifier * x) {
    if (x == NULL) return;
    if (MARKED(x)) return;
    MARK(x);
    markHashSymbol(x->var);
    markTinPolyType(x->quantifiedType);
}

void markTinContext(struct TinContext * x) {
    if (x == NULL) return;
    if (MARKED(x)) return;
    MARK(x);
    markHashTable(x->varFrame);
    markHashTable(x->tcFrame);
    markTinContext(x->next);
}

void markTinSubstitution(struct TinSubstitution * x) {
    if (x == NULL) return;
    if (MARKED(x)) return;
    MARK(x);
    markHashTable(x->map);
}

void markTinArgsResult(struct TinArgsResult * x) {
    if (x == NULL) return;
    if (MARKED(x)) return;
    MARK(x);
    markTinContext(x->context);
    markTinMonoTypeList(x->vec);
}

void markTinVarResult(struct TinVarResult * x) {
    if (x == NULL) return;
    if (MARKED(x)) return;
    MARK(x);
    markTinSubstitution(x->substitution);
    markTinContext(x->context);
    markTinMonoType(x->monoType);
    markHashTable(x->set);
}

void markTinVarsResult(struct TinVarsResult * x) {
    if (x == NULL) return;
    if (MARKED(x)) return;
    MARK(x);
    markTinContext(x->context);
    markHashTable(x->set);
}

void markTinMonoType(struct TinMonoType * x) {
    if (x == NULL) return;
    if (MARKED(x)) return;
    MARK(x);
    switch(x->type) {
        case TINMONOTYPE_TYPE_VAR:
            markHashSymbol(x->val.var);
            break;
        case TINMONOTYPE_TYPE_FUN:
            markTinFunctionApplication(x->val.fun);
            break;
        default:
            cant_happen("unrecognised type %d in markTinMonoType", x->type);
    }
}

void markTinPolyType(struct TinPolyType * x) {
    if (x == NULL) return;
    if (MARKED(x)) return;
    MARK(x);
    switch(x->type) {
        case TINPOLYTYPE_TYPE_MONOTYPE:
            markTinMonoType(x->val.monoType);
            break;
        case TINPOLYTYPE_TYPE_QUANTIFIER:
            markTinTypeQuantifier(x->val.quantifier);
            break;
        default:
            cant_happen("unrecognised type %d in markTinPolyType", x->type);
    }
}


void markTinObj(struct Header *h) {
    switch(h->type) {
        case OBJTYPE_TINFUNCTIONAPPLICATION:
            markTinFunctionApplication((TinFunctionApplication *)h);
            break;
        case OBJTYPE_TINMONOTYPELIST:
            markTinMonoTypeList((TinMonoTypeList *)h);
            break;
        case OBJTYPE_TINTYPEQUANTIFIER:
            markTinTypeQuantifier((TinTypeQuantifier *)h);
            break;
        case OBJTYPE_TINCONTEXT:
            markTinContext((TinContext *)h);
            break;
        case OBJTYPE_TINSUBSTITUTION:
            markTinSubstitution((TinSubstitution *)h);
            break;
        case OBJTYPE_TINARGSRESULT:
            markTinArgsResult((TinArgsResult *)h);
            break;
        case OBJTYPE_TINVARRESULT:
            markTinVarResult((TinVarResult *)h);
            break;
        case OBJTYPE_TINVARSRESULT:
            markTinVarsResult((TinVarsResult *)h);
            break;
        case OBJTYPE_TINMONOTYPE:
            markTinMonoType((TinMonoType *)h);
            break;
        case OBJTYPE_TINPOLYTYPE:
            markTinPolyType((TinPolyType *)h);
            break;
        default:
            cant_happen("unrecognised type %d in markTinObj\n", h->type);
    }
}

/************************************/

void freeTinFunctionApplication(struct TinFunctionApplication * x) {
    FREE(x, TinFunctionApplication);
}

void freeTinMonoTypeList(struct TinMonoTypeList * x) {
    FREE(x, TinMonoTypeList);
}

void freeTinTypeQuantifier(struct TinTypeQuantifier * x) {
    FREE(x, TinTypeQuantifier);
}

void freeTinContext(struct TinContext * x) {
    FREE(x, TinContext);
}

void freeTinSubstitution(struct TinSubstitution * x) {
    FREE(x, TinSubstitution);
}

void freeTinArgsResult(struct TinArgsResult * x) {
    FREE(x, TinArgsResult);
}

void freeTinVarResult(struct TinVarResult * x) {
    FREE(x, TinVarResult);
}

void freeTinVarsResult(struct TinVarsResult * x) {
    FREE(x, TinVarsResult);
}

void freeTinMonoType(struct TinMonoType * x) {
    FREE(x, TinMonoType);
}

void freeTinPolyType(struct TinPolyType * x) {
    FREE(x, TinPolyType);
}


void freeTinObj(struct Header *h) {
    switch(h->type) {
        case OBJTYPE_TINFUNCTIONAPPLICATION:
            freeTinFunctionApplication((TinFunctionApplication *)h);
            break;
        case OBJTYPE_TINMONOTYPELIST:
            freeTinMonoTypeList((TinMonoTypeList *)h);
            break;
        case OBJTYPE_TINTYPEQUANTIFIER:
            freeTinTypeQuantifier((TinTypeQuantifier *)h);
            break;
        case OBJTYPE_TINCONTEXT:
            freeTinContext((TinContext *)h);
            break;
        case OBJTYPE_TINSUBSTITUTION:
            freeTinSubstitution((TinSubstitution *)h);
            break;
        case OBJTYPE_TINARGSRESULT:
            freeTinArgsResult((TinArgsResult *)h);
            break;
        case OBJTYPE_TINVARRESULT:
            freeTinVarResult((TinVarResult *)h);
            break;
        case OBJTYPE_TINVARSRESULT:
            freeTinVarsResult((TinVarsResult *)h);
            break;
        case OBJTYPE_TINMONOTYPE:
            freeTinMonoType((TinMonoType *)h);
            break;
        case OBJTYPE_TINPOLYTYPE:
            freeTinPolyType((TinPolyType *)h);
            break;
        default:
            cant_happen("unrecognised type %d in freeTinObj\n", h->type);
    }
}

char *typenameTinObj(int type) {
    switch(type) {
        case OBJTYPE_TINFUNCTIONAPPLICATION:
            return "TinFunctionApplication";
        case OBJTYPE_TINMONOTYPELIST:
            return "TinMonoTypeList";
        case OBJTYPE_TINTYPEQUANTIFIER:
            return "TinTypeQuantifier";
        case OBJTYPE_TINCONTEXT:
            return "TinContext";
        case OBJTYPE_TINSUBSTITUTION:
            return "TinSubstitution";
        case OBJTYPE_TINARGSRESULT:
            return "TinArgsResult";
        case OBJTYPE_TINVARRESULT:
            return "TinVarResult";
        case OBJTYPE_TINVARSRESULT:
            return "TinVarsResult";
        case OBJTYPE_TINMONOTYPE:
            return "TinMonoType";
        case OBJTYPE_TINPOLYTYPE:
            return "TinPolyType";
        default:
            cant_happen("unrecognised type %d in typenameTinObj\n", type);
    }
}


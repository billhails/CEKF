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

#include "tc_helper.h"
#include "symbol.h"
#include "tc_analyze.h"
#include "types.h"
#include "utils.h"
#include <stdlib.h>
#include <string.h>

void ppTcType(TcType *type) {
    if (type == NULL) {
        eprintf("<null type>");
        return;
    }
    switch (type->type) {
    case TCTYPE_TYPE_FUNCTION:
        ppTcFunction(type->val.function);
        break;
    case TCTYPE_TYPE_PAIR:
        ppTcPair(type->val.pair);
        break;
    case TCTYPE_TYPE_THUNK:
        ppTcThunk(type->val.thunk);
        break;
    case TCTYPE_TYPE_VAR:
        ppTcVar(type->val.var);
        break;
    case TCTYPE_TYPE_BIGINTEGER:
        eprintf("number");
        break;
    case TCTYPE_TYPE_SMALLINTEGER:
        eprintf("smallint");
        break;
    case TCTYPE_TYPE_CHARACTER:
        eprintf("char");
        break;
    case TCTYPE_TYPE_UNKNOWN:
        eprintf("unknown:%s", type->val.unknown->name);
        break;
    case TCTYPE_TYPE_TYPESIG:
        ppTcTypeSig(type->val.typeSig);
        break;
    case TCTYPE_TYPE_TUPLE:
        ppTcTuple(type->val.tuple);
        break;
    case TCTYPE_TYPE_ENV:
        eprintf("<env>");
        break;
    case TCTYPE_TYPE_OPAQUE:
        eprintf("opaque:%s", type->val.opaque->name);
        break;
    default:
        eprintf("unrecognized type %s", tcTypeTypeName(type->type));
    }
}

void ppTcFunction(TcFunction *function) {
    if (function->arg->type == TCTYPE_TYPE_FUNCTION) {
        eprintf("(");
        ppTcType(function->arg);
        eprintf(")");
    } else {
        ppTcType(function->arg);
    }
    eprintf(" -> ");
    ppTcType(function->result);
}

void ppTcPair(TcPair *pair) {
    eprintf("#(");
    ppTcType(pair->first);
    eprintf(", ");
    ppTcType(pair->second);
    eprintf(")");
}

void ppTcThunk(TcThunk *thunk) {
    eprintf("#() -> ");
    ppTcType(thunk->type);
}

void ppTcVar(TcVar *var) {
    eprintf("%s", var->name->name);
    if (var->instance != NULL) {
        eprintf(" [");
        ppTcType(var->instance);
        eprintf("]");
    }
}

void ppTcTuple(TcTypeArray *tuple) {
    eprintf("#(");
    for (Index i = 0; i < tuple->size; i++) {
        ppTcType(tuple->entries[i]);
        if (i + 1 < tuple->size) {
            eprintf(", ");
        }
    }
    eprintf(")");
}

static void ppTypeSigArgs(TcTypeSigArgs *args) {
    while (args != NULL) {
        ppTcType(args->type);
        if (args->next)
            eprintf(", ");
        args = args->next;
    }
}

void ppTcTypeSig(TcTypeSig *typeSig) {
    eprintf("%s", typeSig->name->name);
    if (typeSig->args != NULL) {
        eprintf("(");
        ppTypeSigArgs(typeSig->args);
        eprintf(")");
    }
}

// Bespoke implementation.
bool eqTcVar(struct TcVar *a, struct TcVar *b, HashTable *map) {
    if (a == b)
        return true;
    if (a->name == b->name)
        return true;
    HashSymbol *common = NULL;
    if (hashGet(map, a->name, &common)) {
        HashSymbol *other = NULL;
        if (hashGet(map, b->name, &other)) {
            return common == other;
        } else {
            return false;
        }
    } else if (hashGet(map, b->name, &common)) {
        return false;
    } else {
        // symmetric
        common = genSym("tt$");
        hashSet(map, a->name, &common);
        hashSet(map, b->name, &common);
    }
    return true;
}

static inline void pad(int depth) { eprintf("%*s", depth * 2, ""); }

static void _ppTcEnv(TcEnv *env, int depth, bool done_nameSpaces);

static void _ppTcNameSpaces(TcNameSpaceArray *nameSpaces, int depth) {
    if (nameSpaces == NULL)
        return;
    for (Index i = 0; i < nameSpaces->size; i++) {
        pad(depth);
        eprintf("[%u]:\n", i);
        if (nameSpaces->entries[i]->type == TCTYPE_TYPE_ENV) {
            _ppTcEnv(nameSpaces->entries[i]->val.env, depth + 1, true);
        } else {
            eprintf("%s\n", tcTypeTypeName(nameSpaces->entries[i]->type));
        }
    }
}

static void _ppTcEnv(TcEnv *env, int depth, bool done_nameSpaces) {
    if (env == NULL) {
        pad(depth);
        eprintf("<NULL> env\n");
        return;
    }
    pad(depth);
    eprintf("{\n");
    HashSymbol *name;
    Index i = 0;
    TcType *value;
    while ((name = iterateTcTypeTable(env->table, &i, &value)) != NULL) {
        pad(depth);
        if (value->type == TCTYPE_TYPE_NSID) {
            eprintf("  %s => %s [%d]\n", name->name,
                    tcTypeTypeName(value->type), value->val.nsId);
        } else if (value->type == TCTYPE_TYPE_NAMESPACES) {
            if (done_nameSpaces) {
                eprintf("  %s => %s\n", name->name,
                        tcTypeTypeName(value->type));
            } else {
                eprintf("  %s => %s [\n", name->name,
                        tcTypeTypeName(value->type));
                _ppTcNameSpaces(value->val.nameSpaces, depth + 1);
                pad(depth);
                eprintf("  ]\n");
            }
        } else {
            eprintf("  %s => %s\n", name->name, tcTypeTypeName(value->type));
        }
    }
    _ppTcEnv(env->next, depth + 1, done_nameSpaces);
    pad(depth);
    eprintf("}\n");
}

void ppTcEnv(TcEnv *env) { _ppTcEnv(env, 0, false); }

// Helper function to append to a dynamically growing buffer
void appendStringToSCharArray(SCharArray *buffer, char *str) {
    int len = strlen(str);
    addSCharArray(buffer, len);
    for (char *p = str; *p; p++) {
        pushSCharArray(buffer, *p);
    }
}

// Forward declarations for string conversion
static void tcTypeToStringHelper(TcType *type, SCharArray *buffer);
static void tcFunctionToString(TcFunction *function, SCharArray *buffer);
static void tcPairToString(TcPair *pair, SCharArray *buffer);
static void tcThunkToString(TcThunk *thunk, SCharArray *buffer);
static void tcVarToString(TcVar *var, SCharArray *buffer);
static void tcTupleToString(TcTypeArray *tuple, SCharArray *buffer);
static void tcTypeSigToString(TcTypeSig *typeSig, SCharArray *buffer);
static void tcTypeSigArgsToString(TcTypeSigArgs *args, SCharArray *buffer);

static void tcTypeToStringHelper(TcType *type, SCharArray *buffer) {
    if (type == NULL) {
        appendStringToSCharArray(buffer, "<null>");
        return;
    }
    switch (type->type) {
    case TCTYPE_TYPE_FUNCTION:
        tcFunctionToString(type->val.function, buffer);
        break;
    case TCTYPE_TYPE_PAIR:
        tcPairToString(type->val.pair, buffer);
        break;
    case TCTYPE_TYPE_THUNK:
        tcThunkToString(type->val.thunk, buffer);
        break;
    case TCTYPE_TYPE_VAR:
        tcVarToString(type->val.var, buffer);
        break;
    case TCTYPE_TYPE_BIGINTEGER:
        appendStringToSCharArray(buffer, "number");
        break;
    case TCTYPE_TYPE_SMALLINTEGER:
        appendStringToSCharArray(buffer, "smallint");
        break;
    case TCTYPE_TYPE_CHARACTER:
        appendStringToSCharArray(buffer, "char");
        break;
    case TCTYPE_TYPE_UNKNOWN:
        appendStringToSCharArray(buffer, "unknown:");
        appendStringToSCharArray(buffer, type->val.unknown->name);
        break;
    case TCTYPE_TYPE_TYPESIG:
        tcTypeSigToString(type->val.typeSig, buffer);
        break;
    case TCTYPE_TYPE_TUPLE:
        tcTupleToString(type->val.tuple, buffer);
        break;
    case TCTYPE_TYPE_ENV:
        appendStringToSCharArray(buffer, "<env>");
        break;
    case TCTYPE_TYPE_OPAQUE:
        appendStringToSCharArray(buffer, "opaque:");
        appendStringToSCharArray(buffer, type->val.opaque->name);
        break;
    default:
        appendStringToSCharArray(buffer, "<unknown type>");
    }
}

static void tcFunctionToString(TcFunction *function, SCharArray *buffer) {
    if (function->arg->type == TCTYPE_TYPE_FUNCTION) {
        appendStringToSCharArray(buffer, "(");
        tcTypeToStringHelper(function->arg, buffer);
        appendStringToSCharArray(buffer, ")");
    } else {
        tcTypeToStringHelper(function->arg, buffer);
    }
    appendStringToSCharArray(buffer, " -> ");
    tcTypeToStringHelper(function->result, buffer);
}

static void tcPairToString(TcPair *pair, SCharArray *buffer) {
    appendStringToSCharArray(buffer, "#(");
    tcTypeToStringHelper(pair->first, buffer);
    appendStringToSCharArray(buffer, ", ");
    tcTypeToStringHelper(pair->second, buffer);
    appendStringToSCharArray(buffer, ")");
}

static void tcThunkToString(TcThunk *thunk, SCharArray *buffer) {
    appendStringToSCharArray(buffer, "#() -> ");
    tcTypeToStringHelper(thunk->type, buffer);
}

static void tcVarToString(TcVar *var, SCharArray *buffer) {
    if (var->instance != NULL) {
        // If the var is bound to a concrete type, show that instead
        tcTypeToStringHelper(var->instance, buffer);
    } else {
        // Show as a type variable
        appendStringToSCharArray(buffer, "#");
        appendStringToSCharArray(buffer, var->name->name);
    }
}

static void tcTupleToString(TcTypeArray *tuple, SCharArray *buffer) {
    appendStringToSCharArray(buffer, "#(");
    for (Index i = 0; i < tuple->size; i++) {
        tcTypeToStringHelper(tuple->entries[i], buffer);
        if (i + 1 < tuple->size) {
            appendStringToSCharArray(buffer, ", ");
        }
    }
    appendStringToSCharArray(buffer, ")");
}

static void tcTypeSigArgsToString(TcTypeSigArgs *args, SCharArray *buffer) {
    while (args != NULL) {
        tcTypeToStringHelper(args->type, buffer);
        if (args->next) {
            appendStringToSCharArray(buffer, ", ");
        }
        args = args->next;
    }
}

static void tcTypeSigToString(TcTypeSig *typeSig, SCharArray *buffer) {
    appendStringToSCharArray(buffer, typeSig->name->name);
    if (typeSig->args != NULL) {
        appendStringToSCharArray(buffer, "(");
        tcTypeSigArgsToString(typeSig->args, buffer);
        appendStringToSCharArray(buffer, ")");
    }
}

// Public function to convert TcType to SCharArray
SCharArray *tcTypeToSCharArray(TcType *type) {

    SCharArray *buffer = newSCharArray();
    int save = PROTECT(buffer);
    tcTypeToStringHelper(type, buffer);
    UNPROTECT(save);
    return buffer; // Caller will null-terminate if needed
}

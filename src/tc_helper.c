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

#include <string.h>
#include <stdlib.h>
#include "tc_helper.h"
#include "tc_analyze.h"
#include "symbol.h"
#include "types.h"

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
    eprintf("(");
    ppTcType(function->arg);
    eprintf(") -> ");
    ppTcType(function->result);
}

void ppTcPair(TcPair *pair) {
    eprintf("#(");
    ppTcType(pair->first);
    eprintf(", ");
    ppTcType(pair->second);
    eprintf(")");
}

void ppTcVar(TcVar *var) {
    eprintf("<var>%s", var->name->name);
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

static inline void pad(int depth) {
    eprintf("%*s", depth * 2, "");
}

static void _ppTcEnv(TcEnv *env, int depth, bool done_namespaces);

static void _ppTcNamespaces(TcNamespaceArray *namespaces, int depth) {
    if (namespaces == NULL) return;
    for (Index i = 0; i < namespaces->size; i++) {
        pad(depth);
        eprintf("[%u]:\n", i);
        if (namespaces->entries[i]->type == TCTYPE_TYPE_ENV) {
            _ppTcEnv(namespaces->entries[i]->val.env, depth + 1, true);
        } else {
            eprintf("%s\n", tcTypeTypeName(namespaces->entries[i]->type));
        }
    }
}

static void _ppTcEnv(TcEnv *env, int depth, bool done_namespaces) {
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
            eprintf("  %s => %s [%d]\n", name->name, tcTypeTypeName(value->type), value->val.nsid);
        } else if (value->type == TCTYPE_TYPE_NAMESPACES) {
            if (done_namespaces) {
                eprintf("  %s => %s\n", name->name, tcTypeTypeName(value->type));
            } else {
                eprintf("  %s => %s [\n", name->name, tcTypeTypeName(value->type));
                _ppTcNamespaces(value->val.namespaces, depth + 1);
                pad(depth);
                eprintf("  ]\n");
            }
        } else {
            eprintf("  %s => %s\n", name->name, tcTypeTypeName(value->type));
        }
    }
    _ppTcEnv(env->next, depth + 1, done_namespaces);
    pad(depth);
    eprintf("}\n");
}

void ppTcEnv(TcEnv *env) {
    _ppTcEnv(env, 0, false);
}

// Helper function to append to a dynamically growing buffer
static void appendToBuffer(char **buffer, int *size, int *capacity, const char *str) {
    int len = strlen(str);
    while (*size + len >= *capacity) {
        *capacity *= 2;
        *buffer = realloc(*buffer, *capacity);
        if (buffer == NULL) {
            eprintf("Out of memory in appendToBuffer\n");
            exit(1);
        }
    }
    strcpy(*buffer + *size, str);
    *size += len;
}

// Forward declarations for string conversion
static void tcTypeToStringHelper(TcType *type, char **buffer, int *size, int *capacity);
static void tcFunctionToString(TcFunction *function, char **buffer, int *size, int *capacity);
static void tcPairToString(TcPair *pair, char **buffer, int *size, int *capacity);
static void tcVarToString(TcVar *var, char **buffer, int *size, int *capacity);
static void tcTupleToString(TcTypeArray *tuple, char **buffer, int *size, int *capacity);
static void tcTypeSigToString(TcTypeSig *typeSig, char **buffer, int *size, int *capacity);
static void tcTypeSigArgsToString(TcTypeSigArgs *args, char **buffer, int *size, int *capacity);

static void tcTypeToStringHelper(TcType *type, char **buffer, int *size, int *capacity) {
    if (type == NULL) {
        appendToBuffer(buffer, size, capacity, "<null>");
        return;
    }
    switch (type->type) {
        case TCTYPE_TYPE_FUNCTION:
            tcFunctionToString(type->val.function, buffer, size, capacity);
            break;
        case TCTYPE_TYPE_PAIR:
            tcPairToString(type->val.pair, buffer, size, capacity);
            break;
        case TCTYPE_TYPE_VAR:
            tcVarToString(type->val.var, buffer, size, capacity);
            break;
        case TCTYPE_TYPE_BIGINTEGER:
            appendToBuffer(buffer, size, capacity, "number");
            break;
        case TCTYPE_TYPE_SMALLINTEGER:
            appendToBuffer(buffer, size, capacity, "smallint");
            break;
        case TCTYPE_TYPE_CHARACTER:
            appendToBuffer(buffer, size, capacity, "char");
            break;
        case TCTYPE_TYPE_UNKNOWN:
            appendToBuffer(buffer, size, capacity, "unknown:");
            appendToBuffer(buffer, size, capacity, type->val.unknown->name);
            break;
        case TCTYPE_TYPE_TYPESIG:
            tcTypeSigToString(type->val.typeSig, buffer, size, capacity);
            break;
        case TCTYPE_TYPE_TUPLE:
            tcTupleToString(type->val.tuple, buffer, size, capacity);
            break;
        case TCTYPE_TYPE_ENV:
            appendToBuffer(buffer, size, capacity, "<env>");
            break;
        case TCTYPE_TYPE_OPAQUE:
            appendToBuffer(buffer, size, capacity, "opaque:");
            appendToBuffer(buffer, size, capacity, type->val.opaque->name);
            break;
        default:
            appendToBuffer(buffer, size, capacity, "<unknown type>");
    }
}

static void tcFunctionToString(TcFunction *function, char **buffer, int *size, int *capacity) {
    appendToBuffer(buffer, size, capacity, "(");
    tcTypeToStringHelper(function->arg, buffer, size, capacity);
    appendToBuffer(buffer, size, capacity, ") -> ");
    tcTypeToStringHelper(function->result, buffer, size, capacity);
}

static void tcPairToString(TcPair *pair, char **buffer, int *size, int *capacity) {
    appendToBuffer(buffer, size, capacity, "#(");
    tcTypeToStringHelper(pair->first, buffer, size, capacity);
    appendToBuffer(buffer, size, capacity, ", ");
    tcTypeToStringHelper(pair->second, buffer, size, capacity);
    appendToBuffer(buffer, size, capacity, ")");
}

static void tcVarToString(TcVar *var, char **buffer, int *size, int *capacity) {
    if (var->instance != NULL) {
        // If the var is bound to a concrete type, show that instead
        tcTypeToStringHelper(var->instance, buffer, size, capacity);
    } else {
        // Show as a type variable
        appendToBuffer(buffer, size, capacity, "#");
        appendToBuffer(buffer, size, capacity, var->name->name);
    }
}

static void tcTupleToString(TcTypeArray *tuple, char **buffer, int *size, int *capacity) {
    appendToBuffer(buffer, size, capacity, "#(");
    for (Index i = 0; i < tuple->size; i++) {
        tcTypeToStringHelper(tuple->entries[i], buffer, size, capacity);
        if (i + 1 < tuple->size) {
            appendToBuffer(buffer, size, capacity, ", ");
        }
    }
    appendToBuffer(buffer, size, capacity, ")");
}

static void tcTypeSigArgsToString(TcTypeSigArgs *args, char **buffer, int *size, int *capacity) {
    while (args != NULL) {
        tcTypeToStringHelper(args->type, buffer, size, capacity);
        if (args->next) {
            appendToBuffer(buffer, size, capacity, ", ");
        }
        args = args->next;
    }
}

static void tcTypeSigToString(TcTypeSig *typeSig, char **buffer, int *size, int *capacity) {
    appendToBuffer(buffer, size, capacity, typeSig->name->name);
    if (typeSig->args != NULL) {
        appendToBuffer(buffer, size, capacity, "(");
        tcTypeSigArgsToString(typeSig->args, buffer, size, capacity);
        appendToBuffer(buffer, size, capacity, ")");
    }
}

// Public function to convert TcType to string
char *tcTypeToString(TcType *type) {
    int capacity = 256;
    int size = 0;
    char *buffer = malloc(capacity);
    buffer[0] = '\0';
    
    tcTypeToStringHelper(type, &buffer, &size, &capacity);
    
    return buffer;
}

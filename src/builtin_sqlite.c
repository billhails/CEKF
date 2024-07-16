/*
 * CEKF - VM supporting amb
 * Copyright (C) 2022-2024  Bill Hails
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

#include <sqlite3.h>
#include "value.h"
#include "cekf.h"
#include "cekfs.h"
#include "utf8.h"
#include "symbol.h"
#include "builtin_sqlite.h"
#include "tc_analyze.h"

#ifdef DEBUG_SQLITE
#  include "debugging_on.h"
#else
#  include "debugging_off.h"
#endif

static void registerSQLiteOpen(BuiltIns *registry);
static void registerSQLiteClose(BuiltIns *registry);
static void registerSQLitePrepare(BuiltIns *registry);
static void registerSQLiteFinalize(BuiltIns *registry);
static void registerSQLiteBind(BuiltIns *registry);
static void registerSQLiteFetch(BuiltIns *registry);
static void registerSQLiteNames(BuiltIns *registry);

void registerSQLite(BuiltIns *registry) {
    registerSQLiteOpen(registry);
    registerSQLiteClose(registry);
    registerSQLitePrepare(registry);
    registerSQLiteFinalize(registry);
    registerSQLiteBind(registry);
    registerSQLiteFetch(registry);
    registerSQLiteNames(registry);
}

static void opaque_sqlite3_close(Opaque *data) {
    if (data == NULL) return;
    if (data->data == NULL) return;
    DEBUG("closing sqlite %p", data->data);
    sqlite3_close(data->data);
    data->data = NULL;
}

static void opaque_sqlite3_finalize(Opaque *data) {
    if (data == NULL) return;
    if (data->data == NULL) return;
    DEBUG("finalizing sqlite statement %p", data->data);
    sqlite3_finalize(data->data);
    data->data = NULL;
}

static Value builtin_sqlite3_open(Vec *v) {
    char *buf = listToUtf8(v->entries[0]);
    sqlite3 *ppDb = NULL;
    int status = sqlite3_open(buf, &ppDb);
    FREE_ARRAY(char, buf, strlen(buf) + 1);
    if (status != SQLITE_OK) {
        Value errMsg = utf8ToList(sqlite3_errmsg(ppDb));
        int save = protectValue(errMsg);
        sqlite3_close(ppDb);
        Value result = makeTryResult(0, errMsg);
        UNPROTECT(save);
        return result;
    }
    DEBUG("sqlite open %p", ppDb);
    Opaque *wrapper = newOpaque(ppDb, opaque_sqlite3_close, NULL);
    Value opaque = value_Opaque(wrapper);
    int save = protectValue(opaque);
    Value result = makeTryResult(1, opaque);
    UNPROTECT(save);
    return result;
}

static Value builtin_sqlite3_close(Vec *vec) {
#ifdef SAFETY_CHECKS
    if (vec->entries[0].type != VALUE_TYPE_OPAQUE) {
        cant_happen("unexpected %s", valueTypeName(vec->entries[0].type));
    }
#endif
    Opaque *data = vec->entries[0].val.opaque;
    opaque_sqlite3_close(data);
    vec->entries[0].val.opaque = NULL;
    return value_Stdint(1);
}

static Value builtin_sqlite3_finalize(Vec *vec) {
#ifdef SAFETY_CHECKS
    if (vec->entries[0].type != VALUE_TYPE_OPAQUE) {
        cant_happen("unexpected %s", valueTypeName(vec->entries[0].type));
    }
#endif
    Opaque *data = vec->entries[0].val.opaque;
    opaque_sqlite3_finalize(data);
    vec->entries[0].val.opaque = NULL;
    return value_Stdint(1);
}

static Value builtin_sqlite3_prepare(Vec *vec) {
#ifdef SAFETY_CHECKS
    if (vec->entries[0].type != VALUE_TYPE_OPAQUE) {
        cant_happen("unexpected %s", valueTypeName(vec->entries[0].type));
    }
#endif
    Opaque *data = vec->entries[0].val.opaque;
    char *string = listToUtf8(vec->entries[1]);
    sqlite3_stmt *stmt = NULL;
    int res = sqlite3_prepare_v2(data->data, string, -1, &stmt, NULL);
    FREE_ARRAY(char, string, strlen(string) + 1);
    if (res != SQLITE_OK) {
        Value errCode = value_Stdint(res);
        Value result = makeTryResult(0, errCode);
        return result;
    }
    DEBUG("sqlite prepare %p", stmt);
    Opaque *wrapper = newOpaque(stmt, opaque_sqlite3_finalize, NULL);
    Value opaque = value_Opaque(wrapper);
    int save = protectValue(opaque);
    Value result = makeTryResult(1, opaque);
    UNPROTECT(save);
    return result;
}

static void helper_free_str(void *v) {
    char *str = (char *) v;
    FREE_ARRAY(char, str, strlen(str) + 1);
}

static int helper_bind_bigint(sqlite3_stmt *stmt, int index, BigInt *bigint) {
    int size = bigint_write_size(&bigint->bi, 10);
    char *buf = NEW_ARRAY(char, size);
    bigint_write(buf, size, &bigint->bi);
    return sqlite3_bind_text(stmt, index, buf, strlen(buf), helper_free_str);
}

static int helper_bind_number(sqlite3_stmt *stmt, int index, Value number) {
    switch (number.type) {
        case VALUE_TYPE_STDINT:
            return sqlite3_bind_int(stmt, index, number.val.stdint);
        case VALUE_TYPE_BIGINT:
            return helper_bind_bigint(stmt, index, number.val.bigint);
        case VALUE_TYPE_IRRATIONAL:
            return sqlite3_bind_double(stmt, index, number.val.irrational);
        case VALUE_TYPE_RATIONAL:
        case VALUE_TYPE_RATIONAL_IMAG:
        case VALUE_TYPE_STDINT_IMAG:
        case VALUE_TYPE_BIGINT_IMAG:
        case VALUE_TYPE_IRRATIONAL_IMAG:
        case VALUE_TYPE_COMPLEX:
            eprintf("numeric type %s not supported yet\n", valueTypeName(number.type));
            return SQLITE_ERROR;
            break;
        default:
            cant_happen("unexpected %s", valueTypeName(number.type));
    }
}

static int helper_bind_string(sqlite3_stmt *stmt, int index, Value string) {
    char *buf = listToUtf8(string);
    return sqlite3_bind_text(stmt, index, buf, strlen(buf), helper_free_str);
}

static int helper_bind_char(sqlite3_stmt *stmt, int index, Value character) {
    unsigned char str[80];
    unsigned char *end = writeChar(str, character.val.character);
    *end = '\0';
    int size = strlen((char *)str);
    char *buf = NEW_ARRAY(char, size + 1);
    strcpy(buf, (char *)str);
    return sqlite3_bind_text(stmt, index, buf, size, helper_free_str);
}

static Value builtin_sqlite3_bind(Vec *vec) {
#ifdef SAFETY_CHECKS
    if (vec->entries[0].type != VALUE_TYPE_OPAQUE) {
        cant_happen("unexpected %s", valueTypeName(vec->entries[0].type));
    }
#endif
    sqlite3_stmt *stmt = (sqlite3_stmt *)vec->entries[0].val.opaque->data;
    Value bindings = vec->entries[1];
    int index = 1;
    int status = SQLITE_OK;
    while (bindings.val.vec->entries[0].val.stdint == 1) { // tag == pair
        Value car = bindings.val.vec->entries[1];
        switch (car.val.vec->entries[0].val.stdint) {
            case BASIC_TYPE_NULL:
                DEBUG("binding NULL");
                status = sqlite3_bind_null(stmt, index);
                break;
            case BASIC_TYPE_NUMBER:
                DEBUG("binding NUMBER");
                status = helper_bind_number(stmt, index, car.val.vec->entries[1]);
                break;
            case BASIC_TYPE_STRING:
                DEBUG("binding STRING");
                status = helper_bind_string(stmt, index, car.val.vec->entries[1]);
                break;
            case BASIC_TYPE_CHAR:
                DEBUG("binding CHAR");
                status = helper_bind_char(stmt, index, car.val.vec->entries[1]);
                break;
            default:
                cant_happen("unexpected %d", car.val.vec->entries[0].val.stdint);
        }
        if (status != SQLITE_OK) {
            break;
        }
        bindings = bindings.val.vec->entries[2]; // cdr
        index++;
    }
    return value_Stdint(status);
}

static Value helper_fetch_integer(sqlite3_stmt *stmt, int iCol, Value cdr) {
    Value i = value_Stdint(sqlite3_column_int(stmt, iCol));
    Value bi = makeBasic(i, BASIC_TYPE_NUMBER);
    protectValue(bi);
    Value v = makePair(bi, cdr);
    protectValue(v);
    return v;
}

static Value helper_fetch_float(sqlite3_stmt *stmt, int iCol, Value cdr) {
    Value f = value_Irrational(sqlite3_column_double(stmt, iCol));
    Value bf = makeBasic(f, BASIC_TYPE_NUMBER);
    protectValue(bf);
    Value v = makePair(bf, cdr);
    protectValue(v);
    return v;
}

static Value helper_fetch_text(sqlite3_stmt *stmt, int iCol, Value cdr) {
    const unsigned char *text = sqlite3_column_text(stmt, iCol);
    Value s = utf8ToList((const char *) text);
    protectValue(s);
    Value bs = makeBasic(s, BASIC_TYPE_STRING);
    protectValue(bs);
    Value v = makePair(bs, cdr);
    protectValue(v);
    return v;
}

static Value helper_fetch_null(Value cdr) {
    Value bn = makeBasic(value_None(), BASIC_TYPE_NULL);
    protectValue(bn);
    Value v = makePair(bn, cdr);
    protectValue(v);
    return v;
}

static Value helper_fetch_row(sqlite3_stmt *stmt) {
    int nrows = sqlite3_column_count(stmt);
    // build the result right to left
    Value result = makeNull();
    int save = protectValue(result);
    for (int i = nrows; i > 0; i--) {
        int iCol = i - 1;
        int type = sqlite3_column_type(stmt, iCol);
        switch (type) {
            case SQLITE_INTEGER:
                result = helper_fetch_integer(stmt, iCol, result);
                break;
            case SQLITE_FLOAT:
                result = helper_fetch_float(stmt, iCol, result);
                break;
            case SQLITE_TEXT:
                result = helper_fetch_text(stmt, iCol, result);
                break;
            case SQLITE_BLOB:
                cant_happen("blob not supported yet");
                break;
            case SQLITE_NULL:
                result = helper_fetch_null(result);
                break;
            default:
                cant_happen("unexpected %d", type);
        }
    }
    result = makeSome(result);
    UNPROTECT(save);
    return result;
}

static Value builtin_sqlite3_fetch(Vec *vec) {
#ifdef SAFETY_CHECKS
    if (vec->entries[0].type != VALUE_TYPE_OPAQUE) {
        cant_happen("unexpected %s", valueTypeName(vec->entries[0].type));
    }
#endif
    sqlite3_stmt *stmt = (sqlite3_stmt *)vec->entries[0].val.opaque->data;
    if (stmt == NULL) {
        return makeNothing();
    }
    int res = sqlite3_step(stmt);
    switch (res) {
        case SQLITE_ROW:
            return helper_fetch_row(stmt);
        case SQLITE_DONE:
            return makeNothing();
        default:
            eprintf("sqlite3_step returned %d\n", res);
            return makeNothing();
    }
}

static Value builtin_sqlite3_names(Vec *vec) {
#ifdef SAFETY_CHECKS
    if (vec->entries[0].type != VALUE_TYPE_OPAQUE) {
        cant_happen("unexpected %s", valueTypeName(vec->entries[0].type));
    }
#endif
    sqlite3_stmt *stmt = (sqlite3_stmt *)vec->entries[0].val.opaque->data;
    if (stmt == NULL) {
        return makeEmptyList();
    }
    int nrows = sqlite3_column_count(stmt);
    Value result = makeNull();
    int save = protectValue(result);
    for (int i = nrows; i > 0; i--) {
        int iCol = i - 1;
        const char *name = sqlite3_column_name(stmt, iCol);
        Value s = utf8ToList(name);
        protectValue(s);
        result = makePair(s, result);
        protectValue(result);
    }
    UNPROTECT(save);
    return result;
}

static HashSymbol *sqliteSymbol(void) {
    static HashSymbol *symbol = NULL;
    if (symbol == NULL) {
        symbol = newSymbol("sqlite3");
    }
    return symbol;
}

static HashSymbol *sqliteStatementSymbol(void) {
    static HashSymbol *symbol = NULL;
    if (symbol == NULL) {
        symbol = newSymbol("sqlite3_stmt");
    }
    return symbol;
}

static TcType *makeSqliteType(void) {
    return newTcType_Opaque(sqliteSymbol());
}

static TcType *makeSqliteStatementType(void) {
    return newTcType_Opaque(sqliteStatementSymbol());
}

static void registerSQLiteOpen(BuiltIns *registry) {
    BuiltInArgs *args = newBuiltInArgs();
    int save = PROTECT(args);
    TcType *stringType = makeStringType();
    PROTECT(stringType);
    TcType *sqliteType = makeSqliteType();
    PROTECT(sqliteType);
    TcType *trySqliteType = makeTryType(stringType, sqliteType);
    PROTECT(trySqliteType);
    pushBuiltInArgs(args, stringType);
    BuiltIn *decl = newBuiltIn(newSymbol("sqlite3_open"), trySqliteType, args, (void *)builtin_sqlite3_open);
    PROTECT(decl);
    pushBuiltIns(registry, decl);
    UNPROTECT(save);
}

static void registerSQLiteClose(BuiltIns *registry) {
    BuiltInArgs *args = newBuiltInArgs();
    int save = PROTECT(args);
    TcType *ppDb = makeSqliteType();
    PROTECT(ppDb);
    TcType *b = makeBoolean();
    PROTECT(b);
    pushBuiltInArgs(args, ppDb);
    BuiltIn *decl = newBuiltIn(newSymbol("sqlite3_close"), b, args, (void *)builtin_sqlite3_close);
    PROTECT(decl);
    pushBuiltIns(registry, decl);
    UNPROTECT(save);
}

static void registerSQLitePrepare(BuiltIns *registry) {
    BuiltInArgs *args = newBuiltInArgs();
    int save = PROTECT(args);
    TcType *sqlite = makeSqliteType();
    PROTECT(sqlite);
    TcType *bigIntType = newTcType_Biginteger();
    PROTECT(bigIntType);
    TcType *string = makeStringType();
    PROTECT(string);
    TcType *statementType = makeSqliteStatementType();
    PROTECT(statementType);
    TcType *tryStatementType = makeTryType(bigIntType, statementType);
    PROTECT(tryStatementType);
    pushBuiltInArgs(args, sqlite);
    pushBuiltInArgs(args, string);
    BuiltIn *decl = newBuiltIn(newSymbol("sqlite3_prepare"), tryStatementType, args, (void *)builtin_sqlite3_prepare);
    PROTECT(decl);
    pushBuiltIns(registry, decl);
    UNPROTECT(save);
}

static void registerSQLiteFinalize(BuiltIns *registry) {
    BuiltInArgs *args = newBuiltInArgs();
    int save = PROTECT(args);
    TcType *statementType = makeSqliteStatementType();
    PROTECT(statementType);
    TcType *b = makeBoolean();
    PROTECT(b);
    pushBuiltInArgs(args, statementType);
    BuiltIn *decl = newBuiltIn(newSymbol("sqlite3_finalize"), b, args, (void *)builtin_sqlite3_finalize);
    PROTECT(decl);
    pushBuiltIns(registry, decl);
    UNPROTECT(save);
}

static TcType *makeListBasicType(void) {
    TcType *basicType = makeBasicType();
    int save = PROTECT(basicType);
    TcType *basicListType = makeListType(basicType);
    UNPROTECT(save);
    return basicListType;
}

static TcType *makeMaybeListBasicType(void) {
    TcType *basicListType = makeListBasicType();
    int save = PROTECT(basicListType);
    TcType *maybeListBasicType = makeMaybeType(basicListType);
    UNPROTECT(save);
    return maybeListBasicType;
}

static void registerSQLiteBind(BuiltIns *registry) {
    BuiltInArgs *args = newBuiltInArgs();
    int save = PROTECT(args);
    TcType *statementType = makeSqliteStatementType();
    PROTECT(statementType);
    TcType *basicListType = makeListBasicType();
    PROTECT(basicListType);
    TcType *bigIntType = newTcType_Biginteger();
    PROTECT(bigIntType);
    pushBuiltInArgs(args, statementType);
    pushBuiltInArgs(args, basicListType);
    BuiltIn *decl = newBuiltIn(newSymbol("sqlite3_bind"), bigIntType, args, (void *)builtin_sqlite3_bind);
    PROTECT(decl);
    pushBuiltIns(registry, decl);
    UNPROTECT(save);
}

static void registerSQLiteFetch(BuiltIns *registry) {
    BuiltInArgs *args = newBuiltInArgs();
    int save = PROTECT(args);
    TcType *statementType = makeSqliteStatementType();
    PROTECT(statementType);
    pushBuiltInArgs(args, statementType);
    TcType *maybeListBasicType = makeMaybeListBasicType();
    PROTECT(maybeListBasicType);
    BuiltIn *decl = newBuiltIn(newSymbol("sqlite3_fetch"), maybeListBasicType, args, (void *)builtin_sqlite3_fetch);
    PROTECT(decl);
    pushBuiltIns(registry, decl);
    UNPROTECT(save);
}

static void registerSQLiteNames(BuiltIns *registry) {
    BuiltInArgs *args = newBuiltInArgs();
    int save = PROTECT(args);
    TcType *statementType = makeSqliteStatementType();
    PROTECT(statementType);
    pushBuiltInArgs(args, statementType);
    TcType *stringType = makeStringType();
    PROTECT(stringType);
    TcType *listStringType = makeListType(stringType);
    PROTECT(listStringType);
    BuiltIn *decl = newBuiltIn(newSymbol("sqlite3_names"), listStringType, args, (void *)builtin_sqlite3_names);
    PROTECT(decl);
    pushBuiltIns(registry, decl);
    UNPROTECT(save);
}


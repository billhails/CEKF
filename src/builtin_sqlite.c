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

void registerSQLite(BuiltIns *registry) {
    registerSQLiteOpen(registry);
    registerSQLiteClose(registry);
}

static void opaque_sqlite3_close(Opaque *data) {
    if (data == NULL) return;
    if (data->data == NULL) return;
    DEBUG("closing sqlite %p\n", data->data);
    sqlite3_close(data->data);
    data->data = NULL;
}

// should really return a maybe of the opaque value
static Value builtin_sqlite3_open(Vec *v) {
    char *buf = listToUtf8(v->entries[0]);
    sqlite3 *ppDb = NULL;
    int status = sqlite3_open(buf, &ppDb);
    if (status != SQLITE_OK) {
        cant_happen("sqlite3_open returned status %s", sqlite3_errmsg(ppDb));
    }
    FREE_ARRAY(char, buf, strlen(buf) + 1);
    Opaque *wrapper = newOpaque(ppDb, opaque_sqlite3_close, NULL);
    Value result = value_Opaque(wrapper);
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

static void registerSQLiteOpen(BuiltIns *registry) {
    BuiltInArgs *args = newBuiltInArgs();
    int save = PROTECT(args);
    TcType *ppDb = newTcType_Opaque(newSymbol("sqlite3"));
    PROTECT(ppDb);
    TcType *string = makeStringType();
    PROTECT(string);
    pushBuiltInArgs(args, string);
    BuiltIn *decl = newBuiltIn(newSymbol("sqlite3_open"), ppDb, args, (void *)builtin_sqlite3_open);
    PROTECT(decl);
    pushBuiltIns(registry, decl);
    UNPROTECT(save);
}

static void registerSQLiteClose(BuiltIns *registry) {
    BuiltInArgs *args = newBuiltInArgs();
    int save = PROTECT(args);
    TcType *ppDb = newTcType_Opaque(newSymbol("sqlite3"));
    PROTECT(ppDb);
    TcType *b = makeBoolean();
    PROTECT(b);
    pushBuiltInArgs(args, ppDb);
    BuiltIn *decl = newBuiltIn(newSymbol("sqlite3_close"), b, args, (void *)builtin_sqlite3_close);
    PROTECT(decl);
    pushBuiltIns(registry, decl);
    UNPROTECT(save);
}

/* * CEKF - VM supporting amb
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

#include <stdarg.h>
#include <stdio.h>
#include <sys/stat.h>
#include <sys/sysmacros.h>
#include <sys/types.h>
#include <unistd.h>

#include "ast.h"
#include "ast_pp.h"
#include "bigint.h"
#include "pratt.h"
#include "pratt_debug.h"
#include "pratt_functions.h"
#include "pratt_parser.h"
#include "pratt_scanner.h"
#include "print_generator.h"
#include "symbols.h"
#include "utils.h"

static void ppAstDefLazy(SCharArray *, AstDefLazy *);
static void ppAstDefMulti(SCharArray *, AstMultiDefine *);
static void ppAstDefinitions(SCharArray *, AstDefinitions *);
static void ppAstDefinition(SCharArray *, AstDefinition *);
static void ppAstDefine(SCharArray *, AstDefine *);
static void ppAstTypeDef(SCharArray *, AstTypeDef *);
static void ppAstAlias(SCharArray *, AstAlias *);
static void ppAstTypeSig(SCharArray *, AstTypeSig *);
static void ppAstType(SCharArray *, AstType *);
static void ppAstTypeBody(SCharArray *, AstTypeBody *);
static void ppAstTypeConstructor(SCharArray *, AstTypeConstructor *);
static void ppAstTypeConstructorArgs(SCharArray *, AstTypeConstructorArgs *);
static void ppAstTypeList(SCharArray *, AstTypeList *);
static void ppAstTypeMap(SCharArray *, AstTypeMap *);
static void ppAstExpressions(SCharArray *, AstExpressions *);
static void ppAstTypeSymbols(SCharArray *, AstTypeSymbols *);
static void ppAstTypeClause(SCharArray *, AstTypeClause *);
static void ppAstTypeFunction(SCharArray *, AstTypeFunction *);
static void ppAstLookUpOrSymbol(SCharArray *, AstLookUpOrSymbol *);
static void ppAstLookUpSymbol(SCharArray *, AstLookUpSymbol *);
static void ppAstCompositeFunction(SCharArray *, AstCompositeFunction *);
static void ppAstFunction(SCharArray *, AstFunction *);
static void ppAstFargList(SCharArray *, AstFargList *);
static void ppAstTaggedArgList(SCharArray *, AstTaggedArgList *);
static void ppastFarg(SCharArray *, AstFarg *);
static void ppAstNamedArg(SCharArray *, AstNamedArg *);
static void ppAstUnpack(SCharArray *, AstUnpack *);
static void ppAstUnpackStruct(SCharArray *, AstUnpackStruct *);
static void ppAstIff(SCharArray *, AstIff *);
static void ppAstLookUp(SCharArray *, AstLookUp *);
static void ppAstPrint(SCharArray *, AstPrint *);
static void ppAstStruct(SCharArray *, AstStruct *);
static void ppAstTaggedExpressions(SCharArray *, AstTaggedExpressions *);

static void ppMaybeBigInt(SCharArray *, MaybeBigInt *);
static void ppUnicodeChar(SCharArray *, Character);
static void ppHashSymbol(SCharArray *, HashSymbol *);

void ppAstNest(SCharArray *dest, AstNest *nest) {
    psprintf(dest, "{ ");
    if (nest) {
        if (nest->definitions) {
            if (nest->expressions) {
                psprintf(dest, "let ");
                ppAstDefinitions(dest, nest->definitions);
                psprintf(dest, "in ");
                ppAstExpressions(dest, nest->expressions);
            } else {
                psprintf(dest, "nameSpace ");
                ppAstDefinitions(dest, nest->definitions);
            }
        } else {
            ppAstExpressions(dest, nest->expressions);
        }
    }
    psprintf(dest, "}");
}

void ppAstNameSpaceImpl(SCharArray *dest, AstNameSpaceImpl *impl) {
    psprintf(dest, "\"%u:%u:%lu\": {", major(impl->id->stDev),
             minor(impl->id->stDev), impl->id->stIno);
    ppAstDefinitions(dest, impl->definitions);
    psprintf(dest, "}");
}

void ppAstProg(SCharArray *dest, AstProg *prog) {
    psprintf(dest, "preamble: {");
    ppAstDefinitions(dest, prog->preamble);
    psprintf(dest, "} nameSpaces: [");
    for (Index i = 0; i < prog->nameSpaces->size; ++i) {
        ppAstNameSpaceImpl(dest, prog->nameSpaces->entries[i]);
    }
    psprintf(dest, "] body: {");
    ppAstExpressions(dest, prog->body);
    psprintf(dest, "}");
}

void ppAstDefinitions(SCharArray *dest, AstDefinitions *definitions) {
    while (definitions) {
        ppAstDefinition(dest, definitions->definition);
        psprintf(dest, "; ");
        definitions = definitions->next;
    }
}

static void ppAstDefinition(SCharArray *dest, AstDefinition *definition) {
    switch (definition->type) {
    case AST_DEFINITION_TYPE_DEFINE:
        ppAstDefine(dest, definition->val.define);
        break;
    case AST_DEFINITION_TYPE_TYPEDEF:
        ppAstTypeDef(dest, definition->val.typeDef);
        break;
    case AST_DEFINITION_TYPE_ALIAS:
        ppAstAlias(dest, definition->val.alias);
        break;
    case AST_DEFINITION_TYPE_BLANK:
        break;
    case AST_DEFINITION_TYPE_LAZY:
        ppAstDefLazy(dest, definition->val.lazy);
        break;
    case AST_DEFINITION_TYPE_MULTI:
        ppAstDefMulti(dest, definition->val.multi);
        break;
    default:
        cant_happen("unrecognised %s", astDefinitionTypeName(definition->type));
    }
}

static void ppAstDefLazy(SCharArray *dest, AstDefLazy *defLazy) {
    psprintf(dest, "lazy fn ");
    ppHashSymbol(dest, defLazy->name);
    psprintf(dest, "(");
    ppAstFargList(dest, defLazy->definition->altArgs->argList);
    psprintf(dest, ") ");
    ppAstNest(dest, defLazy->definition->nest);
}

static void ppAstSymbolList(SCharArray *dest, AstSymbolList *symbolList) {
    if (symbolList) {
        ppHashSymbol(dest, symbolList->symbol);
        if (symbolList->next) {
            psprintf(dest, ", ");
            ppAstSymbolList(dest, symbolList->next);
        }
    }
}

static void ppAstDefMulti(SCharArray *dest, AstMultiDefine *define) {
    psprintf(dest, "#(");
    ppAstSymbolList(dest, define->symbols);
    psprintf(dest, ") = ");
    ppAstExpression(dest, define->expression);
    psprintf(dest, "; ");
}

static void ppAstDefine(SCharArray *dest, AstDefine *define) {
    ppHashSymbol(dest, define->symbol);
    psprintf(dest, " = ");
    ppAstExpression(dest, define->expression);
}

static void ppAstTypeDef(SCharArray *dest, AstTypeDef *typeDef) {
    psprintf(dest, "typedef ");
    ppAstTypeSig(dest, typeDef->typeSig);
    psprintf(dest, " {");
    ppAstTypeBody(dest, typeDef->typeBody);
    psprintf(dest, "}");
}

static void ppAstAlias(SCharArray *dest, AstAlias *alias) {
    psprintf(dest, "alias ");
    ppHashSymbol(dest, alias->name);
    psprintf(dest, " = ");
    ppAstType(dest, alias->type);
}

static void ppAstTypeSig(SCharArray *dest, AstTypeSig *typeSig) {
    ppHashSymbol(dest, typeSig->symbol);
    if (typeSig->typeSymbols != NULL) {
        psprintf(dest, "(");
        ppAstTypeSymbols(dest, typeSig->typeSymbols);
        psprintf(dest, ")");
    }
}

static void ppAstType(SCharArray *dest, AstType *type) {
    if (type != NULL) {
        ppAstTypeClause(dest, type->typeClause);
        if (type->next) {
            psprintf(dest, ", ");
            ppAstType(dest, type->next);
        }
    }
}

static void ppAstTypeBody(SCharArray *dest, AstTypeBody *typeBody) {
    if (typeBody != NULL) {
        ppAstTypeConstructor(dest, typeBody->typeConstructor);
        if (typeBody->next) {
            psprintf(dest, " | ");
            ppAstTypeBody(dest, typeBody->next);
        }
    }
}

static void ppAstTypeConstructor(SCharArray *dest,
                                 AstTypeConstructor *typeConstructor) {
    ppHashSymbol(dest, typeConstructor->symbol);
    ppAstTypeConstructorArgs(dest, typeConstructor->args);
}

static void
ppAstTypeConstructorArgs(SCharArray *dest,
                         AstTypeConstructorArgs *typeConstructorArgs) {
    if (typeConstructorArgs) {
        switch (typeConstructorArgs->type) {
        case AST_TYPECONSTRUCTORARGS_TYPE_LIST:
            psprintf(dest, "(");
            ppAstTypeList(dest, typeConstructorArgs->val.list);
            psprintf(dest, ")");
            break;
        case AST_TYPECONSTRUCTORARGS_TYPE_MAP:
            psprintf(dest, "{ ");
            ppAstTypeMap(dest, typeConstructorArgs->val.map);
            psprintf(dest, " }");
            break;
        default:
            cant_happen("unrecognised %s", astTypeConstructorArgsTypeName(
                                               typeConstructorArgs->type));
        }
    }
}

static void ppAstTypeList(SCharArray *dest, AstTypeList *typeList) {
    if (typeList) {
        ppAstType(dest, typeList->type);
        if (typeList->next) {
            psprintf(dest, ", ");
            ppAstTypeList(dest, typeList->next);
        }
    }
}

static void ppAstTypeMap(SCharArray *dest, AstTypeMap *typeMap) {
    if (typeMap) {
        ppHashSymbol(dest, typeMap->key);
        psprintf(dest, ": ");
        ppAstType(dest, typeMap->type);
        if (typeMap->next) {
            psprintf(dest, ", ");
            ppAstTypeMap(dest, typeMap->next);
        }
    }
}

static void ppAstExpressions(SCharArray *dest, AstExpressions *expressions) {
    if (expressions) {
        ppAstExpression(dest, expressions->expression);
        psprintf(dest, "; ");
        if (expressions->next) {
            ppAstExpressions(dest, expressions->next);
        }
    }
}

static void ppAstTypeSymbols(SCharArray *dest, AstTypeSymbols *typeSymbols) {
    if (typeSymbols) {
        ppHashSymbol(dest, typeSymbols->typeSymbol);
        if (typeSymbols->next) {
            psprintf(dest, ", ");
            ppAstTypeSymbols(dest, typeSymbols->next);
        }
    }
}

static void ppAstTypeClause(SCharArray *dest, AstTypeClause *typeClause) {
    switch (typeClause->type) {
    case AST_TYPECLAUSE_TYPE_INTEGER:
        psprintf(dest, "number");
        break;
    case AST_TYPECLAUSE_TYPE_CHARACTER:
        psprintf(dest, "char");
        break;
    case AST_TYPECLAUSE_TYPE_VAR:
        ppHashSymbol(dest, typeClause->val.var);
        break;
    case AST_TYPECLAUSE_TYPE_TYPEFUNCTION:
        ppAstTypeFunction(dest, typeClause->val.typeFunction);
        break;
    case AST_TYPECLAUSE_TYPE_TYPETUPLE:
        psprintf(dest, "<tuple>(");
        ppAstTypeList(dest, typeClause->val.typeTuple);
        psprintf(dest, ")");
        break;
    default:
        cant_happen("unrecognised %s", astTypeClauseTypeName(typeClause->type));
    }
}

static void ppAstTypeFunction(SCharArray *dest, AstTypeFunction *typeFunction) {
    ppAstLookUpOrSymbol(dest, typeFunction->symbol);
    if (typeFunction->typeList) {
        psprintf(dest, "(");
        ppAstTypeList(dest, typeFunction->typeList);
        psprintf(dest, ")");
    }
}

static void ppAstLookUpOrSymbol(SCharArray *dest,
                                AstLookUpOrSymbol *lookUpOrSymbol) {
    switch (lookUpOrSymbol->type) {
    case AST_LOOKUPORSYMBOL_TYPE_LOOKUP:
        ppAstLookUpSymbol(dest, lookUpOrSymbol->val.lookUp);
        break;
    case AST_LOOKUPORSYMBOL_TYPE_SYMBOL:
        ppHashSymbol(dest, lookUpOrSymbol->val.symbol);
        break;
    default:
        cant_happen("unrecognised %s",
                    astLookUpOrSymbolTypeName(lookUpOrSymbol->type));
    }
}

static void ppAstLookUpSymbol(SCharArray *dest, AstLookUpSymbol *lookUpSymbol) {
    ppHashSymbol(dest, lookUpSymbol->nsSymbol);
    psprintf(dest, "<%d>.", lookUpSymbol->nsId);
    ppHashSymbol(dest, lookUpSymbol->symbol);
}

static void ppAstLookUp(SCharArray *dest, AstLookUp *lookUp) {
    ppHashSymbol(dest, lookUp->nsSymbol);
    psprintf(dest, "<%d>.", lookUp->nsId);
    ppAstExpression(dest, lookUp->expression);
}

static void ppFunctionComponents(SCharArray *dest,
                                 AstCompositeFunction *compositeFunction) {
    if (compositeFunction) {
        ppAstFunction(dest, compositeFunction->function);
        psprintf(dest, " ");
        ppFunctionComponents(dest, compositeFunction->next);
    }
}

static void ppAstCompositeFunction(SCharArray *dest,
                                   AstCompositeFunction *compositeFunction) {
    if (compositeFunction == NULL)
        return;
    if (compositeFunction->unsafe) {
        psprintf(dest, "unsafe ");
    }
    psprintf(dest, "fn { ");
    ppFunctionComponents(dest, compositeFunction);
    psprintf(dest, "}");
}

static void ppAstFunction(SCharArray *dest, AstFunction *function) {
    psprintf(dest, "(");
    ppAstFargList(dest, function->argList);
    psprintf(dest, ") ");
    ppAstNest(dest, function->nest);
}

static void ppAstFargList(SCharArray *dest, AstFargList *argList) {
    if (argList) {
        ppastFarg(dest, argList->arg);
        if (argList->next) {
            psprintf(dest, ", ");
            ppAstFargList(dest, argList->next);
        }
    }
}

static void ppastFarg(SCharArray *dest, AstFarg *arg) {
    switch (arg->type) {
    case AST_FARG_TYPE_WILDCARD:
        psprintf(dest, "_");
        break;
    case AST_FARG_TYPE_SYMBOL:
        ppHashSymbol(dest, arg->val.symbol);
        break;
    case AST_FARG_TYPE_LOOKUP:
        ppAstLookUpSymbol(dest, arg->val.lookUp);
        break;
    case AST_FARG_TYPE_NAMED:
        ppAstNamedArg(dest, arg->val.named);
        break;
    case AST_FARG_TYPE_UNPACK:
        ppAstUnpack(dest, arg->val.unpack);
        break;
    case AST_FARG_TYPE_UNPACKSTRUCT:
        ppAstUnpackStruct(dest, arg->val.unpackStruct);
        break;
    case AST_FARG_TYPE_NUMBER:
        ppMaybeBigInt(dest, arg->val.number);
        break;
    case AST_FARG_TYPE_CHARACTER:
        ppUnicodeChar(dest, arg->val.character);
        break;
    case AST_FARG_TYPE_TUPLE:
        psprintf(dest, "<tuple>(");
        ppAstFargList(dest, arg->val.tuple);
        psprintf(dest, ")");
        break;
    default:
        break;
    }
}

static void ppAstNamedArg(SCharArray *dest, AstNamedArg *namedArg) {
    ppHashSymbol(dest, namedArg->name);
    psprintf(dest, " = ");
    ppastFarg(dest, namedArg->arg);
}

static void ppAstUnpack(SCharArray *dest, AstUnpack *unpack) {
    ppAstLookUpOrSymbol(dest, unpack->symbol);
    psprintf(dest, "(");
    ppAstFargList(dest, unpack->argList);
    psprintf(dest, ")");
}

static void ppAstUnpackStruct(SCharArray *dest, AstUnpackStruct *unpackStruct) {
    ppAstLookUpOrSymbol(dest, unpackStruct->symbol);
    psprintf(dest, "{ ");
    ppAstTaggedArgList(dest, unpackStruct->argList);
    psprintf(dest, " }");
}

static void ppAstTaggedArgList(SCharArray *dest,
                               AstTaggedArgList *taggedArgList) {
    if (taggedArgList) {
        ppHashSymbol(dest, taggedArgList->tag);
        psprintf(dest, ": ");
        ppastFarg(dest, taggedArgList->arg);
        if (taggedArgList->next) {
            psprintf(dest, ", ");
            ppAstTaggedArgList(dest, taggedArgList->next);
        }
    }
}

static void ppMaybeBigInt(SCharArray *dest, MaybeBigInt *maybe) {
    size_t size = printSizeMaybeBigInt(maybe);
    extendSCharArray(dest, dest->size + size);
    char *start = &dest->entries[dest->size];
    size = sprintMaybeBigInt(start, maybe);
    dest->size += size;
    dest->size--;
}

static void ppUnicodeChar(SCharArray *dest, Character c) {
    char buf[MB_LEN_MAX];
    int len = wctomb(buf, c);
    if (len > 0) {
        buf[len] = '\0';
        psprintf(dest, "%s", buf);
    }
}

static void ppHashSymbol(SCharArray *dest, HashSymbol *symbol) {
    psprintf(dest, "%s", symbol->name);
}

void ppAstFunCall(SCharArray *dest, AstFunCall *funCall) {
    ppAstExpression(dest, funCall->function);
    psprintf(dest, "(");
    for (AstExpressions *expressions = funCall->arguments; expressions != NULL;
         expressions = expressions->next) {
        ppAstExpression(dest, expressions->expression);
        if (expressions->next) {
            psprintf(dest, ", ");
        }
    }
    psprintf(dest, ")");
}

void ppAstCharacter(SCharArray *dest, Character c) {
    char buffer[MB_LEN_MAX];
    int len = wctomb(buffer, c);
    if (len > 0) {
        buffer[len] = '\0';
        psprintf(dest, "'%s'", buffer);
    }
}

void ppAstTuple(SCharArray *dest, AstExpressions *expressions) {
    psprintf(dest, "<tuple>(");
    while (expressions) {
        ppAstExpression(dest, expressions->expression);
        if (expressions->next)
            psprintf(dest, ", ");
        expressions = expressions->next;
    }
    psprintf(dest, ")");
}

static void ppAstIff(SCharArray *dest, AstIff *iff) {
    psprintf(dest, "if (");
    ppAstExpression(dest, iff->test);
    psprintf(dest, ") ");
    ppAstNest(dest, iff->consequent);
    psprintf(dest, " else ");
    ppAstNest(dest, iff->alternative);
}

static void ppAstPrint(SCharArray *dest, AstPrint *print) {
    psprintf(dest, "print(");
    ppAstExpression(dest, print->exp);
    psprintf(dest, ")");
}

static void ppAstStruct(SCharArray *dest, AstStruct *structure) {
    ppAstLookUpOrSymbol(dest, structure->symbol);
    psprintf(dest, "{ ");
    ppAstTaggedExpressions(dest, structure->expressions);
    psprintf(dest, " }");
}

static void ppAstTaggedExpressions(SCharArray *dest,
                                   AstTaggedExpressions *taggedExpressions) {
    if (taggedExpressions) {
        ppHashSymbol(dest, taggedExpressions->tag);
        psprintf(dest, ": (");
        ppAstExpression(dest, taggedExpressions->expression);
        psprintf(dest, ")");
        if (taggedExpressions->next) {
            psprintf(dest, ", ");
            ppAstTaggedExpressions(dest, taggedExpressions->next);
        }
    }
}

void ppAstExpression(SCharArray *dest, AstExpression *expr) {
    switch (expr->type) {
    case AST_EXPRESSION_TYPE_NUMBER:
        ppMaybeBigInt(dest, expr->val.number);
        break;
    case AST_EXPRESSION_TYPE_SYMBOL:
        psprintf(dest, "%s", expr->val.symbol->name);
        break;
    case AST_EXPRESSION_TYPE_ANNOTATEDSYMBOL:
        psprintf(dest, "%s", expr->val.annotatedSymbol->symbol->name);
        psprintf(dest, "/*orig:");
        ppAstExpression(dest, expr->val.annotatedSymbol->originalImpl);
        psprintf(dest, "*/");
        break;
    case AST_EXPRESSION_TYPE_FUNCALL:
        ppAstFunCall(dest, expr->val.funCall);
        break;
    case AST_EXPRESSION_TYPE_CHARACTER:
        ppAstCharacter(dest, expr->val.character);
        break;
    case AST_EXPRESSION_TYPE_TUPLE:
        ppAstTuple(dest, expr->val.tuple);
        break;
    case AST_EXPRESSION_TYPE_FUN:
        ppAstCompositeFunction(dest, expr->val.fun);
        break;
    case AST_EXPRESSION_TYPE_IFF:
        ppAstIff(dest, expr->val.iff);
        break;
    case AST_EXPRESSION_TYPE_NEST:
        ppAstNest(dest, expr->val.nest);
        break;
    case AST_EXPRESSION_TYPE_BACK:
        psprintf(dest, "back");
        break;
    case AST_EXPRESSION_TYPE_ENV:
        psprintf(dest, "env");
        break;
    case AST_EXPRESSION_TYPE_LOOKUP:
        ppAstLookUp(dest, expr->val.lookUp);
        break;
    case AST_EXPRESSION_TYPE_PRINT:
        ppAstPrint(dest, expr->val.print);
        break;
    case AST_EXPRESSION_TYPE_STRUCTURE:
        ppAstStruct(dest, expr->val.structure);
        break;
    case AST_EXPRESSION_TYPE_ASSERTION:
        psprintf(dest, "assert(");
        ppAstExpression(dest, expr->val.assertion);
        psprintf(dest, ")");
        break;
    case AST_EXPRESSION_TYPE_ERROR:
        psprintf(dest, "error(");
        ppAstExpression(dest, expr->val.error);
        psprintf(dest, ")");
        break;
    case AST_EXPRESSION_TYPE_TYPEOF:
        psprintf(dest, "(typeof ");
        ppAstExpression(dest, expr->val.typeOf->exp);
        psprintf(dest, ")");
        break;
    default:
        cant_happen("unexpected %s", astExpressionTypeName(expr->type));
    }
}

void psprintf(SCharArray *utf8, const char *message, ...) {
    va_list args;
    va_start(args, message);
    va_list copy;
    va_copy(copy, args);
    size_t size = vsnprintf(NULL, 0, message, args) + 1;
    extendSCharArray(utf8, utf8->size + size);
    char *start = &utf8->entries[utf8->size];
    vsnprintf(start, size, message, copy);
    va_end(args);
    va_end(copy);
    utf8->size += size;
    utf8->size--;
}

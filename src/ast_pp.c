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

#include <stdio.h>
#include <stdarg.h>

#include "pratt.h"
#include "pratt_parser.h"
#include "pratt_debug.h"
#include "pratt_functions.h"
#include "pratt_scanner.h"
#include "symbols.h"
#include "ast.h"
#include "bigint.h"
#include "utf8.h"
#include "print_generator.h"
#include "file_id.h"
#include "ast_pp.h"

static void ppAstDefMacro(PrattUTF8 *, AstDefMacro *);
static void ppAstDefMulti(PrattUTF8 *, AstMultiDefine *);
static void ppAstDefinitions(PrattUTF8 *, AstDefinitions *);
static void ppAstDefinition(PrattUTF8 *, AstDefinition *);
static void ppAstDefine(PrattUTF8 *, AstDefine *);
static void ppAstTypeDef(PrattUTF8 *, AstTypeDef *);
static void ppAstAlias(PrattUTF8 *, AstAlias *);
static void ppAstTypeSig(PrattUTF8 *, AstTypeSig *);
static void ppAstType(PrattUTF8 *, AstType *);
static void ppAstTypeBody(PrattUTF8 *, AstTypeBody *);
static void ppAstTypeConstructor(PrattUTF8 *, AstTypeConstructor *);
static void ppAstTypeConstructorArgs(PrattUTF8 *, AstTypeConstructorArgs *);
static void ppAstTypeList(PrattUTF8 *, AstTypeList *);
static void ppAstTypeMap(PrattUTF8 *, AstTypeMap *);
static void ppAstExpressions(PrattUTF8 *, AstExpressions *);
static void ppAstTypeSymbols(PrattUTF8 *, AstTypeSymbols *);
static void ppAstTypeClause(PrattUTF8 *, AstTypeClause *);
static void ppAstTypeFunction(PrattUTF8 *, AstTypeFunction *);
static void ppAstLookUpOrSymbol(PrattUTF8 *, AstLookUpOrSymbol *);
static void ppAstLookUpSymbol(PrattUTF8 *, AstLookUpSymbol *);
static void ppAstCompositeFunction(PrattUTF8 *, AstCompositeFunction *);
static void ppAstFunction(PrattUTF8 *, AstFunction *);
static void ppAstFargList(PrattUTF8 *, AstFargList *);
static void ppAstTaggedArgList(PrattUTF8 *, AstTaggedArgList *);
static void ppastFarg(PrattUTF8 *, AstFarg *);
static void ppAstNamedArg(PrattUTF8 *, AstNamedArg *);
static void ppAstUnpack(PrattUTF8 *, AstUnpack *);
static void ppAstUnpackStruct(PrattUTF8 *, AstUnpackStruct *);
static void ppAstIff(PrattUTF8 *, AstIff *);
static void ppAstLookUp(PrattUTF8 *, AstLookUp *);
static void ppAstPrint(PrattUTF8 *, AstPrint *);
static void ppAstStruct(PrattUTF8 *, AstStruct *);
static void ppAstTaggedExpressions(PrattUTF8 *, AstTaggedExpressions *);

static void ppMaybeBigInt(PrattUTF8 *, MaybeBigInt *);
static void ppUnicodeChar(PrattUTF8 *, Character);
static void ppHashSymbol(PrattUTF8 *, HashSymbol *);

void ppAstNest(PrattUTF8 *dest, AstNest *nest) {
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

void ppAstNameSpaceImpl(PrattUTF8 *dest, AstNameSpaceImpl *impl) {
    psprintf(dest, "\"%s\": {", impl->id->name);
    ppAstDefinitions(dest, impl->definitions);
    psprintf(dest, "}");
}

void ppAstProg(PrattUTF8 *dest, AstProg *prog) {
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

void ppAstDefinitions(PrattUTF8 *dest, AstDefinitions *definitions) {
    while (definitions) {
        ppAstDefinition(dest, definitions->definition);
        psprintf(dest, "; ");
        definitions = definitions->next;
    }
}

static void ppAstDefinition(PrattUTF8 *dest, AstDefinition *definition) {
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
        case AST_DEFINITION_TYPE_MACRO:
            ppAstDefMacro(dest, definition->val.macro);
            break;
        case AST_DEFINITION_TYPE_MULTI:
            ppAstDefMulti(dest, definition->val.multi);
            break;
        default:
            cant_happen("unrecognised %s", astDefinitionTypeName(definition->type));
    }
}

static void ppAstDefMacro(PrattUTF8 *dest, AstDefMacro *defMacro) {
    psprintf(dest, "macro ");
    ppHashSymbol(dest, defMacro->name);
    psprintf(dest, "(");
    ppAstFargList(dest, defMacro->definition->altArgs->argList);
    psprintf(dest, ") ");
    ppAstNest(dest, defMacro->definition->nest);
}

static void ppAstSymbolList(PrattUTF8 *dest, AstSymbolList *symbolList) {
    if (symbolList) {
        ppHashSymbol(dest, symbolList->symbol);
        if (symbolList->next) {
            psprintf(dest, ", ");
            ppAstSymbolList(dest, symbolList->next);
        }
    }
}

static void ppAstDefMulti(PrattUTF8 *dest, AstMultiDefine *define) {
    psprintf(dest, "#(");
    ppAstSymbolList(dest, define->symbols);
    psprintf(dest, ") = ");
    ppAstExpression(dest, define->expression);
    psprintf(dest, "; ");
}

static void ppAstDefine(PrattUTF8 *dest, AstDefine *define) {
    ppHashSymbol(dest, define->symbol);
    psprintf(dest, " = ");
    ppAstExpression(dest, define->expression);
}

static void ppAstTypeDef(PrattUTF8 *dest, AstTypeDef *typeDef) {
    psprintf(dest, "typedef ");
    ppAstTypeSig(dest, typeDef->typeSig);
    psprintf(dest, " {");
    ppAstTypeBody(dest, typeDef->typeBody);
    psprintf(dest, "}");
}

static void ppAstAlias(PrattUTF8 *dest, AstAlias *alias) {
    psprintf(dest, "alias ");
    ppHashSymbol(dest, alias->name);
    psprintf(dest, " = ");
    ppAstType(dest, alias->type);
}

static void ppAstTypeSig(PrattUTF8 *dest, AstTypeSig *typeSig) {
    ppHashSymbol(dest, typeSig->symbol);
    if (typeSig->typeSymbols != NULL) {
        psprintf(dest, "(");
        ppAstTypeSymbols(dest, typeSig->typeSymbols);
        psprintf(dest, ")");
    }
}

static void ppAstType(PrattUTF8 *dest, AstType *type) {
    if (type != NULL) {
        ppAstTypeClause(dest, type->typeClause);
        if (type->next) {
            psprintf(dest, ", ");
            ppAstType(dest, type->next);
        }
    }
}

static void ppAstTypeBody(PrattUTF8 *dest, AstTypeBody *typeBody) {
    if (typeBody != NULL) {
        ppAstTypeConstructor(dest, typeBody->typeConstructor);
        if (typeBody->next) {
            psprintf(dest, " | ");
            ppAstTypeBody(dest, typeBody->next);
        }
    }
}

static void ppAstTypeConstructor(PrattUTF8 *dest, AstTypeConstructor *typeConstructor) {
    ppHashSymbol(dest, typeConstructor->symbol);
    ppAstTypeConstructorArgs(dest, typeConstructor->args);
}

static void ppAstTypeConstructorArgs(PrattUTF8 *dest, AstTypeConstructorArgs *typeConstructorArgs) {
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
                cant_happen("unrecognised %s", astTypeConstructorArgsTypeName(typeConstructorArgs->type));
        }
    }
}

static void ppAstTypeList(PrattUTF8 *dest, AstTypeList *typeList) {
    if (typeList) {
        ppAstType(dest, typeList->type);
        if (typeList->next) {
            psprintf(dest, ", ");
            ppAstTypeList(dest, typeList->next);
        }
    }
}

static void ppAstTypeMap(PrattUTF8 *dest, AstTypeMap *typeMap) {
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

static void ppAstExpressions(PrattUTF8 *dest, AstExpressions *expressions) {
    if (expressions) {
        ppAstExpression(dest, expressions->expression);
        psprintf(dest, "; ");
        if (expressions->next) {
            ppAstExpressions(dest, expressions->next);
        }
    }
}

static void ppAstTypeSymbols(PrattUTF8 *dest, AstTypeSymbols *typeSymbols) {
    if (typeSymbols) {
        ppHashSymbol(dest, typeSymbols->typeSymbol);
        if (typeSymbols->next) {
            psprintf(dest, ", ");
            ppAstTypeSymbols(dest, typeSymbols->next);
        }
    }
}

static void ppAstTypeClause(PrattUTF8 *dest, AstTypeClause *typeClause) {
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

static void ppAstTypeFunction(PrattUTF8 *dest, AstTypeFunction *typeFunction) {
    ppAstLookUpOrSymbol(dest, typeFunction->symbol);
    if (typeFunction->typeList) {
        psprintf(dest, "(");
        ppAstTypeList(dest, typeFunction->typeList);
        psprintf(dest, ")");
    }
}

static void ppAstLookUpOrSymbol(PrattUTF8 *dest, AstLookUpOrSymbol *lookUpOrSymbol) {
    switch (lookUpOrSymbol->type) {
        case AST_LOOKUPORSYMBOL_TYPE_LOOKUP:
            ppAstLookUpSymbol(dest, lookUpOrSymbol->val.lookUp);
            break;
        case AST_LOOKUPORSYMBOL_TYPE_SYMBOL:
            ppHashSymbol(dest, lookUpOrSymbol->val.symbol);
            break;
        default:
            cant_happen("unrecognised %s", astLookUpOrSymbolTypeName(lookUpOrSymbol->type));
    }
}

static void ppAstLookUpSymbol(PrattUTF8 *dest, AstLookUpSymbol *lookUpSymbol) {
    ppHashSymbol(dest, lookUpSymbol->nsSymbol);
    psprintf(dest, "<%d>.", lookUpSymbol->nsid);
    ppHashSymbol(dest, lookUpSymbol->symbol);
}

static void ppAstLookUp(PrattUTF8 *dest, AstLookUp *lookUp) {
    ppHashSymbol(dest, lookUp->nsSymbol);
    psprintf(dest, "<%d>.", lookUp->nsid);
    ppAstExpression(dest, lookUp->expression);
}

static void ppFunctionComponents(PrattUTF8 *dest, AstCompositeFunction *compositeFunction) {
    if (compositeFunction) {
        ppAstFunction(dest, compositeFunction->function);
        psprintf(dest, " ");
        ppFunctionComponents(dest, compositeFunction->next);
    }
}

static void ppAstCompositeFunction(PrattUTF8 *dest, AstCompositeFunction *compositeFunction) {
    if (compositeFunction == NULL) return;
    if (compositeFunction->unsafe) {
        psprintf(dest, "unsafe ");
    }
    psprintf(dest, "fn { ");
    ppFunctionComponents(dest, compositeFunction);
    psprintf(dest, "}");
}

static void ppAstFunction(PrattUTF8 *dest, AstFunction *function) {
    psprintf(dest, "(");
    ppAstFargList(dest, function->argList);
    psprintf(dest, ") ");
    ppAstNest(dest, function->nest);
    
}

static void ppAstFargList(PrattUTF8 *dest, AstFargList *argList) {
    if (argList) {
        ppastFarg(dest, argList->arg);
        if (argList->next) {
            psprintf(dest, ", ");
            ppAstFargList(dest, argList->next);
        }
    }
}

static void ppastFarg(PrattUTF8 *dest, AstFarg *arg) {
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

static void ppAstNamedArg(PrattUTF8 *dest, AstNamedArg *namedArg) {
    ppHashSymbol(dest, namedArg->name);
    psprintf(dest, " = ");
    ppastFarg(dest, namedArg->arg);
}

static void ppAstUnpack(PrattUTF8 *dest, AstUnpack *unpack) {
    ppAstLookUpOrSymbol(dest, unpack->symbol);
    psprintf(dest, "(");
    ppAstFargList(dest, unpack->argList);
    psprintf(dest, ")");
}

static void ppAstUnpackStruct(PrattUTF8 *dest, AstUnpackStruct *unpackStruct) {
    ppAstLookUpOrSymbol(dest, unpackStruct->symbol);
    psprintf(dest, "{ ");
    ppAstTaggedArgList(dest, unpackStruct->argList);
    psprintf(dest, " }");
}

static void ppAstTaggedArgList(PrattUTF8 *dest, AstTaggedArgList *taggedArgList) {
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

static void ppMaybeBigInt(PrattUTF8 *dest, MaybeBigInt *maybe) {
    size_t size = printSizeMaybeBigInt(maybe);
    extendPrattUTF8(dest, dest->size + size);
    unsigned char *start = &dest->entries[dest->size];
    size = sprintMaybeBigInt((char *)start, maybe);
    dest->size += size;
    dest->size--;
}

static void ppUnicodeChar(PrattUTF8 *dest, Character c) {
    unsigned char buf[8];
    unsigned char *end = writeChar(buf, c);
    *end = 0;
    psprintf(dest, "%s", buf);
}

static void ppHashSymbol(PrattUTF8 *dest, HashSymbol *symbol) {
    psprintf(dest, "%s", symbol->name);
}

void ppAstFunCall(PrattUTF8 *dest, AstFunCall *funCall) {
    ppAstExpression(dest, funCall->function);
    psprintf(dest, "(");
    for (AstExpressions *expressions = funCall->arguments; expressions != NULL; expressions = expressions->next) {
        ppAstExpression(dest, expressions->expression);
        if (expressions->next) {
            psprintf(dest, ", ");
        }
    }
    psprintf(dest, ")");
}

void ppAstCharacter(PrattUTF8 *dest, Character c) {
    unsigned char buffer[8];
    unsigned char *end = writeChar(buffer, c);
    *end = 0;
    psprintf(dest, "'%s'", buffer);
}

void ppAstTuple(PrattUTF8 *dest, AstExpressions *expressions) {
    psprintf(dest, "<tuple>(");
    while (expressions) {
        ppAstExpression(dest, expressions->expression);
        if (expressions->next)
            psprintf(dest, ", ");
        expressions = expressions->next;
    }
    psprintf(dest, ")");
}

static void ppAstIff(PrattUTF8 *dest, AstIff *iff) {
    psprintf(dest, "if (");
    ppAstExpression(dest, iff->test);
    psprintf(dest, ") ");
    ppAstNest(dest, iff->consequent);
    psprintf(dest, " else ");
    ppAstNest(dest, iff->alternative);
}

static void ppAstPrint(PrattUTF8 *dest, AstPrint *print) {
    psprintf(dest, "print(");
    ppAstExpression(dest, print->exp);
    psprintf(dest, ")");
}

static void ppAstStruct(PrattUTF8 *dest, AstStruct *structure) {
    ppAstLookUpOrSymbol(dest, structure->symbol);
    psprintf(dest, "{ ");
    ppAstTaggedExpressions(dest, structure->expressions);
    psprintf(dest, " }");
}

static void ppAstTaggedExpressions(PrattUTF8 *dest, AstTaggedExpressions *taggedExpressions) {
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

void ppAstExpression(PrattUTF8 *dest, AstExpression *expr) {
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

void psprintf(PrattUTF8 *utf8, const char *message, ...) {
    va_list args;
    va_start(args, message);
    va_list copy;
    va_copy(copy, args);
    size_t size = vsnprintf(NULL, 0, message, args) + 1;
    extendPrattUTF8(utf8, utf8->size + size);
    unsigned char *start = &utf8->entries[utf8->size];
    vsnprintf((char *)start, size, message, copy);
    va_end(args);
    va_end(copy);
    utf8->size += size;
    utf8->size--;
}

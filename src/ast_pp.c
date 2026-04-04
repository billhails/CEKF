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
#include "utils_helper.h"

static void ppAstDefLazy(FILE *out, AstDefLazy *);
static void ppAstDefMulti(FILE *out, AstMultiDefine *);
static void ppAstDefinitions(FILE *out, AstDefinitions *);
static void ppAstDefinition(FILE *out, AstDefinition *);
static void ppAstDefine(FILE *out, AstDefine *);
static void ppAstTypeDef(FILE *out, AstTypeDef *);
static void ppAstAlias(FILE *out, AstAlias *);
static void ppAstTypeSig(FILE *out, AstTypeSig *);
static void ppAstType(FILE *out, AstType *);
static void ppAstTypeBody(FILE *out, AstTypeBody *);
static void ppAstTypeConstructor(FILE *out, AstTypeConstructor *);
static void ppAstTypeConstructorArgs(FILE *out, AstTypeConstructorArgs *);
static void ppAstTypeList(FILE *out, AstTypeList *);
static void ppAstTypeMap(FILE *out, AstTypeMap *);
static void ppAstExpressions(FILE *out, AstExpressions *);
static void ppAstTypeSymbols(FILE *out, AstTypeSymbols *);
static void ppAstTypeClause(FILE *out, AstTypeClause *);
static void ppAstTypeFunction(FILE *out, AstTypeFunction *);
static void ppAstLookUpOrSymbol(FILE *out, AstLookUpOrSymbol *);
static void ppAstLookUpSymbol(FILE *out, AstLookUpSymbol *);
static void ppAstCompositeFunction(FILE *out, AstCompositeFunction *);
static void ppAstFunction(FILE *out, AstFunction *);
static void ppAstFargList(FILE *out, AstFargList *);
static void ppAstTaggedArgList(FILE *out, AstTaggedArgList *);
static void ppastFarg(FILE *out, AstFarg *);
static void ppAstNamedArg(FILE *out, AstNamedArg *);
static void ppAstUnpack(FILE *out, AstUnpack *);
static void ppAstUnpackStruct(FILE *out, AstUnpackStruct *);
static void ppAstIff(FILE *out, AstIff *);
static void ppAstLookUp(FILE *out, AstLookUp *);
static void ppAstPrint(FILE *out, AstPrint *);
static void ppAstStruct(FILE *out, AstStruct *);
static void ppAstTaggedExpressions(FILE *out, AstTaggedExpressions *);

static void ppMaybeBigInt(FILE *out, MaybeBigInt *);
static void ppUnicodeChar(FILE *out, Character);
static void ppAstHashSymbol(FILE *out, HashSymbol *);

void ppAstNest(FILE *out, AstNest *nest) {
    fprintf(out, "{ ");
    if (nest) {
        if (nest->definitions) {
            if (nest->expressions) {
                fprintf(out, "let ");
                ppAstDefinitions(out, nest->definitions);
                fprintf(out, "in ");
                ppAstExpressions(out, nest->expressions);
            } else {
                fprintf(out, "nameSpace ");
                ppAstDefinitions(out, nest->definitions);
            }
        } else {
            ppAstExpressions(out, nest->expressions);
        }
    }
    fprintf(out, "}");
}

void ppAstNameSpaceImpl(FILE *out, AstNameSpaceImpl *impl) {
    fprintf(out, "\"%u:%u:%lu\": {", major(impl->id->stDev),
            minor(impl->id->stDev), impl->id->stIno);
    ppAstDefinitions(out, impl->definitions);
    fprintf(out, "}");
}

void ppAstProg(FILE *out, AstProg *prog) {
    fprintf(out, "preamble: {");
    ppAstDefinitions(out, prog->preamble);
    fprintf(out, "} nameSpaces: [");
    for (Index i = 0; i < prog->nameSpaces->size; ++i) {
        ppAstNameSpaceImpl(out, prog->nameSpaces->entries[i]);
    }
    fprintf(out, "] body: {");
    ppAstExpressions(out, prog->body);
    fprintf(out, "}");
}

void ppAstDefinitions(FILE *out, AstDefinitions *definitions) {
    while (definitions) {
        ppAstDefinition(out, definitions->definition);
        fprintf(out, "; ");
        definitions = definitions->next;
    }
}

static void ppAstDefinition(FILE *out, AstDefinition *definition) {
    switch (definition->type) {
    case AST_DEFINITION_TYPE_DEFINE:
        ppAstDefine(out, definition->val.define);
        break;
    case AST_DEFINITION_TYPE_TYPEDEF:
        ppAstTypeDef(out, definition->val.typeDef);
        break;
    case AST_DEFINITION_TYPE_ALIAS:
        ppAstAlias(out, definition->val.alias);
        break;
    case AST_DEFINITION_TYPE_BLANK:
        break;
    case AST_DEFINITION_TYPE_LAZY:
        ppAstDefLazy(out, definition->val.lazy);
        break;
    case AST_DEFINITION_TYPE_MULTI:
        ppAstDefMulti(out, definition->val.multi);
        break;
    default:
        cant_happen("unrecognised %s", astDefinitionTypeName(definition->type));
    }
}

static void ppAstDefLazy(FILE *out, AstDefLazy *defLazy) {
    fprintf(out, "lazy fn ");
    ppAstHashSymbol(out, defLazy->name);
    fprintf(out, "(");
    ppAstFargList(out, defLazy->definition->altArgs->argList);
    fprintf(out, ") ");
    ppAstNest(out, defLazy->definition->nest);
}

static void ppAstSymbolList(FILE *out, AstSymbolList *symbolList) {
    if (symbolList) {
        ppAstHashSymbol(out, symbolList->symbol);
        if (symbolList->next) {
            fprintf(out, ", ");
            ppAstSymbolList(out, symbolList->next);
        }
    }
}

static void ppAstDefMulti(FILE *out, AstMultiDefine *define) {
    fprintf(out, "#(");
    ppAstSymbolList(out, define->symbols);
    fprintf(out, ") = ");
    ppAstExpression(out, define->expression);
    fprintf(out, "; ");
}

static void ppAstDefine(FILE *out, AstDefine *define) {
    ppAstHashSymbol(out, define->symbol);
    fprintf(out, " = ");
    ppAstExpression(out, define->expression);
}

static void ppAstTypeDef(FILE *out, AstTypeDef *typeDef) {
    fprintf(out, "typedef ");
    ppAstTypeSig(out, typeDef->typeSig);
    fprintf(out, " {");
    ppAstTypeBody(out, typeDef->typeBody);
    fprintf(out, "}");
}

static void ppAstAlias(FILE *out, AstAlias *alias) {
    fprintf(out, "alias ");
    ppAstHashSymbol(out, alias->name);
    fprintf(out, " = ");
    ppAstType(out, alias->type);
}

static void ppAstTypeSig(FILE *out, AstTypeSig *typeSig) {
    ppAstHashSymbol(out, typeSig->symbol);
    if (typeSig->typeSymbols != NULL) {
        fprintf(out, "(");
        ppAstTypeSymbols(out, typeSig->typeSymbols);
        fprintf(out, ")");
    }
}

static void ppAstType(FILE *out, AstType *type) {
    if (type != NULL) {
        ppAstTypeClause(out, type->typeClause);
        if (type->next) {
            fprintf(out, ", ");
            ppAstType(out, type->next);
        }
    }
}

static void ppAstTypeBody(FILE *out, AstTypeBody *typeBody) {
    if (typeBody != NULL) {
        ppAstTypeConstructor(out, typeBody->typeConstructor);
        if (typeBody->next) {
            fprintf(out, " | ");
            ppAstTypeBody(out, typeBody->next);
        }
    }
}

static void ppAstTypeConstructor(FILE *out,
                                 AstTypeConstructor *typeConstructor) {
    ppAstHashSymbol(out, typeConstructor->symbol);
    ppAstTypeConstructorArgs(out, typeConstructor->args);
}

static void
ppAstTypeConstructorArgs(FILE *out,
                         AstTypeConstructorArgs *typeConstructorArgs) {
    if (typeConstructorArgs) {
        switch (typeConstructorArgs->type) {
        case AST_TYPECONSTRUCTORARGS_TYPE_LIST:
            fprintf(out, "(");
            ppAstTypeList(out, typeConstructorArgs->val.list);
            fprintf(out, ")");
            break;
        case AST_TYPECONSTRUCTORARGS_TYPE_MAP:
            fprintf(out, "{ ");
            ppAstTypeMap(out, typeConstructorArgs->val.map);
            fprintf(out, " }");
            break;
        default:
            cant_happen("unrecognised %s", astTypeConstructorArgsTypeName(
                                               typeConstructorArgs->type));
        }
    }
}

static void ppAstTypeList(FILE *out, AstTypeList *typeList) {
    if (typeList) {
        ppAstType(out, typeList->type);
        if (typeList->next) {
            fprintf(out, ", ");
            ppAstTypeList(out, typeList->next);
        }
    }
}

static void ppAstTypeMap(FILE *out, AstTypeMap *typeMap) {
    if (typeMap) {
        ppAstHashSymbol(out, typeMap->key);
        fprintf(out, ": ");
        ppAstType(out, typeMap->type);
        if (typeMap->next) {
            fprintf(out, ", ");
            ppAstTypeMap(out, typeMap->next);
        }
    }
}

static void ppAstExpressions(FILE *out, AstExpressions *expressions) {
    if (expressions) {
        ppAstExpression(out, expressions->expression);
        fprintf(out, "; ");
        if (expressions->next) {
            ppAstExpressions(out, expressions->next);
        }
    }
}

static void ppAstTypeSymbols(FILE *out, AstTypeSymbols *typeSymbols) {
    if (typeSymbols) {
        ppAstHashSymbol(out, typeSymbols->typeSymbol);
        if (typeSymbols->next) {
            fprintf(out, ", ");
            ppAstTypeSymbols(out, typeSymbols->next);
        }
    }
}

static void ppAstTypeClause(FILE *out, AstTypeClause *typeClause) {
    switch (typeClause->type) {
    case AST_TYPECLAUSE_TYPE_INTEGER:
        fprintf(out, "number");
        break;
    case AST_TYPECLAUSE_TYPE_CHARACTER:
        fprintf(out, "char");
        break;
    case AST_TYPECLAUSE_TYPE_VAR:
        ppAstHashSymbol(out, typeClause->val.var);
        break;
    case AST_TYPECLAUSE_TYPE_TYPEFUNCTION:
        ppAstTypeFunction(out, typeClause->val.typeFunction);
        break;
    case AST_TYPECLAUSE_TYPE_TYPETUPLE:
        fprintf(out, "<tuple>(");
        ppAstTypeList(out, typeClause->val.typeTuple);
        fprintf(out, ")");
        break;
    default:
        cant_happen("unrecognised %s", astTypeClauseTypeName(typeClause->type));
    }
}

static void ppAstTypeFunction(FILE *out, AstTypeFunction *typeFunction) {
    ppAstLookUpOrSymbol(out, typeFunction->symbol);
    if (typeFunction->typeList) {
        fprintf(out, "(");
        ppAstTypeList(out, typeFunction->typeList);
        fprintf(out, ")");
    }
}

static void ppAstLookUpOrSymbol(FILE *out, AstLookUpOrSymbol *lookUpOrSymbol) {
    switch (lookUpOrSymbol->type) {
    case AST_LOOKUPORSYMBOL_TYPE_LOOKUP:
        ppAstLookUpSymbol(out, lookUpOrSymbol->val.lookUp);
        break;
    case AST_LOOKUPORSYMBOL_TYPE_SYMBOL:
        ppAstHashSymbol(out, lookUpOrSymbol->val.symbol);
        break;
    default:
        cant_happen("unrecognised %s",
                    astLookUpOrSymbolTypeName(lookUpOrSymbol->type));
    }
}

static void ppAstLookUpSymbol(FILE *out, AstLookUpSymbol *lookUpSymbol) {
    ppAstHashSymbol(out, lookUpSymbol->nsSymbol);
    fprintf(out, "<%d>.", lookUpSymbol->nsId);
    ppAstHashSymbol(out, lookUpSymbol->symbol);
}

static void ppAstLookUp(FILE *out, AstLookUp *lookUp) {
    ppAstHashSymbol(out, lookUp->nsSymbol);
    fprintf(out, "<%d>.", lookUp->nsId);
    ppAstExpression(out, lookUp->expression);
}

static void ppFunctionComponents(FILE *out,
                                 AstCompositeFunction *compositeFunction) {
    if (compositeFunction) {
        ppAstFunction(out, compositeFunction->function);
        fprintf(out, " ");
        ppFunctionComponents(out, compositeFunction->next);
    }
}

static void ppAstCompositeFunction(FILE *out,
                                   AstCompositeFunction *compositeFunction) {
    if (compositeFunction == NULL)
        return;
    if (compositeFunction->unsafe) {
        fprintf(out, "unsafe ");
    }
    fprintf(out, "fn { ");
    ppFunctionComponents(out, compositeFunction);
    fprintf(out, "}");
}

static void ppAstFunction(FILE *out, AstFunction *function) {
    fprintf(out, "(");
    ppAstFargList(out, function->argList);
    fprintf(out, ") ");
    ppAstNest(out, function->nest);
}

static void ppAstFargList(FILE *out, AstFargList *argList) {
    if (argList) {
        ppastFarg(out, argList->arg);
        if (argList->next) {
            fprintf(out, ", ");
            ppAstFargList(out, argList->next);
        }
    }
}

static void ppastFarg(FILE *out, AstFarg *arg) {
    switch (arg->type) {
    case AST_FARG_TYPE_WILDCARD:
        fprintf(out, "_");
        break;
    case AST_FARG_TYPE_SYMBOL:
        ppAstHashSymbol(out, arg->val.symbol);
        break;
    case AST_FARG_TYPE_LOOKUP:
        ppAstLookUpSymbol(out, arg->val.lookUp);
        break;
    case AST_FARG_TYPE_NAMED:
        ppAstNamedArg(out, arg->val.named);
        break;
    case AST_FARG_TYPE_UNPACK:
        ppAstUnpack(out, arg->val.unpack);
        break;
    case AST_FARG_TYPE_UNPACKSTRUCT:
        ppAstUnpackStruct(out, arg->val.unpackStruct);
        break;
    case AST_FARG_TYPE_NUMBER:
        ppMaybeBigInt(out, arg->val.number);
        break;
    case AST_FARG_TYPE_CHARACTER:
        ppUnicodeChar(out, arg->val.character);
        break;
    case AST_FARG_TYPE_TUPLE:
        fprintf(out, "<tuple>(");
        ppAstFargList(out, arg->val.tuple);
        fprintf(out, ")");
        break;
    default:
        break;
    }
}

static void ppAstNamedArg(FILE *out, AstNamedArg *namedArg) {
    ppAstHashSymbol(out, namedArg->name);
    fprintf(out, " = ");
    ppastFarg(out, namedArg->arg);
}

static void ppAstUnpack(FILE *out, AstUnpack *unpack) {
    ppAstLookUpOrSymbol(out, unpack->symbol);
    fprintf(out, "(");
    ppAstFargList(out, unpack->argList);
    fprintf(out, ")");
}

static void ppAstUnpackStruct(FILE *out, AstUnpackStruct *unpackStruct) {
    ppAstLookUpOrSymbol(out, unpackStruct->symbol);
    fprintf(out, "{ ");
    ppAstTaggedArgList(out, unpackStruct->argList);
    fprintf(out, " }");
}

static void ppAstTaggedArgList(FILE *out, AstTaggedArgList *taggedArgList) {
    if (taggedArgList) {
        ppAstHashSymbol(out, taggedArgList->tag);
        fprintf(out, ": ");
        ppastFarg(out, taggedArgList->arg);
        if (taggedArgList->next) {
            fprintf(out, ", ");
            ppAstTaggedArgList(out, taggedArgList->next);
        }
    }
}

static void ppMaybeBigInt(FILE *out, MaybeBigInt *maybe) {
    fprintMaybeBigInt2(out, maybe);
}

static void ppUnicodeChar(FILE *out, Character c) {
    char buf[MB_LEN_MAX];
    int len = wctomb(buf, c);
    if (len > 0) {
        buf[len] = '\0';
        fprintf(out, "%s", buf);
    }
}

static void ppAstHashSymbol(FILE *out, HashSymbol *symbol) {
    fprintf(out, "%s", symbol->name);
}

void ppAstFunCall(FILE *out, AstFunCall *funCall) {
    ppAstExpression(out, funCall->function);
    fprintf(out, "(");
    for (AstExpressions *expressions = funCall->arguments; expressions != NULL;
         expressions = expressions->next) {
        ppAstExpression(out, expressions->expression);
        if (expressions->next) {
            fprintf(out, ", ");
        }
    }
    fprintf(out, ")");
}

void ppAstCharacter(FILE *out, Character c) {
    char buffer[MB_LEN_MAX];
    int len = wctomb(buffer, c);
    if (len > 0) {
        buffer[len] = '\0';
        fprintf(out, "'%s'", buffer);
    }
}

void ppAstTuple(FILE *out, AstExpressions *expressions) {
    fprintf(out, "<tuple>(");
    while (expressions) {
        ppAstExpression(out, expressions->expression);
        if (expressions->next)
            fprintf(out, ", ");
        expressions = expressions->next;
    }
    fprintf(out, ")");
}

static void ppAstIff(FILE *out, AstIff *iff) {
    fprintf(out, "if (");
    ppAstExpression(out, iff->test);
    fprintf(out, ") ");
    ppAstNest(out, iff->consequent);
    fprintf(out, " else ");
    ppAstNest(out, iff->alternative);
}

static void ppAstPrint(FILE *out, AstPrint *print) {
    fprintf(out, "print(");
    ppAstExpression(out, print->exp);
    fprintf(out, ")");
}

static void ppAstStruct(FILE *out, AstStruct *structure) {
    ppAstLookUpOrSymbol(out, structure->symbol);
    fprintf(out, "{ ");
    ppAstTaggedExpressions(out, structure->expressions);
    fprintf(out, " }");
}

static void ppAstTaggedExpressions(FILE *out,
                                   AstTaggedExpressions *taggedExpressions) {
    if (taggedExpressions) {
        ppAstHashSymbol(out, taggedExpressions->tag);
        fprintf(out, ": (");
        ppAstExpression(out, taggedExpressions->expression);
        fprintf(out, ")");
        if (taggedExpressions->next) {
            fprintf(out, ", ");
            ppAstTaggedExpressions(out, taggedExpressions->next);
        }
    }
}

void ppAstExpression(FILE *out, AstExpression *expr) {
    switch (expr->type) {
    case AST_EXPRESSION_TYPE_NUMBER:
        ppMaybeBigInt(out, expr->val.number);
        break;
    case AST_EXPRESSION_TYPE_SYMBOL:
        fprintf(out, "%s", expr->val.symbol->name);
        break;
    case AST_EXPRESSION_TYPE_ANNOTATEDSYMBOL:
        fprintf(out, "%s", expr->val.annotatedSymbol->symbol->name);
        fprintf(out, "/*orig:");
        ppAstExpression(out, expr->val.annotatedSymbol->originalImpl);
        fprintf(out, "*/");
        break;
    case AST_EXPRESSION_TYPE_FUNCALL:
        ppAstFunCall(out, expr->val.funCall);
        break;
    case AST_EXPRESSION_TYPE_CHARACTER:
        ppAstCharacter(out, expr->val.character);
        break;
    case AST_EXPRESSION_TYPE_TUPLE:
        ppAstTuple(out, expr->val.tuple);
        break;
    case AST_EXPRESSION_TYPE_FUN:
        ppAstCompositeFunction(out, expr->val.fun);
        break;
    case AST_EXPRESSION_TYPE_IFF:
        ppAstIff(out, expr->val.iff);
        break;
    case AST_EXPRESSION_TYPE_NEST:
        ppAstNest(out, expr->val.nest);
        break;
    case AST_EXPRESSION_TYPE_BACK:
        fprintf(out, "back");
        break;
    case AST_EXPRESSION_TYPE_ENV:
        fprintf(out, "env");
        break;
    case AST_EXPRESSION_TYPE_LOOKUP:
        ppAstLookUp(out, expr->val.lookUp);
        break;
    case AST_EXPRESSION_TYPE_PRINT:
        ppAstPrint(out, expr->val.print);
        break;
    case AST_EXPRESSION_TYPE_STRUCTURE:
        ppAstStruct(out, expr->val.structure);
        break;
    case AST_EXPRESSION_TYPE_ASSERTION:
        fprintf(out, "assert(");
        ppAstExpression(out, expr->val.assertion);
        fprintf(out, ")");
        break;
    case AST_EXPRESSION_TYPE_ERROR:
        fprintf(out, "error(");
        ppAstExpression(out, expr->val.error);
        fprintf(out, ")");
        break;
    case AST_EXPRESSION_TYPE_TYPEOF:
        fprintf(out, "(typeof ");
        ppAstExpression(out, expr->val.typeOf->exp);
        fprintf(out, ")");
        break;
    default:
        cant_happen("unexpected %s", astExpressionTypeName(expr->type));
    }
}
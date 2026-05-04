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
static void ppSymbolArray(FILE *out, SymbolArray *);
static void ppAstSyntaxDecl(FILE *out, AstSyntaxDecl *);
static void ppAstExprSyntaxUse(FILE *out, AstExprSyntaxUse *);
static void ppAstDefSyntaxUse(FILE *out, AstDefSyntaxUse *);
static void ppAstSyntaxAlternatives(FILE *out, AstSyntaxAlternatives *);
static void ppAstSyntaxAlternative(FILE *out, AstSyntaxAlternative *);
static void ppAstSyntaxPatternItems(FILE *out, AstSyntaxPatternItems *);
static void ppAstSyntaxPatternItem(FILE *out, AstSyntaxPatternItem *);
static void ppAstSyntaxHole(FILE *out, AstSyntaxHole *);
static void ppAstSyntaxBindings(FILE *out, AstSyntaxBindings *);
static void ppAstSyntaxBinding(FILE *out, AstSyntaxBinding *);
static void ppAstSyntaxTemplate(FILE *out, AstSyntaxTemplate *);
static void ppAstSyntaxTemplateDefinitions(FILE *out,
                                           AstSyntaxTemplateDefinitions *);
static void ppAstSyntaxTemplateDefinition(FILE *out,
                                          AstSyntaxTemplateDefinition *);
static void ppAstSyntaxTemplateNest(FILE *out, AstSyntaxTemplateNest *);
static void ppAstSyntaxTemplateExpr(FILE *out, AstSyntaxTemplateExpr *);
static void ppAstSyntaxTemplateExprs(FILE *out, AstSyntaxTemplateExprs *);
static void ppAstSyntaxTemplateNameRef(FILE *out, AstSyntaxTemplateNameRef *);
static void ppAstSyntaxTemplateBinder(FILE *out, AstSyntaxTemplateBinder *);
static void ppAstSyntaxTemplateBinders(FILE *out, AstSyntaxTemplateBinders *);
static void ppAstSyntaxTemplateFarg(FILE *out, AstSyntaxTemplateFarg *);
static void ppAstSyntaxTemplateFargList(FILE *out, AstSyntaxTemplateFargList *);
static void ppAstSyntaxTemplateTaggedArgs(FILE *out,
                                          AstSyntaxTemplateTaggedArgs *);
static void
ppAstSyntaxTemplateTaggedExpressions(FILE *out,
                                     AstSyntaxTemplateTaggedExpressions *);
static void ppAstSyntaxTemplateAltArgs(FILE *out, AstSyntaxTemplateAltArgs *);
static void ppAstSyntaxTemplateAltFunction(FILE *out,
                                           AstSyntaxTemplateAltFunction *);
static void ppAstSyntaxTemplateFunction(FILE *out, AstSyntaxTemplateFunction *);

static void ppMaybeBigInt(FILE *out, MaybeBigInt *);
static void ppUnicodeChar(FILE *out, Character);
static void ppAstHashSymbol(FILE *out, HashSymbol *);

static int sPpDepth = 0;

static void indent(FILE *out) {
    for (int i = 0; i < sPpDepth; ++i) {
        fprintf(out, "  ");
    }
}

static void newlineIndent(FILE *out) {
    fprintf(out, "\n");
    indent(out);
}

static void pushIndent(void) { sPpDepth++; }

static void popIndent(void) {
    if (sPpDepth > 0) {
        sPpDepth--;
    }
}

void ppAstNest(FILE *out, AstNest *nest) {
    fprintf(out, "{");
    if (nest != NULL &&
        (nest->definitions != NULL || nest->expressions != NULL)) {
        pushIndent();
        if (nest->definitions != NULL) {
            newlineIndent(out);
            if (nest->expressions != NULL) {
                fprintf(out, "let");
                pushIndent();
                newlineIndent(out);
                ppAstDefinitions(out, nest->definitions);
                popIndent();
                newlineIndent(out);
                fprintf(out, "in");
                pushIndent();
                newlineIndent(out);
                ppAstExpressions(out, nest->expressions);
                popIndent();
            } else {
                fprintf(out, "nameSpace");
                pushIndent();
                newlineIndent(out);
                ppAstDefinitions(out, nest->definitions);
                popIndent();
            }
        } else {
            newlineIndent(out);
            ppAstExpressions(out, nest->expressions);
        }
        popIndent();
        newlineIndent(out);
    }
    fprintf(out, "}");
}

void ppAstNameSpaceImpl(FILE *out, AstNameSpaceImpl *impl) {
    fprintf(out, "\"%u:%u:%lu\": {", major(impl->id->stDev),
            minor(impl->id->stDev), impl->id->stIno);
    if (impl->definitions != NULL) {
        pushIndent();
        newlineIndent(out);
        ppAstDefinitions(out, impl->definitions);
        popIndent();
        newlineIndent(out);
    }
    fprintf(out, "}");
}

void ppAstProg(FILE *out, AstProg *prog) {
    fprintf(out, "preamble: {");
    if (prog->preamble != NULL) {
        pushIndent();
        newlineIndent(out);
        ppAstDefinitions(out, prog->preamble);
        popIndent();
        newlineIndent(out);
    }
    fprintf(out, "}");
    newlineIndent(out);
    fprintf(out, "nameSpaces: [");
    if (prog->nameSpaces->size > 0) {
        pushIndent();
    }
    for (Index i = 0; i < prog->nameSpaces->size; ++i) {
        newlineIndent(out);
        ppAstNameSpaceImpl(out, prog->nameSpaces->entries[i]);
    }
    if (prog->nameSpaces->size > 0) {
        popIndent();
        newlineIndent(out);
    }
    fprintf(out, "]");
    newlineIndent(out);
    fprintf(out, "body: {");
    if (prog->body != NULL) {
        pushIndent();
        newlineIndent(out);
        ppAstExpressions(out, prog->body);
        popIndent();
        newlineIndent(out);
    }
    fprintf(out, "}");
}

void ppAstDefinitions(FILE *out, AstDefinitions *definitions) {
    bool first = true;
    while (definitions) {
        if (!first) {
            newlineIndent(out);
        }
        ppAstDefinition(out, definitions->definition);
        fprintf(out, ";");
        first = false;
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
    case AST_DEFINITION_TYPE_SYNTAXDECL:
        ppAstSyntaxDecl(out, definition->val.syntaxDecl);
        break;
    case AST_DEFINITION_TYPE_SYNTAXUSE:
        ppAstDefSyntaxUse(out, definition->val.syntaxUse);
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
    bool first = true;
    if (expressions) {
        while (expressions != NULL) {
            if (!first) {
                newlineIndent(out);
            }
            ppAstExpression(out, expressions->expression);
            fprintf(out, ";");
            first = false;
            expressions = expressions->next;
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
    bool first = true;
    while (compositeFunction != NULL) {
        if (!first) {
            newlineIndent(out);
        }
        ppAstFunction(out, compositeFunction->function);
        first = false;
        compositeFunction = compositeFunction->next;
    }
}

static void ppAstCompositeFunction(FILE *out,
                                   AstCompositeFunction *compositeFunction) {
    if (compositeFunction == NULL)
        return;
    if (compositeFunction->unsafe) {
        fprintf(out, "unsafe ");
    }
    fprintf(out, "fn {");
    pushIndent();
    newlineIndent(out);
    ppFunctionComponents(out, compositeFunction);
    popIndent();
    newlineIndent(out);
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

static void ppSymbolArray(FILE *out, SymbolArray *symbols) {
    if (symbols == NULL) {
        return;
    }
    for (Index i = 0; i < sizeSymbolArray(symbols); ++i) {
        ppAstHashSymbol(out, getSymbolArray(symbols, i));
        if (i + 1 < sizeSymbolArray(symbols)) {
            fprintf(out, ", ");
        }
    }
}

static void ppAstSyntaxDecl(FILE *out, AstSyntaxDecl *syntaxDecl) {
    fprintf(out, "syntax-decl[%d ", syntaxDecl->declarationId);
    ppAstHashSymbol(out, syntaxDecl->ruleName);
    fprintf(out, " entry=%s result=%s",
            astSyntaxEntryKindName(syntaxDecl->entryKind),
            astSyntaxResultKindName(syntaxDecl->resultKind));
    if (syntaxDecl->surfaceHead != NULL) {
        fprintf(out, " head=");
        ppAstHashSymbol(out, syntaxDecl->surfaceHead);
    }
    if (syntaxDecl->parameters != NULL &&
        sizeSymbolArray(syntaxDecl->parameters) > 0) {
        fprintf(out, " params=(");
        ppSymbolArray(out, syntaxDecl->parameters);
        fprintf(out, ")");
    }
    fprintf(out, "] {");
    if (syntaxDecl->alternatives != NULL &&
        sizeAstSyntaxAlternatives(syntaxDecl->alternatives) > 0) {
        pushIndent();
        newlineIndent(out);
        ppAstSyntaxAlternatives(out, syntaxDecl->alternatives);
        popIndent();
        newlineIndent(out);
    }
    fprintf(out, "}");
}

static void ppAstExprSyntaxUse(FILE *out, AstExprSyntaxUse *syntaxUse) {
    fprintf(out, "syntax-use-expr[decl=%d alt=%d]", syntaxUse->declarationId,
            syntaxUse->alternativeIndex);
    if (syntaxUse->bindings != NULL &&
        sizeAstSyntaxBindings(syntaxUse->bindings) > 0) {
        fprintf(out, " (");
        pushIndent();
        newlineIndent(out);
        ppAstSyntaxBindings(out, syntaxUse->bindings);
        popIndent();
        newlineIndent(out);
        fprintf(out, ")");
    }
}

static void ppAstDefSyntaxUse(FILE *out, AstDefSyntaxUse *syntaxUse) {
    fprintf(out, "syntax-use-def[decl=%d alt=%d]", syntaxUse->declarationId,
            syntaxUse->alternativeIndex);
    if (syntaxUse->bindings != NULL &&
        sizeAstSyntaxBindings(syntaxUse->bindings) > 0) {
        fprintf(out, " (");
        pushIndent();
        newlineIndent(out);
        ppAstSyntaxBindings(out, syntaxUse->bindings);
        popIndent();
        newlineIndent(out);
        fprintf(out, ")");
    }
}

static void ppAstSyntaxAlternatives(FILE *out,
                                    AstSyntaxAlternatives *alternatives) {
    if (alternatives == NULL) {
        return;
    }
    for (Index i = 0; i < sizeAstSyntaxAlternatives(alternatives); ++i) {
        if (i > 0) {
            newlineIndent(out);
            fprintf(out, "| ");
        }
        ppAstSyntaxAlternative(out, getAstSyntaxAlternatives(alternatives, i));
    }
}

static void ppAstSyntaxAlternative(FILE *out,
                                   AstSyntaxAlternative *alternative) {
    ppAstSyntaxPatternItems(out, alternative->patternItems);
    fprintf(out, " => ");
    ppAstSyntaxTemplate(out, alternative->template);
}

static void ppAstSyntaxPatternItems(FILE *out,
                                    AstSyntaxPatternItems *patternItems) {
    if (patternItems == NULL) {
        fprintf(out, "<empty-pattern>");
        return;
    }
    for (Index i = 0; i < sizeAstSyntaxPatternItems(patternItems); ++i) {
        ppAstSyntaxPatternItem(out, getAstSyntaxPatternItems(patternItems, i));
        if (i + 1 < sizeAstSyntaxPatternItems(patternItems)) {
            fprintf(out, " ");
        }
    }
}

static void ppAstSyntaxPatternItem(FILE *out,
                                   AstSyntaxPatternItem *patternItem) {
    switch (patternItem->type) {
    case AST_SYNTAXPATTERNITEM_TYPE_QUOTEDTERMINAL:
        fprintf(out, "terminal(");
        ppAstHashSymbol(out,
                        getAstSyntaxPatternItem_QuotedTerminal(patternItem));
        fprintf(out, ")");
        break;
    case AST_SYNTAXPATTERNITEM_TYPE_TYPEDHOLE:
        ppAstSyntaxHole(out, getAstSyntaxPatternItem_TypedHole(patternItem));
        break;
    default:
        cant_happen("unrecognised %s",
                    astSyntaxPatternItemTypeName(patternItem->type));
    }
}

static void ppAstSyntaxHole(FILE *out, AstSyntaxHole *hole) {
    fprintf(out, "hole(");
    ppAstHashSymbol(out, hole->name);
    fprintf(out, ": %s", astSyntaxClassName(hole->syntaxClass));
    if (hole->callTarget != NULL) {
        fprintf(out, " -> ");
        ppAstHashSymbol(out, hole->callTarget);
        if (hole->callArguments != NULL &&
            sizeSymbolArray(hole->callArguments) > 0) {
            fprintf(out, "(");
            ppSymbolArray(out, hole->callArguments);
            fprintf(out, ")");
        }
    }
    fprintf(out, ")");
}

static void ppAstSyntaxBindings(FILE *out, AstSyntaxBindings *bindings) {
    if (bindings == NULL) {
        return;
    }
    for (Index i = 0; i < sizeAstSyntaxBindings(bindings); ++i) {
        if (i > 0) {
            newlineIndent(out);
        }
        ppAstSyntaxBinding(out, getAstSyntaxBindings(bindings, i));
        if (i + 1 < sizeAstSyntaxBindings(bindings)) {
            fprintf(out, ",");
        }
    }
}

static void ppAstSyntaxBinding(FILE *out, AstSyntaxBinding *binding) {
    ppAstHashSymbol(out, binding->name);
    fprintf(out, binding->inherited ? " :=[inherited] " : " := ");
    ppAstExpression(out, binding->value);
}

static void ppAstSyntaxTemplate(FILE *out, AstSyntaxTemplate *syntaxTemplate) {
    if (syntaxTemplate == NULL) {
        fprintf(out, "<null-template>");
        return;
    }
    fprintf(out, "template[%s] ",
            astSyntaxResultKindName(syntaxTemplate->resultKind));
    if (syntaxTemplate->expr != NULL) {
        ppAstSyntaxTemplateExpr(out, syntaxTemplate->expr);
        return;
    }
    if (syntaxTemplate->definition != NULL) {
        ppAstSyntaxTemplateDefinition(out, syntaxTemplate->definition);
        return;
    }
    fprintf(out, "<empty>");
}

static void
ppAstSyntaxTemplateDefinitions(FILE *out,
                               AstSyntaxTemplateDefinitions *definitions) {
    bool first = true;
    while (definitions != NULL) {
        if (!first) {
            newlineIndent(out);
        }
        ppAstSyntaxTemplateDefinition(out, definitions->definition);
        if (definitions->next != NULL) {
            fprintf(out, ";");
        }
        first = false;
        definitions = definitions->next;
    }
}

static void
ppAstSyntaxTemplateDefinition(FILE *out,
                              AstSyntaxTemplateDefinition *definition) {
    switch (definition->type) {
    case AST_SYNTAXTEMPLATEDEFINITION_TYPE_DEFINE: {
        AstSyntaxTemplateDefine *define =
            getAstSyntaxTemplateDefinition_Define(definition);
        ppAstSyntaxTemplateBinder(out, define->symbol);
        fprintf(out, " = ");
        ppAstSyntaxTemplateExpr(out, define->expression);
        break;
    }
    case AST_SYNTAXTEMPLATEDEFINITION_TYPE_MULTI: {
        AstSyntaxTemplateMultiDefine *multi =
            getAstSyntaxTemplateDefinition_Multi(definition);
        fprintf(out, "#(");
        ppAstSyntaxTemplateBinders(out, multi->symbols);
        fprintf(out, ") = ");
        ppAstSyntaxTemplateExpr(out, multi->expression);
        break;
    }
    case AST_SYNTAXTEMPLATEDEFINITION_TYPE_LAZY: {
        AstSyntaxTemplateDefLazy *lazy =
            getAstSyntaxTemplateDefinition_Lazy(definition);
        fprintf(out, "lazy fn ");
        ppAstSyntaxTemplateBinder(out, lazy->name);
        fprintf(out, " ");
        ppAstSyntaxTemplateAltFunction(out, lazy->definition);
        break;
    }
    case AST_SYNTAXTEMPLATEDEFINITION_TYPE_ALIAS: {
        AstSyntaxTemplateAlias *alias =
            getAstSyntaxTemplateDefinition_Alias(definition);
        fprintf(out, "alias ");
        ppAstSyntaxTemplateBinder(out, alias->name);
        fprintf(out, " = ");
        ppAstType(out, alias->type);
        break;
    }
    case AST_SYNTAXTEMPLATEDEFINITION_TYPE_TYPEDEF: {
        AstSyntaxTemplateTypeDef *typeDef =
            getAstSyntaxTemplateDefinition_TypeDef(definition);
        fprintf(out, "typedef ");
        ppAstTypeSig(out, typeDef->typeSig);
        fprintf(out, " {");
        ppAstTypeBody(out, typeDef->typeBody);
        fprintf(out, "}");
        break;
    }
    default:
        cant_happen("unrecognised %s",
                    astSyntaxTemplateDefinitionTypeName(definition->type));
    }
}

static void ppAstSyntaxTemplateNameRef(FILE *out,
                                       AstSyntaxTemplateNameRef *nameRef) {
    switch (nameRef->type) {
    case AST_SYNTAXTEMPLATENAMEREF_TYPE_LITERAL: {
        AstSyntaxLiteralRef *literal =
            getAstSyntaxTemplateNameRef_Literal(nameRef);
        ppAstHashSymbol(out, literal->writtenName);
        if (literal->resolvedName != NULL &&
            literal->resolvedName != literal->writtenName) {
            fprintf(out, "->");
            ppAstHashSymbol(out, literal->resolvedName);
        }
        break;
    }
    case AST_SYNTAXTEMPLATENAMEREF_TYPE_INTRODUCED: {
        AstSyntaxIntroducedRef *introduced =
            getAstSyntaxTemplateNameRef_Introduced(nameRef);
        fprintf(out, "<introduced-ref:%d>", introduced->binderId);
        break;
    }
    case AST_SYNTAXTEMPLATENAMEREF_TYPE_UNQUOTE: {
        AstSyntaxUnquote *unquote =
            getAstSyntaxTemplateNameRef_Unquote(nameRef);
        fprintf(out, "unquote(");
        ppAstHashSymbol(out, unquote->bindingName);
        fprintf(out, ": %s)", astSyntaxClassName(unquote->syntaxClass));
        break;
    }
    default:
        cant_happen("unrecognised %s",
                    astSyntaxTemplateNameRefTypeName(nameRef->type));
    }
}

static void ppAstSyntaxTemplateBinder(FILE *out,
                                      AstSyntaxTemplateBinder *binder) {
    switch (binder->type) {
    case AST_SYNTAXTEMPLATEBINDER_TYPE_INTRODUCED: {
        AstSyntaxIntroducedBinder *introduced =
            getAstSyntaxTemplateBinder_Introduced(binder);
        fprintf(out, "<introduced-binder:%d:", introduced->binderId);
        ppAstHashSymbol(out, introduced->writtenName);
        fprintf(out, ">");
        break;
    }
    case AST_SYNTAXTEMPLATEBINDER_TYPE_UNQUOTE: {
        AstSyntaxUnquote *unquote = getAstSyntaxTemplateBinder_Unquote(binder);
        fprintf(out, "unquote(");
        ppAstHashSymbol(out, unquote->bindingName);
        fprintf(out, ": %s)", astSyntaxClassName(unquote->syntaxClass));
        break;
    }
    default:
        cant_happen("unrecognised %s",
                    astSyntaxTemplateBinderTypeName(binder->type));
    }
}

static void ppAstSyntaxTemplateBinders(FILE *out,
                                       AstSyntaxTemplateBinders *binders) {
    if (binders == NULL) {
        return;
    }
    for (Index i = 0; i < sizeAstSyntaxTemplateBinders(binders); ++i) {
        ppAstSyntaxTemplateBinder(out, getAstSyntaxTemplateBinders(binders, i));
        if (i + 1 < sizeAstSyntaxTemplateBinders(binders)) {
            fprintf(out, ", ");
        }
    }
}

static void ppAstSyntaxTemplateExprs(FILE *out,
                                     AstSyntaxTemplateExprs *expressions) {
    if (expressions == NULL) {
        return;
    }
    for (Index i = 0; i < sizeAstSyntaxTemplateExprs(expressions); ++i) {
        ppAstSyntaxTemplateExpr(out, getAstSyntaxTemplateExprs(expressions, i));
        if (i + 1 < sizeAstSyntaxTemplateExprs(expressions)) {
            fprintf(out, ", ");
        }
    }
}

static void ppAstSyntaxTemplateAltArgs(FILE *out,
                                       AstSyntaxTemplateAltArgs *altArgs) {
    while (altArgs != NULL) {
        fprintf(out, "(");
        ppAstSyntaxTemplateFargList(out, altArgs->argList);
        fprintf(out, ")");
        altArgs = altArgs->next;
    }
}

static void
ppAstSyntaxTemplateAltFunction(FILE *out,
                               AstSyntaxTemplateAltFunction *altFunction) {
    ppAstSyntaxTemplateAltArgs(out, altFunction->altArgs);
    fprintf(out, " ");
    ppAstSyntaxTemplateNest(out, altFunction->nest);
}

static void ppAstSyntaxTemplateFunction(FILE *out,
                                        AstSyntaxTemplateFunction *function) {
    if (function == NULL) {
        return;
    }
    if (function->unsafe) {
        fprintf(out, "unsafe ");
    }
    fprintf(out, "fn {");
    pushIndent();
    for (AstSyntaxTemplateFunction *cursor = function; cursor != NULL;
         cursor = cursor->next) {
        newlineIndent(out);
        ppAstSyntaxTemplateAltFunction(out, cursor->function);
    }
    popIndent();
    newlineIndent(out);
    fprintf(out, "}");
}

static void ppAstSyntaxTemplateNest(FILE *out, AstSyntaxTemplateNest *nest) {
    fprintf(out, "{");
    if (nest != NULL &&
        (nest->definitions != NULL || nest->expressions != NULL)) {
        pushIndent();
        if (nest->definitions != NULL) {
            newlineIndent(out);
            fprintf(out, "let");
            pushIndent();
            newlineIndent(out);
            ppAstSyntaxTemplateDefinitions(out, nest->definitions);
            popIndent();
            if (nest->expressions != NULL) {
                newlineIndent(out);
                fprintf(out, "in");
            }
        }
        if (nest->expressions != NULL) {
            pushIndent();
            newlineIndent(out);
            ppAstSyntaxTemplateExprs(out, nest->expressions);
            popIndent();
        }
        popIndent();
        newlineIndent(out);
    }
    fprintf(out, "}");
}

static void ppAstSyntaxTemplateExpr(FILE *out,
                                    AstSyntaxTemplateExpr *expression) {
    switch (expression->type) {
    case AST_SYNTAXTEMPLATEEXPR_TYPE_BACK:
        fprintf(out, "back");
        break;
    case AST_SYNTAXTEMPLATEEXPR_TYPE_WILDCARD:
        fprintf(out, "_");
        break;
    case AST_SYNTAXTEMPLATEEXPR_TYPE_NAMEREF:
        ppAstSyntaxTemplateNameRef(
            out, getAstSyntaxTemplateExpr_NameRef(expression));
        break;
    case AST_SYNTAXTEMPLATEEXPR_TYPE_NUMBER:
        ppMaybeBigInt(out, getAstSyntaxTemplateExpr_Number(expression));
        break;
    case AST_SYNTAXTEMPLATEEXPR_TYPE_CHARACTER:
        ppAstCharacter(out, getAstSyntaxTemplateExpr_Character(expression));
        break;
    case AST_SYNTAXTEMPLATEEXPR_TYPE_FUNCALL: {
        AstSyntaxTemplateFunCall *funCall =
            getAstSyntaxTemplateExpr_FunCall(expression);
        ppAstSyntaxTemplateExpr(out, funCall->function);
        fprintf(out, "(");
        ppAstSyntaxTemplateExprs(out, funCall->arguments);
        fprintf(out, ")");
        break;
    }
    case AST_SYNTAXTEMPLATEEXPR_TYPE_LOOKUP: {
        AstSyntaxTemplateLookUp *lookUp =
            getAstSyntaxTemplateExpr_LookUp(expression);
        fprintf(out, "<%d>.", lookUp->nsId);
        ppAstSyntaxTemplateExpr(out, lookUp->expression);
        break;
    }
    case AST_SYNTAXTEMPLATEEXPR_TYPE_FUN:
        ppAstSyntaxTemplateFunction(out,
                                    getAstSyntaxTemplateExpr_Fun(expression));
        break;
    case AST_SYNTAXTEMPLATEEXPR_TYPE_NEST: {
        AstSyntaxTemplateNest *nest = getAstSyntaxTemplateExpr_Nest(expression);
        ppAstSyntaxTemplateNest(out, nest);
        break;
    }
    case AST_SYNTAXTEMPLATEEXPR_TYPE_IFF: {
        AstSyntaxTemplateIff *iff = getAstSyntaxTemplateExpr_Iff(expression);
        fprintf(out, "if (");
        ppAstSyntaxTemplateExpr(out, iff->test);
        fprintf(out, ") ");
        ppAstSyntaxTemplateNest(out, iff->consequent);
        fprintf(out, " else ");
        ppAstSyntaxTemplateNest(out, iff->alternative);
        break;
    }
    case AST_SYNTAXTEMPLATEEXPR_TYPE_PRINT:
        fprintf(out, "print(");
        ppAstSyntaxTemplateExpr(
            out, getAstSyntaxTemplateExpr_Print(expression)->expression);
        fprintf(out, ")");
        break;
    case AST_SYNTAXTEMPLATEEXPR_TYPE_TYPEOF:
        fprintf(out, "(typeof ");
        ppAstSyntaxTemplateExpr(
            out, getAstSyntaxTemplateExpr_TypeOf(expression)->expression);
        fprintf(out, ")");
        break;
    case AST_SYNTAXTEMPLATEEXPR_TYPE_TUPLE:
        fprintf(out, "<tuple>(");
        ppAstSyntaxTemplateExprs(out,
                                 getAstSyntaxTemplateExpr_Tuple(expression));
        fprintf(out, ")");
        break;
    case AST_SYNTAXTEMPLATEEXPR_TYPE_STRUCTURE: {
        AstSyntaxTemplateStruct *structure =
            getAstSyntaxTemplateExpr_Structure(expression);
        ppAstLookUpOrSymbol(out, structure->symbol);
        fprintf(out, "{ ");
        ppAstSyntaxTemplateTaggedExpressions(out, structure->expressions);
        fprintf(out, " }");
        break;
    }
    case AST_SYNTAXTEMPLATEEXPR_TYPE_ASSERTION:
        fprintf(out, "assert(");
        ppAstSyntaxTemplateExpr(out,
                                getAstSyntaxTemplateExpr_Assertion(expression));
        fprintf(out, ")");
        break;
    case AST_SYNTAXTEMPLATEEXPR_TYPE_ERROR:
        fprintf(out, "error(");
        ppAstSyntaxTemplateExpr(out,
                                getAstSyntaxTemplateExpr_Error(expression));
        fprintf(out, ")");
        break;
    default:
        cant_happen("unrecognised %s",
                    astSyntaxTemplateExprTypeName(expression->type));
    }
}

static void ppAstSyntaxTemplateFarg(FILE *out, AstSyntaxTemplateFarg *arg) {
    switch (arg->type) {
    case AST_SYNTAXTEMPLATEFARG_TYPE_WILDCARD:
        fprintf(out, "_");
        break;
    case AST_SYNTAXTEMPLATEFARG_TYPE_BINDER:
        ppAstSyntaxTemplateBinder(out, getAstSyntaxTemplateFarg_Binder(arg));
        break;
    case AST_SYNTAXTEMPLATEFARG_TYPE_LOOKUP:
        ppAstLookUpSymbol(out, getAstSyntaxTemplateFarg_LookUp(arg));
        break;
    case AST_SYNTAXTEMPLATEFARG_TYPE_NAMED: {
        AstSyntaxTemplateNamedArg *named = getAstSyntaxTemplateFarg_Named(arg);
        ppAstHashSymbol(out, named->name);
        fprintf(out, " = ");
        ppAstSyntaxTemplateFarg(out, named->arg);
        break;
    }
    case AST_SYNTAXTEMPLATEFARG_TYPE_UNPACK: {
        AstSyntaxTemplateUnpack *unpack = getAstSyntaxTemplateFarg_Unpack(arg);
        ppAstLookUpOrSymbol(out, unpack->symbol);
        fprintf(out, "(");
        ppAstSyntaxTemplateFargList(out, unpack->argList);
        fprintf(out, ")");
        break;
    }
    case AST_SYNTAXTEMPLATEFARG_TYPE_UNPACKSTRUCT: {
        AstSyntaxTemplateUnpackStruct *unpackStruct =
            getAstSyntaxTemplateFarg_UnpackStruct(arg);
        ppAstLookUpOrSymbol(out, unpackStruct->symbol);
        fprintf(out, "{ ");
        ppAstSyntaxTemplateTaggedArgs(out, unpackStruct->argList);
        fprintf(out, " }");
        break;
    }
    case AST_SYNTAXTEMPLATEFARG_TYPE_NUMBER:
        ppMaybeBigInt(out, getAstSyntaxTemplateFarg_Number(arg));
        break;
    case AST_SYNTAXTEMPLATEFARG_TYPE_CHARACTER:
        ppAstCharacter(out, getAstSyntaxTemplateFarg_Character(arg));
        break;
    case AST_SYNTAXTEMPLATEFARG_TYPE_TUPLE:
        fprintf(out, "<tuple>(");
        ppAstSyntaxTemplateFargList(out, getAstSyntaxTemplateFarg_Tuple(arg));
        fprintf(out, ")");
        break;
    default:
        cant_happen("unrecognised %s",
                    astSyntaxTemplateFargTypeName(arg->type));
    }
}

static void ppAstSyntaxTemplateFargList(FILE *out,
                                        AstSyntaxTemplateFargList *args) {
    if (args == NULL) {
        return;
    }
    for (Index i = 0; i < sizeAstSyntaxTemplateFargList(args); ++i) {
        ppAstSyntaxTemplateFarg(out, getAstSyntaxTemplateFargList(args, i));
        if (i + 1 < sizeAstSyntaxTemplateFargList(args)) {
            fprintf(out, ", ");
        }
    }
}

static void ppAstSyntaxTemplateTaggedArgs(FILE *out,
                                          AstSyntaxTemplateTaggedArgs *args) {
    if (args == NULL) {
        return;
    }
    for (Index i = 0; i < sizeAstSyntaxTemplateTaggedArgs(args); ++i) {
        AstSyntaxTemplateTaggedArg *arg =
            getAstSyntaxTemplateTaggedArgs(args, i);
        ppAstHashSymbol(out, arg->tag);
        fprintf(out, ": ");
        ppAstSyntaxTemplateFarg(out, arg->arg);
        if (i + 1 < sizeAstSyntaxTemplateTaggedArgs(args)) {
            fprintf(out, ", ");
        }
    }
}

static void ppAstSyntaxTemplateTaggedExpressions(
    FILE *out, AstSyntaxTemplateTaggedExpressions *expressions) {
    if (expressions == NULL) {
        return;
    }
    for (Index i = 0; i < sizeAstSyntaxTemplateTaggedExpressions(expressions);
         ++i) {
        AstSyntaxTemplateTaggedExpression *expression =
            getAstSyntaxTemplateTaggedExpressions(expressions, i);
        ppAstHashSymbol(out, expression->tag);
        fprintf(out, ": (");
        ppAstSyntaxTemplateExpr(out, expression->expression);
        fprintf(out, ")");
        if (i + 1 < sizeAstSyntaxTemplateTaggedExpressions(expressions)) {
            fprintf(out, ", ");
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
    case AST_EXPRESSION_TYPE_SYNTAXUSE:
        ppAstExprSyntaxUse(out, expr->val.syntaxUse);
        break;
    default:
        cant_happen("unexpected %s", astExpressionTypeName(expr->type));
    }
}
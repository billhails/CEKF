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
 * Abstract Syntax Tree structures generated by the parser.
 *
 * Generated from src/ast.yaml by tools/makeAST.py
 */

#include <stdio.h>

#include "ast_debug.h"
#include "bigint.h"

/*
 * helper functions
 */

static void pad(int depth) { eprintf("%*s", depth * 4, ""); }

/*
 * print functions
 */

void printAstNest(struct AstNest * x, int depth) {
    pad(depth);
    if (x == NULL) { eprintf("AstNest (NULL)"); return; }
    eprintf("AstNest[\n");
    printAstDefinitions(x->definitions, depth + 1);
    eprintf("\n");
    printAstExpressions(x->expressions, depth + 1);
    eprintf("\n");
    pad(depth);
    eprintf("]");
}

void printAstDefinitions(struct AstDefinitions * x, int depth) {
    pad(depth);
    if (x == NULL) { eprintf("AstDefinitions (NULL)"); return; }
    eprintf("AstDefinitions[\n");
    printAstDefinition(x->definition, depth + 1);
    eprintf("\n");
    printAstDefinitions(x->next, depth + 1);
    eprintf("\n");
    pad(depth);
    eprintf("]");
}

void printAstDefine(struct AstDefine * x, int depth) {
    pad(depth);
    if (x == NULL) { eprintf("AstDefine (NULL)"); return; }
    eprintf("AstDefine[\n");
    printAstSymbol(x->symbol, depth + 1);
    eprintf("\n");
    printAstExpression(x->expression, depth + 1);
    eprintf("\n");
    pad(depth);
    eprintf("]");
}

void printAstPrototype(struct AstPrototype * x, int depth) {
    pad(depth);
    if (x == NULL) { eprintf("AstPrototype (NULL)"); return; }
    eprintf("AstPrototype[\n");
    printAstSymbol(x->symbol, depth + 1);
    eprintf("\n");
    printAstPrototypeBody(x->body, depth + 1);
    eprintf("\n");
    pad(depth);
    eprintf("]");
}

void printAstPrototypeBody(struct AstPrototypeBody * x, int depth) {
    pad(depth);
    if (x == NULL) { eprintf("AstPrototypeBody (NULL)"); return; }
    eprintf("AstPrototypeBody[\n");
    printAstSinglePrototype(x->single, depth + 1);
    eprintf("\n");
    printAstPrototypeBody(x->next, depth + 1);
    eprintf("\n");
    pad(depth);
    eprintf("]");
}

void printAstPrototypeSymbolType(struct AstPrototypeSymbolType * x, int depth) {
    pad(depth);
    if (x == NULL) { eprintf("AstPrototypeSymbolType (NULL)"); return; }
    eprintf("AstPrototypeSymbolType[\n");
    printAstSymbol(x->symbol, depth + 1);
    eprintf("\n");
    printAstType(x->type, depth + 1);
    eprintf("\n");
    pad(depth);
    eprintf("]");
}

void printAstLoad(struct AstLoad * x, int depth) {
    pad(depth);
    if (x == NULL) { eprintf("AstLoad (NULL)"); return; }
    eprintf("AstLoad[\n");
    printAstPackage(x->package, depth + 1);
    eprintf("\n");
    printAstSymbol(x->symbol, depth + 1);
    eprintf("\n");
    pad(depth);
    eprintf("]");
}

void printAstTypeDef(struct AstTypeDef * x, int depth) {
    pad(depth);
    if (x == NULL) { eprintf("AstTypeDef (NULL)"); return; }
    eprintf("AstTypeDef[\n");
    printAstFlatType(x->flatType, depth + 1);
    eprintf("\n");
    printAstTypeBody(x->typeBody, depth + 1);
    eprintf("\n");
    pad(depth);
    eprintf("]");
}

void printAstFlatType(struct AstFlatType * x, int depth) {
    pad(depth);
    if (x == NULL) { eprintf("AstFlatType (NULL)"); return; }
    eprintf("AstFlatType[\n");
    printAstSymbol(x->symbol, depth + 1);
    eprintf("\n");
    printAstTypeSymbols(x->typeSymbols, depth + 1);
    eprintf("\n");
    pad(depth);
    eprintf("]");
}

void printAstTypeSymbols(struct AstTypeSymbols * x, int depth) {
    pad(depth);
    if (x == NULL) { eprintf("AstTypeSymbols (NULL)"); return; }
    eprintf("AstTypeSymbols[\n");
    printAstSymbol(x->typeSymbol, depth + 1);
    eprintf("\n");
    printAstTypeSymbols(x->next, depth + 1);
    eprintf("\n");
    pad(depth);
    eprintf("]");
}

void printAstTypeBody(struct AstTypeBody * x, int depth) {
    pad(depth);
    if (x == NULL) { eprintf("AstTypeBody (NULL)"); return; }
    eprintf("AstTypeBody[\n");
    printAstTypeConstructor(x->typeConstructor, depth + 1);
    eprintf("\n");
    printAstTypeBody(x->next, depth + 1);
    eprintf("\n");
    pad(depth);
    eprintf("]");
}

void printAstTypeConstructor(struct AstTypeConstructor * x, int depth) {
    pad(depth);
    if (x == NULL) { eprintf("AstTypeConstructor (NULL)"); return; }
    eprintf("AstTypeConstructor[\n");
    printAstSymbol(x->symbol, depth + 1);
    eprintf("\n");
    printAstTypeList(x->typeList, depth + 1);
    eprintf("\n");
    pad(depth);
    eprintf("]");
}

void printAstTypeFunction(struct AstTypeFunction * x, int depth) {
    pad(depth);
    if (x == NULL) { eprintf("AstTypeFunction (NULL)"); return; }
    eprintf("AstTypeFunction[\n");
    printAstSymbol(x->symbol, depth + 1);
    eprintf("\n");
    printAstTypeList(x->typeList, depth + 1);
    eprintf("\n");
    pad(depth);
    eprintf("]");
}

void printAstTypeList(struct AstTypeList * x, int depth) {
    pad(depth);
    if (x == NULL) { eprintf("AstTypeList (NULL)"); return; }
    eprintf("AstTypeList[\n");
    printAstType(x->type, depth + 1);
    eprintf("\n");
    printAstTypeList(x->next, depth + 1);
    eprintf("\n");
    pad(depth);
    eprintf("]");
}

void printAstType(struct AstType * x, int depth) {
    pad(depth);
    if (x == NULL) { eprintf("AstType (NULL)"); return; }
    eprintf("AstType[\n");
    printAstTypeClause(x->typeClause, depth + 1);
    eprintf("\n");
    printAstType(x->next, depth + 1);
    eprintf("\n");
    pad(depth);
    eprintf("]");
}

void printAstCompositeFunction(struct AstCompositeFunction * x, int depth) {
    pad(depth);
    if (x == NULL) { eprintf("AstCompositeFunction (NULL)"); return; }
    eprintf("AstCompositeFunction[\n");
    printAstFunction(x->function, depth + 1);
    eprintf("\n");
    printAstCompositeFunction(x->next, depth + 1);
    eprintf("\n");
    pad(depth);
    eprintf("]");
}

void printAstFunction(struct AstFunction * x, int depth) {
    pad(depth);
    if (x == NULL) { eprintf("AstFunction (NULL)"); return; }
    eprintf("AstFunction[\n");
    printAstArgList(x->argList, depth + 1);
    eprintf("\n");
    printAstNest(x->nest, depth + 1);
    eprintf("\n");
    pad(depth);
    eprintf("]");
}

void printAstArgList(struct AstArgList * x, int depth) {
    pad(depth);
    if (x == NULL) { eprintf("AstArgList (NULL)"); return; }
    eprintf("AstArgList[\n");
    printAstArg(x->arg, depth + 1);
    eprintf("\n");
    printAstArgList(x->next, depth + 1);
    eprintf("\n");
    pad(depth);
    eprintf("]");
}

void printAstAltArgs(struct AstAltArgs * x, int depth) {
    pad(depth);
    if (x == NULL) { eprintf("AstAltArgs (NULL)"); return; }
    eprintf("AstAltArgs[\n");
    printAstArgList(x->argList, depth + 1);
    eprintf("\n");
    printAstAltArgs(x->next, depth + 1);
    eprintf("\n");
    pad(depth);
    eprintf("]");
}

void printAstAltFunction(struct AstAltFunction * x, int depth) {
    pad(depth);
    if (x == NULL) { eprintf("AstAltFunction (NULL)"); return; }
    eprintf("AstAltFunction[\n");
    printAstAltArgs(x->altArgs, depth + 1);
    eprintf("\n");
    printAstNest(x->nest, depth + 1);
    eprintf("\n");
    pad(depth);
    eprintf("]");
}

void printAstUnpack(struct AstUnpack * x, int depth) {
    pad(depth);
    if (x == NULL) { eprintf("AstUnpack (NULL)"); return; }
    eprintf("AstUnpack[\n");
    printAstSymbol(x->symbol, depth + 1);
    eprintf("\n");
    printAstArgList(x->argList, depth + 1);
    eprintf("\n");
    pad(depth);
    eprintf("]");
}

void printAstNamedArg(struct AstNamedArg * x, int depth) {
    pad(depth);
    if (x == NULL) { eprintf("AstNamedArg (NULL)"); return; }
    eprintf("AstNamedArg[\n");
    printAstSymbol(x->name, depth + 1);
    eprintf("\n");
    printAstArg(x->arg, depth + 1);
    eprintf("\n");
    pad(depth);
    eprintf("]");
}

void printAstEnvType(struct AstEnvType * x, int depth) {
    pad(depth);
    if (x == NULL) { eprintf("AstEnvType (NULL)"); return; }
    eprintf("AstEnvType[\n");
    printAstSymbol(x->name, depth + 1);
    eprintf("\n");
    printAstSymbol(x->prototype, depth + 1);
    eprintf("\n");
    pad(depth);
    eprintf("]");
}

void printAstFunCall(struct AstFunCall * x, int depth) {
    pad(depth);
    if (x == NULL) { eprintf("AstFunCall (NULL)"); return; }
    eprintf("AstFunCall[\n");
    printAstExpression(x->function, depth + 1);
    eprintf("\n");
    printAstExpressions(x->arguments, depth + 1);
    eprintf("\n");
    pad(depth);
    eprintf("]");
}

void printAstPackage(struct AstPackage * x, int depth) {
    pad(depth);
    if (x == NULL) { eprintf("AstPackage (NULL)"); return; }
    eprintf("AstPackage[\n");
    printAstSymbol(x->symbol, depth + 1);
    eprintf("\n");
    printAstPackage(x->next, depth + 1);
    eprintf("\n");
    pad(depth);
    eprintf("]");
}

void printAstExpressions(struct AstExpressions * x, int depth) {
    pad(depth);
    if (x == NULL) { eprintf("AstExpressions (NULL)"); return; }
    eprintf("AstExpressions[\n");
    printAstExpression(x->expression, depth + 1);
    eprintf("\n");
    printAstExpressions(x->next, depth + 1);
    eprintf("\n");
    pad(depth);
    eprintf("]");
}

void printAstEnv(struct AstEnv * x, int depth) {
    pad(depth);
    if (x == NULL) { eprintf("AstEnv (NULL)"); return; }
    eprintf("AstEnv[\n");
    printAstPackage(x->package, depth + 1);
    eprintf("\n");
    printAstDefinitions(x->definitions, depth + 1);
    eprintf("\n");
    pad(depth);
    eprintf("]");
}

void printAstIff(struct AstIff * x, int depth) {
    pad(depth);
    if (x == NULL) { eprintf("AstIff (NULL)"); return; }
    eprintf("AstIff[\n");
    printAstExpression(x->test, depth + 1);
    eprintf("\n");
    printAstNest(x->consequent, depth + 1);
    eprintf("\n");
    printAstNest(x->alternative, depth + 1);
    eprintf("\n");
    pad(depth);
    eprintf("]");
}

void printAstDefinition(struct AstDefinition * x, int depth) {
    pad(depth);
    if (x == NULL) { eprintf("AstDefinition (NULL)"); return; }
    eprintf("AstDefinition[\n");
    switch(x->type) {
        case AST_DEFINITION_TYPE_DEFINE:
            pad(depth + 1);
            eprintf("AST_DEFINITION_TYPE_DEFINE\n");
            printAstDefine(x->val.define, depth + 1);
            break;
        case AST_DEFINITION_TYPE_PROTOTYPE:
            pad(depth + 1);
            eprintf("AST_DEFINITION_TYPE_PROTOTYPE\n");
            printAstPrototype(x->val.prototype, depth + 1);
            break;
        case AST_DEFINITION_TYPE_LOAD:
            pad(depth + 1);
            eprintf("AST_DEFINITION_TYPE_LOAD\n");
            printAstLoad(x->val.load, depth + 1);
            break;
        case AST_DEFINITION_TYPE_TYPEDEF:
            pad(depth + 1);
            eprintf("AST_DEFINITION_TYPE_TYPEDEF\n");
            printAstTypeDef(x->val.typeDef, depth + 1);
            break;
        default:
            cant_happen("unrecognised type %d in printAstDefinition", x->type);
    }
    eprintf("\n");
    pad(depth);
    eprintf("]");
}

void printAstSinglePrototype(struct AstSinglePrototype * x, int depth) {
    pad(depth);
    if (x == NULL) { eprintf("AstSinglePrototype (NULL)"); return; }
    eprintf("AstSinglePrototype[\n");
    switch(x->type) {
        case AST_SINGLEPROTOTYPE_TYPE_SYMBOLTYPE:
            pad(depth + 1);
            eprintf("AST_SINGLEPROTOTYPE_TYPE_SYMBOLTYPE\n");
            printAstPrototypeSymbolType(x->val.symbolType, depth + 1);
            break;
        case AST_SINGLEPROTOTYPE_TYPE_PROTOTYPE:
            pad(depth + 1);
            eprintf("AST_SINGLEPROTOTYPE_TYPE_PROTOTYPE\n");
            printAstPrototype(x->val.prototype, depth + 1);
            break;
        default:
            cant_happen("unrecognised type %d in printAstSinglePrototype", x->type);
    }
    eprintf("\n");
    pad(depth);
    eprintf("]");
}

void printAstTypeClause(struct AstTypeClause * x, int depth) {
    pad(depth);
    if (x == NULL) { eprintf("AstTypeClause (NULL)"); return; }
    eprintf("AstTypeClause[\n");
    switch(x->type) {
        case AST_TYPECLAUSE_TYPE_INTEGER:
            pad(depth + 1);
            eprintf("AST_TYPECLAUSE_TYPE_INTEGER\n");
            pad(depth + 1);
eprintf("void * %p", x->val.integer);
            break;
        case AST_TYPECLAUSE_TYPE_CHARACTER:
            pad(depth + 1);
            eprintf("AST_TYPECLAUSE_TYPE_CHARACTER\n");
            pad(depth + 1);
eprintf("void * %p", x->val.character);
            break;
        case AST_TYPECLAUSE_TYPE_VAR:
            pad(depth + 1);
            eprintf("AST_TYPECLAUSE_TYPE_VAR\n");
            printAstSymbol(x->val.var, depth + 1);
            break;
        case AST_TYPECLAUSE_TYPE_TYPEFUNCTION:
            pad(depth + 1);
            eprintf("AST_TYPECLAUSE_TYPE_TYPEFUNCTION\n");
            printAstTypeFunction(x->val.typeFunction, depth + 1);
            break;
        default:
            cant_happen("unrecognised type %d in printAstTypeClause", x->type);
    }
    eprintf("\n");
    pad(depth);
    eprintf("]");
}

void printAstArg(struct AstArg * x, int depth) {
    pad(depth);
    if (x == NULL) { eprintf("AstArg (NULL)"); return; }
    eprintf("AstArg[\n");
    switch(x->type) {
        case AST_ARG_TYPE_WILDCARD:
            pad(depth + 1);
            eprintf("AST_ARG_TYPE_WILDCARD\n");
            pad(depth + 1);
eprintf("void * %p", x->val.wildcard);
            break;
        case AST_ARG_TYPE_SYMBOL:
            pad(depth + 1);
            eprintf("AST_ARG_TYPE_SYMBOL\n");
            printAstSymbol(x->val.symbol, depth + 1);
            break;
        case AST_ARG_TYPE_NAMED:
            pad(depth + 1);
            eprintf("AST_ARG_TYPE_NAMED\n");
            printAstNamedArg(x->val.named, depth + 1);
            break;
        case AST_ARG_TYPE_ENV:
            pad(depth + 1);
            eprintf("AST_ARG_TYPE_ENV\n");
            printAstEnvType(x->val.env, depth + 1);
            break;
        case AST_ARG_TYPE_UNPACK:
            pad(depth + 1);
            eprintf("AST_ARG_TYPE_UNPACK\n");
            printAstUnpack(x->val.unpack, depth + 1);
            break;
        case AST_ARG_TYPE_NUMBER:
            pad(depth + 1);
            eprintf("AST_ARG_TYPE_NUMBER\n");
            printBigInt(x->val.number, depth + 1);
            break;
        case AST_ARG_TYPE_CHARACTER:
            pad(depth + 1);
            eprintf("AST_ARG_TYPE_CHARACTER\n");
            pad(depth + 1);
eprintf("char %c", x->val.character);
            break;
        default:
            cant_happen("unrecognised type %d in printAstArg", x->type);
    }
    eprintf("\n");
    pad(depth);
    eprintf("]");
}

void printAstExpression(struct AstExpression * x, int depth) {
    pad(depth);
    if (x == NULL) { eprintf("AstExpression (NULL)"); return; }
    eprintf("AstExpression[\n");
    switch(x->type) {
        case AST_EXPRESSION_TYPE_BACK:
            pad(depth + 1);
            eprintf("AST_EXPRESSION_TYPE_BACK\n");
            pad(depth + 1);
eprintf("void * %p", x->val.back);
            break;
        case AST_EXPRESSION_TYPE_FUNCALL:
            pad(depth + 1);
            eprintf("AST_EXPRESSION_TYPE_FUNCALL\n");
            printAstFunCall(x->val.funCall, depth + 1);
            break;
        case AST_EXPRESSION_TYPE_SYMBOL:
            pad(depth + 1);
            eprintf("AST_EXPRESSION_TYPE_SYMBOL\n");
            printAstSymbol(x->val.symbol, depth + 1);
            break;
        case AST_EXPRESSION_TYPE_NUMBER:
            pad(depth + 1);
            eprintf("AST_EXPRESSION_TYPE_NUMBER\n");
            printBigInt(x->val.number, depth + 1);
            break;
        case AST_EXPRESSION_TYPE_CHARACTER:
            pad(depth + 1);
            eprintf("AST_EXPRESSION_TYPE_CHARACTER\n");
            pad(depth + 1);
eprintf("char %c", x->val.character);
            break;
        case AST_EXPRESSION_TYPE_FUN:
            pad(depth + 1);
            eprintf("AST_EXPRESSION_TYPE_FUN\n");
            printAstCompositeFunction(x->val.fun, depth + 1);
            break;
        case AST_EXPRESSION_TYPE_ENV:
            pad(depth + 1);
            eprintf("AST_EXPRESSION_TYPE_ENV\n");
            printAstEnv(x->val.env, depth + 1);
            break;
        case AST_EXPRESSION_TYPE_NEST:
            pad(depth + 1);
            eprintf("AST_EXPRESSION_TYPE_NEST\n");
            printAstNest(x->val.nest, depth + 1);
            break;
        case AST_EXPRESSION_TYPE_IFF:
            pad(depth + 1);
            eprintf("AST_EXPRESSION_TYPE_IFF\n");
            printAstIff(x->val.iff, depth + 1);
            break;
        default:
            cant_happen("unrecognised type %d in printAstExpression", x->type);
    }
    eprintf("\n");
    pad(depth);
    eprintf("]");
}

void printAstCharArray(struct AstCharArray * x, int depth) {
    pad(depth);
    if (x == NULL) { eprintf("AstCharArray (NULL)"); return; }
    eprintf("AstCharArray(%d)[\n", x->size);
    for (int i = 0; i < x->size; i++) {
        pad(depth + 1);
eprintf("char %c", x->entries[i]);
        eprintf("\n");
    }
    pad(depth);
    eprintf("]");
}


/*
 * compare functions
 */

bool eqAstNest(struct AstNest * a, struct AstNest * b) {
    if (a == b) return true;
    if (a == NULL || b == NULL) return false;
    if (!eqAstDefinitions(a->definitions, b->definitions)) return false;
    if (!eqAstExpressions(a->expressions, b->expressions)) return false;
    return true;
}

bool eqAstDefinitions(struct AstDefinitions * a, struct AstDefinitions * b) {
    if (a == b) return true;
    if (a == NULL || b == NULL) return false;
    if (!eqAstDefinition(a->definition, b->definition)) return false;
    if (!eqAstDefinitions(a->next, b->next)) return false;
    return true;
}

bool eqAstDefine(struct AstDefine * a, struct AstDefine * b) {
    if (a == b) return true;
    if (a == NULL || b == NULL) return false;
    if (a->symbol != b->symbol) return false;
    if (!eqAstExpression(a->expression, b->expression)) return false;
    return true;
}

bool eqAstPrototype(struct AstPrototype * a, struct AstPrototype * b) {
    if (a == b) return true;
    if (a == NULL || b == NULL) return false;
    if (a->symbol != b->symbol) return false;
    if (!eqAstPrototypeBody(a->body, b->body)) return false;
    return true;
}

bool eqAstPrototypeBody(struct AstPrototypeBody * a, struct AstPrototypeBody * b) {
    if (a == b) return true;
    if (a == NULL || b == NULL) return false;
    if (!eqAstSinglePrototype(a->single, b->single)) return false;
    if (!eqAstPrototypeBody(a->next, b->next)) return false;
    return true;
}

bool eqAstPrototypeSymbolType(struct AstPrototypeSymbolType * a, struct AstPrototypeSymbolType * b) {
    if (a == b) return true;
    if (a == NULL || b == NULL) return false;
    if (a->symbol != b->symbol) return false;
    if (!eqAstType(a->type, b->type)) return false;
    return true;
}

bool eqAstLoad(struct AstLoad * a, struct AstLoad * b) {
    if (a == b) return true;
    if (a == NULL || b == NULL) return false;
    if (!eqAstPackage(a->package, b->package)) return false;
    if (a->symbol != b->symbol) return false;
    return true;
}

bool eqAstTypeDef(struct AstTypeDef * a, struct AstTypeDef * b) {
    if (a == b) return true;
    if (a == NULL || b == NULL) return false;
    if (!eqAstFlatType(a->flatType, b->flatType)) return false;
    if (!eqAstTypeBody(a->typeBody, b->typeBody)) return false;
    return true;
}

bool eqAstFlatType(struct AstFlatType * a, struct AstFlatType * b) {
    if (a == b) return true;
    if (a == NULL || b == NULL) return false;
    if (a->symbol != b->symbol) return false;
    if (!eqAstTypeSymbols(a->typeSymbols, b->typeSymbols)) return false;
    return true;
}

bool eqAstTypeSymbols(struct AstTypeSymbols * a, struct AstTypeSymbols * b) {
    if (a == b) return true;
    if (a == NULL || b == NULL) return false;
    if (a->typeSymbol != b->typeSymbol) return false;
    if (!eqAstTypeSymbols(a->next, b->next)) return false;
    return true;
}

bool eqAstTypeBody(struct AstTypeBody * a, struct AstTypeBody * b) {
    if (a == b) return true;
    if (a == NULL || b == NULL) return false;
    if (!eqAstTypeConstructor(a->typeConstructor, b->typeConstructor)) return false;
    if (!eqAstTypeBody(a->next, b->next)) return false;
    return true;
}

bool eqAstTypeConstructor(struct AstTypeConstructor * a, struct AstTypeConstructor * b) {
    if (a == b) return true;
    if (a == NULL || b == NULL) return false;
    if (a->symbol != b->symbol) return false;
    if (!eqAstTypeList(a->typeList, b->typeList)) return false;
    return true;
}

bool eqAstTypeFunction(struct AstTypeFunction * a, struct AstTypeFunction * b) {
    if (a == b) return true;
    if (a == NULL || b == NULL) return false;
    if (a->symbol != b->symbol) return false;
    if (!eqAstTypeList(a->typeList, b->typeList)) return false;
    return true;
}

bool eqAstTypeList(struct AstTypeList * a, struct AstTypeList * b) {
    if (a == b) return true;
    if (a == NULL || b == NULL) return false;
    if (!eqAstType(a->type, b->type)) return false;
    if (!eqAstTypeList(a->next, b->next)) return false;
    return true;
}

bool eqAstType(struct AstType * a, struct AstType * b) {
    if (a == b) return true;
    if (a == NULL || b == NULL) return false;
    if (!eqAstTypeClause(a->typeClause, b->typeClause)) return false;
    if (!eqAstType(a->next, b->next)) return false;
    return true;
}

bool eqAstCompositeFunction(struct AstCompositeFunction * a, struct AstCompositeFunction * b) {
    if (a == b) return true;
    if (a == NULL || b == NULL) return false;
    if (!eqAstFunction(a->function, b->function)) return false;
    if (!eqAstCompositeFunction(a->next, b->next)) return false;
    return true;
}

bool eqAstFunction(struct AstFunction * a, struct AstFunction * b) {
    if (a == b) return true;
    if (a == NULL || b == NULL) return false;
    if (!eqAstArgList(a->argList, b->argList)) return false;
    if (!eqAstNest(a->nest, b->nest)) return false;
    return true;
}

bool eqAstArgList(struct AstArgList * a, struct AstArgList * b) {
    if (a == b) return true;
    if (a == NULL || b == NULL) return false;
    if (!eqAstArg(a->arg, b->arg)) return false;
    if (!eqAstArgList(a->next, b->next)) return false;
    return true;
}

bool eqAstAltArgs(struct AstAltArgs * a, struct AstAltArgs * b) {
    if (a == b) return true;
    if (a == NULL || b == NULL) return false;
    if (!eqAstArgList(a->argList, b->argList)) return false;
    if (!eqAstAltArgs(a->next, b->next)) return false;
    return true;
}

bool eqAstAltFunction(struct AstAltFunction * a, struct AstAltFunction * b) {
    if (a == b) return true;
    if (a == NULL || b == NULL) return false;
    if (!eqAstAltArgs(a->altArgs, b->altArgs)) return false;
    if (!eqAstNest(a->nest, b->nest)) return false;
    return true;
}

bool eqAstUnpack(struct AstUnpack * a, struct AstUnpack * b) {
    if (a == b) return true;
    if (a == NULL || b == NULL) return false;
    if (a->symbol != b->symbol) return false;
    if (!eqAstArgList(a->argList, b->argList)) return false;
    return true;
}

bool eqAstNamedArg(struct AstNamedArg * a, struct AstNamedArg * b) {
    if (a == b) return true;
    if (a == NULL || b == NULL) return false;
    if (a->name != b->name) return false;
    if (!eqAstArg(a->arg, b->arg)) return false;
    return true;
}

bool eqAstEnvType(struct AstEnvType * a, struct AstEnvType * b) {
    if (a == b) return true;
    if (a == NULL || b == NULL) return false;
    if (a->name != b->name) return false;
    if (a->prototype != b->prototype) return false;
    return true;
}

bool eqAstFunCall(struct AstFunCall * a, struct AstFunCall * b) {
    if (a == b) return true;
    if (a == NULL || b == NULL) return false;
    if (!eqAstExpression(a->function, b->function)) return false;
    if (!eqAstExpressions(a->arguments, b->arguments)) return false;
    return true;
}

bool eqAstPackage(struct AstPackage * a, struct AstPackage * b) {
    if (a == b) return true;
    if (a == NULL || b == NULL) return false;
    if (a->symbol != b->symbol) return false;
    if (!eqAstPackage(a->next, b->next)) return false;
    return true;
}

bool eqAstExpressions(struct AstExpressions * a, struct AstExpressions * b) {
    if (a == b) return true;
    if (a == NULL || b == NULL) return false;
    if (!eqAstExpression(a->expression, b->expression)) return false;
    if (!eqAstExpressions(a->next, b->next)) return false;
    return true;
}

bool eqAstEnv(struct AstEnv * a, struct AstEnv * b) {
    if (a == b) return true;
    if (a == NULL || b == NULL) return false;
    if (!eqAstPackage(a->package, b->package)) return false;
    if (!eqAstDefinitions(a->definitions, b->definitions)) return false;
    return true;
}

bool eqAstIff(struct AstIff * a, struct AstIff * b) {
    if (a == b) return true;
    if (a == NULL || b == NULL) return false;
    if (!eqAstExpression(a->test, b->test)) return false;
    if (!eqAstNest(a->consequent, b->consequent)) return false;
    if (!eqAstNest(a->alternative, b->alternative)) return false;
    return true;
}

bool eqAstDefinition(struct AstDefinition * a, struct AstDefinition * b) {
    if (a == b) return true;
    if (a == NULL || b == NULL) return false;
    if (a->type != b->type) return false;
    switch(a->type) {
        case AST_DEFINITION_TYPE_DEFINE:
            if (!eqAstDefine(a->val.define, b->val.define)) return false;
            break;
        case AST_DEFINITION_TYPE_PROTOTYPE:
            if (!eqAstPrototype(a->val.prototype, b->val.prototype)) return false;
            break;
        case AST_DEFINITION_TYPE_LOAD:
            if (!eqAstLoad(a->val.load, b->val.load)) return false;
            break;
        case AST_DEFINITION_TYPE_TYPEDEF:
            if (!eqAstTypeDef(a->val.typeDef, b->val.typeDef)) return false;
            break;
        default:
            cant_happen("unrecognised type %d in eqAstDefinition", a->type);
    }
    return true;
}

bool eqAstSinglePrototype(struct AstSinglePrototype * a, struct AstSinglePrototype * b) {
    if (a == b) return true;
    if (a == NULL || b == NULL) return false;
    if (a->type != b->type) return false;
    switch(a->type) {
        case AST_SINGLEPROTOTYPE_TYPE_SYMBOLTYPE:
            if (!eqAstPrototypeSymbolType(a->val.symbolType, b->val.symbolType)) return false;
            break;
        case AST_SINGLEPROTOTYPE_TYPE_PROTOTYPE:
            if (!eqAstPrototype(a->val.prototype, b->val.prototype)) return false;
            break;
        default:
            cant_happen("unrecognised type %d in eqAstSinglePrototype", a->type);
    }
    return true;
}

bool eqAstTypeClause(struct AstTypeClause * a, struct AstTypeClause * b) {
    if (a == b) return true;
    if (a == NULL || b == NULL) return false;
    if (a->type != b->type) return false;
    switch(a->type) {
        case AST_TYPECLAUSE_TYPE_INTEGER:
            if (a->val.integer != b->val.integer) return false;
            break;
        case AST_TYPECLAUSE_TYPE_CHARACTER:
            if (a->val.character != b->val.character) return false;
            break;
        case AST_TYPECLAUSE_TYPE_VAR:
            if (a->val.var != b->val.var) return false;
            break;
        case AST_TYPECLAUSE_TYPE_TYPEFUNCTION:
            if (!eqAstTypeFunction(a->val.typeFunction, b->val.typeFunction)) return false;
            break;
        default:
            cant_happen("unrecognised type %d in eqAstTypeClause", a->type);
    }
    return true;
}

bool eqAstArg(struct AstArg * a, struct AstArg * b) {
    if (a == b) return true;
    if (a == NULL || b == NULL) return false;
    if (a->type != b->type) return false;
    switch(a->type) {
        case AST_ARG_TYPE_WILDCARD:
            if (a->val.wildcard != b->val.wildcard) return false;
            break;
        case AST_ARG_TYPE_SYMBOL:
            if (a->val.symbol != b->val.symbol) return false;
            break;
        case AST_ARG_TYPE_NAMED:
            if (!eqAstNamedArg(a->val.named, b->val.named)) return false;
            break;
        case AST_ARG_TYPE_ENV:
            if (!eqAstEnvType(a->val.env, b->val.env)) return false;
            break;
        case AST_ARG_TYPE_UNPACK:
            if (!eqAstUnpack(a->val.unpack, b->val.unpack)) return false;
            break;
        case AST_ARG_TYPE_NUMBER:
            if (a->val.number != b->val.number) return false;
            break;
        case AST_ARG_TYPE_CHARACTER:
            if (a->val.character != b->val.character) return false;
            break;
        default:
            cant_happen("unrecognised type %d in eqAstArg", a->type);
    }
    return true;
}

bool eqAstExpression(struct AstExpression * a, struct AstExpression * b) {
    if (a == b) return true;
    if (a == NULL || b == NULL) return false;
    if (a->type != b->type) return false;
    switch(a->type) {
        case AST_EXPRESSION_TYPE_BACK:
            if (a->val.back != b->val.back) return false;
            break;
        case AST_EXPRESSION_TYPE_FUNCALL:
            if (!eqAstFunCall(a->val.funCall, b->val.funCall)) return false;
            break;
        case AST_EXPRESSION_TYPE_SYMBOL:
            if (a->val.symbol != b->val.symbol) return false;
            break;
        case AST_EXPRESSION_TYPE_NUMBER:
            if (a->val.number != b->val.number) return false;
            break;
        case AST_EXPRESSION_TYPE_CHARACTER:
            if (a->val.character != b->val.character) return false;
            break;
        case AST_EXPRESSION_TYPE_FUN:
            if (!eqAstCompositeFunction(a->val.fun, b->val.fun)) return false;
            break;
        case AST_EXPRESSION_TYPE_ENV:
            if (!eqAstEnv(a->val.env, b->val.env)) return false;
            break;
        case AST_EXPRESSION_TYPE_NEST:
            if (!eqAstNest(a->val.nest, b->val.nest)) return false;
            break;
        case AST_EXPRESSION_TYPE_IFF:
            if (!eqAstIff(a->val.iff, b->val.iff)) return false;
            break;
        default:
            cant_happen("unrecognised type %d in eqAstExpression", a->type);
    }
    return true;
}

bool eqAstCharArray(struct AstCharArray * a, struct AstCharArray * b) {
    if (a == b) return true;
    if (a == NULL || b == NULL) return false;
    if (a->size != b->size) return false;
    for (int i = 0; i < a->size; i++) {
        if (a->entries[i] != b->entries[i]) return false;
    }
    return true;
}


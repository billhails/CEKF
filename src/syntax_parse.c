/*
 * CEKF - VM supporting amb
 * Copyright (C) 2022-2026  Bill Hails
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

#include "syntax_parse.h"
#include "memory.h"
#include "pratt.h"
#include "pratt_parser.h"
#include "pratt_scanner.h"
#include "syntax_template.h"

static int sNextDeclarationId = 1;

int prattNextDeclarationId(void) { return sNextDeclarationId++; }

static AstSyntaxClass convertSyntaxClass(PrattSyntaxClass prattClass) {
    switch (prattClass) {
    case PRATTSYNTAXCLASS_TYPE_EXPR:
        return AST_SYNTAXCLASS_TYPE_EXPR;
    case PRATTSYNTAXCLASS_TYPE_NAME:
        return AST_SYNTAXCLASS_TYPE_NAME;
    case PRATTSYNTAXCLASS_TYPE_NEST:
        return AST_SYNTAXCLASS_TYPE_NEST;
    case PRATTSYNTAXCLASS_TYPE_STRING:
        return AST_SYNTAXCLASS_TYPE_STRING;
    case PRATTSYNTAXCLASS_TYPE_TYPE:
        return AST_SYNTAXCLASS_TYPE_TYPE;
    case PRATTSYNTAXCLASS_TYPE_SYNTAX:
        return AST_SYNTAXCLASS_TYPE_SYNTAX;
    default:
        cant_happen("unrecognised PrattSyntaxClass %d", prattClass);
    }
}

AstSyntaxPatternItems *prattConvertPatternItems(ParserInfo PI,
                                                PrattMacroPatternItems *items) {
    AstSyntaxPatternItems *result = newAstSyntaxPatternItems();
    int save = PROTECT(result);
    if (items != NULL) {
        for (Index i = 0; i < countPrattMacroPatternItems(items); ++i) {
            PrattMacroPatternItem *item = getPrattMacroPatternItems(items, i);
            AstSyntaxPatternItem *astItem = NULL;
            if (item->type == PRATTMACROPATTERNITEM_TYPE_QUOTEDTERMINAL) {
                astItem = newAstSyntaxPatternItem_QuotedTerminal(
                    PI, getPrattMacroPatternItem_QuotedTerminal(item));
            } else {
                PrattMacroHole *hole = getPrattMacroPatternItem_TypedHole(item);
                AstSyntaxHole *astHole = newAstSyntaxHole(
                    PI, convertSyntaxClass(hole->syntaxClass), hole->name);
                int save2 = PROTECT(astHole);
                astHole->callTarget = hole->callTarget;
                astHole->callArguments = hole->callArguments;
                astItem = newAstSyntaxPatternItem_TypedHole(PI, astHole);
                UNPROTECT(save2);
            }
            int save2 = PROTECT(astItem);
            pushAstSyntaxPatternItems(result, astItem);
            UNPROTECT(save2);
        }
    }
    UNPROTECT(save);
    return result;
}

PrattMacroSpec *prattFindLocalSyntaxSpecForHead(PrattParser *parser,
                                                HashSymbol *head) {
    Index i = 0;
    PrattMacroSpec *spec = NULL;
    while (iteratePrattMacroTable(parser->macros, &i, &spec) != NULL) {
        if (spec != NULL && spec->isExprEntry && spec->headSymbol == head) {
            return spec;
        }
    }
    return NULL;
}

PrattMacroSpec *prattFindSyntaxSpecByName(PrattParser *parser,
                                          HashSymbol *name) {
    PrattMacroSpec *spec = NULL;
    getPrattMacroTable(parser->macros, name, &spec);
    if (spec != NULL) {
        return spec;
    } else if (parser->next != NULL) {
        return prattFindSyntaxSpecByName(parser->next, name);
    } else {
        return NULL;
    }
}

PrattMacroSpec *prattFindSyntaxSpecForHead(PrattParser *parser,
                                           HashSymbol *head) {
    PrattMacroSpec *spec = prattFindLocalSyntaxSpecForHead(parser, head);
    if (spec != NULL) {
        return spec;
    } else if (parser->next != NULL) {
        return prattFindSyntaxSpecForHead(parser->next, head);
    } else {
        return NULL;
    }
}

static bool syntaxPatternNameEquivalent(HashSymbol *left, HashSymbol *right,
                                        SymbolArray *leftNames,
                                        SymbolArray *rightNames,
                                        Index mappedCount) {
    bool rightMapped = false;

    for (Index i = 0; i < mappedCount; ++i) {
        if (getSymbolArray(leftNames, i) == left) {
            return getSymbolArray(rightNames, i) == right;
        }
        if (getSymbolArray(rightNames, i) == right) {
            rightMapped = true;
        }
    }

    if (rightMapped) {
        return false;
    }

    return left == right;
}

static bool recordSyntaxPatternNameMap(HashSymbol *left, HashSymbol *right,
                                       SymbolArray *leftNames,
                                       SymbolArray *rightNames,
                                       Index *mappedCount) {
    for (Index i = 0; i < *mappedCount; ++i) {
        if (getSymbolArray(leftNames, i) == left) {
            return getSymbolArray(rightNames, i) == right;
        }
        if (getSymbolArray(rightNames, i) == right) {
            return false;
        }
    }

    pushSymbolArray(leftNames, left);
    pushSymbolArray(rightNames, right);
    (*mappedCount)++;
    return true;
}

static bool syntaxCallArgumentsEquivalent(SymbolArray *left, SymbolArray *right,
                                          SymbolArray *leftNames,
                                          SymbolArray *rightNames,
                                          Index mappedCount) {
    if (left == NULL || right == NULL) {
        return left == right;
    }

    if (sizeSymbolArray(left) != sizeSymbolArray(right)) {
        return false;
    }

    for (Index i = 0; i < sizeSymbolArray(left); ++i) {
        if (!syntaxPatternNameEquivalent(getSymbolArray(left, i),
                                         getSymbolArray(right, i), leftNames,
                                         rightNames, mappedCount)) {
            return false;
        }
    }

    return true;
}

static bool syntaxPatternItemsMatch(PrattMacroPatternItems *leftItems,
                                    PrattMacroPatternItems *rightItems,
                                    bool requireRightLonger) {
    Index leftCount = countPrattMacroPatternItems(leftItems);
    Index rightCount = countPrattMacroPatternItems(rightItems);

    if (requireRightLonger) {
        if (leftCount >= rightCount) {
            return false;
        }
    } else if (leftCount != rightCount) {
        return false;
    }

    SymbolArray *leftNames = newSymbolArray();
    int save = PROTECT(leftNames);
    SymbolArray *rightNames = newSymbolArray();
    PROTECT(rightNames);
    bool matched = true;
    Index mappedCount = 0;

    for (Index i = 0; i < leftCount; ++i) {
        PrattMacroPatternItem *leftItem =
            getPrattMacroPatternItems(leftItems, i);
        PrattMacroPatternItem *rightItem =
            getPrattMacroPatternItems(rightItems, i);
        if (leftItem->type != rightItem->type) {
            matched = false;
            break;
        }

        if (leftItem->type == PRATTMACROPATTERNITEM_TYPE_QUOTEDTERMINAL) {
            if (getPrattMacroPatternItem_QuotedTerminal(leftItem) !=
                getPrattMacroPatternItem_QuotedTerminal(rightItem)) {
                matched = false;
                break;
            }
            continue;
        }

        PrattMacroHole *leftHole = getPrattMacroPatternItem_TypedHole(leftItem);
        PrattMacroHole *rightHole =
            getPrattMacroPatternItem_TypedHole(rightItem);

        if (leftHole->syntaxClass != rightHole->syntaxClass ||
            leftHole->callTarget != rightHole->callTarget ||
            !syntaxCallArgumentsEquivalent(leftHole->callArguments,
                                           rightHole->callArguments, leftNames,
                                           rightNames, mappedCount) ||
            !recordSyntaxPatternNameMap(leftHole->name, rightHole->name,
                                        leftNames, rightNames, &mappedCount)) {
            matched = false;
            break;
        }
    }

    UNPROTECT(save);
    return matched;
}

static bool syntaxAlternativesEquivalent(PrattMacroAlternative *left,
                                         PrattMacroAlternative *right) {
    return syntaxPatternItemsMatch(left->patternItems, right->patternItems,
                                   false);
}

static bool syntaxAlternativeShadows(PrattMacroAlternative *earlier,
                                     PrattMacroAlternative *later) {
    if (countPrattMacroPatternItems(earlier->patternItems) == 0) {
        return false;
    }

    return syntaxPatternItemsMatch(earlier->patternItems, later->patternItems,
                                   true);
}

void prattValidateSyntaxAlternatives(PrattParser *parser, HashSymbol *ruleName,
                                     PrattMacroAlternatives *alternatives) {
    bool sawEmpty = false;

    for (Index i = 0; i < sizePrattMacroAlternatives(alternatives); ++i) {
        PrattMacroAlternative *alternative =
            getPrattMacroAlternatives(alternatives, i);
        if (countPrattMacroPatternItems(alternative->patternItems) == 0) {
            if (sawEmpty) {
                parserError(parser,
                            "syntax %s has more than one empty alternative",
                            ruleName->name);
                return;
            }
            sawEmpty = true;
        }

        for (Index j = 0; j < i; ++j) {
            PrattMacroAlternative *earlier =
                getPrattMacroAlternatives(alternatives, j);
            if (syntaxAlternativesEquivalent(earlier, alternative)) {
                parserError(
                    parser,
                    "syntax %s alternative %ld duplicates alternative %ld",
                    ruleName->name, (long)(i + 1), (long)(j + 1));
                return;
            }
            if (syntaxAlternativeShadows(earlier, alternative)) {
                parserError(
                    parser,
                    "syntax %s alternative %ld is shadowed by alternative %ld",
                    ruleName->name, (long)(i + 1), (long)(j + 1));
                return;
            }
        }
    }
}

static bool symbolArrayContains(SymbolArray *symbols, HashSymbol *symbol) {
    if (symbols == NULL) {
        return false;
    }
    for (Index i = 0; i < sizeSymbolArray(symbols); ++i) {
        if (getSymbolArray(symbols, i) == symbol) {
            return true;
        }
    }
    return false;
}

static SyntaxLexerCheckpoint captureSyntaxLexerCheckpoint(PrattParser *parser) {
    PrattLexer *lexer = parser->lexer;
    SyntaxLexerCheckpoint checkpoint = {
        .bufList = lexer->bufList,
        .queuedTokens = NULL,
        .snapshots = NULL,
        .panicMode = parser->panicMode,
    };
    int save = STARTPROTECT();

    if (lexer->tokenHead != NULL) {
        checkpoint.queuedTokens = newPrattTokens();
        PROTECT(checkpoint.queuedTokens);
        for (PrattToken *token = lexer->tokenHead; token != NULL;
             token = token->next) {
            pushPrattTokens(checkpoint.queuedTokens, token);
        }
    }

    SyntaxLexerSnapshot *tail = NULL;
    for (PrattBufList *bufList = lexer->bufList; bufList != NULL;
         bufList = bufList->next) {
        SyntaxLexerSnapshot *snapshot = newSyntaxLexerSnapshot(bufList);
        snapshot->start = bufList->buffer->start;
        snapshot->offset = bufList->buffer->offset;
        snapshot->lineNo = bufList->lineNo;
        if (checkpoint.snapshots == NULL) {
            checkpoint.snapshots = snapshot;
            PROTECT(checkpoint.snapshots);
        } else {
            tail->next = snapshot;
        }
        tail = snapshot;
    }

    UNPROTECT(save);
    return checkpoint;
}

static void restoreSyntaxLexerCheckpoint(PrattParser *parser,
                                         SyntaxLexerCheckpoint *checkpoint) {
    parser->lexer->bufList = checkpoint->bufList;

    for (SyntaxLexerSnapshot *snapshot = checkpoint->snapshots;
         snapshot != NULL; snapshot = snapshot->next) {
        snapshot->bufList->buffer->start = snapshot->start;
        snapshot->bufList->buffer->offset = snapshot->offset;
        snapshot->bufList->lineNo = snapshot->lineNo;
    }

    if (checkpoint->queuedTokens == NULL ||
        sizePrattTokens(checkpoint->queuedTokens) == 0) {
        parser->lexer->tokenHead = NULL;
        parser->lexer->tokenTail = NULL;
    } else {
        Index queuedCount = sizePrattTokens(checkpoint->queuedTokens);
        for (Index i = 0; i < queuedCount; ++i) {
            PrattToken *token = getPrattTokens(checkpoint->queuedTokens, i);
            token->next = i + 1 < queuedCount
                              ? getPrattTokens(checkpoint->queuedTokens, i + 1)
                              : NULL;
        }
        parser->lexer->tokenHead = getPrattTokens(checkpoint->queuedTokens, 0);
        parser->lexer->tokenTail =
            getPrattTokens(checkpoint->queuedTokens, queuedCount - 1);
    }

    parser->panicMode = checkpoint->panicMode;
}

static AstExpression *lookupSyntaxBindingCopy(SyntaxExprBindings *bindings,
                                              HashSymbol *name) {
    if (bindings == NULL) {
        return NULL;
    }
    for (Index i = 0; i < sizeAstExpressionArray(bindings->values); ++i) {
        if (getSymbolArray(bindings->names, i) == name) {
            return copyAstExpression(
                getAstExpressionArray(bindings->values, i));
        }
    }
    return NULL;
}

static HashSymbol *lookupSyntaxBindingSymbol(SyntaxExprBindings *bindings,
                                             HashSymbol *name) {
    if (bindings == NULL) {
        return NULL;
    }
    for (Index i = 0; i < sizeAstExpressionArray(bindings->values); ++i) {
        if (getSymbolArray(bindings->names, i) == name) {
            AstExpression *value = getAstExpressionArray(bindings->values, i);
            if (value->type == AST_EXPRESSION_TYPE_SYMBOL) {
                return getAstExpression_Symbol(value);
            }
            return NULL;
        }
    }
    return NULL;
}

static bool syntaxUnquoteExpressionName(AstExpression *expression,
                                        HashSymbol **name) {
    if (expression->type != AST_EXPRESSION_TYPE_FUNCALL) {
        return false;
    }

    AstFunCall *call = getAstExpression_FunCall(expression);
    if (call->function->type == AST_EXPRESSION_TYPE_SYMBOL) {
        if (getAstExpression_Symbol(call->function) !=
            prattSyntaxUnquoteSymbol()) {
            return false;
        }
    } else if (call->function->type == AST_EXPRESSION_TYPE_ANNOTATEDSYMBOL) {
        if (getAstExpression_AnnotatedSymbol(call->function)->symbol !=
            prattSyntaxUnquoteSymbol()) {
            return false;
        }
    } else {
        return false;
    }

    if (call->arguments != NULL && call->arguments->next == NULL &&
        call->arguments->expression->type == AST_EXPRESSION_TYPE_SYMBOL) {
        *name = getAstExpression_Symbol(call->arguments->expression);
    } else {
        *name = NULL;
    }

    return true;
}

static bool syntaxUnquoteFargName(AstFarg *arg, HashSymbol **name) {
    if (arg->type != AST_FARG_TYPE_UNPACK) {
        return false;
    }

    AstUnpack *unpack = getAstFarg_Unpack(arg);
    if (!isAstLookUpOrSymbol_Symbol(unpack->symbol) ||
        getAstLookUpOrSymbol_Symbol(unpack->symbol) !=
            prattSyntaxUnquoteSymbol()) {
        return false;
    }

    if (unpack->argList != NULL && unpack->argList->next == NULL &&
        unpack->argList->arg->type == AST_FARG_TYPE_SYMBOL) {
        *name = getAstFarg_Symbol(unpack->argList->arg);
    } else {
        *name = NULL;
    }

    return true;
}

static AstFarg *substituteSyntaxFarg(AstFarg *arg, SyntaxExprBindings *bindings,
                                     PrattParser *parser,
                                     bool explicitUnquoteOnly);
static AstExpression *substituteSyntaxExpression(AstExpression *expression,
                                                 SyntaxExprBindings *bindings,
                                                 PrattParser *parser,
                                                 bool explicitUnquoteOnly);
static AstExpressions *substituteSyntaxExpressions(AstExpressions *expressions,
                                                   SyntaxExprBindings *bindings,
                                                   PrattParser *parser,
                                                   bool explicitUnquoteOnly);

static AstFargList *substituteSyntaxFargList(AstFargList *args,
                                             SyntaxExprBindings *bindings,
                                             PrattParser *parser,
                                             bool explicitUnquoteOnly) {
    if (args == NULL) {
        return NULL;
    }
    args->arg =
        substituteSyntaxFarg(args->arg, bindings, parser, explicitUnquoteOnly);
    args->next = substituteSyntaxFargList(args->next, bindings, parser,
                                          explicitUnquoteOnly);
    return args;
}

static AstTaggedArgList *
substituteSyntaxTaggedArgList(AstTaggedArgList *args,
                              SyntaxExprBindings *bindings, PrattParser *parser,
                              bool explicitUnquoteOnly) {
    if (args == NULL) {
        return NULL;
    }
    args->arg =
        substituteSyntaxFarg(args->arg, bindings, parser, explicitUnquoteOnly);
    args->next = substituteSyntaxTaggedArgList(args->next, bindings, parser,
                                               explicitUnquoteOnly);
    return args;
}

static AstAltArgs *substituteSyntaxAltArgs(AstAltArgs *args,
                                           SyntaxExprBindings *bindings,
                                           PrattParser *parser,
                                           bool explicitUnquoteOnly) {
    if (args == NULL) {
        return NULL;
    }
    args->argList = substituteSyntaxFargList(args->argList, bindings, parser,
                                             explicitUnquoteOnly);
    args->next = substituteSyntaxAltArgs(args->next, bindings, parser,
                                         explicitUnquoteOnly);
    return args;
}

static AstDefinitions *substituteSyntaxDefinitions(AstDefinitions *definitions,
                                                   SyntaxExprBindings *bindings,
                                                   PrattParser *parser,
                                                   bool explicitUnquoteOnly);

static AstNest *substituteSyntaxNest(AstNest *nest,
                                     SyntaxExprBindings *bindings,
                                     PrattParser *parser,
                                     bool explicitUnquoteOnly) {
    if (nest == NULL) {
        return NULL;
    }
    nest->definitions = substituteSyntaxDefinitions(
        nest->definitions, bindings, parser, explicitUnquoteOnly);
    nest->expressions = substituteSyntaxExpressions(
        nest->expressions, bindings, parser, explicitUnquoteOnly);
    return nest;
}

static AstAltFunction *substituteSyntaxAltFunction(AstAltFunction *function,
                                                   SyntaxExprBindings *bindings,
                                                   PrattParser *parser,
                                                   bool explicitUnquoteOnly) {
    if (function == NULL) {
        return NULL;
    }
    function->altArgs = substituteSyntaxAltArgs(function->altArgs, bindings,
                                                parser, explicitUnquoteOnly);
    function->nest = substituteSyntaxNest(function->nest, bindings, parser,
                                          explicitUnquoteOnly);
    return function;
}

static AstCompositeFunction *substituteSyntaxCompositeFunction(
    AstCompositeFunction *function, SyntaxExprBindings *bindings,
    PrattParser *parser, bool explicitUnquoteOnly) {
    if (function == NULL) {
        return NULL;
    }
    function->function->argList = substituteSyntaxFargList(
        function->function->argList, bindings, parser, explicitUnquoteOnly);
    function->function->nest = substituteSyntaxNest(
        function->function->nest, bindings, parser, explicitUnquoteOnly);
    function->next = substituteSyntaxCompositeFunction(
        function->next, bindings, parser, explicitUnquoteOnly);
    return function;
}

static AstTaggedExpressions *substituteSyntaxTaggedExpressions(
    AstTaggedExpressions *expressions, SyntaxExprBindings *bindings,
    PrattParser *parser, bool explicitUnquoteOnly) {
    if (expressions == NULL) {
        return NULL;
    }
    expressions->expression = substituteSyntaxExpression(
        expressions->expression, bindings, parser, explicitUnquoteOnly);
    expressions->next = substituteSyntaxTaggedExpressions(
        expressions->next, bindings, parser, explicitUnquoteOnly);
    return expressions;
}

static AstExpressions *substituteSyntaxExpressions(AstExpressions *expressions,
                                                   SyntaxExprBindings *bindings,
                                                   PrattParser *parser,
                                                   bool explicitUnquoteOnly) {
    if (expressions == NULL) {
        return NULL;
    }
    expressions->expression = substituteSyntaxExpression(
        expressions->expression, bindings, parser, explicitUnquoteOnly);
    expressions->next = substituteSyntaxExpressions(
        expressions->next, bindings, parser, explicitUnquoteOnly);
    return expressions;
}

static AstFarg *substituteSyntaxFarg(AstFarg *arg, SyntaxExprBindings *bindings,
                                     PrattParser *parser,
                                     bool explicitUnquoteOnly) {
    if (arg == NULL) {
        return NULL;
    }

    HashSymbol *unquoteName = NULL;
    if (syntaxUnquoteFargName(arg, &unquoteName)) {
        if (!explicitUnquoteOnly) {
            parserErrorAt(CPI(arg), parser,
                          "unquote is only valid inside quote templates");
            return newAstFarg_WildCard(CPI(arg));
        }
        if (unquoteName == NULL) {
            parserErrorAt(CPI(arg), parser,
                          "unquote expects a single bound name");
            return newAstFarg_WildCard(CPI(arg));
        }

        HashSymbol *bound = lookupSyntaxBindingSymbol(bindings, unquoteName);
        if (bound == NULL) {
            parserErrorAt(CPI(arg), parser,
                          "syntax unquote %s is not bound as a name",
                          unquoteName->name);
            return newAstFarg_WildCard(CPI(arg));
        }

        return newAstFarg_Symbol(CPI(arg), bound);
    }

    switch (arg->type) {
    case AST_FARG_TYPE_WILDCARD:
    case AST_FARG_TYPE_LOOKUP:
    case AST_FARG_TYPE_NUMBER:
    case AST_FARG_TYPE_CHARACTER:
        return arg;
    case AST_FARG_TYPE_SYMBOL: {
        if (!explicitUnquoteOnly) {
            HashSymbol *bound =
                lookupSyntaxBindingSymbol(bindings, getAstFarg_Symbol(arg));
            if (bound != NULL) {
                setAstFarg_Symbol(arg, bound);
            }
        }
        return arg;
    }
    case AST_FARG_TYPE_NAMED:
        getAstFarg_Named(arg)->arg = substituteSyntaxFarg(
            getAstFarg_Named(arg)->arg, bindings, parser, explicitUnquoteOnly);
        return arg;
    case AST_FARG_TYPE_UNPACK:
        getAstFarg_Unpack(arg)->argList =
            substituteSyntaxFargList(getAstFarg_Unpack(arg)->argList, bindings,
                                     parser, explicitUnquoteOnly);
        return arg;
    case AST_FARG_TYPE_UNPACKSTRUCT:
        getAstFarg_UnpackStruct(arg)->argList = substituteSyntaxTaggedArgList(
            getAstFarg_UnpackStruct(arg)->argList, bindings, parser,
            explicitUnquoteOnly);
        return arg;
    case AST_FARG_TYPE_TUPLE:
        setAstFarg_Tuple(arg, substituteSyntaxFargList(getAstFarg_Tuple(arg),
                                                       bindings, parser,
                                                       explicitUnquoteOnly));
        return arg;
    }

    cant_happen("unrecognised farg type %s in substituteSyntaxFarg",
                astFargTypeName(arg->type));
}

static AstDefinitions *substituteSyntaxDefinitions(AstDefinitions *definitions,
                                                   SyntaxExprBindings *bindings,
                                                   PrattParser *parser,
                                                   bool explicitUnquoteOnly) {
    if (definitions == NULL) {
        return NULL;
    }
    switch (definitions->definition->type) {
    case AST_DEFINITION_TYPE_DEFINE:
        getAstDefinition_Define(definitions->definition)->expression =
            substituteSyntaxExpression(
                getAstDefinition_Define(definitions->definition)->expression,
                bindings, parser, explicitUnquoteOnly);
        break;
    case AST_DEFINITION_TYPE_MULTI:
        getAstDefinition_Multi(definitions->definition)->expression =
            substituteSyntaxExpression(
                getAstDefinition_Multi(definitions->definition)->expression,
                bindings, parser, explicitUnquoteOnly);
        break;
    case AST_DEFINITION_TYPE_LAZY:
        getAstDefinition_Lazy(definitions->definition)->definition =
            substituteSyntaxAltFunction(
                getAstDefinition_Lazy(definitions->definition)->definition,
                bindings, parser, explicitUnquoteOnly);
        break;
    case AST_DEFINITION_TYPE_ALIAS:
    case AST_DEFINITION_TYPE_TYPEDEF:
    case AST_DEFINITION_TYPE_BLANK:
    case AST_DEFINITION_TYPE_BUILTINSSLOT:
        break;
    case AST_DEFINITION_TYPE_SYNTAXDECL:
    case AST_DEFINITION_TYPE_SYNTAXUSE:
        cant_happen("syntax carrier in substituteSyntaxDefinitions");
    }
    definitions->next = substituteSyntaxDefinitions(
        definitions->next, bindings, parser, explicitUnquoteOnly);
    return definitions;
}

static AstExpression *substituteSyntaxExpression(AstExpression *expression,
                                                 SyntaxExprBindings *bindings,
                                                 PrattParser *parser,
                                                 bool explicitUnquoteOnly) {
    if (expression == NULL) {
        return NULL;
    }

    HashSymbol *unquoteName = NULL;
    if (syntaxUnquoteExpressionName(expression, &unquoteName)) {
        if (!explicitUnquoteOnly) {
            parserErrorAt(CPI(expression), parser,
                          "unquote is only valid inside quote templates");
            return prattErrorExpression(CPI(expression));
        }
        if (unquoteName == NULL) {
            parserErrorAt(CPI(expression), parser,
                          "unquote expects a single bound name");
            return prattErrorExpression(CPI(expression));
        }

        AstExpression *bound = lookupSyntaxBindingCopy(bindings, unquoteName);
        if (bound == NULL) {
            parserErrorAt(CPI(expression), parser,
                          "syntax unquote %s is not bound", unquoteName->name);
            return prattErrorExpression(CPI(expression));
        }
        return bound;
    }

    switch (expression->type) {
    case AST_EXPRESSION_TYPE_BACK:
    case AST_EXPRESSION_TYPE_WILDCARD:
    case AST_EXPRESSION_TYPE_NUMBER:
    case AST_EXPRESSION_TYPE_CHARACTER:
    case AST_EXPRESSION_TYPE_ENV:
        return expression;
    case AST_EXPRESSION_TYPE_ALIAS:
        getAstExpression_Alias(expression)->value = substituteSyntaxExpression(
            getAstExpression_Alias(expression)->value, bindings, parser,
            explicitUnquoteOnly);
        return expression;
    case AST_EXPRESSION_TYPE_ANNOTATEDSYMBOL:
        getAstExpression_AnnotatedSymbol(expression)->originalImpl =
            substituteSyntaxExpression(
                getAstExpression_AnnotatedSymbol(expression)->originalImpl,
                bindings, parser, explicitUnquoteOnly);
        return expression;
    case AST_EXPRESSION_TYPE_FUNCALL:
        getAstExpression_FunCall(expression)->function =
            substituteSyntaxExpression(
                getAstExpression_FunCall(expression)->function, bindings,
                parser, explicitUnquoteOnly);
        getAstExpression_FunCall(expression)->arguments =
            substituteSyntaxExpressions(
                getAstExpression_FunCall(expression)->arguments, bindings,
                parser, explicitUnquoteOnly);
        return expression;
    case AST_EXPRESSION_TYPE_LOOKUP:
        getAstExpression_LookUp(expression)->expression =
            substituteSyntaxExpression(
                getAstExpression_LookUp(expression)->expression, bindings,
                parser, explicitUnquoteOnly);
        return expression;
    case AST_EXPRESSION_TYPE_SYMBOL: {
        if (!explicitUnquoteOnly) {
            AstExpression *bound = lookupSyntaxBindingCopy(
                bindings, getAstExpression_Symbol(expression));
            return bound != NULL ? bound : expression;
        }
        return expression;
    }
    case AST_EXPRESSION_TYPE_FUN:
        substituteSyntaxCompositeFunction(getAstExpression_Fun(expression),
                                          bindings, parser,
                                          explicitUnquoteOnly);
        return expression;
    case AST_EXPRESSION_TYPE_NEST:
        getAstExpression_Nest(expression)->definitions =
            substituteSyntaxDefinitions(
                getAstExpression_Nest(expression)->definitions, bindings,
                parser, explicitUnquoteOnly);
        getAstExpression_Nest(expression)->expressions =
            substituteSyntaxExpressions(
                getAstExpression_Nest(expression)->expressions, bindings,
                parser, explicitUnquoteOnly);
        return expression;
    case AST_EXPRESSION_TYPE_IFF:
        getAstExpression_Iff(expression)->test =
            substituteSyntaxExpression(getAstExpression_Iff(expression)->test,
                                       bindings, parser, explicitUnquoteOnly);
        getAstExpression_Iff(expression)->consequent =
            substituteSyntaxNest(getAstExpression_Iff(expression)->consequent,
                                 bindings, parser, explicitUnquoteOnly);
        getAstExpression_Iff(expression)->alternative =
            substituteSyntaxNest(getAstExpression_Iff(expression)->alternative,
                                 bindings, parser, explicitUnquoteOnly);
        return expression;
    case AST_EXPRESSION_TYPE_PRINT:
        getAstExpression_Print(expression)->exp =
            substituteSyntaxExpression(getAstExpression_Print(expression)->exp,
                                       bindings, parser, explicitUnquoteOnly);
        return expression;
    case AST_EXPRESSION_TYPE_TYPEOF:
        getAstExpression_TypeOf(expression)->exp =
            substituteSyntaxExpression(getAstExpression_TypeOf(expression)->exp,
                                       bindings, parser, explicitUnquoteOnly);
        return expression;
    case AST_EXPRESSION_TYPE_TUPLE:
        expression->val.tuple =
            substituteSyntaxExpressions(getAstExpression_Tuple(expression),
                                        bindings, parser, explicitUnquoteOnly);
        return expression;
    case AST_EXPRESSION_TYPE_STRUCTURE:
        getAstExpression_Structure(expression)->expressions =
            substituteSyntaxTaggedExpressions(
                getAstExpression_Structure(expression)->expressions, bindings,
                parser, explicitUnquoteOnly);
        return expression;
    case AST_EXPRESSION_TYPE_ASSERTION:
        expression->val.assertion =
            substituteSyntaxExpression(getAstExpression_Assertion(expression),
                                       bindings, parser, explicitUnquoteOnly);
        return expression;
    case AST_EXPRESSION_TYPE_ERROR:
        expression->val.error =
            substituteSyntaxExpression(getAstExpression_Error(expression),
                                       bindings, parser, explicitUnquoteOnly);
        return expression;
    case AST_EXPRESSION_TYPE_SYNTAXUSE:
        cant_happen("syntax carrier in substituteSyntaxExpression");
    }

    cant_happen("unrecognised expression type %s in substituteSyntaxExpression",
                astExpressionTypeName(expression->type));
}

static AstExpression *parseSyntaxHoleExpression(PrattParser *parser,
                                                PrattMacroHole *hole,
                                                SyntaxExprBindings *bindings) {
    switch (hole->syntaxClass) {
    case PRATTSYNTAXCLASS_TYPE_EXPR:
        return prattExpressionPrecedence(parser, 0);
    case PRATTSYNTAXCLASS_TYPE_NAME: {
        PrattToken *token = next(parser);
        HashSymbol *name = prattTokenTypeOrAtom(token);
        if (name == TOK_EOF() || name == TOK_SEMI() || name == TOK_CLOSE() ||
            name == TOK_COMMA() || name == TOK_PRODUCTION() ||
            name == TOK_COLON()) {
            parserError(parser, "expected identifier for syntax hole %s",
                        hole->name->name);
            return prattErrorExpression(TOKPI(token));
        }
        return newAstExpression_Symbol(TOKPI(token), name);
    }
    case PRATTSYNTAXCLASS_TYPE_NEST: {
        AstNest *body = prattNest(parser);
        int save = PROTECT(body);
        AstExpression *result = newAstExpression_Nest(CPI(body), body);
        UNPROTECT(save);
        return result;
    }
    case PRATTSYNTAXCLASS_TYPE_STRING: {
        PrattToken *token = peek(parser);
        if (token->type != TOK_STRING()) {
            parserError(parser, "expected string for syntax hole %s",
                        hole->name->name);
            return prattErrorExpression(TOKPI(token));
        }
        token = next(parser);
        enqueueToken(parser->lexer, token);
        WCharArray *uni = prattString(parser);
        int save = PROTECT(uni);
        AstFunCall *list = prattMakeStringList(TOKPI(token), uni);
        PROTECT(list);
        AstExpression *result = newAstExpression_FunCall(TOKPI(token), list);
        UNPROTECT(save);
        return result;
    }
    case PRATTSYNTAXCLASS_TYPE_SYNTAX: {
        PrattMacroSpec *spec =
            prattFindSyntaxSpecByName(parser, hole->callTarget);
        if (spec == NULL) {
            parserError(parser, "unknown syntax helper %s",
                        hole->callTarget->name);
            return prattErrorExpression(LEXPI(parser->lexer));
        }

        Index parameterCount =
            spec->parameters != NULL ? sizeSymbolArray(spec->parameters) : 0;
        Index argumentCount = hole->callArguments != NULL
                                  ? sizeSymbolArray(hole->callArguments)
                                  : 0;
        if (parameterCount != argumentCount) {
            parserError(parser,
                        "syntax helper %s expects %ld arguments but got %ld",
                        hole->callTarget->name, (long)parameterCount,
                        (long)argumentCount);
            return prattErrorExpression(LEXPI(parser->lexer));
        }

        AstExpressionArray *inheritedValues = newAstExpressionArray();
        int save = PROTECT(inheritedValues);
        for (Index i = 0; i < argumentCount; ++i) {
            HashSymbol *actual = getSymbolArray(hole->callArguments, i);
            AstExpression *value =
                bindings != NULL ? lookupSyntaxBindingCopy(bindings, actual)
                                 : NULL;
            if (value == NULL) {
                parserError(parser, "syntax call argument %s is not bound",
                            actual->name);
                UNPROTECT(save);
                return prattErrorExpression(LEXPI(parser->lexer));
            }
            int save2 = PROTECT(value);
            pushAstExpressionArray(inheritedValues, value);
            UNPROTECT(save2);
        }

        SyntaxExprBindings inherited = {
            .names = spec->parameters,
            .values = inheritedValues,
        };
        PrattToken *start = peek(parser);
        AstExpression *result = prattExpandSyntaxExprWithBindings(
            parser, start, spec, false, &inherited);
        UNPROTECT(save);
        return result;
    }
    default:
        parserError(parser,
                    "syntax class %s is not yet supported in expression syntax",
                    prattSyntaxClassName(hole->syntaxClass));
        return prattErrorExpression(LEXPI(parser->lexer));
    }
}

static AstExpression *expandSyntaxAlternative(
    PrattParser *parser, PrattToken *tok, PrattMacroSpec *spec,
    PrattMacroPatternItems *patternItems, bool headAlreadyConsumed,
    SyntaxExprBindings *inherited, int alternativeIndex) {
    Index parameterCount =
        spec->parameters != NULL ? sizeSymbolArray(spec->parameters) : 0;
    Index inheritedCount =
        inherited != NULL ? sizeAstExpressionArray(inherited->values) : 0;
    if (parameterCount != inheritedCount) {
        parserError(
            parser, "syntax %s expects %ld inherited arguments but got %ld",
            spec->headSymbol->name, (long)parameterCount, (long)inheritedCount);
        return prattErrorExpression(TOKPI(tok));
    }

    Index itemCount = countPrattMacroPatternItems(patternItems);
    SymbolArray *names = newSymbolArray();
    AstExpressionArray *values = newAstExpressionArray();
    int save = STARTPROTECT();
    PROTECT(names);
    PROTECT(values);

    for (Index i = 0; i < parameterCount; ++i) {
        AstExpression *value =
            copyAstExpression(getAstExpressionArray(inherited->values, i));
        int save2 = PROTECT(value);
        pushSymbolArray(names, getSymbolArray(spec->parameters, i));
        pushAstExpressionArray(values, value);
        UNPROTECT(save2);
    }

    SyntaxExprBindings bindings = {.names = names, .values = values};

    bool consumedPattern = headAlreadyConsumed;
    if (spec->isExprEntry && !headAlreadyConsumed) {
        PrattToken *nextTok = peek(parser);
        if (!prattIsTokenTypeOrAtom(nextTok, spec->headSymbol)) {
            UNPROTECT(save);
            return NULL;
        }
        next(parser);
        consumedPattern = true;
    }

    for (Index i = 0; i < itemCount; ++i) {
        PrattMacroPatternItem *item =
            getPrattMacroPatternItems(patternItems, i);
        if (item->type == PRATTMACROPATTERNITEM_TYPE_QUOTEDTERMINAL) {
            HashSymbol *expected =
                getPrattMacroPatternItem_QuotedTerminal(item);
            PrattToken *nextTok = peek(parser);
            if (!prattIsTokenTypeOrAtom(nextTok, expected)) {
                if (consumedPattern) {
                    parserErrorAt(TOKPI(nextTok), parser,
                                  "expected syntax token '%s'", expected->name);
                }
                UNPROTECT(save);
                return consumedPattern ? prattErrorExpression(TOKPI(nextTok))
                                       : NULL;
            }
            next(parser);
            consumedPattern = true;
        } else {
            PrattMacroHole *hole = getPrattMacroPatternItem_TypedHole(item);
            if (symbolArrayContains(names, hole->name)) {
                parserError(parser, "syntax binding %s already defined",
                            hole->name->name);
                UNPROTECT(save);
                return prattErrorExpression(TOKPI(tok));
            }
            AstExpression *value =
                parseSyntaxHoleExpression(parser, hole, &bindings);
            if (value == NULL) {
                PrattToken *nextTok = peek(parser);
                UNPROTECT(save);
                return consumedPattern ? prattErrorExpression(TOKPI(nextTok))
                                       : NULL;
            }
            int save2 = PROTECT(value);
            pushSymbolArray(names, hole->name);
            pushAstExpressionArray(values, value);
            UNPROTECT(save2);
            consumedPattern = true;
        }
    }

    // Build AstSyntaxBindings from collected captures.
    AstSyntaxBindings *astBindings = newAstSyntaxBindings();
    PROTECT(astBindings);
    Index totalCount = sizeSymbolArray(names);
    for (Index i = 0; i < totalCount; ++i) {
        AstSyntaxBinding *binding =
            newAstSyntaxBinding(TOKPI(tok), getSymbolArray(names, i),
                                getAstExpressionArray(values, i));
        int save2 = PROTECT(binding);
        binding->inherited = (i < parameterCount);
        pushAstSyntaxBindings(astBindings, binding);
        UNPROTECT(save2);
    }

    AstExpression *result = makeAstExpression_SyntaxUse(
        TOKPI(tok), spec->declarationId, alternativeIndex, astBindings);
    PROTECT(result);
    UNPROTECT(save);
    return result;
}

static bool syntaxAlternativeIsEmpty(PrattMacroAlternative *alternative) {
    return countPrattMacroPatternItems(alternative->patternItems) == 0;
}

static AstExpression *
trySyntaxAlternative(PrattParser *parser, PrattToken *tok, PrattMacroSpec *spec,
                     PrattMacroAlternative *alternative,
                     bool headAlreadyConsumed, SyntaxExprBindings *inherited,
                     bool *hadCommittedFailure, int alternativeIndex) {
    SyntaxLexerCheckpoint checkpoint = captureSyntaxLexerCheckpoint(parser);
    int save = STARTPROTECT();
    if (checkpoint.bufList != NULL) {
        PROTECT(checkpoint.bufList);
    }
    if (checkpoint.queuedTokens != NULL) {
        PROTECT(checkpoint.queuedTokens);
    }

    bool suppressErrors = parser->suppressErrors;
    parser->suppressErrors = true;
    AstExpression *result = expandSyntaxAlternative(
        parser, tok, spec, alternative->patternItems, headAlreadyConsumed,
        inherited, alternativeIndex);
    bool alternativeFailed = parser->panicMode;
    parser->suppressErrors = suppressErrors;

    if (!alternativeFailed && result != NULL) {
        UNPROTECT(save);
        return result;
    }

    restoreSyntaxLexerCheckpoint(parser, &checkpoint);
    UNPROTECT(save);
    *hadCommittedFailure = *hadCommittedFailure || alternativeFailed;
    return NULL;
}

AstExpression *prattExpandSyntaxExprWithBindings(
    PrattParser *parser, PrattToken *tok, PrattMacroSpec *spec,
    bool headAlreadyConsumed, SyntaxExprBindings *inherited) {
    if (spec->alternatives == NULL ||
        sizePrattMacroAlternatives(spec->alternatives) == 0) {
        return expandSyntaxAlternative(parser, tok, spec, spec->patternItems,
                                       headAlreadyConsumed, inherited, 0);
    }

    bool hadCommittedFailure = false;
    bool helperOnly = !spec->isExprEntry;
    for (int pass = 0; pass < (helperOnly ? 2 : 1); ++pass) {
        for (Index i = 0; i < sizePrattMacroAlternatives(spec->alternatives);
             ++i) {
            PrattMacroAlternative *alternative =
                getPrattMacroAlternatives(spec->alternatives, i);
            bool isEmpty = syntaxAlternativeIsEmpty(alternative);

            if (helperOnly &&
                ((pass == 0 && isEmpty) || (pass == 1 && !isEmpty))) {
                continue;
            }

            AstExpression *result = trySyntaxAlternative(
                parser, tok, spec, alternative, headAlreadyConsumed, inherited,
                &hadCommittedFailure, (int)i);
            if (result != NULL) {
                return result;
            }
        }
    }

    if (hadCommittedFailure) {
        parserErrorAt(TOKPI(tok), parser,
                      "no syntax alternative matched for %s",
                      spec->headSymbol->name);
        return prattErrorExpression(TOKPI(tok));
    }

    return NULL;
}

AstExpression *prattExpandSyntaxExpr(PrattParser *parser, PrattToken *tok,
                                     PrattMacroSpec *spec) {
    return prattExpandSyntaxExprWithBindings(parser, tok, spec, true, NULL);
}

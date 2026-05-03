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

#include <stdlib.h>
#include <wchar.h>
#include <wctype.h>

#include "memory.h"
#include "pratt_parser.h"
#include "pratt_scanner.h"
#include "symbols.h"
#include "syntax_template.h"

static HashSymbol *unicodeToSymbol(WCharArray *unicode) {
    size_t len = wcstombs(NULL, unicode->entries, 0);
    SCharVec *mbStr = newSCharVec(len + 1);
    int save = PROTECT(mbStr);
    wcstombs(mbStr->entries, unicode->entries, len + 1);
    setSCharVec(mbStr, len, 0);
    HashSymbol *res = newSymbol(mbStr->entries);
    UNPROTECT(save);
    return res;
}

static void checkTerminal(PrattParser *parser, WCharArray *string) {
    if (countWCharArray(string) == 0) {
        parserError(parser, "empty identifier string");
    }
    if (getWCharArray(string, 0) == 0) {
        parserError(parser, "empty identifier string");
    }
    for (Index i = 0;
         i < countWCharArray(string) && getWCharArray(string, i) != 0; ++i) {
        if (iswspace(getWCharArray(string, i))) {
            parserError(parser, "space in quoted token");
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

HashSymbol *prattSyntaxQuoteSymbol(void) {
    static HashSymbol *symbol = NULL;
    if (symbol == NULL) {
        symbol = newSymbol("quote");
    }
    return symbol;
}

HashSymbol *prattSyntaxUnquoteSymbol(void) {
    static HashSymbol *symbol = NULL;
    if (symbol == NULL) {
        symbol = newSymbol("unquote");
    }
    return symbol;
}

bool prattIsSyntaxQuoteToken(PrattToken *token) {
    return prattIsTokenTypeOrAtom(token, prattSyntaxQuoteSymbol());
}

bool prattIsSyntaxPatternBoundaryToken(PrattToken *token) {
    return prattIsSyntaxQuoteToken(token) || token->type == TOK_LCURLY() ||
           token->type == TOK_PIPE() || token->type == TOK_SEMI() ||
           token->type == TOK_EOF();
}

AstExpression *prattMakeSyntaxQuotedTemplate(ParserInfo PI,
                                             AstExpression *body) {
    AstExpression *quoteSymbol =
        newAstExpression_Symbol(PI, prattSyntaxQuoteSymbol());
    int save = PROTECT(quoteSymbol);
    AstExpressions *arguments = newAstExpressions(PI, body, NULL);
    PROTECT(arguments);
    AstExpression *result =
        makeAstExpression_FunCall(PI, quoteSymbol, arguments);
    UNPROTECT(save);
    return result;
}

AstExpression *prattUnwrapSyntaxQuotedTemplate(AstExpression *template) {
    if (template == NULL || template->type != AST_EXPRESSION_TYPE_FUNCALL) {
        return NULL;
    }

    AstFunCall *call = getAstExpression_FunCall(template);
    if (call->function->type != AST_EXPRESSION_TYPE_SYMBOL ||
        getAstExpression_Symbol(call->function) != prattSyntaxQuoteSymbol() ||
        call->arguments == NULL || call->arguments->next != NULL) {
        return NULL;
    }

    return call->arguments->expression;
}

HashSymbol *prattSyntaxSymbol(PrattParser *parser) {
    PrattToken *token = next(parser);
    return prattTokenTypeOrAtom(token);
}

SymbolArray *prattParseSyntaxCallArguments(PrattParser *parser) {
    if (!match(parser, TOK_OPEN())) {
        return NULL;
    }

    SymbolArray *arguments = newSymbolArray();
    int save = PROTECT(arguments);

    for (;;) {
        pushSymbolArray(arguments, prattSyntaxSymbol(parser));
        if (!match(parser, TOK_COMMA())) {
            break;
        }
    }

    consume(parser, TOK_CLOSE());
    UNPROTECT(save);
    return arguments;
}

static PrattMacroHole *typedHole(PrattParser *parser) {
    PrattToken *token = next(parser);
    int save = PROTECT(token);
    HashSymbol *bindingName = prattTokenTypeOrAtom(token);
    consume(parser, TOK_COLON());
    token = next(parser);
    REPLACE_PROTECT(save, token);
    PrattMacroHole *result = NULL;
    if (prattIsTokenTypeOrAtom(token, TOK_EXPRTYPE())) {
        result = newPrattMacroHole(PRATTSYNTAXCLASS_TYPE_EXPR, bindingName);
    } else if (prattIsTokenTypeOrAtom(token, TOK_NAMETYPE())) {
        result = newPrattMacroHole(PRATTSYNTAXCLASS_TYPE_NAME, bindingName);
    } else if (prattIsTokenTypeOrAtom(token, TOK_NESTTYPE())) {
        result = newPrattMacroHole(PRATTSYNTAXCLASS_TYPE_NEST, bindingName);
    } else if (prattIsTokenTypeOrAtom(token, TOK_STRINGTYPE()) ||
               prattIsTokenTypeOrAtom(token, newSymbol("String"))) {
        result = newPrattMacroHole(PRATTSYNTAXCLASS_TYPE_STRING, bindingName);
    } else if (prattIsTokenTypeOrAtom(token, TOK_TYPETYPE())) {
        result = newPrattMacroHole(PRATTSYNTAXCLASS_TYPE_TYPE, bindingName);
    } else if (prattIsTokenTypeOrAtom(token, newSymbol("Syntax"))) {
        result = newPrattMacroHole(PRATTSYNTAXCLASS_TYPE_SYNTAX, bindingName);
        int save2 = PROTECT(result);
        consume(parser, TOK_OPEN());
        result->callTarget = prattSyntaxSymbol(parser);
        result->callArguments = prattParseSyntaxCallArguments(parser);
        consume(parser, TOK_CLOSE());
        UNPROTECT(save2);
    } else {
        parserError(parser, "unrecognised syntax class %s",
                    prattTokenTypeOrAtom(token)->name);
    }
    UNPROTECT(save);
    return result;
}

static PrattMacroPatternItems *macroPatternItems(PrattParser *parser) {
    PrattMacroPatternItems *result = newPrattMacroPatternItems();
    int save = PROTECT(result);
    for (;;) {
        if (check(parser, TOK_STRING())) {
            WCharArray *str = prattRawString(parser);
            int save2 = PROTECT(str);
            checkTerminal(parser, str);
            HashSymbol *symbol = unicodeToSymbol(str);
            PrattMacroPatternItem *this =
                newPrattMacroPatternItem_QuotedTerminal(symbol);
            PROTECT(this);
            pushPrattMacroPatternItems(result, this);
            UNPROTECT(save2);
        } else if (!prattIsSyntaxPatternBoundaryToken(peek(parser))) {
            if (prattIsSyntaxQuoteToken(peek(parser))) {
                break;
            }
            PrattMacroHole *hole = typedHole(parser);
            if (hole == NULL)
                break;
            else {
                int save2 = PROTECT(hole);
                PrattMacroPatternItem *this =
                    newPrattMacroPatternItem_TypedHole(hole);
                PROTECT(this);
                pushPrattMacroPatternItems(result, this);
                UNPROTECT(save2);
            }
        } else {
            break;
        }
    }
    UNPROTECT(save);
    return result;
}

SymbolArray *prattParseOptionalSyntaxParameters(PrattParser *parser) {
    if (!match(parser, TOK_OPEN())) {
        return NULL;
    }

    SymbolArray *parameters = newSymbolArray();
    int save = PROTECT(parameters);

    for (;;) {
        HashSymbol *parameter = prattSyntaxSymbol(parser);
        if (symbolArrayContains(parameters, parameter)) {
            parserError(parser, "duplicate syntax parameter %s",
                        parameter->name);
        } else {
            pushSymbolArray(parameters, parameter);
        }
        if (!match(parser, TOK_COMMA())) {
            break;
        }
    }

    consume(parser, TOK_CLOSE());
    UNPROTECT(save);
    return parameters;
}

PrattMacroAlternative *prattParseSyntaxAlternative(PrattParser *parser,
                                                   SymbolArray *parameters) {
    PrattMacroPatternItems *patternItems = macroPatternItems(parser);
    int save = PROTECT(patternItems);
    AstExpression *template = NULL;
    if (prattIsSyntaxQuoteToken(peek(parser))) {
        PrattToken *quoteToken = next(parser);
        int save2 = PROTECT(quoteToken);

        SymbolArray *overrideSymbols = newSymbolArray();
        int save3 = PROTECT(overrideSymbols);
        pushSymbolArray(overrideSymbols, prattSyntaxUnquoteSymbol());
        for (Index i = 0; i < countPrattMacroPatternItems(patternItems); ++i) {
            PrattMacroPatternItem *item =
                getPrattMacroPatternItems(patternItems, i);
            if (item->type == PRATTMACROPATTERNITEM_TYPE_TYPEDHOLE) {
                HashSymbol *name =
                    getPrattMacroPatternItem_TypedHole(item)->name;
                if (!symbolArrayContains(overrideSymbols, name)) {
                    pushSymbolArray(overrideSymbols, name);
                }
            }
        }
        if (parameters != NULL) {
            for (Index i = 0; i < sizeSymbolArray(parameters); ++i) {
                HashSymbol *name = getSymbolArray(parameters, i);
                if (!symbolArrayContains(overrideSymbols, name)) {
                    pushSymbolArray(overrideSymbols, name);
                }
            }
        }

        Index overrideCount = sizeSymbolArray(overrideSymbols);
        PrattRecord **savedRecords = calloc(
            overrideCount == 0 ? 1 : overrideCount, sizeof(PrattRecord *));
        for (Index i = 0; i < overrideCount; ++i) {
            HashSymbol *symbol = getSymbolArray(overrideSymbols, i);
            getPrattRecordTable(parser->rules, symbol, &savedRecords[i]);
            if (savedRecords[i] != NULL) {
                PrattRecord *overrideRecord = copyPrattRecord(savedRecords[i]);
                int save4 = PROTECT(overrideRecord);
                overrideRecord->prefix.op = prattMakeAtomParselet;
                overrideRecord->prefix.prec = 0;
                setPrattRecordTable(parser->rules, symbol, overrideRecord);
                UNPROTECT(save4);
            }
        }

        AstNest *body = prattNest(parser);

        for (Index i = 0; i < overrideCount; ++i) {
            if (savedRecords[i] != NULL) {
                setPrattRecordTable(parser->rules,
                                    getSymbolArray(overrideSymbols, i),
                                    savedRecords[i]);
            }
        }
        free(savedRecords);
        UNPROTECT(save3);

        if (body != NULL) {
            PROTECT(body);
            AstExpression *quotedBody = newAstExpression_Nest(CPI(body), body);
            PROTECT(quotedBody);
            template =
                prattMakeSyntaxQuotedTemplate(TOKPI(quoteToken), quotedBody);
            PROTECT(template);
        }
        UNPROTECT(save2);
    } else {
        AstNest *body = prattNest(parser);
        if (body != NULL) {
            PROTECT(body);
            template = newAstExpression_Nest(CPI(body), body);
            PROTECT(template);
        }
    }
    PrattMacroAlternative *alternative =
        newPrattMacroAlternative(patternItems, template);
    PROTECT(alternative);
    UNPROTECT(save);
    return alternative;
}

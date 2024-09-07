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

#include <stdio.h>

#include"pratt.h"
#include"pratt_debug.h"
#include"pratt_functions.h"
#include"pratt_scanner.h"
#include"symbols.h"
#include"ast.h"
#include"bigint.h"
#include"utf8.h"

#ifdef DEBUG_PRATT_PARSER
#  include "debugging_on.h"
#else
#  include "debugging_off.h"
#endif

// UTF8
// re-entrant
// stacked input stream
// modified during parse
// integrates with existing AST

// only one precedence
// operators can't be both infix and postfix
// left associative infix operators parse the rhs with prec + 1
// right associative infix operators parse the rhs with prec - 1
// pareslets for grouping know their own matching close brace

void ppAstExpression(AstExpression *expr);
AstExpression *expr_bp(PrattLexer *lexer, PrattParser *parser, int min_bp);
static AstExpression *grouping(PrattRecord *, PrattLexer *, PrattParser *, AstExpression *);
static AstExpression *prefix(PrattRecord *, PrattLexer *, PrattParser *, AstExpression *);
static AstExpression *prefixC(PrattRecord *, PrattLexer *, PrattParser *, AstExpression *);
static AstExpression *postfix(PrattRecord *,
                          PrattLexer * __attribute__((unused)),
                          PrattParser * __attribute__((unused)),
                          AstExpression *);
static AstExpression *tuple(PrattRecord *record __attribute__((unused)),
                            PrattLexer *lexer,
                            PrattParser *parser,
                            AstExpression *lhs);
static AstExpression *postfixArg(PrattRecord *, PrattLexer *, PrattParser *, AstExpression *);
static AstExpression *infixLeft(PrattRecord *, PrattLexer *, PrattParser *, AstExpression *);
static AstExpression *infixRight(PrattRecord *, PrattLexer *, PrattParser *, AstExpression *);


static void addRecord(PrattTable *table, char *token, PrattOp prefix, PrattOp infix, PrattOp postfix, int precedence) {
    PrattRecord *record = newPrattRecord(newSymbol(token), prefix, infix, postfix, precedence);
    int save = PROTECT(record);
    setPrattTable(table, record->symbol, record);
    UNPROTECT(save);
}

static PrattParser *makePrattParser() {
    PrattParser *res = newPrattParser(NULL);
    int save = PROTECT(res);
    PrattTable *table = res->rules;
    addRecord(table, "#(",   tuple,    NULL,       NULL,       0);
    addRecord(table, "(",    grouping, NULL,       postfixArg, 0);
    addRecord(table, ")",    NULL,     NULL,       NULL,       0);
    addRecord(table, ",",    NULL,     NULL,       NULL,       0);

    addRecord(table, "->",   NULL,     infixRight, NULL,      10);

    addRecord(table, "then", NULL,     infixRight, NULL,      20);

    addRecord(table, "and",  NULL,     infixLeft,  NULL,      30);
    addRecord(table, "or",   NULL,     infixLeft,  NULL,      30);
    addRecord(table, "xor",  NULL,     infixLeft,  NULL,      30);
    addRecord(table, "nand", NULL,     infixLeft,  NULL,      30);
    addRecord(table, "nor",  NULL,     infixLeft,  NULL,      30);
    addRecord(table, "xnor", NULL,     infixLeft,  NULL,      30);

    addRecord(table, "not",  prefix,   NULL,       NULL,      40);

    addRecord(table, "==",   NULL,     infixLeft,  NULL,      50);
    addRecord(table, "!=",   NULL,     infixLeft,  NULL,      50);
    addRecord(table, ">",    prefixC,  infixLeft,  NULL,      50);
    addRecord(table, "<",    prefixC,  infixLeft,  NULL,      50);
    addRecord(table, ">=",   NULL,     infixLeft,  NULL,      50);
    addRecord(table, "<=",   NULL,     infixLeft,  NULL,      50);
    addRecord(table, "<=>",  NULL,     infixLeft,  NULL,      50);

    addRecord(table, "=",    NULL,     infixRight, NULL,      60);

    addRecord(table, ":",    NULL,     infixLeft,  NULL,      70);

    addRecord(table, "@@",   NULL,     infixRight, NULL,      80);

    addRecord(table, "@",    NULL,     infixRight, NULL,      90);
    addRecord(table, "+",    prefixC,  infixLeft,  NULL,      90);
    addRecord(table, "-",    prefixC,  infixLeft,  NULL,      90);

    addRecord(table, "*",    NULL,     infixLeft,  NULL,     100);
    addRecord(table, "/",    NULL,     infixLeft,  NULL,     100);
    addRecord(table, "%",    NULL,     infixLeft,  NULL,     100);

    addRecord(table, "**",   NULL,     infixRight, NULL,     110);

    addRecord(table, "here", prefix,   NULL,       NULL,     120);
    addRecord(table, "#",    prefix,   NULL,       NULL,     120);
    addRecord(table, "!",    NULL,     NULL,       postfix,  120);

    addRecord(table, ".",    NULL,     infixRight, NULL,     130);

    UNPROTECT(save);
    return res;
}

static void bigint_mul_by_n(bigint *b, int n) {
    bigint old;
    bigint_init(&old);
    bigint_cpy(&old, b);
    bigint_free(b);
    bigint_init(b);
    bigint mul;
    bigint_init(&mul);
    bigint_from_int(&mul, n);
    bigint_mul(b, &mul, &old);
    bigint_free(&mul);
    bigint_free(&old);
}

static void bigint_add_n(bigint *b, int n) {
    bigint old;
    bigint_init(&old);
    bigint_cpy(&old, b);
    bigint_free(b);
    bigint_init(b);
    bigint_add_word(b, &old, n);
    bigint_free(&old);
}

static int convert_char(char c) {
    switch (c) {
        case '0': case '1': case '2': case '3': case '4': case '5':
        case '6': case '7': case '8': case '9':
            return c - '0';
        case 'a': case 'b': case 'c': case 'd': case 'e': case 'f':
            return 10 + (c - 'a');
        case 'A': case 'B': case 'C': case 'D': case 'E': case 'F':
            return 10 + (c - 'A');
        default:
            cant_happen("unrecognised numeric digit '%c'", c);
    }
}

static MaybeBigInt *makeMaybeBigInt(char *digits, int length) {
    bool overflowed = false;
    bool imag = false;
    int a = 0;
    bigint bi;
    int multiplier = 10;
    if (digits[0] == '0' && (digits[1] == 'x' || digits[1] == 'X')) {
        digits += 2;
        length -= 2;
        multiplier = 16;
    }
    for (char *p = digits; length > 0; --length, ++p) {
        if (*p == '_') continue;
        if (*p == 'i') {
            imag = true;
            continue;
        }
        int n = convert_char(*p);
        if(overflowed) {
            bigint_mul_by_n(&bi, multiplier);
            bigint_add_n(&bi, n);
        } else {
            int c;
            if (__builtin_mul_overflow(a, multiplier, &c)) {
                overflowed = true;
                bigint_init(&bi);
                bigint_from_int(&bi, a);
                bigint_mul_by_n(&bi, multiplier);
                bigint_add_n(&bi, n);
            } else {
                a = c;
                if (__builtin_add_overflow(a, n, &c)) {
                    overflowed = true;
                    bigint_init(&bi);
                    bigint_from_int(&bi, a);
                    bigint_add_n(&bi, n);
                } else {
                    a = c;
                }
            }
        }
    }
    if (overflowed) {
        MaybeBigInt *bbi = newMaybeBigInt(bi, imag);
        return bbi;
    } else {
        return fakeBigInt(a, imag);
    }
}

static MaybeBigInt *makeIrrational(char *str, int length) {
    bool imag = false;
    bool frac = false;
    Double f = 0.0;
    Double div = 1.0;
    for (char *p = str; length > 0; --length, ++p) {
        switch (*p) {
            case '_':
                continue;
            case 'i':
                imag = true;
                continue;
            case '.':
                frac = true;
                continue;
            default:
                int n = convert_char(*p);
                f *= 10.0;
                f += n;
                if (frac) div *= 10.0;
        }
    }
    return irrationalBigInt(f/div, imag);
}

static PrattTrie *makePrattTrie(PrattParser *parser) {
    PrattTrie *C = NULL;
    HashSymbol *token;
    Index i = 0;
    int save = PROTECT(parser); // not C because we need to have a slot for REPLACE_PROTECT
    while ((token = iteratePrattTable(parser->rules, &i, NULL)) != NULL) {
        C = insertPrattTrie(C, token);
        REPLACE_PROTECT(save, C);
    }
    UNPROTECT(save);
    return C;
}

void ppAstFunCall(AstFunCall *funCall) {
    ppAstExpression(funCall->function);
    printf("(");
    for (AstExpressions *expressions = funCall->arguments; expressions != NULL; expressions = expressions->next) {
        ppAstExpression(expressions->expression);
        if (expressions->next) {
            printf(", ");
        }
    }
    printf(")");
}

void ppAstCharacter(Character c) {
    unsigned char buffer[8];
    unsigned char *end = writeChar(buffer, c);
    *end = 0;
    printf("'%s'", buffer);
}

void ppAstTuple(AstExpressions *expressions) {
    printf("#(");
    while (expressions) {
        ppAstExpression(expressions->expression);
        if (expressions->next)
            printf(", ");
        expressions = expressions->next;
    }
    printf(")");
}

void ppAstExpression(AstExpression *expr) {
    switch (expr->type) {
        case AST_EXPRESSION_TYPE_NUMBER:
            fprintMaybeBigInt(stdout, expr->val.number);
            break;
        case AST_EXPRESSION_TYPE_SYMBOL:
            printf("%s", expr->val.symbol->name);
            break;
        case AST_EXPRESSION_TYPE_FUNCALL:
            ppAstFunCall(expr->val.funCall);
            break;
        case AST_EXPRESSION_TYPE_CHARACTER:
            ppAstCharacter(expr->val.character);
            break;
        case AST_EXPRESSION_TYPE_TUPLE:
            ppAstTuple(expr->val.tuple);
            break;
        default:
            cant_happen("unexpected %s", astExpressionTypeName(expr->type));
    }
}

static AstExpression *makePrattBinary(ParserInfo I, HashSymbol *op, AstExpression *lhs, AstExpression *rhs) {
    AstExpressions *arguments = newAstExpressions(CPI(rhs), rhs, NULL);
    int save = PROTECT(arguments);
    arguments = newAstExpressions(CPI(lhs), lhs, arguments);
    REPLACE_PROTECT(save, arguments);
    AstExpression *symbol = newAstExpression_Symbol(I, op);
    PROTECT(symbol);
    AstFunCall *funCall = newAstFunCall(I, symbol, arguments);
    REPLACE_PROTECT(save, funCall);
    AstExpression *res = newAstExpression_FunCall(I, funCall);

    UNPROTECT(save);
    return res;
}

static AstExpression *makePrattUnary(ParserInfo I, HashSymbol *op, AstExpression *expr) {
    AstExpressions *arguments = newAstExpressions(CPI(expr), expr, NULL);
    int save = PROTECT(arguments);
    AstExpression *symbol = newAstExpression_Symbol(I, op);
    PROTECT(symbol);
    AstFunCall *funCall = newAstFunCall(I, symbol, arguments);
    REPLACE_PROTECT(save, funCall);
    AstExpression *res = newAstExpression_FunCall(I, funCall);
    UNPROTECT(save);
    return res;
}

static PrattRecord *fetchRecord(PrattParser *parser, HashSymbol *symbol) {
    PrattRecord *record = NULL;
    if (getPrattTable(parser->rules, symbol, &record)) {
        return record;
    } else {
        cant_happen("unrecognised op %s", symbol->name);
    }
}

static AstExpression *grouping(PrattRecord *record, PrattLexer *lexer, PrattParser *parser, AstExpression *lhs __attribute__((unused))) {
    ENTER(grouping);
    AstExpression *res = expr_bp(lexer, parser, record->precedence);
    int save = PROTECT(res);
    consume(lexer, S(")"), "expect ')'");
    LEAVE(grouping);
    UNPROTECT(save);
    return res;
}

static AstExpression *prefix(PrattRecord *record, PrattLexer *lexer, PrattParser *parser, AstExpression *lhs __attribute__((unused))) {
    ENTER(prefix);
    AstExpression *res = expr_bp(lexer, parser, record->precedence + 1);
    int save = PROTECT(res);
    res = makePrattUnary(CPI(res), record->symbol, res);
    LEAVE(prefix);
    UNPROTECT(save);
    return res;
}

static AstExpression *prefixC(PrattRecord *record, PrattLexer *lexer, PrattParser *parser, AstExpression *lhs __attribute__((unused))) {
    ENTER(prefixC);
    AstExpression *res = expr_bp(lexer, parser, 100);
    int save = PROTECT(res);
    res = makePrattUnary(CPI(res), record->symbol, res);
    LEAVE(prefixC);
    UNPROTECT(save);
    return res;
}

static AstExpression *postfix(PrattRecord *record,
                          PrattLexer *lexer __attribute__((unused)),
                          PrattParser *parser __attribute__((unused)),
                          AstExpression *lhs) {
    return makePrattUnary(CPI(lhs), record->symbol, lhs);
}

static AstExpressions *collectArguments(PrattLexer *lexer, PrattParser *parser) {
    AstExpression *arg = expr_bp(lexer, parser, 0);
    int save = PROTECT(arg);
    AstExpressions *next = NULL;
    if (match(lexer, S(","))) {
        next = collectArguments(lexer, parser);
        PROTECT(next);
    }
    AstExpressions *this = newAstExpressions(CPI(arg), arg, next);
    UNPROTECT(save);
    return this;
}

static AstExpressions *collectArgs(PrattLexer *lexer, PrattParser *parser) {
    AstExpressions *args = NULL;
    int save = PROTECT(args);
    if (!check(lexer, S(")"))) {
        args = collectArguments(lexer, parser);
        PROTECT(args);
    }
    consume(lexer, S(")"), "expected ')'");
    UNPROTECT(save);
    return args;
}

static AstExpression *postfixArg(PrattRecord *record __attribute__((unused)),
                                 PrattLexer *lexer,
                                 PrattParser *parser,
                                 AstExpression *lhs) {
    ENTER(postfixArg);
    AstExpressions *args = collectArgs(lexer, parser);
    int save = PROTECT(args);
    AstFunCall *funCall = newAstFunCall(CPI(lhs), lhs, args);
    PROTECT(funCall);
    AstExpression *res = newAstExpression_FunCall(CPI(funCall), funCall);
    LEAVE(postfixArg);
    UNPROTECT(save);
    return res;
}

static AstExpression *tuple(PrattRecord *record __attribute__((unused)),
                            PrattLexer *lexer,
                            PrattParser *parser,
                            AstExpression *lhs __attribute__((unused))) {
    ENTER(tuple);
    PrattToken *peeked = peek(lexer);
    AstExpressions *args = collectArgs(lexer, parser);
    int save = PROTECT(args);
    AstExpression *res = newAstExpression_Tuple(TOKPI(peeked), args);
    LEAVE(tuple);
    UNPROTECT(save);
    return res;
}

static AstExpression *infixLeft(PrattRecord *record, PrattLexer *lexer, PrattParser *parser, AstExpression *lhs) {
    ENTER(infixLeft);
    AstExpression *rhs = expr_bp(lexer, parser, record->precedence + 1);
    int save = PROTECT(rhs);
    rhs = makePrattBinary(CPI(lhs), record->symbol, lhs, rhs);
    LEAVE(infixLeft);
    UNPROTECT(save);
    return rhs;
}

static AstExpression *infixRight(PrattRecord *record, PrattLexer *lexer, PrattParser *parser, AstExpression *lhs) {
    ENTER(infixRight);
    AstExpression *rhs = expr_bp(lexer, parser, record->precedence - 1);
    int save = PROTECT(rhs);
    rhs = makePrattBinary(CPI(rhs), record->symbol, lhs, rhs);
    LEAVE(infixRight);
    UNPROTECT(save);
    return rhs;
}

static AstExpression *makeAtom(PrattToken *token) {
    HashSymbol *name = newSymbolLength(token->value->start, token->value->length);
    return newAstExpression_Symbol(TOKPI(token), name);
}

static AstExpression *makeInt(PrattToken *token) {
    MaybeBigInt *mbi = makeMaybeBigInt(token->value->start, token->value->length);
    int save = PROTECT(mbi);
    AstExpression *res = newAstExpression_Number(TOKPI(token), mbi);
    UNPROTECT(save);
    return res;
}

static AstExpression *makeFloat(PrattToken *token) {
    MaybeBigInt *mbi = makeIrrational(token->value->start, token->value->length);
    int save = PROTECT(mbi);
    AstExpression *res = newAstExpression_Number(TOKPI(token), mbi);
    UNPROTECT(save);
    return res;
}

static Character parseChar(char *text, int length) {
    Character res = 0;
    while (length > 0) {
        if (*text == '\'') {
            text++;
            length--;
        } else if (*text == '\\') {
            text++;
            length--;
            switch (*text) {
                case 'n':
                    res = '\n';
                    break;
                case 't':
                    res = '\t';
                    break;
                case 'u': {
                    text++;
                    length--;
                    bool finished = false;
                    while (!finished) {
                        switch (*text) {
                            case '0': case '1': case '2': case '3': case '4': case '5':
                            case '6': case '7': case '8': case '9':
                                res <<= 4;
                                res |= (*text - '0');
                                text++;
                                length--;
                                break;
                            case 'a': case 'b': case 'c': case 'd': case 'e': case 'f':
                                res <<= 4;
                                res |= (10 + (*text - 'a'));
                                text++;
                                length--;
                                break;
                            case 'A': case 'B': case 'C': case 'D': case 'E': case 'F':
                                res <<= 4;
                                res |= (10 + (*text - 'A'));
                                text++;
                                length--;
                                break;
                            case ';':
                                finished = true;
                                break;
                            default:
                                cant_happen("error parsing unicode escape");
                        }
                    }
                }
                break;
                default:
                    utf8_to_unicode_char(&res, (unsigned char *)text);
                    length = 1;
            }
            text++;
            length--;
        } else {
            utf8_to_unicode_char(&res, (unsigned char *)text);
            length = 0;
        }
    }
    DEBUG("parseChar got %u", (unsigned int) res);
    return res;
}

static AstExpression *makeChar(PrattToken *tok) {
    Character c = parseChar(tok->value->start, tok->value->length);
    AstExpression *res = newAstExpression_Character(TOKPI(tok), c);
    return res;
}

AstExpression *expr_bp(PrattLexer *lexer, PrattParser *parser, int min_bp) {
    ENTER(expr_bp);
    AstExpression *lhs = NULL;
    PrattToken *tok = next(lexer);
    int save = PROTECT(tok);
    if (tok->type == S("ATOM")) {
        lhs = makeAtom(tok);
    } else if (tok->type == S("INT")) {
        lhs = makeInt(tok);
    } else if (tok->type == S("FLOAT")) {
        lhs = makeFloat(tok);
    } else if (tok->type == S("CHAR")) {
        lhs = makeChar(tok);
    } else {
        PrattRecord *record = fetchRecord(parser, tok->type);
        if (record->prefixOp == NULL) {
            errorAt(tok, "not a prefix operator");
        } else {
            lhs = record->prefixOp(record, lexer, parser, NULL);
        }
    }
    REPLACE_PROTECT(save, lhs);
    for (;;) {
        PrattToken *op = peek(lexer);
        if (op->type == S("EOF")) {
            DEBUG("PEEKED EOF");
            break;
        } else {
            DEBUG("PEEKED OP %s", op->type->name);
            PrattRecord *record = fetchRecord(parser, op->type);
            if(record->postfixOp != NULL) {
                DEBUG("postfix %d %d", record->precedence, min_bp);
                if (record->precedence < min_bp) {
                    break;
                }
                next(lexer);
                lhs = record->postfixOp(record, lexer, parser, lhs);
                REPLACE_PROTECT(save, lhs);
            } else if (record->infixOp != NULL) {
                DEBUG("infix %d %d", record->precedence, min_bp);
                if (record->precedence < min_bp) {
                    break;
                }
                next(lexer);
                lhs = record->infixOp(record, lexer, parser, lhs);
                REPLACE_PROTECT(save, lhs);
            } else {
                DEBUG("prefix");
                break;
            }
        }
    }
    LEAVE(expr_bp);
    UNPROTECT(save);
    return lhs;
}

static void test(PrattParser *parser, PrattTrie *trie, char *expr) {
    eprintf("%-30s ", expr);
    NEWLINE();
    PrattLexer *lexer = makePrattLexer(trie, expr);
    int save = PROTECT(lexer);
    AstExpression *result = expr_bp(lexer, parser, 0);
    PROTECT(result);
    if (lexer->bufList != NULL) {
        PrattToken *tok = next(lexer);
        errorAt(tok, "unconsumed tokens");
    }
    ppAstExpression(result);
    eprintf("\n");
    UNPROTECT(save);
}

int main(int argc __attribute__((unused)), char *argv[] __attribute__((unused))) {
    initProtection();
    PrattParser *p = makePrattParser();
    PROTECT(p);
    PrattTrie *t = makePrattTrie(p);
    PROTECT(t);
    test(p, t, "1");
    test(p, t, "5!");
    test(p, t, "1i");
    test(p, t, "Σ");
    test(p, t, "'Σ'");
    test(p, t, "a123");
    test(p, t, "1 + 2");
    test(p, t, "1 <=> 2 <=> 3");
    test(p, t, "1 + 2 * 3");
    test(p, t, "1 * 2 + 3");
    test(p, t, "an and android");
    test(p, t, "1 * 2 * 3");
    test(p, t, "1 . 2 . 3");
    test(p, t, "- 1 . - 2 . 3");
    test(p, t, "--1 * 2");
    test(p, t, "--1 * 2!");
    test(p, t, "1 * ((2 + 3))");
    test(p, t, "a -> b -> c");
    test(p, t, "(a -> b) -> c");
    test(p, t, "<a @ b");
    test(p, t, "a @ b < c");
    test(p, t, "1 then 2 then 3");
    test(p, t, "a(b)");
    test(p, t, "a(b, c)");
    test(p, t, "#(b)");
    test(p, t, "#(b, c)");
    test(p, t, "#a + b");
    test(p, t, "a + #b");
    test(p, t, "a #b");
    test(p, t, "a @ b @@ c @ d");
    test(p, t, "123456789012345678901234567890");
    test(p, t, "12345.6789i");

    // test("1 * ((2 + 3[4 + 5]))");
    // test("aa = bb = 3 ? 4 ? 5 : 6 : 7 ? 8 : 9");
    return 0;
}

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

#define DEBUG_PRATT

#ifdef DEBUG_PRATT
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

void ppPrattExpr(PrattExpr *expr);
PrattExpr *expr_bp(PrattLexer *lexer, PrattParser *parser, int min_bp);
static PrattExpr *grouping(PrattRecord *record, PrattLexer *lexer, PrattParser *parser);
static PrattExpr *prefix(PrattRecord *record, PrattLexer *lexer, PrattParser *parser);
static PrattExpr *prefixC(PrattRecord *record, PrattLexer *lexer, PrattParser *parser);
static PrattExpr *postfix(PrattRecord *record,
                          PrattLexer *lexer __attribute__((unused)),
                          PrattParser *parser __attribute__((unused)),
                          PrattExpr *lhs);
static PrattExpr *postfixArg(PrattRecord *, PrattLexer *, PrattParser *, PrattExpr *);
static PrattExpr *infixLeft(PrattRecord *, PrattLexer *, PrattParser *, PrattExpr *);
static PrattExpr *infixRight(PrattRecord *, PrattLexer *, PrattParser *, PrattExpr *);

static inline HashSymbol *S(char *name) { return newSymbol(name); }

static void addRecord(PrattParser *parser,
                      char *token,
                      PrattPrefixOp prefix,
                      PrattPostfixOp infix,
                      PrattPostfixOp postfix,
                      int precedence) {
    PrattRecord *record = newPrattRecord(newSymbol(token), prefix, infix, postfix, precedence);
    int save = PROTECT(record);
    setPrattParser(parser, record->token, record);
    UNPROTECT(save);
}

static PrattParser *makePrattParser() {
    PrattParser *res = newPrattParser();
    int save = PROTECT(res);
    addRecord(res, "(",    grouping, NULL,       postfixArg, 0);
    addRecord(res, ")",    NULL,     NULL,       NULL,       0);

    addRecord(res, "->",   NULL,     infixRight, NULL,      10);

    addRecord(res, "then", NULL,     infixRight, NULL,      20);

    addRecord(res, "and",  NULL,     infixLeft,  NULL,      30);
    addRecord(res, "or",   NULL,     infixLeft,  NULL,      30);
    addRecord(res, "xor",  NULL,     infixLeft,  NULL,      30);
    addRecord(res, "nand", NULL,     infixLeft,  NULL,      30);
    addRecord(res, "nor",  NULL,     infixLeft,  NULL,      30);
    addRecord(res, "xnor", NULL,     infixLeft,  NULL,      30);

    addRecord(res, "not",  prefix,   NULL,       NULL,      40);

    addRecord(res, "==",   NULL,     infixLeft,  NULL,      50);
    addRecord(res, "!=",   NULL,     infixLeft,  NULL,      50);
    addRecord(res, ">",    prefixC,  infixLeft,  NULL,      50);
    addRecord(res, "<",    prefixC,  infixLeft,  NULL,      50);
    addRecord(res, ">=",   NULL,     infixLeft,  NULL,      50);
    addRecord(res, "<=",   NULL,     infixLeft,  NULL,      50);
    addRecord(res, "<=>",  NULL,     infixLeft,  NULL,      50);

    addRecord(res, "=",    NULL,     infixRight, NULL,      60);

    addRecord(res, ":",    NULL,     infixLeft,  NULL,      70);

    addRecord(res, "@@",   NULL,     infixRight, NULL,      80);

    addRecord(res, "@",    NULL,     infixRight, NULL,      90);
    addRecord(res, "+",    prefixC,  infixLeft,  NULL,      90);
    addRecord(res, "-",    prefixC,  infixLeft,  NULL,      90);

    addRecord(res, "*",    NULL,     infixLeft,  NULL,     100);
    addRecord(res, "/",    NULL,     infixLeft,  NULL,     100);
    addRecord(res, "%",    NULL,     infixLeft,  NULL,     100);

    addRecord(res, "**",   NULL,     infixRight, NULL,     110);

    addRecord(res, "here", prefix,   NULL,       NULL,     120);
    addRecord(res, "#",    prefix,   NULL,       NULL,     120);
    addRecord(res, "!",    NULL,     NULL,       postfix,  120);

    addRecord(res, ".",    NULL,     infixRight, NULL,     130);

    UNPROTECT(save);
    return res;
}

static PrattTrie *makePrattTrie(PrattParser *parser) {
    PrattTrie *C = NULL;
    HashSymbol *token;
    Index i = 0;
    int save = PROTECT(parser); // not C because we need to have a slot for REPLACE_PROTECT
    while ((token = iteratePrattParser(parser, &i, NULL)) != NULL) {
        C = insertPrattTrie(C, token);
        REPLACE_PROTECT(save, C);
    }
    UNPROTECT(save);
    return C;
}

void ppPrattBinary(PrattBinary *binary) {
    printf("(%s ", binary->token->name);
    ppPrattExpr(binary->left);
    printf(" ");
    ppPrattExpr(binary->right);
    printf(")");
}

void ppPrattUnary(PrattUnary *unary) {
    printf("(%s ", unary->token->name);
    ppPrattExpr(unary->expr);
    printf(")");
}

void ppPrattExpr(PrattExpr *expr) {
    switch (expr->type) {
        case PRATTEXPR_TYPE_NUMBER:
            printf("%d", expr->val.number);
            break;
        case PRATTEXPR_TYPE_ATOM:
            printf("%s", expr->val.atom->name);
            break;
        case PRATTEXPR_TYPE_UNARY:
            ppPrattUnary(expr->val.unary);
            break;
        case PRATTEXPR_TYPE_BINARY:
            ppPrattBinary(expr->val.binary);
            break;
        default:
            cant_happen("unexpected %s", prattExprTypeName(expr->type));
    }
}

static PrattExpr *makePrattBinary(HashSymbol *op, PrattExpr *lhs, PrattExpr *rhs) {
    PrattBinary *bin = newPrattBinary(op, lhs, rhs);
    int save = PROTECT(bin);
    PrattExpr *result = newPrattExpr_Binary(bin);
    UNPROTECT(save);
    return result;
}

static PrattExpr *makePrattUnary(HashSymbol *op, PrattExpr *expr) {
    PrattUnary *un = newPrattUnary(op,expr);
    int save = PROTECT(un);
    PrattExpr *result = newPrattExpr_Unary(un);
    UNPROTECT(save);
    return result;
}

static PrattRecord *fetchRecord(PrattParser *parser, HashSymbol *symbol) {
    PrattRecord *record = NULL;
    if (getPrattParser(parser, symbol, &record)) {
        return record;
    } else {
        cant_happen("unrecognised op %s", symbol->name);
    }
}

static PrattExpr *grouping(PrattRecord *record, PrattLexer *lexer, PrattParser *parser) {
    ENTER(grouping);
    PrattExpr *res = expr_bp(lexer, parser, record->precedence);
    int save = PROTECT(res);
    PrattToken *tok = next(lexer);
    if (tok->type != PRATTTOKEN_TYPE_OP || tok->val.op != S(")")) {
        can_happen("mismatched closing bracket");
    }
    LEAVE(grouping);
    UNPROTECT(save);
    return res;
}

static PrattExpr *prefix(PrattRecord *record, PrattLexer *lexer, PrattParser *parser) {
    ENTER(prefix);
    PrattExpr *res = expr_bp(lexer, parser, record->precedence + 1);
    int save = PROTECT(res);
    res = makePrattUnary(record->token, res);
    LEAVE(prefix);
    UNPROTECT(save);
    return res;
}

static PrattExpr *prefixC(PrattRecord *record, PrattLexer *lexer, PrattParser *parser) {
    ENTER(prefixC);
    PrattExpr *res = expr_bp(lexer, parser, 100);
    int save = PROTECT(res);
    res = makePrattUnary(record->token, res);
    LEAVE(prefixC);
    UNPROTECT(save);
    return res;
}

static PrattExpr *postfix(PrattRecord *record,
                          PrattLexer *lexer __attribute__((unused)),
                          PrattParser *parser __attribute__((unused)),
                          PrattExpr *lhs) {
    return makePrattUnary(record->token, lhs);
}

static PrattExpr *postfixArg(PrattRecord *record,
                                       PrattLexer *lexer,
                                       PrattParser *parser,
                                       PrattExpr *lhs) {
    ENTER(postfixArg);
    PrattExpr *rhs = expr_bp(lexer, parser, 0);
    int save = PROTECT(rhs);
    PrattToken *tok = next(lexer);
    if (tok->type != PRATTTOKEN_TYPE_OP || tok->val.op != S(")")) {
        can_happen("mismatched closing bracket");
    }
    rhs = makePrattBinary(record->token, lhs, rhs);
    LEAVE(postfixArg);
    UNPROTECT(save);
    return rhs;
}

static PrattExpr *infixLeft(PrattRecord *record, PrattLexer *lexer, PrattParser *parser, PrattExpr *lhs) {
    ENTER(infixLeft);
    PrattExpr *rhs = expr_bp(lexer, parser, record->precedence + 1);
    int save = PROTECT(rhs);
    rhs = makePrattBinary(record->token, lhs, rhs);
    LEAVE(infixLeft);
    UNPROTECT(save);
    return rhs;
}

static PrattExpr *infixRight(PrattRecord *record, PrattLexer *lexer, PrattParser *parser, PrattExpr *lhs) {
    ENTER(infixRight);
    PrattExpr *rhs = expr_bp(lexer, parser, record->precedence - 1);
    int save = PROTECT(rhs);
    rhs = makePrattBinary(record->token, lhs, rhs);
    LEAVE(infixRight);
    UNPROTECT(save);
    return rhs;
}

PrattExpr *expr_bp(PrattLexer *lexer, PrattParser *parser, int min_bp) {
    ENTER(expr_bp);
    PrattExpr *lhs = NULL;
    PrattToken *tok = next(lexer);
    int save = PROTECT(tok);
    switch(tok->type) {
        case PRATTTOKEN_TYPE_ATOM:
            DEBUG("ATOM %s", tok->val.atom->name);
            lhs = newPrattExpr_Atom(tok->val.atom);
            break;
        case PRATTTOKEN_TYPE_NUMBER:
            DEBUG("NUMBER %d", tok->val.number);
            lhs = newPrattExpr_Number(tok->val.number);
            break;
        case PRATTTOKEN_TYPE_OP: {
            DEBUG("OP %s", tok->val.op->name);
            PrattRecord *record = fetchRecord(parser, tok->val.op);
            if (record->prefixOp == NULL) {
                can_happen("unexpected %s", tok->val.op->name);
            } else {
                lhs = record->prefixOp(record, lexer, parser);
            }
        }
        break;
        default:
            can_happen("unexpected %s", prattTokenTypeName(tok->type));
    }
    REPLACE_PROTECT(save, lhs);
    for (;;) {
        PrattToken *op = peek(lexer);
        if (op->type == PRATTTOKEN_TYPE_EOF) {
            DEBUG("PEEKED EOF");
            break;
        } else if (op->type == PRATTTOKEN_TYPE_OP) {
            DEBUG("PEEKED OP %s", op->val.op->name);
            PrattRecord *record = fetchRecord(parser, op->val.op);
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
        } else {
            can_happen("syntax error!");
            next(lexer);
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
    PrattExpr *result = expr_bp(lexer, parser, 0);
    if (lexer->size > 1) {
        can_happen("unconsumed %u tokens", lexer->size);
    }
    PROTECT(result);
    ppPrattExpr(result);
    eprintf("\n");
    UNPROTECT(save);
}

int main(int argc __attribute__((unused)), char *argv[] __attribute__((unused))) {
    initProtection();
    PrattParser *p = makePrattParser();
    PROTECT(p);
    PrattTrie *t = makePrattTrie(p);
    PROTECT(t);
    // test("a b c");
    test(p, t, "1");
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
    test(p, t, "#(b)");
    test(p, t, "#a + b");
    test(p, t, "a + #b");
    test(p, t, "a #b");
    test(p, t, "a @ b @@ c @ d");

    // test("1 * ((2 + 3[4 + 5]))");
    // test("aa = bb = 3 ? 4 ? 5 : 6 : 7 ? 8 : 9");
    return 0;
}

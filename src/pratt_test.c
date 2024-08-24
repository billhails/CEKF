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
#include <ctype.h>

#include"symbol.h"
#include"pratt.h"
#include"pratt_debug.h"
#include"pratt_functions.h"
#include"symbols.h"

// UTF8
// re-entrant
// stacked input stream
// modified during parse
// integrates with existing AST

void ppPrattExpr(PrattExpr *expr);
PrattExpr *expr_bp(PrattLexer *lexer, PrattParser *parser, int min_bp);
static PrattExpr *grouping(PrattRecord *record, PrattLexer *lexer, PrattParser *parser);
static PrattExpr *unaryPrefix(PrattRecord *record, PrattLexer *lexer, PrattParser *parser);
static PrattExpr *unaryPostfix(PrattRecord *record,
                               PrattLexer *lexer __attribute__((unused)),
                               PrattParser *parser __attribute__((unused)),
                               PrattExpr *lhs);
static PrattExpr *unaryPostfixGrp(PrattRecord *, PrattLexer *, PrattParser *, PrattExpr *);
static PrattExpr *binaryInfix(PrattRecord *, PrattLexer *, PrattParser *, PrattExpr *);
static PrattExpr *binaryMfix(PrattRecord *, PrattLexer *, PrattParser *, PrattExpr *);

static void addParserRecord(PrattParser *parser,
                            char *k,
                            int left, int right, int prefix, int postfix,
                            char *m,
                            PrattPrefixOp prefixOp,
                            PrattPostfixOp postfixOp,
                            PrattPostfixOp infixOp) {
    HashSymbol *key = newSymbol(k);
    HashSymbol *match = NULL;
    if (m != NULL) {
        match = newSymbol(m);
    }
    PrattRecord *record = newPrattRecord(key, left, right, prefix, postfix, match, prefixOp, postfixOp, infixOp);
    int save = PROTECT(record);
    setPrattParser(parser, key, record);
    UNPROTECT(save);
}

static inline HashSymbol *S(char *name) { return newSymbol(name); }

static PrattParser *makePrattParser() {
    PrattParser *res = newPrattParser();
    int save = PROTECT(res);
    addParserRecord(res, "=",  2,  1, 9,  0, NULL, NULL,        NULL,            binaryInfix);
    addParserRecord(res, "+",  5,  6, 9,  0, NULL, unaryPrefix, NULL,            binaryInfix);
    addParserRecord(res, "-",  5,  6, 9,  0, NULL, unaryPrefix, NULL,            binaryInfix);
    addParserRecord(res, "*",  7,  8, 0,  0, NULL, NULL,        NULL,            binaryInfix);
    addParserRecord(res, "/",  7,  8, 0,  0, NULL, NULL,        NULL,            binaryInfix);
    addParserRecord(res, ".", 14, 13, 0,  0, NULL, NULL,        NULL,            binaryInfix);
    addParserRecord(res, "!",  0,  0, 0, 11, NULL, NULL,        unaryPostfix,    NULL);
    addParserRecord(res, "[",  0,  0, 0, 11, "]",  NULL,        unaryPostfixGrp, NULL);
    addParserRecord(res, "(",  0,  0, 0,  0, ")",  grouping,    NULL,            NULL);
    addParserRecord(res, ")",  0,  0, 0,  0, NULL, NULL,        NULL,            NULL);
    addParserRecord(res, "]",  0,  0, 0,  0, NULL, NULL,        NULL,            NULL);
    addParserRecord(res, "?",  4,  3, 0,  0, ":",  NULL,        NULL,            binaryMfix);
    addParserRecord(res, ":",  0,  0, 0,  0, NULL, NULL,        NULL,            NULL);
    UNPROTECT(save);
    return res;
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
        case PRATTEXPR_TYPE_ATOM:
            printf("%d", expr->val.atom);
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

static void reverseLexer(PrattLexer *lexer) {
    int i = 0;
    int j = lexer->size - 1;
    while (i < j) {
        PrattToken *tmp = lexer->entries[i];
        lexer->entries[i] = lexer->entries[j];
        lexer->entries[j] = tmp;
        i++;
        j--;
    }
}

static PrattToken *makePrattTokenOp(char op) {
    char buf[8];
    sprintf(buf, "%c", op);
    HashSymbol *name = newSymbol(buf);
    PrattToken *result = newPrattToken_Op(name);
    return result;
}

PrattLexer *makePrattLexer(char *input) {
    PrattLexer *lexer = newPrattLexer();
    int save = PROTECT(lexer);
    while(*input) {
        if(isspace(*input)) {
            // skip
        } else if (isdigit(*input)) {
            PrattToken *atom = newPrattToken_Atom(*input - '0');
            int save2 = PROTECT(atom);
            pushPrattLexer(lexer, atom);
            UNPROTECT(save2);
        } else if (ispunct(*input)) {
            PrattToken *op = makePrattTokenOp(*input);
            int save2 = PROTECT(op);
            pushPrattLexer(lexer, op);
            UNPROTECT(save2);
        } else {
            cant_happen("unexpected %c", *input);
        }
        ++input;
    }
    PrattToken *eof = newPrattToken_Eof();
    PROTECT(eof);
    pushPrattLexer(lexer, eof);
    reverseLexer(lexer);
    UNPROTECT(save);
    return lexer;
}

PrattToken *next(PrattLexer *lexer) {
    return popPrattLexer(lexer);
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
    PrattExpr *res = expr_bp(lexer, parser, 0);
    int save = PROTECT(res);
    PrattToken *tok = next(lexer);
    if (tok->type != PRATTTOKEN_TYPE_OP || tok->val.op != record->matchingRight) {
        cant_happen("mismatched closing bracket");
    }
    UNPROTECT(save);
    return res;
}

static PrattExpr *unaryPrefix(PrattRecord *record, PrattLexer *lexer, PrattParser *parser) {
    PrattExpr *res = expr_bp(lexer, parser, record->right);
    int save = PROTECT(res);
    res = makePrattUnary(record->token, res);
    UNPROTECT(save);
    return res;
}

static PrattExpr *unaryPostfix(PrattRecord *record,
                               PrattLexer *lexer __attribute__((unused)),
                               PrattParser *parser __attribute__((unused)),
                               PrattExpr *lhs) {
    return makePrattUnary(record->token, lhs);
}

static PrattExpr *unaryPostfixGrp(PrattRecord *record,
                                       PrattLexer *lexer,
                                       PrattParser *parser,
                                       PrattExpr *lhs) {
    PrattExpr *rhs = expr_bp(lexer, parser, 0);
    int save = PROTECT(rhs);
    PrattToken *tok = next(lexer);
    if (tok->type != PRATTTOKEN_TYPE_OP || tok->val.op != record->matchingRight) {
        cant_happen("mismatched closing bracket");
    }
    rhs = makePrattBinary(record->token, lhs, rhs);
    UNPROTECT(save);
    return rhs;
}

static PrattExpr *binaryInfix(PrattRecord *record, PrattLexer *lexer, PrattParser *parser, PrattExpr *lhs) {
    PrattExpr *rhs = expr_bp(lexer, parser, record->right);
    int save = PROTECT(rhs);
    rhs = makePrattBinary(record->token, lhs, rhs);
    UNPROTECT(save);
    return rhs;
}

static PrattExpr *binaryMfix(PrattRecord *record, PrattLexer *lexer, PrattParser *parser, PrattExpr *lhs) {
    PrattExpr *mhs = expr_bp(lexer, parser, 0);
    int save = PROTECT(mhs);
    PrattToken *tok = next(lexer);
    if (tok->type != PRATTTOKEN_TYPE_OP || tok->val.op != record->matchingRight) {
        cant_happen("unmatched '%s'", record->matchingRight->name);
    }
    PrattExpr *rhs = expr_bp(lexer, parser, record->right);
    PROTECT(rhs);
    mhs = makePrattBinary(record->matchingRight, mhs, rhs);
    PROTECT(mhs);
    mhs = makePrattBinary(record->token, lhs, mhs);
    UNPROTECT(save);
    return mhs;
}

PrattExpr *expr_bp(PrattLexer *lexer, PrattParser *parser, int min_bp) {
    // printPrattLexer(lexer, 0);
    // printf("\n");
    PrattExpr *lhs = NULL;
    PrattToken *tok = next(lexer);
    int save = PROTECT(tok);
    switch(tok->type) {
        case PRATTTOKEN_TYPE_ATOM: {
            lhs = newPrattExpr_Atom(tok->val.atom);
        }
        break;
        case PRATTTOKEN_TYPE_OP: {
            PrattRecord *record = fetchRecord(parser, tok->val.op);
            if (record->prefixOp == NULL) {
                cant_happen("unexpected %s", tok->val.op->name);
            } else {
                lhs = record->prefixOp(record, lexer, parser);
            }
        }
        break;
        default:
            cant_happen("unexpected %s", prattTokenTypeName(tok->type));
    }
    PROTECT(lhs);
    for (;;) {
        PrattToken *op = peekPrattLexer(lexer);
        PROTECT(op);
        if (op->type == PRATTTOKEN_TYPE_EOF) {
            break;
        } else if (op->type == PRATTTOKEN_TYPE_OP) {
            PrattRecord *record = fetchRecord(parser, op->val.op);
            if(record->postfix > 0) {
                if (record->postfix < min_bp) {
                    break;
                }
                next(lexer);
                lhs = record->postfixOp(record, lexer, parser, lhs);
                PROTECT(lhs);
            } else if (record->left > 0) {
                if (record->left < min_bp) {
                    break;
                }
                next(lexer);
                lhs = record->infixOp(record, lexer, parser, lhs);
                PROTECT(lhs);
            } else {
                break;
            }
        }
    }
    UNPROTECT(save);
    return lhs;
}

static void test(char *expr) {
    PrattLexer *lexer = makePrattLexer(expr);
    int save = PROTECT(lexer);
    PrattParser *parser = makePrattParser();
    PROTECT(parser);
    PrattExpr *result = expr_bp(lexer, parser, 0);
    PROTECT(result);
    ppPrattExpr(result);
    printf("\n");
    UNPROTECT(save);
}


int main(int argc __attribute__((unused)), char *argv[] __attribute__((unused))) {
    initProtection();
    test("1");
    test("1 + 2");
    test("1 + 2 * 3");
    test("1 * 2 + 3");
    test("1 * 2 * 3");
    test("1 . 2 . 3");
    test("- 1 . - 2 . 3");
    test("--1 * 2");
    test("--1 * 2!");
    test("1 * ((2 + 3))");
    test("1 * ((2 + 3[4 + 5]))");
    test("1 = 2 = 3 ? 4 : 5 ? 6 : 7");
    return 0;
}

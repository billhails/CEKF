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
#include"symbols.h"

// UTF8
// re-entrant
// stacked input stream
// modified during parse
// integrates with existing AST

void ppTestExpr(TestExpr *expr);
TestExpr *expr_bp(TestLexer *lexer, int min_bp);

void ppTestBinary(TestBinary *binary) {
    printf("(%s ", binary->token->name);
    ppTestExpr(binary->left);
    printf(" ");
    ppTestExpr(binary->right);
    printf(")");
}

void ppTestUnary(TestUnary *unary) {
    printf("(%s ", unary->token->name);
    ppTestExpr(unary->expr);
    printf(")");
}

void ppTestExpr(TestExpr *expr) {
    switch (expr->type) {
        case TESTEXPR_TYPE_ATOM:
            printf("%d", expr->val.atom);
            break;
        case TESTEXPR_TYPE_UNARY:
            ppTestUnary(expr->val.unary);
            break;
        case TESTEXPR_TYPE_BINARY:
            ppTestBinary(expr->val.binary);
            break;
        default:
            cant_happen("unexpected %s", testExprTypeName(expr->type));
    }
}

static void reverseLexer(TestLexer *lexer) {
    int i = 0;
    int j = lexer->size - 1;
    while (i < j) {
        TestToken *tmp = lexer->entries[i];
        lexer->entries[i] = lexer->entries[j];
        lexer->entries[j] = tmp;
        i++;
        j--;
    }
}

static TestToken *makeTestTokenOp(char op) {
    char buf[8];
    sprintf(buf, "%c", op);
    HashSymbol *name = newSymbol(buf);
    TestToken *result = newTestToken_Op(name);
    return result;
}

TestLexer *makeTestLexer(char *input) {
    TestLexer *lexer = newTestLexer();
    int save = PROTECT(lexer);
    while(*input) {
        if(isspace(*input)) {
            // skip
        } else if (isdigit(*input)) {
            TestToken *atom = newTestToken_Atom(*input - '0');
            int save2 = PROTECT(atom);
            pushTestLexer(lexer, atom);
            UNPROTECT(save2);
        } else if (ispunct(*input)) {
            TestToken *op = makeTestTokenOp(*input);
            int save2 = PROTECT(op);
            pushTestLexer(lexer, op);
            UNPROTECT(save2);
        } else {
            cant_happen("unexpected %c", *input);
        }
        ++input;
    }
    TestToken *eof = newTestToken_Eof();
    PROTECT(eof);
    pushTestLexer(lexer, eof);
    reverseLexer(lexer);
    UNPROTECT(save);
    return lexer;
}

TestToken *next(TestLexer *lexer) {
    return popTestLexer(lexer);
}

static void get_infix_binding_power(HashSymbol *op, int *l_bp, int *r_bp) {
    switch (op->name[0]) {
        case '+':
        case '-':
            *l_bp = 1;
            *r_bp = 2;
            break;
        case '*':
        case '/':
            *l_bp = 3;
            *r_bp = 4;
            break;
        case '.':
            *l_bp = 10;
            *r_bp = 9;
            break;
        default:
            *l_bp = 0;
            *r_bp = 0;
            break;
    }
}

static void get_prefix_binding_power(HashSymbol *op, int *r_bp) {
    switch (op->name[0]) {
        case '+':
        case '-':
            *r_bp = 5;
            break;
        default:
            cant_happen("unexpected %s", op->name);
    }
}

static void get_postfix_binding_power(HashSymbol *op, int *l_bp) {
    switch (op->name[0]) {
        case '!':
        case '[':
            *l_bp = 7;
            break;
        default:
            *l_bp = 0;
    }
}

static TestExpr *makeTestBinary(HashSymbol *op, TestExpr *lhs, TestExpr *rhs) {
    TestBinary *bin = newTestBinary(op, lhs, rhs);
    int save = PROTECT(bin);
    TestExpr *result = newTestExpr_Binary(bin);
    UNPROTECT(save);
    return result;
}

static TestExpr *makeTestUnary(HashSymbol *op, TestExpr *expr) {
    TestUnary *un = newTestUnary(op,expr);
    int save = PROTECT(un);
    TestExpr *result = newTestExpr_Unary(un);
    UNPROTECT(save);
    return result;
}

static HashSymbol *getRhBracket(HashSymbol *left) {
    if (left == leftCurlySymbol()) {
        return rightCurlySymbol();
    }
    if (left == leftRoundSymbol()) {
        return rightRoundSymbol();
    }
    if (left == leftSquareSymbol()) {
        return rightSquareSymbol();
    }
    return NULL;
}

TestExpr *expr_bp(TestLexer *lexer, int min_bp) {
    // printTestLexer(lexer, 0);
    // printf("\n");
    TestExpr *lhs = NULL;
    TestToken *tok = next(lexer);
    int save = PROTECT(tok);
    switch(tok->type) {
        case TESTTOKEN_TYPE_ATOM: {
            lhs = newTestExpr_Atom(tok->val.atom);
        }
        break;
        case TESTTOKEN_TYPE_OP: {
            HashSymbol *rhBracket = getRhBracket(tok->val.op);
            if (rhBracket == NULL) {
                int r_bp;
                get_prefix_binding_power(tok->val.op, &r_bp);
                TestExpr *rhs = expr_bp(lexer, r_bp);
                PROTECT(rhs);
                lhs = makeTestUnary(tok->val.op, rhs);
            } else {
                lhs = expr_bp(lexer, 0);
                tok = next(lexer);
                if (tok->type != TESTTOKEN_TYPE_OP || tok->val.op != rhBracket) {
                    cant_happen("mismatched closing bracket");
                }
            }
        }
        break;
        default:
            cant_happen("unexpected %s", testTokenTypeName(tok->type));
    }
    PROTECT(lhs);
    for (;;) {
        TestToken *op = peekTestLexer(lexer);
        PROTECT(op);
        if (op->type == TESTTOKEN_TYPE_EOF) {
            break;
        } else if (op->type == TESTTOKEN_TYPE_OP) {
            int l_bp = 0, r_bp = 0;
            get_postfix_binding_power(op->val.op, &l_bp);
            if(l_bp > 0) {
                if (l_bp < min_bp) {
                    break;
                }
                next(lexer);
                HashSymbol *rhBracket = getRhBracket(op->val.op);
                if (rhBracket == NULL) {
                    lhs = makeTestUnary(op->val.op, lhs);
                } else {
                    TestExpr *rhs = expr_bp(lexer, 0);
                    PROTECT(rhs);
                    TestToken *tok = next(lexer);
                    if (tok->type != TESTTOKEN_TYPE_OP || tok->val.op != rhBracket) {
                        cant_happen("mismatched closing bracket");
                    }
                    lhs = makeTestBinary(op->val.op, lhs, rhs);
                }
                PROTECT(lhs);
            } else {
                get_infix_binding_power(op->val.op, &l_bp, &r_bp);
                if (l_bp > 0) {
                    if (l_bp < min_bp) {
                        break;
                    }
                    next(lexer);
                    TestExpr *rhs = expr_bp(lexer, r_bp);
                    PROTECT(rhs);
                    lhs = makeTestBinary(op->val.op, lhs, rhs);
                    PROTECT(lhs);
                } else {
                    break;
                }
            }
        }
    }
    UNPROTECT(save);
    return lhs;
}

static void test(char *expr) {
    TestLexer *lexer = makeTestLexer(expr);
    int save = PROTECT(lexer);
    TestExpr *result = expr_bp(lexer, 0);
    PROTECT(result);
    ppTestExpr(result);
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
    return 0;
}

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
#include <string.h>

#include "pratt.h"
#include "pratt_parser.h"
#include "ast_pp.h"
#include "pratt_debug.h"
#include "pratt_functions.h"
#include "pratt_scanner.h"
#include "symbols.h"
#include "ast.h"
#include "bigint.h"
#include "utf8.h"
#include "print_generator.h"
#include "file_id.h"

extern AstStringArray *include_paths;

static bool failed = false;

static void test(char *expr, char *expected, bool expectError) {
    clearErrors();
    PrattParser *parser = makePrattParser();
    int save = PROTECT(parser);
    PrattTrie *trie = makePrattTrie(parser, NULL);
    PROTECT(trie);
    printf("*** %s ***\n", expr);
    parser->lexer = makePrattLexer(trie, expr, expr);
    AstNest *result = top(parser);
    PROTECT(result);
    if (parser->lexer->bufList != NULL) {
        parserError(parser, "unconsumed tokens");
    }
    PrattUTF8 *dest = newPrattUTF8();
    PROTECT(dest);
    ppAstNest(dest, result);
    if (strcmp(dest->entries, expected) != 0) {
        printf("%s - expected %s got %s\n", expr, expected, dest->entries);
        failed = true;
    }
    if (hadErrors() != expectError) {
        printf("%s - %sexpected error\n", expr, expectError ? "" : "un");
        failed = true;
    }
    UNPROTECT(save);
    validateLastAlloc();
}

static void testFile(char *filename) {
    clearErrors();
    PrattParser *parser = makePrattParser();
    int save = PROTECT(parser);
    PrattTrie *trie = makePrattTrie(parser, NULL);
    PROTECT(trie);
    parser->lexer = makePrattLexerFromFilename(trie, filename);
    AstNest *result = prattParseTopLevel(parser);
    PROTECT(result);
    if (parser->lexer->bufList != NULL) {
        parserError(parser, "unconsumed tokens");
    }
    PrattUTF8 *dest = newPrattUTF8();
    PROTECT(dest);
    ppAstNest(dest, result);
    printf("%s\n", dest->entries);
    if (hadErrors()) {
        failed = true;
    }
    UNPROTECT(save);
}

int main(int argc __attribute__((unused)), char *argv[] __attribute__((unused))) {
    initProtection();
    include_paths = newAstStringArray();
    int save = PROTECT(include_paths);
    initFileIdStack();
    initNamespaces();
    pushAstStringArray(include_paths, strdup("fn"));
    test("h then one_of(t)",               "{ then(h, one_of(t)); }", false);
    test("1",                              "{ 1; }", false);
    test("1i",                             "{ 1i; }", false);
    test("Σ",                              "{ Σ; }", false);
    test("'Σ'",                            "{ 'Σ'; }", false);
    test("a123",                           "{ a123; }", false);
    test("1 + 2",                          "{ add$(1, 2); }", false);
    test("n then a(n + 1)",                "{ then(n, a(add$(n, 1))); }", false);
    test("1 <=> 2 <=> 3",                  "{ cmp$(cmp$(1, 2), 3); }", false);
    test("1 + 2 * 3",                      "{ add$(1, mul$(2, 3)); }", false);
    test("1 * 2 + 3",                      "{ add$(mul$(1, 2), 3); }", false);
    test("an and android",                 "{ and(an, android); }", false);
    test("1 * 2 * 3",                      "{ mul$(mul$(1, 2), 3); }", false);
    test("a . b",                          "{ a<0>.b; }", true);
    test("--1 * 2",                        "{ neg$(neg$(mul$(1, 2))); }", false);
    test("1 * ((2 + 3))",                  "{ mul$(1, add$(2, 3)); }", false);
    test("a -> b -> c",                    "{ ->(a, ->(b, c)); }", false);
    test("(a -> b) -> c",                  "{ ->(->(a, b), c); }", false);
    test("1 then 2 then 3",                "{ then(1, then(2, 3)); }", false);
    test("a(b)",                           "{ a(b); }", false);
    test("a(b, c)",                        "{ a(b, c); }", false);
    test("#(b)",                           "{ #(b); }", false);
    test("#(b, c)",                        "{ #(b, c); }", false);
    test("#a + b",                         "{ add$(#(a), b); }", false); // ??
    test("a + #b",                         "{ add$(a, #(b)); }", false); // ??
    test("a #b",                           "{ a; }", true);
    test("0x100",                          "{ 256; }", false);
    test("0X100i",                         "{ 256i; }", false);
    test("123456789012345678901234567_890","{ 123456789012345678901234567890; }", false);
    test("12345678901234567890123456789i", "{ 12345678901234567890123456789i; }", false);
    test("12345.6789i",                    "{ 12345.678900i; }", false);
    test("let fn i(x) { x } in i(0)",      "{ let i = fn { (x) { x; } }; in i(0); }", false);
    test("let unsafe fn i(x) { x } in i(0)", "{ let i = unsafe fn { (x) { x; } }; in i(0); }", false);
    test("if (a > 2) { 3 } else { 4 }",    "{ if (gt$(a, 2)) { 3; } else { 4; }; }", false);
    test("let x = 1 then 2 then 3; in x",  "{ let x = then(1, then(2, 3)); in x; }", false);
    test("let typedef named_list(#t) { nl(str, list(#t)) } in nl",
               "{ let typedef named_list(t) {nl(str, list(t))}; in nl; }", false);
    test("let link \"bar\" as foo; in f",  "{ let ; in f; }", true);
    test("fn { (0) { 1 } (n) { n * fact(n - 1) } }",
               "{ fn { (0) { 1; } (n) { mul$(n, fact(sub$(n, 1))); } }; }", false);
    test("unsafe fn { (0) { 1 } (n) { n * fact(n - 1) } }",
               "{ unsafe fn { (0) { 1; } (n) { mul$(n, fact(sub$(n, 1))); } }; }", false);
    test("switch (x) { (0) { 1 } } }",       "{ fn { (0) { 1; } }(x); }", true);
    test("unsafe switch (x) { (0) { 1 } }",  "{ unsafe fn { (0) { 1; } }(x); }", false);
    test("let alias string = list(char); in foo;",
               "{ let alias string = list(char); in foo; }", false);
    test("let print list(a, b) { c; } in foo;",
               "{ let print$list = fn { (a, b) { c; } }; in foo; }", false);
    testFile("fn/fact2.fn");
    testFile("fn/qqsort.fn");
    testFile("fn/import_dictionary.fn");

    UNPROTECT(save);
    return failed ? 1 : 0;
}

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

static bool failed = false;

static void test(PrattParser *parser, PrattTrie *trie, char *expr, char *expected) {
    parser->lexer = makePrattLexer(trie, expr, expr);
    AstNest *result = top(parser);
    int save = PROTECT(result);
    if (parser->lexer->bufList != NULL) {
        PrattToken *tok = next(parser->lexer);
        errorAt(tok, "unconsumed tokens");
    }
    PrattUTF8 *dest = newPrattUTF8();
    PROTECT(dest);
    ppAstNest(dest, result);
    if (strcmp(dest->entries, expected) != 0) {
        printf("%s - expected %s got %s\n", expr, expected, dest->entries);
        failed = true;
    }
    UNPROTECT(save);
}

int main(int argc __attribute__((unused)), char *argv[] __attribute__((unused))) {
    initProtection();
    PrattParser *p = makePrattParser();
    PROTECT(p);
    PrattTrie *t = makePrattTrie(p, NULL);
    PROTECT(t);
    test(p, t, "1",                              "{ 1; }");
    test(p, t, "5!",                             "{ !(5); }");
    test(p, t, "1i",                             "{ 1i; }");
    test(p, t, "Σ",                              "{ Σ; }");
    test(p, t, "'Σ'",                            "{ 'Σ'; }");
    test(p, t, "a123",                           "{ a123; }");
    test(p, t, "1 + 2",                          "{ +(1, 2); }");
    test(p, t, "1 <=> 2 <=> 3",                  "{ <=>(<=>(1, 2), 3); }");
    test(p, t, "1 + 2 * 3",                      "{ +(1, *(2, 3)); }");
    test(p, t, "1 * 2 + 3",                      "{ +(*(1, 2), 3); }");
    test(p, t, "an and android",                 "{ and(an, android); }");
    test(p, t, "1 * 2 * 3",                      "{ *(*(1, 2), 3); }");
    test(p, t, "1 . 2 . 3",                      "{ .(1, .(2, 3)); }");
    test(p, t, "- 1 . - 2 . 3",                  "{ -(.(1, -(.(2, 3)))); }");
    test(p, t, "--1 * 2",                        "{ -(-(*(1, 2))); }");
    test(p, t, "--1 * 2!",                       "{ -(-(*(1, !(2)))); }");
    test(p, t, "1 * ((2 + 3))",                  "{ *(1, +(2, 3)); }");
    test(p, t, "a -> b -> c",                    "{ ->(a, ->(b, c)); }");
    test(p, t, "(a -> b) -> c",                  "{ ->(->(a, b), c); }");
    test(p, t, "<a @ b",                         "{ cons(car(a), b); }");
    test(p, t, "a @ b < c",                      "{ <(cons(a, b), c); }");
    test(p, t, "1 then 2 then 3",                "{ then(1, then(2, 3)); }");
    test(p, t, "a(b)",                           "{ a(b); }");
    test(p, t, "a(b, c)",                        "{ a(b, c); }");
    test(p, t, "#(b)",                           "{ #(b); }");
    test(p, t, "#(b, c)",                        "{ #(b, c); }");
    test(p, t, "#a + b",                         "{ +(#(a), b); }"); // ??
    test(p, t, "a + #b",                         "{ +(a, #(b)); }"); // ??
    test(p, t, "a #b",                           "{ a; }");
    test(p, t, "a @ b @@ c @ d",                 "{ append(cons(a, b), cons(c, d)); }");
    test(p, t, "123456789012345678901234567890", "{ 123456789012345678901234567890; }");
    test(p, t, "12345678901234567890123456789i", "{ 12345678901234567890123456789i; }");
    test(p, t, "12345.6789i",                    "{ 12345.678900i; }");
    test(p, t, "let fn i(x) { x } in i(0)",      "{ let i = fn { (x) { x; } }; in i(0); }");
    test(p, t, "let unsafe fn i(x) { x } in i(0)", "{ let i = unsafe fn { (x) { x; } }; in i(0); }");
    test(p, t, "if (a > 2) { 3 } else { 4 }",    "{ if (>(a, 2)) { 3; } else { 4; }; }");
    test(p, t, "let x = 1 then 2 then 3; in x",  "{ let x = then(1, then(2, 3)); in x; }");
    test(p, t, "let typedef named_list(#t) { nl(str, list(#t)) } in nl", "{ let typedef named_list(t) {nl(str, list(t))}; in nl; }");
    test(p, t, "let link \"bar\" as foo; in f",  "{ let ; in f; }");
    test(p, t, "fn { (0) { 1 } (n) { n * fact(n - 1) } }", "{ fn { (0) { 1; } (n) { *(n, fact)(-(n, 1)); } }; }");
    test(p, t, "unsafe fn { (0) { 1 } (n) { n * fact(n - 1) } }", "{ unsafe fn { (0) { 1; } (n) { *(n, fact)(-(n, 1)); } }; }");
    test(p, t, "switch (x) { (0) { 1 } } }",   "{ fn { (0) { 1; } }(x); }");

    return failed ? 1 : 0;
}

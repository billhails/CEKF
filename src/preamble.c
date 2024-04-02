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
 */

// these are typedefs and functions that the compiler requires to be
// pre-defined, for example the infix `@` is mapped to `cons`, `@@` to `append`,
// prefix `<` to `car` etc.
// `puts` is required for the print system, and `cmp` for the `<=>` operator.

// *INDENT-OFF*
const char *preamble =
    "let"
    "    typedef cmp { lt | eq | gt }"
    "    typedef bool { false | true }"
    "    typedef list(#t) { nil | cons(#t, list(#t)) }"
    "    fn append {"
    "        ([], b) { b }"
    "        (h @ t, b) { h @ append(t, b) }"
    "    }"
    "    fn car {"
    "        (h @ _) { h }"
    "    }"
    "    fn cdr {"
    "        (_ @ t) { t }"
    "    }"
    "    fn puts(s) {"
    "        let"
    "            fn helper {"
    "                ([]) { true }"
    "                (h @ t) {"
    "                    putc(h);"
    "                    helper(t)"
    "                }"
    "            }"
    "        in"
    "            helper(s);"
    "            s"
    "    }"
    "    fn print_list(helper, l) {"
    "        let"
    "            fn h1 {"
    "                ([]) { true }"
    "                (h @ t) {"
    "                    helper(h);"
    "                    h2(t)"
    "                }"
    "            }"
    "            fn h2 {"
    "                ([]) { true }"
    "                (h @ t) {"
    "                    puts(\", \");"
    "                    helper(h);"
    "                    h2(t)"
    "                }"
    "            }"
    "        in"
    "            puts(\"[\");"
    "            h1(l);"
    "            puts(\"]\");"
    "            l"
    "    }"
    "    fn print_fn(f) {"
    "        puts(\"<function>\");"
    "        f"
    "    }"
    "    fn print_int(n) {"
    "        putn(n);"
    "        n"
    "    }"
    "    fn print_char(c) {"
    "        putc('\\'');"
    "        putc(c);"
    "        putc('\\'');"
    "        c"
    "    }"
    "    fn print_(v) {"
    "        putv(v);"
    "        v"
    "    }"
    "    fn print_string(s) {"
    "        putc('\"');"
    "        puts(s);"
    "        putc('\"');"
    "        s"
    "    }"
    "    fn print_tuple_0(t) {"
    "       puts(\"#()\");"
    "       t"
    "    }"
    "    fn print_tuple_1(p1, t=#(a)) {"
    "       puts(\"#(\");"
    "       p1(a);"
    "       puts(\")\");"
    "       t"
    "    }"
    "    fn print_tuple_2(p1, p2, t=#(a, b)) {"
    "       puts(\"#(\");"
    "       p1(a);"
    "       puts(\", \");"
    "       p2(b);"
    "       puts(\")\");"
    "       t"
    "    }"
    "    fn print_tuple_3(p1, p2, p3, t=#(a, b, c)) {"
    "       puts(\"#(\");"
    "       p1(a);"
    "       puts(\", \");"
    "       p2(b);"
    "       puts(\", \");"
    "       p3(c);"
    "       puts(\")\");"
    "       t"
    "    }"
    "    fn print_tuple_4(p1, p2, p3, p4, t=#(a, b, c, d)) {"
    "       puts(\"#(\");"
    "       p1(a);"
    "       puts(\", \");"
    "       p2(b);"
    "       puts(\", \");"
    "       p3(c);"
    "       puts(\", \");"
    "       p4(d);"
    "       puts(\")\");"
    "       t"
    "    }"
    "in {";
// *INDENT-ON*

const char *postamble = "}";

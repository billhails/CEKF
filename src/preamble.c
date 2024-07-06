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
    "    namespace"
    "    typedef cmp { lt | eq | gt }\n"
    "    typedef bool { false | true }\n"
    "    typedef list(#t) { nil | cons(#t, list(#t)) }\n"
    "    alias string = list(char);\n"

    "    fn assert_(line, file, condition) {\n"
    "        if (condition) {\n"
    "            true\n"
    "        } else {\n"
    "            puts(\"assertion failed in \");\n"
    "            puts(file);\n"
    "            puts(\" line \");\n"
    "            putn(line);\n"
    "            putc('\\n');\n"
    "            assertion();\n"
    "        }\n"
    "    }\n"

    "    fn append {\n"
    "        ([], b) { b }\n"
    "        (h @ t, b) { h @ append(t, b) }\n"
    "    }\n"

    "    unsafe fn car {\n"
    "        (h @ _) { h }\n"
    "    }\n"

    "    unsafe fn cdr {\n"
    "        (_ @ t) { t }\n"
    "    }\n"

    "    fn puts(s) {\n"
    "        let\n"
    "            fn helper {\n"
    "                ([]) { true }\n"
    "                (h @ t) {\n"
    "                    putc(h);\n"
    "                    helper(t)\n"
    "                }\n"
    "            }\n"
    "        in\n"
    "            helper(s);\n"
    "            s\n"
    "    }\n"

    "    fn print_list(helper, l) {\n"
    "        let\n"
    "            fn h1 {\n"
    "                ([]) { true }\n"
    "                (h @ t) {\n"
    "                    helper(h);\n"
    "                    h2(t)\n"
    "                }\n"
    "            }\n"
    "            fn h2 {\n"
    "                ([]) { true }\n"
    "                (h @ t) {\n"
    "                    puts(\", \");\n"
    "                    helper(h);\n"
    "                    h2(t)\n"
    "                }\n"
    "            }\n"
    "        in\n"
    "            puts(\"[\");\n"
    "            h1(l);\n"
    "            puts(\"]\");\n"
    "            l\n"
    "    }\n"

    "    fn print_fn(f) {\n"
    "        puts(\"<function>\");\n"
    "        f\n"
    "    }\n"

    "    fn print_opaque(f) {\n"
    "        puts(\"<opaque>\");\n"
    "        f\n"
    "    }\n"

    "    fn print_int(n) {\n"
    "        putn(n);\n"
    "        n\n"
    "    }\n"

    "    fn print_char(c) {\n"
    "        putc('\\'');\n"
    "        putc(c);\n"
    "        putc('\\'');\n"
    "        c\n"
    "    }\n"

    "    fn print_(v) {\n"
    "        putv(v);\n"
    "        v\n"
    "    }\n"

    "    fn print_string(s) {\n"
    "        putc('\"');\n"
    "        puts(s);\n"
    "        putc('\"');\n"
    "        s\n"
    "    }\n"

    "    fn print_tuple_0(t) {\n"
    "       puts(\"#()\");\n"
    "       t\n"
    "    }\n"

    "    unsafe fn print_tuple_1(p1, t=#(a)) {\n"
    "       puts(\"#(\");\n"
    "       p1(a);\n"
    "       puts(\")\");\n"
    "       t\n"
    "    }\n"

    "    unsafe fn print_tuple_2(p1, p2, t=#(a, b)) {\n"
    "       puts(\"#(\");\n"
    "       p1(a);\n"
    "       puts(\", \");\n"
    "       p2(b);\n"
    "       puts(\")\");\n"
    "       t\n"
    "    }\n"

    "    unsafe fn print_tuple_3(p1, p2, p3, t=#(a, b, c)) {\n"
    "       puts(\"#(\");\n"
    "       p1(a);\n"
    "       puts(\", \");\n"
    "       p2(b);\n"
    "       puts(\", \");\n"
    "       p3(c);\n"
    "       puts(\")\");\n"
    "       t\n"
    "    }\n"

    "    unsafe fn print_tuple_4(p1, p2, p3, p4, t=#(a, b, c, d)) {\n"
    "       puts(\"#(\");\n"
    "       p1(a);\n"
    "       puts(\", \");\n"
    "       p2(b);\n"
    "       puts(\", \");\n"
    "       p3(c);\n"
    "       puts(\", \");\n"
    "       p4(d);\n"
    "       puts(\")\");\n"
    "       t\n"
    "    }";
// *INDENT-ON*

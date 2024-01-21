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
"in {";

const char *postamble = "}";

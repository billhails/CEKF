#ifndef cekf_parser_info_h
#  define cekf_parser_info_h
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

#  include "common.h"

typedef struct ParserInfo {
    int lineno;
    char *filename;
} ParserInfo;

typedef struct HeaderAndInfo {
    struct Header header;
    struct ParserInfo info;
} HeaderAndInfo;

#  define COPY_PARSER_INFO(from) ((from)->_yy_parser_info)

static inline void _reportParserInfo(HeaderAndInfo *I) {
    eprintf("in %s, line %d\n", I->info.filename, I->info.lineno);
}

#  define REPORT_PARSER_INFO(x) _reportParserInfo((HeaderAndInfo *)(x))

#endif

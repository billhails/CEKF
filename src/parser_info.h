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
 *
 * ParserInfo is a common sub-structure of almost all structures in the interpreter,
 * immediately following the memory-management Header. It contains the filename and line
 * number in the source code that resulted in the creation of the structure.
 * The same information is copied from one stage to the next during translation/compilation
 * by the CPI macro.
 *
 * Note however that it is **not** part of the HashSymbol structure, as the same symbol
 * may occur in multiple places in the source code.
 */

#  include "common.h"
#  include "memory.h"

typedef struct ParserInfo {
    int lineno;
    char *filename;
} ParserInfo;

static inline void _reportParserInfo(ParserInfo I) {
    eprintf("in %s, line %d\n", I.filename, I.lineno);
}

static inline int _eqParserInfo(ParserInfo I, ParserInfo J) {
    return I.filename == J.filename &&
           I.lineno == J.lineno;
}

// generated structs all name the parser info field _yy_parser_info
#  define CPI(from) ((from)->_yy_parser_info)
#  define REPORT_PARSER_INFO(x) _reportParserInfo(CPI(x))
#  define EQ_PARSER_INFO(x, y) _eqParserInfo(CPI(x), CPI(y))

#endif

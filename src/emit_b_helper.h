#ifndef cekf_emit_b_helper_h
#define cekf_emit_b_helper_h
/*
 * CEKF - VM supporting amb
 * Copyright (C) 2022-2026  Bill Hails
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

#include "emit_b.h"

Index bemitter_buffer_pos(BBuffer *);
Index bemitter_pos(BEmitterContext *);
void bemit_comment(BEmitterContext *, SCharArray *);
void bemit_word(BEmitterContext *, UInteger);
void bemit_code(BEmitterContext *, BBC, int, int, int);
void bemit_location(BEmitterContext *, ParserInfo);
void bemit_buffer_label(BBuffer *, HashSymbol *, Index);
void bemit_label(BEmitterContext *, HashSymbol *, Index);
BBuffer *bemitter_newBuffer();
BEmitterContext *bemitter_newContext(EmitterContext);
void bemitter_append(BBuffer *to, BBuffer *from, Index bufferId);
void bemit_fixup_code(BEmitterContext *, HashSymbol *label, Index loc);
void bemit_fixup_inttable(BEmitterContext *, Index tableId, Index loc);
void bemit_fixup_chartable(BEmitterContext *, Index tableId, Index loc);
void bemit_fixup_matchtable(BEmitterContext *, Index tableId, Index loc);

#endif

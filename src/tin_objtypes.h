#ifndef cekf_tin_objtypes_h
#define cekf_tin_objtypes_h
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

// generated from src/tin.yaml by makeAST.py


// Type inference structures used by Algorithm W.

#define TIN_OBJTYPES() OBJTYPE_TINFUNCTIONAPPLICATION, \
OBJTYPE_TINMONOTYPELIST, \
OBJTYPE_TINTYPEQUANTIFIER, \
OBJTYPE_TINCONTEXT, \
OBJTYPE_TINSUBSTITUTION, \
OBJTYPE_TINARGSRESULT, \
OBJTYPE_TINVARRESULT, \
OBJTYPE_TINVARSRESULT, \
OBJTYPE_TINMONOTYPE, \
OBJTYPE_TINPOLYTYPE

#define TIN_OBJTYPE_CASES() \
case OBJTYPE_TINFUNCTIONAPPLICATION:\
case OBJTYPE_TINMONOTYPELIST:\
case OBJTYPE_TINTYPEQUANTIFIER:\
case OBJTYPE_TINCONTEXT:\
case OBJTYPE_TINSUBSTITUTION:\
case OBJTYPE_TINARGSRESULT:\
case OBJTYPE_TINVARRESULT:\
case OBJTYPE_TINVARSRESULT:\
case OBJTYPE_TINMONOTYPE:\
case OBJTYPE_TINPOLYTYPE:\


void markTinObj(struct Header *h);
void freeTinObj(struct Header *h);
char *typenameTinObj(int type);

#endif

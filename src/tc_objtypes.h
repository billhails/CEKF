#ifndef cekf_tc_objtypes_h
#define cekf_tc_objtypes_h
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
 * Structures to support type inference
 *
 * generated from src/tc.yaml by makeAST.py
 */

#define TC_OBJTYPES() OBJTYPE_TCENV, \
OBJTYPE_TCNG, \
OBJTYPE_TCFUNCTION, \
OBJTYPE_TCPAIR, \
OBJTYPE_TCTYPEDEF, \
OBJTYPE_TCTYPEDEFARGS, \
OBJTYPE_TCVAR, \
OBJTYPE_TCTYPE

#define TC_OBJTYPE_CASES() \
case OBJTYPE_TCENV:\
case OBJTYPE_TCNG:\
case OBJTYPE_TCFUNCTION:\
case OBJTYPE_TCPAIR:\
case OBJTYPE_TCTYPEDEF:\
case OBJTYPE_TCTYPEDEFARGS:\
case OBJTYPE_TCVAR:\
case OBJTYPE_TCTYPE:\


void markTcObj(struct Header *h);
void freeTcObj(struct Header *h);
char *typenameTcObj(int type);

#endif

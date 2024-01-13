#ifndef cekf_tpmc_objtypes_h
#define cekf_tpmc_objtypes_h
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
 * Term Pattern Matching Compiler types
 *
 * Generated from src/tpmc.yaml by tools/makeAST.py
 */

/*
 * define objtypes
 */

#define TPMC_OBJTYPES() \
OBJTYPE_TPMCMATCHRULES, \
OBJTYPE_TPMCMATCHRULE, \
OBJTYPE_TPMCCOMPARISONPATTERN, \
OBJTYPE_TPMCASSIGNMENTPATTERN, \
OBJTYPE_TPMCCONSTRUCTORPATTERN, \
OBJTYPE_TPMCPATTERN, \
OBJTYPE_TPMCTESTSTATE, \
OBJTYPE_TPMCFINALSTATE, \
OBJTYPE_TPMCSTATE, \
OBJTYPE_TPMCARC, \
OBJTYPE_TPMCARCLIST, \
OBJTYPE_TPMCINTLIST, \
OBJTYPE_TPMCPATTERNVALUE, \
OBJTYPE_TPMCSTATEVALUE, \
OBJTYPE_TPMCMATCHRULEARRAY, \
OBJTYPE_TPMCVARIABLEARRAY, \
OBJTYPE_TPMCPATTERNARRAY, \
OBJTYPE_TPMCSTATEARRAY, \
OBJTYPE_TPMCARCARRAY, \
OBJTYPE_TPMCINTARRAY, \
OBJTYPE_TPMCMATRIX

/*
 * define cases
 */

#define TPMC_OBJTYPE_CASES() \
case OBJTYPE_TPMCMATCHRULES:\
case OBJTYPE_TPMCMATCHRULE:\
case OBJTYPE_TPMCCOMPARISONPATTERN:\
case OBJTYPE_TPMCASSIGNMENTPATTERN:\
case OBJTYPE_TPMCCONSTRUCTORPATTERN:\
case OBJTYPE_TPMCPATTERN:\
case OBJTYPE_TPMCTESTSTATE:\
case OBJTYPE_TPMCFINALSTATE:\
case OBJTYPE_TPMCSTATE:\
case OBJTYPE_TPMCARC:\
case OBJTYPE_TPMCARCLIST:\
case OBJTYPE_TPMCINTLIST:\
case OBJTYPE_TPMCPATTERNVALUE:\
case OBJTYPE_TPMCSTATEVALUE:\
case OBJTYPE_TPMCMATCHRULEARRAY:\
case OBJTYPE_TPMCVARIABLEARRAY:\
case OBJTYPE_TPMCPATTERNARRAY:\
case OBJTYPE_TPMCSTATEARRAY:\
case OBJTYPE_TPMCARCARRAY:\
case OBJTYPE_TPMCINTARRAY:\
case OBJTYPE_TPMCMATRIX:\


/*
 * declare generic type functions
 */

void markTpmcObj(struct Header *h);
void freeTpmcObj(struct Header *h);
char *typenameTpmcObj(int type);

#endif

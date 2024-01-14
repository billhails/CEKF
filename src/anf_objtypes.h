#ifndef cekf_anf_objtypes_h
#define cekf_anf_objtypes_h
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
 * ANF structures to be converted to bytecode.
 *
 * Generated from src/anf.yaml by tools/makeAST.py
 */

/*
 * define objtypes
 */

#define ANF_OBJTYPES() \
OBJTYPE_AEXPLAM, \
OBJTYPE_AEXPVARLIST, \
OBJTYPE_AEXPANNOTATEDVAR, \
OBJTYPE_AEXPPRIMAPP, \
OBJTYPE_AEXPUNARYAPP, \
OBJTYPE_AEXPLIST, \
OBJTYPE_AEXPINTLIST, \
OBJTYPE_CEXPAPPLY, \
OBJTYPE_AEXPMAKEVEC, \
OBJTYPE_CEXPIF, \
OBJTYPE_CEXPCOND, \
OBJTYPE_CEXPINTCONDCASES, \
OBJTYPE_CEXPCHARCONDCASES, \
OBJTYPE_CEXPMATCH, \
OBJTYPE_MATCHLIST, \
OBJTYPE_CEXPLETREC, \
OBJTYPE_LETRECBINDINGS, \
OBJTYPE_CEXPAMB, \
OBJTYPE_CEXPCUT, \
OBJTYPE_CEXPBOOL, \
OBJTYPE_EXPLET, \
OBJTYPE_TYPEDAEXP, \
OBJTYPE_CEXPCONDCASES, \
OBJTYPE_AEXP, \
OBJTYPE_CEXP, \
OBJTYPE_EXP

/*
 * define cases
 */

#define ANF_OBJTYPE_CASES() \
case OBJTYPE_AEXPLAM:\
case OBJTYPE_AEXPVARLIST:\
case OBJTYPE_AEXPANNOTATEDVAR:\
case OBJTYPE_AEXPPRIMAPP:\
case OBJTYPE_AEXPUNARYAPP:\
case OBJTYPE_AEXPLIST:\
case OBJTYPE_AEXPINTLIST:\
case OBJTYPE_CEXPAPPLY:\
case OBJTYPE_AEXPMAKEVEC:\
case OBJTYPE_CEXPIF:\
case OBJTYPE_CEXPCOND:\
case OBJTYPE_CEXPINTCONDCASES:\
case OBJTYPE_CEXPCHARCONDCASES:\
case OBJTYPE_CEXPMATCH:\
case OBJTYPE_MATCHLIST:\
case OBJTYPE_CEXPLETREC:\
case OBJTYPE_LETRECBINDINGS:\
case OBJTYPE_CEXPAMB:\
case OBJTYPE_CEXPCUT:\
case OBJTYPE_CEXPBOOL:\
case OBJTYPE_EXPLET:\
case OBJTYPE_TYPEDAEXP:\
case OBJTYPE_CEXPCONDCASES:\
case OBJTYPE_AEXP:\
case OBJTYPE_CEXP:\
case OBJTYPE_EXP:\


/*
 * declare generic type functions
 */

void markAnfObj(struct Header *h);
void freeAnfObj(struct Header *h);
char *typenameAnfObj(int type);

#endif

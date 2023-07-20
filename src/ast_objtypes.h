#ifndef cekf_ast_objtypes_h
#define cekf_ast_objtypes_h
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

// generated from src/ast.yaml by makeAST.py



#define AST_OBJTYPES() OBJTYPE_ASTNEST, \
OBJTYPE_ASTDEFINITIONS, \
OBJTYPE_ASTDEFINE, \
OBJTYPE_ASTPROTOTYPE, \
OBJTYPE_ASTPROTOTYPEBODY, \
OBJTYPE_ASTPROTOTYPESYMBOLTYPE, \
OBJTYPE_ASTLOAD, \
OBJTYPE_ASTTYPEDEF, \
OBJTYPE_ASTFLATTYPE, \
OBJTYPE_ASTTYPESYMBOLS, \
OBJTYPE_ASTTYPEBODY, \
OBJTYPE_ASTTYPECONSTRUCTOR, \
OBJTYPE_ASTTYPELIST, \
OBJTYPE_ASTTYPE, \
OBJTYPE_ASTCOMPOSITEFUNCTION, \
OBJTYPE_ASTFUNCTION, \
OBJTYPE_ASTARGLIST, \
OBJTYPE_ASTUNPACK, \
OBJTYPE_ASTNAMEDARG, \
OBJTYPE_ASTENVTYPE, \
OBJTYPE_ASTFUNCALL, \
OBJTYPE_ASTPACKAGE, \
OBJTYPE_ASTEXPRESSIONS, \
OBJTYPE_ASTENV, \
OBJTYPE_ASTDEFINITION, \
OBJTYPE_ASTSINGLEPROTOTYPE, \
OBJTYPE_ASTTYPECLAUSE, \
OBJTYPE_ASTARG, \
OBJTYPE_ASTEXPRESSION

#define AST_OBJTYPE_CASES() \
case OBJTYPE_ASTNEST:\
case OBJTYPE_ASTDEFINITIONS:\
case OBJTYPE_ASTDEFINE:\
case OBJTYPE_ASTPROTOTYPE:\
case OBJTYPE_ASTPROTOTYPEBODY:\
case OBJTYPE_ASTPROTOTYPESYMBOLTYPE:\
case OBJTYPE_ASTLOAD:\
case OBJTYPE_ASTTYPEDEF:\
case OBJTYPE_ASTFLATTYPE:\
case OBJTYPE_ASTTYPESYMBOLS:\
case OBJTYPE_ASTTYPEBODY:\
case OBJTYPE_ASTTYPECONSTRUCTOR:\
case OBJTYPE_ASTTYPELIST:\
case OBJTYPE_ASTTYPE:\
case OBJTYPE_ASTCOMPOSITEFUNCTION:\
case OBJTYPE_ASTFUNCTION:\
case OBJTYPE_ASTARGLIST:\
case OBJTYPE_ASTUNPACK:\
case OBJTYPE_ASTNAMEDARG:\
case OBJTYPE_ASTENVTYPE:\
case OBJTYPE_ASTFUNCALL:\
case OBJTYPE_ASTPACKAGE:\
case OBJTYPE_ASTEXPRESSIONS:\
case OBJTYPE_ASTENV:\
case OBJTYPE_ASTDEFINITION:\
case OBJTYPE_ASTSINGLEPROTOTYPE:\
case OBJTYPE_ASTTYPECLAUSE:\
case OBJTYPE_ASTARG:\
case OBJTYPE_ASTEXPRESSION:\


void markAstObj(struct Header *h);
void freeAstObj(struct Header *h);
char *typenameAstObj(int type);

#endif

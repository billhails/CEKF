#ifndef cekf_lambda_objtypes_h
#define cekf_lambda_objtypes_h
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

// generated from src/lambda.yaml by makeAST.py


// Plain lambda structures generated by lambda conversion.

#define LAMBDA_OBJTYPES() OBJTYPE_LAMLAM, \
OBJTYPE_LAMVARLIST, \
OBJTYPE_LAMPRIMAPP, \
OBJTYPE_LAMUNARYAPP, \
OBJTYPE_LAMLIST, \
OBJTYPE_LAMAPPLY, \
OBJTYPE_LAMMAKEVEC, \
OBJTYPE_LAMCOND, \
OBJTYPE_LAMMATCH, \
OBJTYPE_LAMMATCHLIST, \
OBJTYPE_LAMLETREC, \
OBJTYPE_LAMLETRECBINDINGS, \
OBJTYPE_LAMCONTEXT, \
OBJTYPE_LAMTYPECONSTRUCTORINFO, \
OBJTYPE_LAMEXP

#define LAMBDA_OBJTYPE_CASES() \
case OBJTYPE_LAMLAM:\
case OBJTYPE_LAMVARLIST:\
case OBJTYPE_LAMPRIMAPP:\
case OBJTYPE_LAMUNARYAPP:\
case OBJTYPE_LAMLIST:\
case OBJTYPE_LAMAPPLY:\
case OBJTYPE_LAMMAKEVEC:\
case OBJTYPE_LAMCOND:\
case OBJTYPE_LAMMATCH:\
case OBJTYPE_LAMMATCHLIST:\
case OBJTYPE_LAMLETREC:\
case OBJTYPE_LAMLETRECBINDINGS:\
case OBJTYPE_LAMCONTEXT:\
case OBJTYPE_LAMTYPECONSTRUCTORINFO:\
case OBJTYPE_LAMEXP:\


void markLambdaObj(struct Header *h);
void freeLambdaObj(struct Header *h);
char *typenameLambdaObj(int type);

#endif

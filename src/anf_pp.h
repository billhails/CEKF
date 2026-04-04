#ifndef cekf_anf_pp_h
#define cekf_anf_pp_h
/*
 * CEKF - VM supporting amb
 * Copyright (C) 2022-2025  Bill Hails
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

// Bespoke pretty-printer for anf

#include <stdio.h>
#include <stdlib.h>

#include "anf.h"
#include "common.h"

void ppAnfEnv(FILE *out, AnfEnv *x);
void ppAexpLam(FILE *out, AexpLam *x);
void ppAexpVarList(FILE *out, AexpVarList *x);
void ppAexpVar(FILE *out, HashSymbol *x);
void ppAexpAnnotatedVar(FILE *out, AexpAnnotatedVar *x);
void ppAexpPrimApp(FILE *out, AexpPrimApp *x);
void ppAexpList(FILE *out, AexpList *x);
void ppAexpIntList(FILE *out, AexpIntList *x);
void ppAexpMakeList(FILE *out, AexpList *x);
void ppAexpMakeVec(FILE *out, AexpMakeVec *x);
void ppBareAexpList(FILE *out, AexpList *x);
void ppCexpApply(FILE *out, CexpApply *x);
void ppCexpIf(FILE *out, CexpIf *x);
void ppCexpCond(FILE *out, CexpCond *x);
void ppCexpIntCondCases(FILE *out, CexpIntCondCases *x);
void ppCexpCharCondCases(FILE *out, CexpCharCondCases *x);
void ppCexpCondCases(FILE *out, CexpCondCases *x);
void ppCexpLetRec(FILE *out, CexpLetRec *x);
void ppAnfLetRecBindings(FILE *out, AnfLetRecBindings *x);
void ppCexpAmb(FILE *out, CexpAmb *x);
void ppCexpCut(FILE *out, CexpCut *x);
void ppAnfMatchList(FILE *out, AnfMatchList *x);
void ppCexpMatch(FILE *out, CexpMatch *x);
void ppAexp(FILE *out, Aexp *x);
void ppCexp(FILE *out, Cexp *x);
void ppAnfExp(FILE *out, AnfExp *x);
void ppAnfExpLet(FILE *out, AnfExpLet *x);

#endif

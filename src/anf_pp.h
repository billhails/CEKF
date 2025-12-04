#ifndef cekf_anf_pp_h
#  define cekf_anf_pp_h
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

#  include <stdio.h>
#  include <stdlib.h>

#  include "common.h"
#  include "anf.h"

void ppAnfEnv(AnfEnv *x);
void ppAexpLam(AexpLam *x);
void ppAexpVarList(AexpVarList *x);
void ppAexpVar(HashSymbol *x);
void ppAexpAnnotatedVar(AexpAnnotatedVar *x);
void ppAexpPrimApp(AexpPrimApp *x);
void ppAexpList(AexpList *x);
void ppAexpIntList(AexpIntList *x);
void ppAexpMakeList(AexpList *x);
void ppAexpMakeVec(AexpMakeVec *x);
void ppBareAexpList(AexpList *x);
void ppCexpApply(CexpApply *x);
void ppCexpIf(CexpIf *x);
void ppCexpCond(CexpCond *x);
void ppCexpIntCondCases(CexpIntCondCases *x);
void ppCexpCharCondCases(CexpCharCondCases *x);
void ppCexpCondCases(CexpCondCases *x);
void ppCexpLetRec(CexpLetRec *x);
void ppAnfLetRecBindings(AnfLetRecBindings *x);
void ppCexpAmb(CexpAmb *x);
void ppCexpCut(CexpCut *x);
void ppAnfMatchList(AnfMatchList *x);
void ppCexpMatch(CexpMatch *x);
void ppAexp(Aexp *x);
void ppCexp(Cexp *x);
void ppAnfExp(AnfExp *x);
void ppAnfExpLet(AnfExpLet *x);

#endif

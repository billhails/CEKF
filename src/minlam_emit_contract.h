#ifndef cekf_minlam_emit_contract_h
#define cekf_minlam_emit_contract_h
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

///////////////////////////////////////////////////////////////
// Procedures defined in minlam_emit.inc and used in each emitter
///////////////////////////////////////////////////////////////

static ER *emitSimpleExp(MinExp *, EC *);
static void emitMinExp(MinExp *, EC *);
static void releaseSlot(ER *, EC *);
static HashSymbol *makeLambdaLabel(HashSymbol *symbol);
static ER *claimSlot(EC *);

///////////////////////////////////////////////////////////////
// Procedures defined in each emitter and used by minlam_emit.inc
///////////////////////////////////////////////////////////////

static EC *extendContextForLambda(HashSymbol *, EC *);
static ER *emitArg(MinExp *, EC *);
static ER *emitAddrResult(HashSymbol *, EC *__attribute__((unused)));
static ER *emitIntegerResult(Integer, EC *__attribute__((unused)));
static ER *emitCharacterResult(Character, EC *__attribute__((unused)));
static void comment(EC *, char *, ...) __attribute__((format(printf, 2, 3)));
static void emitAssignPrimOp(MinPrimOp, ER *, ER *, ER *, EC *);
static void emitCallBuiltin(BuiltIn *, ER *, ER *, EC *);
static void emitClosureNew(ER *, HashSymbol *, EC *);
static void emitClosureSetEnv(ER *, ER *, EC *);
static void emitConstructVec(ER *, int, RA *, EC *);
static void emitDone(int, EC *);
static void emitIfThenElse(ER *, MinExp *, MinExp *, EC *);
static void emitJumpToLambda(ER *, EC *);
static ER *emitMaybeBigInt(MaybeBigInt *, EC *);
static void emitMinCond(MinCond *, EC *);
static void emitMinMatch(MinMatch *, EC *);
static void emitTrace(ER *, RA *, EC *);
static void emitUnprotect(EC *);
static ER *emitNone(EC *ctx __attribute__((unused)));
static ER *emitEmptyVec(EC *ctx __attribute__((unused)));
static void emitVecGetImm(ER *, ER *, int, EC *);
static inline RA *newRA();
static inline void pushRA(RA *ra, ER *r);
static inline ER *newResultSlotSymbol(HashSymbol *s);
static inline ER *erForSlot(Index, EC *);
static HashSymbol *tokenForER(ER *, EC *);
static ER *emitAnnotatedVarResult(MinAnnotatedVar *,
                                  EC *ctx __attribute__((unused)));

#endif

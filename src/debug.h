#ifndef cekf_debug_h
#define cekf_debug_h

#include "cekf.h"
#include "exp.h"

void printValue(Value *x);
void printValueList(ValueList *x);
void printClo(Clo *x);
void printCEKF(CEKF *x);
void printEnv(Env *x);
void printKont(Kont *x);
void printLetK(LetK *x);
void printFail(Fail *x);
void printBackTrack(BackTrack *x);
void printAexpLam(AexpLam *x);
void printAexpVarList(AexpVarList *x);
void printAexpVar(AexpVar *x);
void printAexp(Aexp *x);
void printAexpPrimApp(AexpPrimApp *x);
void printAexpList(AexpList *x);
void printCexp(Cexp *x);
void printCexpApply(CexpApply *x);
void printCexpConditional(CexpConditional *x);
void printCexpLetRec(CexpLetRec *x);
void printLetRecBindings(LetRecBindings *x);
void printCexpAmb(CexpAmb *x);
void printExp(Exp *x);
void printExpLet(ExpLet *x);

#endif

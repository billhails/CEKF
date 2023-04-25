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
void printFail(Fail *x);
void printAexpLam(AexpLam *x);
void printAexpVarList(AexpVarList *x);
void printAexpVar(AexpVar *x);
void printAexpPrimApp(AexpPrimApp *x);
void printAexpList(AexpList *x);
void printCexpApply(CexpApply *x);
void printCexpCond(CexpCond *x);
void printCexpLetRec(CexpLetRec *x);
void printLetRecBindings(LetRecBindings *x);
void printCexpAmb(CexpAmb *x);
void printExp(Exp *x);
void printExpLet(ExpLet *x);

#endif

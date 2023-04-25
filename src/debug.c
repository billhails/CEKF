#include <stdio.h>

#include "debug.h"

void printValue(Value *x) {
    printf("Value[");
    switch (x->type) {
        case VALUE_TYPE_VOID:
            printf("VALUE_TYPE_VOID");
            break;
        case VALUE_TYPE_INTEGER:
            printf("VALUE_TYPE_INTEGER, %d", x->val.z);
            break;
        case VALUE_TYPE_TRUE:
            printf("VALUE_TYPE_TRUE");
            break;
        case VALUE_TYPE_FALSE:
            printf("VALUE_TYPE_FALSE");
            break;
        case VALUE_TYPE_CLO:
            printf("VALUE_TYPE_CLO, ");
            printClo(x->val.clo);
            break;
        case VALUE_TYPE_CONT:
            printf("VALUE_TYPE_CONT, ");
            printKont(x->val.k);
            break;
    }
    printf("]");
}

void printValueList(ValueList *x) {
    printf("ValueList[");
    while (x != NULL) {
        printValue(x->value);
        if (x->next != NULL) {
            printf(", ");
        }
        x = x->next;
    }
    printf("]");
}

void printClo(Clo *x) {
    printf("Clo[");
    printAexpLam(x->lam);
    printf(", ");
    printEnv(x->rho);
    printf("]");
}

void printCEKF(CEKF *x) {
    printf("CEKF[");
    printExp(x->C);
    printf(", ");
    printEnv(x->E);
    printf(", ");
    printKont(x->K);
    printf(", ");
    printFail(x->F);
    printf(", ");
    printValue(x->V);
    printf("]\n");
}

void printEnv(Env *x) {
    printf("Env[");
    while (x != NULL) {
        printAexpVar(x->var);
        printf(" => ");
        printValue(x->val);
        if (x->next != NULL) {
            printf(", ");
        }
        x = x->next;
    }
    printf("]");
}

void printKont(Kont *x) {
    printf("Kont[");
    switch (x->type) {
        case KONT_TYPE_LETK:
            printf("KONT_TYPE_LETK, ");
            printLetK(x->val.letK);
            break;
        case KONT_TYPE_HALT:
            printf("KONT_TYPE_HALT");
            break;
    }
    printf("]");
}

void printLetK(LetK *x) {
    printf("LetK[");
    printAexpVar(x->var);
    printf(", ");
    printExp(x->body);
    printf(", ");
    printEnv(x->rho);
    printf(", ");
    printKont(x->k);
    printf("]");
}

void printFail(Fail *x) {
    printf("Fail[");
    switch (x->type) {
        case FAIL_TYPE_BACKTRACK:
            printf("FAIL_TYPE_BACKTRACK, ");
            printBackTrack(x->val.backTrack);
            break;
        case FAIL_TYPE_END:
            printf("FAIL_TYPE_END");
            break;
    }
    printf("]");
}

void printBackTrack(BackTrack *x) {
    printf("BackTrack[");
    printExp(x->exp);
    printf(", ");
    printEnv(x->rho);
    printf(", ");
    printKont(x->k);
    printf(", ");
    printFail(x->f);
    printf("]");
}

void printAexpLam(AexpLam *x) {
    printf("(lambda ");
    printAexpVarList(x->args);
    printf(" ");
    printExp(x->exp);
    printf(")");
}

void printAexpVarList(AexpVarList *x) {
    printf("(");
    while (x != NULL) {
        printAexpVar(x->var);
        if (x->next != NULL) {
            printf(" ");
        }
        x = x->next;
    }
    printf(")");
}

void printAexpVar(AexpVar *x) {
    printf("%s", x->name);
}

void printAexp(Aexp *x) {
    switch (x->type) {
        case AEXP_TYPE_LAM:
            printAexpLam(x->val.lam);
            break;
        case AEXP_TYPE_VAR:
            printAexpVar(x->val.var);
            break;
        case AEXP_TYPE_TRUE:
            printf("#t");
            break;
        case AEXP_TYPE_FALSE:
            printf("#f");
            break;
        case AEXP_TYPE_INT:
            printf("%d", x->val.integer);
            break;
        case AEXP_TYPE_PRIM:
            printAexpPrimApp(x->val.prim);
            break;
    }
}

void printAexpPrimApp(AexpPrimApp *x) {
    printf("(");
    switch(x->op) {
        case AEXP_PRIM_ADD:
            printf("+ ");
            break;
        case AEXP_PRIM_SUB:
            printf("- ");
            break;
        case AEXP_PRIM_MUL:
            printf("* ");
            break;
        case AEXP_PRIM_DIV:
            printf("* ");
            break;
    }
    printAexpList(x->args);
    printf(")");
}

void printAexpList(AexpList *x) {
    printf("(");
    while (x != NULL) {
        printAexp(x->exp);
        if (x->next) {
            printf(" ");
        }
        x = x->next;
    }
    printf(")");
}

void printCexp(Cexp *x) {
    switch (x->type) {
        case CEXP_TYPE_APPLY:
            printCexpApply(x->val.apply);
            break;
        case CEXP_TYPE_CONDITIONAL:
            printCexpConditional(x->val.conditional);
            break;
        case CEXP_TYPE_CALLCC:
            printf("(call/cc ");
            printAexp(x->val.callCC);
            printf(")");
            break;
        case CEXP_TYPE_LETREC:
            printCexpLetRec(x->val.letRec);
            break;
        case CEXP_TYPE_AMB:
            printCexpAmb(x->val.amb);
            break;
        case CEXP_TYPE_BACK:
            printf("(back)");
            break;
    }
}

void printCexpApply(CexpApply *x) {
    printf("(");
    printAexp(x->function);
    printf(" ");
    printAexpList(x->args);
    printf(")");
}

void printCexpConditional(CexpConditional *x) {
    printf("(if ");
    printAexp(x->condition);
    printf(" ");
    printExp(x->consequent);
    printf(" ");
    printExp(x->alternative);
    printf(")");
}

void printCexpLetRec(CexpLetRec *x) {
    printf("(letrec ");
    printLetRecBindings(x->bindings);
    printf(" ");
    printExp(x->body);
    printf(")");
}

void printLetRecBindings(LetRecBindings *x) {
    printf("(");
    while (x != NULL) {
        printf("(");
        printAexpVar(x->var);
        printf(" ");
        printAexp(x->val);
        printf(")");
        if (x->next != NULL) {
            printf(" ");
        }
        x = x->next;
    }
    printf(")");
}

void printCexpAmb(CexpAmb *x) {
    printf("(amb ");
    printExp(x->exp1);
    printf(" ");
    printExp(x->exp2);
    printf(")");
}

void printExp(Exp *x) {
    switch (x->type) {
        case EXP_TYPE_AEXP:
            printAexp(x->val.aexp);
            break;
        case EXP_TYPE_CEXP:
            printCexp(x->val.cexp);
            break;
        case EXP_TYPE_LET:
            printExpLet(x->val.let);
            break;
        case EXP_TYPE_DONE:
            printf("<DONE>");
            break;
        default:
            printf("<unrecognised exp>");
    }
}

void printExpLet(ExpLet *x) {
    printf("(let (");
    printAexpVar(x->var);
    printf(" ");
    printExp(x->val);
    printf(") ");
    printExp(x->body);
    printf(")");
}

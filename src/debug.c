#include <stdio.h>

#include "debug.h"

void printValue(Value *x) {
    printf("Value[");
    switch (x->type) {
        case VALUE_TYPE_VOID:
            printf("VOID");
            break;
        case VALUE_TYPE_INTEGER:
            printf("%d", x->val.z);
            break;
        case VALUE_TYPE_TRUE:
            printf("TRUE");
            break;
        case VALUE_TYPE_FALSE:
            printf("FALSE");
            break;
        case VALUE_TYPE_CLO:
            printClo(x->val.clo);
            break;
        case VALUE_TYPE_CONT:
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
    if (x != NULL) {
        printAexpVar(x->var);
        printf(", ");
        printExp(x->body);
        printf(", ");
        printEnv(x->rho);
        printf(", ");
        printKont(x->next);
    }
    printf("]");
}

void printFail(Fail *x) {
    printf("Fail[");
    if (x != NULL) {
        printExp(x->exp);
        printf(", ");
        printEnv(x->rho);
        printf(", ");
        printKont(x->k);
        printf(", ");
        printFail(x->next);
    }
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
        printExp(x->exp);
        if (x->next) {
            printf(" ");
        }
        x = x->next;
    }
    printf(")");
}

void printCexpApply(CexpApply *x) {
    printf("(");
    printExp(x->function);
    printf(" ");
    printAexpList(x->args);
    printf(")");
}

void printCexpCond(CexpCond *x) {
    printf("(if ");
    printExp(x->condition);
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
        printExp(x->val);
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
        case AEXP_TYPE_LAM:
            printAexpLam(x->val.aexp.lam);
            break;
        case AEXP_TYPE_VAR:
            printAexpVar(x->val.aexp.var);
            break;
        case AEXP_TYPE_TRUE:
            printf("#t");
            break;
        case AEXP_TYPE_FALSE:
            printf("#f");
            break;
        case AEXP_TYPE_INT:
            printf("%d", x->val.aexp.integer);
            break;
        case AEXP_TYPE_PRIM:
            printAexpPrimApp(x->val.aexp.prim);
            break;
        case CEXP_TYPE_APPLY:
            printCexpApply(x->val.cexp.apply);
            break;
        case CEXP_TYPE_COND:
            printCexpCond(x->val.cexp.cond);
            break;
        case CEXP_TYPE_CALLCC:
            printf("(call/cc ");
            printExp(x->val.cexp.callCC);
            printf(")");
            break;
        case CEXP_TYPE_LETREC:
            printCexpLetRec(x->val.cexp.letRec);
            break;
        case CEXP_TYPE_AMB:
            printCexpAmb(x->val.cexp.amb);
            break;
        case CEXP_TYPE_BACK:
            printf("(back)");
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

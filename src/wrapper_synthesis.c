#include "wrapper_synthesis.h"
#include "memory.h"
#include "symbol.h"
#include "ast.h"
#include "parser_info.h"
#include <stdio.h>

AstDefinitions *generatedBuiltins = NULL;
void markGeneratedBuiltins(void) {
    markAstDefinitions(generatedBuiltins);
}

/*
 * Build a formal argument list (AstFargList) and parallel actual argument list
 * (AstExpressions) for n arguments named a$0 .. a$(n-1).
 */
static void makeArgLists(ParserInfo PI, int n, AstFargList **formalOut, AstExpressions **actualOut) {
    AstFargList *formals = NULL;
    AstExpressions *actuals = NULL;
    int saveFormals = PROTECT(NULL);
    int saveActuals = PROTECT(NULL);
    for (int i = n - 1; i >= 0; i--) {
        char buf[32];
        sprintf(buf, "a$%d", i);
        HashSymbol *sym = newSymbol(buf);
        AstFarg *farg = newAstFarg_Symbol(PI, sym);
        PROTECT(farg);
        formals = newAstFargList(PI, farg, formals);
        REPLACE_PROTECT(saveFormals, formals);
        AstExpression *symExpr = newAstExpression_Symbol(PI, sym);
        PROTECT(symExpr);
        actuals = newAstExpressions(PI, symExpr, actuals);
        REPLACE_PROTECT(saveActuals, actuals);
    }
    UNPROTECT(saveActuals);
    UNPROTECT(saveFormals);
    *formalOut = formals;
    *actualOut = actuals;
}

/*
 * Create wrapper definition AST for one builtin.
 */
static AstDefinition *makeWrapper(ParserInfo PI, BuiltIn *builtin) {
    int arity = (int) builtin->args->size;
    AstFargList *formals = NULL;
    AstExpressions *actuals = NULL;
    makeArgLists(PI, arity, &formals, &actuals);
    // reference internal symbol
    int save = PROTECT(formals);
    PROTECT(actuals);
    AstExpression *internalSym = newAstExpression_Symbol(PI, builtin->internalName);
    PROTECT(internalSym);
    AstFunCall *call = newAstFunCall(PI, internalSym, actuals);
    PROTECT(call);
    AstExpression *bodyExpr = newAstExpression_FunCall(PI, call);
    PROTECT(bodyExpr);
    AstExpressions *bodyExprs = newAstExpressions(PI, bodyExpr, NULL);
    PROTECT(bodyExprs);
    AstNest *nest = newAstNest(PI, NULL, bodyExprs);
    PROTECT(nest);
    AstFunction *func = newAstFunction(PI, formals, nest);
    PROTECT(func);
    AstCompositeFunction *comp = newAstCompositeFunction(PI, func, NULL);
    PROTECT(comp);
    AstExpression *funExpr = newAstExpression_Fun(PI, comp);
    PROTECT(funExpr);
    AstDefine *define = newAstDefine(PI, builtin->externalName, funExpr);
    PROTECT(define);
    AstDefinition *def = newAstDefinition_Define(PI, define);
    UNPROTECT(save);
    return def;
}

void generateBuiltinWrappers(BuiltIns *builtIns) {
    if (builtIns == NULL) return;
#ifdef SAFETY_CHECKS
    if (generatedBuiltins != NULL)
        cant_happen("generateBuiltinWrappers called twice");
#endif
    // Use a dummy ParserInfo referencing first preamble definition if available
    ParserInfo PI;
    // default values
    PI.filename = "<builtin-wrapper>";
    PI.lineNo = 0;
    // Prepend wrappers so external names are resolved during type checking of
    // preamble functions/macros that reference them (e.g. assertion, puts, etc.)
    for (Index i = 0; i < builtIns->size; i++) {
        BuiltIn *bi = builtIns->entries[i];
        AstDefinition *wrapper = makeWrapper(PI, bi);
        int save = PROTECT(wrapper);
        generatedBuiltins = newAstDefinitions(CPI(wrapper), wrapper, generatedBuiltins);
        UNPROTECT(save);
    }
}

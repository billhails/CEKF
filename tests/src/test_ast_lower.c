#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#include "ast_helper.h"
#include "ast_lower.h"
#include "ast_ns.h"
#include "ast_prepare.h"
#include "common.h"
#include "init.h"
#include "pratt_parser.h"
#include "test.h"

static AstProg *prepareStandalone(char *string, char *origin) {
    forceInitNameSpaces();
    AstProg *prog = prattParseString(string, origin);
    int save = PROTECT(prog);
    prog = nsAstProg(prog);
    REPLACE_PROTECT(save, prog);
    prog = prepareAst(prog);
    REPLACE_PROTECT(save, prog);
    assert(!hadErrors());
    clearErrors();
    UNPROTECT(save);
    return prog;
}

static AstNest *singleBodyNest(AstProg *prog) {
    assert(prog->body != NULL);
    assert(prog->body->next == NULL);
    assert(prog->body->expression->type == AST_EXPRESSION_TYPE_NEST);
    return getAstExpression_Nest(prog->body->expression);
}

static char *readCapturedOutput(FILE *tmp) {
    assert(fseek(tmp, 0, SEEK_END) == 0);
    long size = ftell(tmp);
    assert(size >= 0);
    rewind(tmp);

    char *buffer = malloc((size_t)size + 1);
    assert(buffer != NULL);
    size_t readCount = fread(buffer, 1, (size_t)size, tmp);
    assert(readCount == (size_t)size);
    buffer[size] = '\0';
    return buffer;
}

static char *captureLowerAstErrors(AstProg *prog) {
    fflush(stderr);
    int savedStderr = dup(fileno(stderr));
    assert(savedStderr != -1);
    FILE *tmp = tmpfile();
    assert(tmp != NULL);
    assert(dup2(fileno(tmp), fileno(stderr)) != -1);

    int save = PROTECT(prog);
    AstProg *lowered = lowerAst(prog);
    PROTECT(lowered);
    fflush(stderr);

    assert(dup2(savedStderr, fileno(stderr)) != -1);
    close(savedStderr);

    char *buffer = readCapturedOutput(tmp);
    fclose(tmp);
    UNPROTECT(save);
    return buffer;
}

static char *captureLambdaConversionErrors(AstProg *prog) {
    fflush(stderr);
    int savedStderr = dup(fileno(stderr));
    assert(savedStderr != -1);
    FILE *tmp = tmpfile();
    assert(tmp != NULL);
    assert(dup2(fileno(tmp), fileno(stderr)) != -1);

    int save = PROTECT(prog);
    LamExp *exp = lamConvertProg(prog);
    PROTECT(exp);
    fflush(stderr);

    assert(dup2(savedStderr, fileno(stderr)) != -1);
    close(savedStderr);

    char *buffer = readCapturedOutput(tmp);
    fclose(tmp);
    UNPROTECT(save);
    return buffer;
}

static AstSyntaxDecl *firstSyntaxDecl(AstProg *prog) {
    AstNest *nest = singleBodyNest(prog);
    assert(nest->definitions != NULL);
    assert(nest->definitions->definition->type ==
           AST_DEFINITION_TYPE_SYNTAXDECL);
    return getAstDefinition_SyntaxDecl(nest->definitions->definition);
}

static AstExprSyntaxUse *firstExprSyntaxUse(AstProg *prog) {
    AstNest *nest = singleBodyNest(prog);
    assert(nest->expressions != NULL);
    assert(nest->expressions->expression->type ==
           AST_EXPRESSION_TYPE_SYNTAXUSE);
    return getAstExpression_SyntaxUse(nest->expressions->expression);
}

static AstDefSyntaxUse *firstDefSyntaxUse(AstProg *prog) {
    AstNest *nest = singleBodyNest(prog);
    assert(nest->definitions != NULL);
    for (AstDefinitions *defs = nest->definitions; defs != NULL;
         defs = defs->next) {
        if (defs->definition->type == AST_DEFINITION_TYPE_SYNTAXUSE) {
            return getAstDefinition_SyntaxUse(defs->definition);
        }
    }
    assert(false);
    return NULL;
}

static void test_expr_syntax_kind_mismatch_reports_error(void) {
    printf("test_expr_syntax_kind_mismatch_reports_error\n");
    AstProg *prog =
        prepareStandalone("let macro sid: Expr pass; syntax pass ::= x: Expr "
                          "quote { unquote(x) }; in sid 7",
                          "test_expr_syntax_kind_mismatch_reports_error");
    int save = PROTECT(prog);
    AstSyntaxDecl *decl = firstSyntaxDecl(prog);
    decl->resultKind = AST_SYNTAXRESULTKIND_TYPE_DEF;
    char expected[160];
    snprintf(expected, sizeof(expected),
             "syntax declaration %d produces %s but was used as Expr",
             decl->declarationId, astSyntaxResultKindName(decl->resultKind));

    char *output = captureLowerAstErrors(prog);
    assert(hadErrors());
    assert(strstr(output, expected) != NULL);

    free(output);
    clearErrors();
    UNPROTECT(save);
}

static void test_expr_syntax_unresolved_declaration_reports_error(void) {
    printf("test_expr_syntax_unresolved_declaration_reports_error\n");
    AstProg *prog = prepareStandalone(
        "let macro sid: Expr pass; syntax pass ::= x: Expr quote { "
        "unquote(x) }; in sid 7",
        "test_expr_syntax_unresolved_declaration_reports_error");
    int save = PROTECT(prog);
    AstExprSyntaxUse *use = firstExprSyntaxUse(prog);
    use->declarationId = 9999;

    char *output = captureLowerAstErrors(prog);
    assert(hadErrors());
    assert(strstr(output, "unresolved syntax declaration 9999 during lowering "
                          "of Expr syntax use") != NULL);

    free(output);
    clearErrors();
    UNPROTECT(save);
}

static void test_def_syntax_kind_mismatch_reports_error(void) {
    printf("test_def_syntax_kind_mismatch_reports_error\n");
    AstProg *prog =
        prepareStandalone("let macro annotate: Def annotateHelper; syntax "
                          "annotateHelper ::= name: "
                          "Name \"=\" value: Expr { "
                          "name = value }; annotate answer = 42; in answer",
                          "test_def_syntax_kind_mismatch_reports_error");
    AstDefSyntaxUse *use = firstDefSyntaxUse(prog);
    assert(use != NULL);
    int save = PROTECT(prog);
    AstSyntaxDecl *decl = firstSyntaxDecl(prog);
    decl->resultKind = AST_SYNTAXRESULTKIND_TYPE_EXPR;
    char expected[160];
    snprintf(expected, sizeof(expected),
             "syntax declaration %d produces %s but was used as Def",
             decl->declarationId, astSyntaxResultKindName(decl->resultKind));

    char *output = captureLowerAstErrors(prog);
    assert(hadErrors());
    assert(strstr(output, expected) != NULL);

    free(output);
    clearErrors();
    UNPROTECT(save);
}

static void test_lambda_conversion_reports_syntax_carrier_leak(void) {
    printf("test_lambda_conversion_reports_syntax_carrier_leak\n");
    AstProg *prog =
        prepareStandalone("let macro sid: Expr pass; syntax pass ::= x: Expr "
                          "quote { unquote(x) }; in sid 7",
                          "test_lambda_conversion_reports_syntax_carrier_leak");
    int save = PROTECT(prog);

    char *output = captureLambdaConversionErrors(prog);
    assert(hadErrors());
    assert(strstr(output, "reached lambda conversion before syntax lowering") !=
           NULL);

    free(output);
    clearErrors();
    UNPROTECT(save);
}

int main(int argc __attribute__((unused)),
         char *argv[] __attribute__((unused))) {
    disableGC();
    initAll();

    test_expr_syntax_kind_mismatch_reports_error();
    test_expr_syntax_unresolved_declaration_reports_error();
    test_def_syntax_kind_mismatch_reports_error();
    test_lambda_conversion_reports_syntax_carrier_leak();

    assert(!hadErrors());
}
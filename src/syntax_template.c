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

#include <stdio.h>
#include <stdlib.h>
#include <wchar.h>
#include <wctype.h>

#include "memory.h"
#include "pratt_parser.h"
#include "pratt_scanner.h"
#include "symbol.h"
#include "symbols.h"
#include "syntax_template.h"
#include "utils_helper.h"

typedef struct TemplateContext {
    PrattParser *parser;
    PrattMacroPatternItems *patternItems;
    SymbolArray *parameters;
    SymbolMap *introducedBinders;
    bool quotedTemplate;
    int nextBinderId;
} TemplateContext;

static bool symbolArrayContains(SymbolArray *symbols, HashSymbol *symbol);
static AstSyntaxClass templateConvertSyntaxClass(PrattSyntaxClass prattClass);

static AstSyntaxTemplateExpr *convertTemplateExpr(AstExpression *expr,
                                                  TemplateContext *context);
static AstSyntaxTemplateExprs *convertTemplateExprs(AstExpressions *expressions,
                                                    TemplateContext *context);
static AstSyntaxTemplateDefinition *
convertTemplateDefinition(AstDefinition *definition, TemplateContext *context);
static AstSyntaxTemplateDefinitions *
convertTemplateDefinitions(AstDefinitions *definitions,
                           TemplateContext *context);
static AstSyntaxTemplateNest *convertTemplateNest(AstNest *nest,
                                                  TemplateContext *context);
static AstSyntaxTemplateFunction *
convertTemplateFunction(AstCompositeFunction *function,
                        TemplateContext *context);
static AstSyntaxTemplateAltFunction *
convertTemplateAltFunction(AstFunction *function, TemplateContext *context);
static AstSyntaxTemplateAltArgs *
convertTemplateAltArgs(AstAltArgs *altArgs, TemplateContext *context);
static AstSyntaxTemplateAltFunction *
convertTemplateLazyAltFunction(AstAltFunction *function,
                               TemplateContext *context);
static AstSyntaxTemplateFarg *convertTemplateFarg(AstFarg *farg,
                                                  TemplateContext *context);
static AstSyntaxTemplateFargList *
convertTemplateFargList(AstFargList *fargs, TemplateContext *context);
static AstSyntaxTemplateTaggedArgs *
convertTemplateTaggedArgs(AstTaggedArgList *args, TemplateContext *context);
static AstSyntaxTemplateTaggedExpressions *
convertTemplateTaggedExpressions(AstTaggedExpressions *expressions,
                                 TemplateContext *context);
static AstSyntaxTemplateBinders *
convertTemplateBinders(AstSymbolList *symbols, TemplateContext *context);

static void initTemplateChildContext(TemplateContext *child,
                                     TemplateContext *parent,
                                     SymbolMap *introducedBinders) {
    child->parser = parent->parser;
    child->patternItems = parent->patternItems;
    child->parameters = parent->parameters;
    child->introducedBinders = introducedBinders;
    child->quotedTemplate = parent->quotedTemplate;
    child->nextBinderId = parent->nextBinderId;
}

static void syncTemplateChildContext(TemplateContext *parent,
                                     TemplateContext *child) {
    parent->nextBinderId = child->nextBinderId;
}

static AstSyntaxClass templateConvertSyntaxClass(PrattSyntaxClass prattClass) {
    switch (prattClass) {
    case PRATTSYNTAXCLASS_TYPE_EXPR:
        return AST_SYNTAXCLASS_TYPE_EXPR;
    case PRATTSYNTAXCLASS_TYPE_NAME:
        return AST_SYNTAXCLASS_TYPE_NAME;
    case PRATTSYNTAXCLASS_TYPE_NEST:
        return AST_SYNTAXCLASS_TYPE_NEST;
    case PRATTSYNTAXCLASS_TYPE_STRING:
        return AST_SYNTAXCLASS_TYPE_STRING;
    case PRATTSYNTAXCLASS_TYPE_TYPE:
        return AST_SYNTAXCLASS_TYPE_TYPE;
    case PRATTSYNTAXCLASS_TYPE_SYNTAX:
        return AST_SYNTAXCLASS_TYPE_SYNTAX;
    default:
        cant_happen("unrecognised PrattSyntaxClass %d", prattClass);
    }
}

static HashSymbol *templateBinderKey(int binderId) {
    char buffer[64];

    snprintf(buffer, sizeof(buffer), "__syntax_binder_%d", binderId);
    return newSymbol(buffer);
}

static int templateBinderId(HashSymbol *key) {
    int binderId = -1;

    if (key == NULL ||
        sscanf(key->name, "__syntax_binder_%d", &binderId) != 1) {
        cant_happen("invalid syntax template binder key");
    }

    return binderId;
}

static bool lookupTemplateBindingClass(HashSymbol *name,
                                       TemplateContext *context,
                                       AstSyntaxClass *syntaxClass) {
    if (symbolArrayContains(context->parameters, name)) {
        *syntaxClass = AST_SYNTAXCLASS_TYPE_EXPR;
        return true;
    }

    if (context->patternItems == NULL) {
        return false;
    }

    for (Index i = 0; i < countPrattMacroPatternItems(context->patternItems);
         ++i) {
        PrattMacroPatternItem *item =
            getPrattMacroPatternItems(context->patternItems, i);
        if (item->type != PRATTMACROPATTERNITEM_TYPE_TYPEDHOLE) {
            continue;
        }

        PrattMacroHole *hole = getPrattMacroPatternItem_TypedHole(item);
        if (hole->name == name) {
            *syntaxClass = templateConvertSyntaxClass(hole->syntaxClass);
            return true;
        }
    }

    return false;
}

static AstSyntaxClass lookupBinderUnquoteClass(HashSymbol *name,
                                               TemplateContext *context) {
    AstSyntaxClass syntaxClass = AST_SYNTAXCLASS_TYPE_NAME;

    if (lookupTemplateBindingClass(name, context, &syntaxClass)) {
        return syntaxClass;
    }

    return AST_SYNTAXCLASS_TYPE_NAME;
}

static bool lookupIntroducedBinderId(HashSymbol *name, TemplateContext *context,
                                     int *binderId) {
    HashSymbol *key = NULL;

    if (!getSymbolMap(context->introducedBinders, name, &key)) {
        return false;
    }

    *binderId = templateBinderId(key);
    return true;
}

static int registerIntroducedBinder(HashSymbol *name,
                                    TemplateContext *context) {
    int binderId = context->nextBinderId++;

    setSymbolMap(context->introducedBinders, name, templateBinderKey(binderId));
    return binderId;
}

static bool isSyntaxUnquoteFunction(AstExpression *function) {
    if (function == NULL) {
        return false;
    }

    switch (function->type) {
    case AST_EXPRESSION_TYPE_SYMBOL:
        return getAstExpression_Symbol(function) == prattSyntaxUnquoteSymbol();
    case AST_EXPRESSION_TYPE_ANNOTATEDSYMBOL: {
        AstAnnotatedSymbol *annotated =
            getAstExpression_AnnotatedSymbol(function);
        if (annotated->symbol == prattSyntaxUnquoteSymbol()) {
            return true;
        }
        return annotated->originalImpl != NULL &&
               annotated->originalImpl->type == AST_EXPRESSION_TYPE_SYMBOL &&
               getAstExpression_Symbol(annotated->originalImpl) ==
                   prattSyntaxUnquoteSymbol();
    }
    default:
        return false;
    }
}

static HashSymbol *templateNameFromExpression(AstExpression *expr) {
    if (expr == NULL) {
        return NULL;
    }

    switch (expr->type) {
    case AST_EXPRESSION_TYPE_SYMBOL:
        return getAstExpression_Symbol(expr);
    case AST_EXPRESSION_TYPE_ANNOTATEDSYMBOL: {
        AstAnnotatedSymbol *annotated = getAstExpression_AnnotatedSymbol(expr);
        if (annotated->originalImpl != NULL &&
            annotated->originalImpl->type == AST_EXPRESSION_TYPE_SYMBOL) {
            return getAstExpression_Symbol(annotated->originalImpl);
        }
        return annotated->symbol;
    }
    default:
        return NULL;
    }
}

static AstExpression *templateUnquoteArgument(AstExpression *expr) {
    if (expr == NULL || expr->type != AST_EXPRESSION_TYPE_FUNCALL) {
        return NULL;
    }

    AstFunCall *call = getAstExpression_FunCall(expr);
    if (!isSyntaxUnquoteFunction(call->function) || call->arguments == NULL ||
        call->arguments->next != NULL) {
        return NULL;
    }

    return call->arguments->expression;
}

static AstSyntaxTemplateNameRef *makeTemplateLiteralRef(ParserInfo PI,
                                                        HashSymbol *name) {
    AstSyntaxLiteralRef *literal = newAstSyntaxLiteralRef(PI, name);
    int save = PROTECT(literal);
    AstSyntaxTemplateNameRef *result =
        newAstSyntaxTemplateNameRef_Literal(PI, literal);
    UNPROTECT(save);
    return result;
}

static AstSyntaxTemplateNameRef *makeTemplateIntroducedRef(ParserInfo PI,
                                                           int binderId) {
    AstSyntaxIntroducedRef *introduced =
        newAstSyntaxIntroducedRef(PI, binderId);
    int save = PROTECT(introduced);
    AstSyntaxTemplateNameRef *result =
        newAstSyntaxTemplateNameRef_Introduced(PI, introduced);
    UNPROTECT(save);
    return result;
}

static AstSyntaxTemplateNameRef *
makeTemplateUnquoteRef(ParserInfo PI, HashSymbol *bindingName,
                       AstSyntaxClass syntaxClass) {
    AstSyntaxUnquote *unquote =
        newAstSyntaxUnquote(PI, bindingName, syntaxClass);
    int save = PROTECT(unquote);
    AstSyntaxTemplateNameRef *result =
        newAstSyntaxTemplateNameRef_Unquote(PI, unquote);
    UNPROTECT(save);
    return result;
}

static AstSyntaxTemplateBinder *
makeTemplateIntroducedBinder(ParserInfo PI, HashSymbol *name,
                             TemplateContext *context) {
    int binderId = registerIntroducedBinder(name, context);
    AstSyntaxIntroducedBinder *introduced =
        newAstSyntaxIntroducedBinder(PI, binderId, name);
    int save = PROTECT(introduced);
    AstSyntaxTemplateBinder *result =
        newAstSyntaxTemplateBinder_Introduced(PI, introduced);
    UNPROTECT(save);
    return result;
}

static AstSyntaxTemplateBinder *
makeTemplateUnquoteBinder(ParserInfo PI, HashSymbol *bindingName,
                          AstSyntaxClass syntaxClass) {
    AstSyntaxUnquote *unquote =
        newAstSyntaxUnquote(PI, bindingName, syntaxClass);
    int save = PROTECT(unquote);
    AstSyntaxTemplateBinder *result =
        newAstSyntaxTemplateBinder_Unquote(PI, unquote);
    UNPROTECT(save);
    return result;
}

static AstSyntaxTemplateExpr *
wrapTemplateNameRef(ParserInfo PI, AstSyntaxTemplateNameRef *nameRef) {
    int save = PROTECT(nameRef);
    AstSyntaxTemplateExpr *result =
        newAstSyntaxTemplateExpr_NameRef(PI, nameRef);
    UNPROTECT(save);
    return result;
}

static AstSyntaxTemplateNameRef *
convertTemplateNameRef(AstExpression *expr, TemplateContext *context) {
    AstExpression *unquoteArg = templateUnquoteArgument(expr);
    if (unquoteArg != NULL) {
        HashSymbol *bindingName = templateNameFromExpression(unquoteArg);
        AstSyntaxClass syntaxClass = AST_SYNTAXCLASS_TYPE_EXPR;

        if (bindingName == NULL) {
            parserErrorAt(CPI(expr), context->parser,
                          "syntax template unquote expects a bound name");
            return makeTemplateLiteralRef(CPI(expr), TOK_ERROR());
        }

        if (!lookupTemplateBindingClass(bindingName, context, &syntaxClass)) {
            parserErrorAt(
                CPI(expr), context->parser,
                "syntax template unquote %s is not a captured binding",
                bindingName->name);
        }

        return makeTemplateUnquoteRef(CPI(expr), bindingName, syntaxClass);
    }

    HashSymbol *name = templateNameFromExpression(expr);
    if (name == NULL) {
        parserErrorAt(CPI(expr), context->parser,
                      "unsupported syntax template name reference");
        return makeTemplateLiteralRef(CPI(expr), TOK_ERROR());
    }

    int binderId = -1;
    if (lookupIntroducedBinderId(name, context, &binderId)) {
        return makeTemplateIntroducedRef(CPI(expr), binderId);
    }

    AstSyntaxClass syntaxClass = AST_SYNTAXCLASS_TYPE_EXPR;
    if (!context->quotedTemplate &&
        lookupTemplateBindingClass(name, context, &syntaxClass)) {
        return makeTemplateUnquoteRef(CPI(expr), name, syntaxClass);
    }

    return makeTemplateLiteralRef(CPI(expr), name);
}

static AstSyntaxTemplateBinder *
convertTemplateBinderFromFarg(AstFarg *farg, TemplateContext *context) {
    if (farg->type == AST_FARG_TYPE_SYMBOL) {
        HashSymbol *name = getAstFarg_Symbol(farg);
        AstSyntaxClass syntaxClass = AST_SYNTAXCLASS_TYPE_NAME;

        if (!context->quotedTemplate &&
            lookupTemplateBindingClass(name, context, &syntaxClass)) {
            return makeTemplateUnquoteBinder(CPI(farg), name, syntaxClass);
        }

        return makeTemplateIntroducedBinder(CPI(farg), name, context);
    }

    if (farg->type == AST_FARG_TYPE_UNPACK) {
        AstUnpack *unpack = getAstFarg_Unpack(farg);
        if (unpack->symbol != NULL &&
            unpack->symbol->type == AST_LOOKUPORSYMBOL_TYPE_SYMBOL &&
            getAstLookUpOrSymbol_Symbol(unpack->symbol) ==
                prattSyntaxUnquoteSymbol() &&
            unpack->argList != NULL && unpack->argList->next == NULL &&
            unpack->argList->arg != NULL &&
            unpack->argList->arg->type == AST_FARG_TYPE_SYMBOL) {
            HashSymbol *bindingName = getAstFarg_Symbol(unpack->argList->arg);
            return makeTemplateUnquoteBinder(
                CPI(farg), bindingName,
                lookupBinderUnquoteClass(bindingName, context));
        }
    }

    parserErrorAt(CPI(farg), context->parser,
                  "unsupported syntax template binder position");
    return makeTemplateIntroducedBinder(CPI(farg), TOK_ERROR(), context);
}

static AstSyntaxTemplateExprs *convertTemplateExprs(AstExpressions *expressions,
                                                    TemplateContext *context) {
    AstSyntaxTemplateExprs *result = newAstSyntaxTemplateExprs();
    int save = PROTECT(result);

    for (; expressions != NULL; expressions = expressions->next) {
        AstSyntaxTemplateExpr *expr =
            convertTemplateExpr(expressions->expression, context);
        PROTECT(expr);
        pushAstSyntaxTemplateExprs(result, expr);
    }

    UNPROTECT(save);
    return result;
}

static AstSyntaxTemplateTaggedExpressions *
convertTemplateTaggedExpressions(AstTaggedExpressions *expressions,
                                 TemplateContext *context) {
    AstSyntaxTemplateTaggedExpressions *result =
        newAstSyntaxTemplateTaggedExpressions();
    int save = PROTECT(result);

    for (; expressions != NULL; expressions = expressions->next) {
        AstSyntaxTemplateExpr *expr =
            convertTemplateExpr(expressions->expression, context);
        PROTECT(expr);
        AstSyntaxTemplateTaggedExpression *tagged =
            newAstSyntaxTemplateTaggedExpression(CPI(expressions),
                                                 expressions->tag, expr);
        PROTECT(tagged);
        pushAstSyntaxTemplateTaggedExpressions(result, tagged);
    }

    UNPROTECT(save);
    return result;
}

static AstSyntaxTemplateBinders *
convertTemplateBinders(AstSymbolList *symbols, TemplateContext *context) {
    AstSyntaxTemplateBinders *result = newAstSyntaxTemplateBinders();
    int save = PROTECT(result);

    for (; symbols != NULL; symbols = symbols->next) {
        AstSyntaxTemplateBinder *binder = makeTemplateIntroducedBinder(
            CPI(symbols), symbols->symbol, context);
        PROTECT(binder);
        pushAstSyntaxTemplateBinders(result, binder);
    }

    UNPROTECT(save);
    return result;
}

static AstSyntaxTemplateTaggedArgs *
convertTemplateTaggedArgs(AstTaggedArgList *args, TemplateContext *context) {
    AstSyntaxTemplateTaggedArgs *result = newAstSyntaxTemplateTaggedArgs();
    int save = PROTECT(result);

    for (; args != NULL; args = args->next) {
        AstSyntaxTemplateFarg *arg = convertTemplateFarg(args->arg, context);
        PROTECT(arg);
        AstSyntaxTemplateTaggedArg *tagged =
            newAstSyntaxTemplateTaggedArg(CPI(args), args->tag, arg);
        PROTECT(tagged);
        pushAstSyntaxTemplateTaggedArgs(result, tagged);
    }

    UNPROTECT(save);
    return result;
}

static AstSyntaxTemplateFargList *
convertTemplateFargList(AstFargList *fargs, TemplateContext *context) {
    AstSyntaxTemplateFargList *result = newAstSyntaxTemplateFargList();
    int save = PROTECT(result);

    for (; fargs != NULL; fargs = fargs->next) {
        AstSyntaxTemplateFarg *arg = convertTemplateFarg(fargs->arg, context);
        PROTECT(arg);
        pushAstSyntaxTemplateFargList(result, arg);
    }

    UNPROTECT(save);
    return result;
}

static AstSyntaxTemplateFarg *convertTemplateFarg(AstFarg *farg,
                                                  TemplateContext *context) {
    if (farg == NULL) {
        return NULL;
    }

    switch (farg->type) {
    case AST_FARG_TYPE_WILDCARD:
        return newAstSyntaxTemplateFarg_WildCard(CPI(farg));
    case AST_FARG_TYPE_SYMBOL: {
        AstSyntaxTemplateBinder *binder =
            convertTemplateBinderFromFarg(farg, context);
        int save = PROTECT(binder);
        AstSyntaxTemplateFarg *result =
            newAstSyntaxTemplateFarg_Binder(CPI(farg), binder);
        UNPROTECT(save);
        return result;
    }
    case AST_FARG_TYPE_LOOKUP:
        return newAstSyntaxTemplateFarg_LookUp(CPI(farg),
                                               getAstFarg_LookUp(farg));
    case AST_FARG_TYPE_NAMED: {
        AstNamedArg *named = getAstFarg_Named(farg);
        AstSyntaxTemplateFarg *arg = convertTemplateFarg(named->arg, context);
        int save = PROTECT(arg);
        AstSyntaxTemplateNamedArg *result =
            newAstSyntaxTemplateNamedArg(CPI(named), named->name, arg);
        PROTECT(result);
        AstSyntaxTemplateFarg *wrapped =
            newAstSyntaxTemplateFarg_Named(CPI(farg), result);
        UNPROTECT(save);
        return wrapped;
    }
    case AST_FARG_TYPE_UNPACK: {
        AstUnpack *unpack = getAstFarg_Unpack(farg);
        if (unpack->symbol != NULL &&
            unpack->symbol->type == AST_LOOKUPORSYMBOL_TYPE_SYMBOL &&
            getAstLookUpOrSymbol_Symbol(unpack->symbol) ==
                prattSyntaxUnquoteSymbol() &&
            unpack->argList != NULL && unpack->argList->next == NULL &&
            unpack->argList->arg != NULL) {
            AstSyntaxTemplateBinder *binder =
                convertTemplateBinderFromFarg(farg, context);
            int save = PROTECT(binder);
            AstSyntaxTemplateFarg *result =
                newAstSyntaxTemplateFarg_Binder(CPI(farg), binder);
            UNPROTECT(save);
            return result;
        }

        AstSyntaxTemplateFargList *argList =
            convertTemplateFargList(unpack->argList, context);
        int save = PROTECT(argList);
        AstSyntaxTemplateUnpack *result =
            newAstSyntaxTemplateUnpack(CPI(unpack), unpack->symbol, argList);
        PROTECT(result);
        AstSyntaxTemplateFarg *wrapped =
            newAstSyntaxTemplateFarg_Unpack(CPI(farg), result);
        UNPROTECT(save);
        return wrapped;
    }
    case AST_FARG_TYPE_UNPACKSTRUCT: {
        AstUnpackStruct *unpack = getAstFarg_UnpackStruct(farg);
        AstSyntaxTemplateTaggedArgs *argList =
            convertTemplateTaggedArgs(unpack->argList, context);
        int save = PROTECT(argList);
        AstSyntaxTemplateUnpackStruct *result =
            newAstSyntaxTemplateUnpackStruct(CPI(unpack), unpack->symbol,
                                             argList);
        PROTECT(result);
        AstSyntaxTemplateFarg *wrapped =
            newAstSyntaxTemplateFarg_UnpackStruct(CPI(farg), result);
        UNPROTECT(save);
        return wrapped;
    }
    case AST_FARG_TYPE_NUMBER:
        return newAstSyntaxTemplateFarg_Number(CPI(farg),
                                               getAstFarg_Number(farg));
    case AST_FARG_TYPE_CHARACTER:
        return newAstSyntaxTemplateFarg_Character(CPI(farg),
                                                  getAstFarg_Character(farg));
    case AST_FARG_TYPE_TUPLE: {
        AstSyntaxTemplateFargList *items =
            convertTemplateFargList(getAstFarg_Tuple(farg), context);
        int save = PROTECT(items);
        AstSyntaxTemplateFarg *result =
            newAstSyntaxTemplateFarg_Tuple(CPI(farg), items);
        UNPROTECT(save);
        return result;
    }
    default:
        cant_happen("unsupported AstFarg type %d in syntax template",
                    farg->type);
    }
}

static AstSyntaxTemplateAltFunction *
convertTemplateAltFunction(AstFunction *function, TemplateContext *context) {
    SymbolMap *introducedBinders = copySymbolMap(context->introducedBinders);
    int save = PROTECT(introducedBinders);
    TemplateContext child;
    initTemplateChildContext(&child, context, introducedBinders);

    AstSyntaxTemplateFargList *argList =
        convertTemplateFargList(function->argList, &child);
    PROTECT(argList);
    AstSyntaxTemplateAltArgs *altArgs =
        newAstSyntaxTemplateAltArgs(CPI(function), argList);
    PROTECT(altArgs);
    AstSyntaxTemplateNest *nest = convertTemplateNest(function->nest, &child);
    PROTECT(nest);
    AstSyntaxTemplateAltFunction *result =
        newAstSyntaxTemplateAltFunction(CPI(function), altArgs, nest);

    syncTemplateChildContext(context, &child);
    UNPROTECT(save);
    return result;
}

static AstSyntaxTemplateAltArgs *
convertTemplateAltArgs(AstAltArgs *altArgs, TemplateContext *context) {
    if (altArgs == NULL) {
        return NULL;
    }

    AstSyntaxTemplateFargList *argList =
        convertTemplateFargList(altArgs->argList, context);
    int save = PROTECT(argList);
    AstSyntaxTemplateAltArgs *next =
        convertTemplateAltArgs(altArgs->next, context);
    PROTECT(next);
    AstSyntaxTemplateAltArgs *result =
        newAstSyntaxTemplateAltArgs(CPI(altArgs), argList);
    result->next = next;
    UNPROTECT(save);
    return result;
}

static AstSyntaxTemplateAltFunction *
convertTemplateLazyAltFunction(AstAltFunction *function,
                               TemplateContext *context) {
    SymbolMap *introducedBinders = copySymbolMap(context->introducedBinders);
    int save = PROTECT(introducedBinders);
    TemplateContext child;
    initTemplateChildContext(&child, context, introducedBinders);

    AstSyntaxTemplateAltArgs *altArgs =
        convertTemplateAltArgs(function->altArgs, &child);
    PROTECT(altArgs);
    AstSyntaxTemplateNest *nest = convertTemplateNest(function->nest, &child);
    PROTECT(nest);
    AstSyntaxTemplateAltFunction *result =
        newAstSyntaxTemplateAltFunction(CPI(function), altArgs, nest);

    syncTemplateChildContext(context, &child);
    UNPROTECT(save);
    return result;
}

static AstSyntaxTemplateFunction *
convertTemplateFunction(AstCompositeFunction *function,
                        TemplateContext *context) {
    if (function == NULL) {
        return NULL;
    }

    AstSyntaxTemplateAltFunction *current =
        convertTemplateAltFunction(function->function, context);
    int save = PROTECT(current);
    AstSyntaxTemplateFunction *next =
        convertTemplateFunction(function->next, context);
    PROTECT(next);
    AstSyntaxTemplateFunction *result =
        newAstSyntaxTemplateFunction(CPI(function), current);
    result->next = next;
    result->unsafe = function->unsafe;
    UNPROTECT(save);
    return result;
}

static AstSyntaxTemplateDefinition *
convertTemplateDefinition(AstDefinition *definition, TemplateContext *context) {
    if (definition == NULL) {
        return NULL;
    }

    switch (definition->type) {
    case AST_DEFINITION_TYPE_DEFINE: {
        AstDefine *define = getAstDefinition_Define(definition);
        AstSyntaxTemplateBinder *binder =
            makeTemplateIntroducedBinder(CPI(define), define->symbol, context);
        int save = PROTECT(binder);
        AstSyntaxTemplateExpr *expression =
            convertTemplateExpr(define->expression, context);
        PROTECT(expression);
        AstSyntaxTemplateDefine *result =
            newAstSyntaxTemplateDefine(CPI(define), binder, expression);
        PROTECT(result);
        AstSyntaxTemplateDefinition *wrapped =
            newAstSyntaxTemplateDefinition_Define(CPI(definition), result);
        UNPROTECT(save);
        return wrapped;
    }
    case AST_DEFINITION_TYPE_MULTI: {
        AstMultiDefine *multi = getAstDefinition_Multi(definition);
        AstSyntaxTemplateBinders *binders =
            convertTemplateBinders(multi->symbols, context);
        int save = PROTECT(binders);
        AstSyntaxTemplateExpr *expression =
            convertTemplateExpr(multi->expression, context);
        PROTECT(expression);
        AstSyntaxTemplateMultiDefine *result =
            newAstSyntaxTemplateMultiDefine(CPI(multi), binders, expression);
        PROTECT(result);
        AstSyntaxTemplateDefinition *wrapped =
            newAstSyntaxTemplateDefinition_Multi(CPI(definition), result);
        UNPROTECT(save);
        return wrapped;
    }
    case AST_DEFINITION_TYPE_LAZY: {
        AstDefLazy *lazy = getAstDefinition_Lazy(definition);
        AstSyntaxTemplateBinder *binder =
            makeTemplateIntroducedBinder(CPI(lazy), lazy->name, context);
        int save = PROTECT(binder);
        AstSyntaxTemplateAltFunction *altFunction =
            convertTemplateLazyAltFunction(lazy->definition, context);
        PROTECT(altFunction);
        AstSyntaxTemplateDefLazy *result =
            newAstSyntaxTemplateDefLazy(CPI(lazy), binder, altFunction);
        PROTECT(result);
        AstSyntaxTemplateDefinition *wrapped =
            newAstSyntaxTemplateDefinition_Lazy(CPI(definition), result);
        UNPROTECT(save);
        return wrapped;
    }
    case AST_DEFINITION_TYPE_ALIAS: {
        AstAlias *alias = getAstDefinition_Alias(definition);
        AstSyntaxTemplateBinder *binder =
            makeTemplateIntroducedBinder(CPI(alias), alias->name, context);
        int save = PROTECT(binder);
        AstSyntaxTemplateAlias *result =
            newAstSyntaxTemplateAlias(CPI(alias), binder, alias->type);
        PROTECT(result);
        AstSyntaxTemplateDefinition *wrapped =
            newAstSyntaxTemplateDefinition_Alias(CPI(definition), result);
        UNPROTECT(save);
        return wrapped;
    }
    case AST_DEFINITION_TYPE_TYPEDEF: {
        AstTypeDef *typeDef = getAstDefinition_TypeDef(definition);
        AstSyntaxTemplateTypeDef *result = newAstSyntaxTemplateTypeDef(
            CPI(typeDef), typeDef->typeSig, typeDef->typeBody);
        int save = PROTECT(result);
        AstSyntaxTemplateDefinition *wrapped =
            newAstSyntaxTemplateDefinition_TypeDef(CPI(definition), result);
        UNPROTECT(save);
        return wrapped;
    }
    default:
        parserErrorAt(CPI(definition), context->parser,
                      "unsupported definition in syntax template");
        return NULL;
    }
}

static AstSyntaxTemplateDefinitions *
convertTemplateDefinitions(AstDefinitions *definitions,
                           TemplateContext *context) {
    if (definitions == NULL) {
        return NULL;
    }

    AstSyntaxTemplateDefinition *definition =
        convertTemplateDefinition(definitions->definition, context);
    int save = PROTECT(definition);
    AstSyntaxTemplateDefinitions *next =
        convertTemplateDefinitions(definitions->next, context);
    PROTECT(next);
    AstSyntaxTemplateDefinitions *result =
        newAstSyntaxTemplateDefinitions(CPI(definitions), definition);
    result->next = next;
    UNPROTECT(save);
    return result;
}

static AstSyntaxTemplateNest *convertTemplateNest(AstNest *nest,
                                                  TemplateContext *context) {
    SymbolMap *introducedBinders = copySymbolMap(context->introducedBinders);
    int save = PROTECT(introducedBinders);
    TemplateContext child;
    initTemplateChildContext(&child, context, introducedBinders);

    AstSyntaxTemplateDefinitions *definitions =
        convertTemplateDefinitions(nest->definitions, &child);
    PROTECT(definitions);
    AstSyntaxTemplateExprs *expressions =
        convertTemplateExprs(nest->expressions, &child);
    PROTECT(expressions);
    AstSyntaxTemplateNest *result =
        newAstSyntaxTemplateNest(CPI(nest), definitions, expressions);

    syncTemplateChildContext(context, &child);
    UNPROTECT(save);
    return result;
}

static AstSyntaxTemplateExpr *convertTemplateExpr(AstExpression *expr,
                                                  TemplateContext *context) {
    if (expr == NULL) {
        return NULL;
    }

    AstExpression *unquoteArg = templateUnquoteArgument(expr);
    if (unquoteArg != NULL) {
        return wrapTemplateNameRef(CPI(expr),
                                   convertTemplateNameRef(expr, context));
    }

    switch (expr->type) {
    case AST_EXPRESSION_TYPE_BACK:
        return newAstSyntaxTemplateExpr_Back(CPI(expr));
    case AST_EXPRESSION_TYPE_WILDCARD:
        return newAstSyntaxTemplateExpr_WildCard(CPI(expr));
    case AST_EXPRESSION_TYPE_SYMBOL:
        return wrapTemplateNameRef(CPI(expr),
                                   convertTemplateNameRef(expr, context));
    case AST_EXPRESSION_TYPE_ANNOTATEDSYMBOL:
        return wrapTemplateNameRef(CPI(expr),
                                   convertTemplateNameRef(expr, context));
    case AST_EXPRESSION_TYPE_NUMBER:
        return newAstSyntaxTemplateExpr_Number(CPI(expr),
                                               getAstExpression_Number(expr));
    case AST_EXPRESSION_TYPE_CHARACTER:
        return newAstSyntaxTemplateExpr_Character(
            CPI(expr), getAstExpression_Character(expr));
    case AST_EXPRESSION_TYPE_FUNCALL: {
        AstFunCall *call = getAstExpression_FunCall(expr);
        AstSyntaxTemplateExpr *function =
            convertTemplateExpr(call->function, context);
        int save = PROTECT(function);
        AstSyntaxTemplateExprs *arguments =
            convertTemplateExprs(call->arguments, context);
        PROTECT(arguments);
        AstSyntaxTemplateFunCall *result =
            newAstSyntaxTemplateFunCall(CPI(call), function, arguments);
        PROTECT(result);
        AstSyntaxTemplateExpr *wrapped =
            newAstSyntaxTemplateExpr_FunCall(CPI(expr), result);
        UNPROTECT(save);
        return wrapped;
    }
    case AST_EXPRESSION_TYPE_LOOKUP: {
        AstLookUp *lookUp = getAstExpression_LookUp(expr);
        AstSyntaxTemplateExpr *inner =
            convertTemplateExpr(lookUp->expression, context);
        int save = PROTECT(inner);
        AstSyntaxTemplateLookUp *result = newAstSyntaxTemplateLookUp(
            CPI(lookUp), lookUp->nsId, lookUp->nsSymbol, inner);
        PROTECT(result);
        AstSyntaxTemplateExpr *wrapped =
            newAstSyntaxTemplateExpr_LookUp(CPI(expr), result);
        UNPROTECT(save);
        return wrapped;
    }
    case AST_EXPRESSION_TYPE_FUN: {
        AstSyntaxTemplateFunction *function =
            convertTemplateFunction(getAstExpression_Fun(expr), context);
        int save = PROTECT(function);
        AstSyntaxTemplateExpr *result =
            newAstSyntaxTemplateExpr_Fun(CPI(expr), function);
        UNPROTECT(save);
        return result;
    }
    case AST_EXPRESSION_TYPE_NEST: {
        AstSyntaxTemplateNest *nest =
            convertTemplateNest(getAstExpression_Nest(expr), context);
        int save = PROTECT(nest);
        AstSyntaxTemplateExpr *result =
            newAstSyntaxTemplateExpr_Nest(CPI(expr), nest);
        UNPROTECT(save);
        return result;
    }
    case AST_EXPRESSION_TYPE_IFF: {
        AstIff *iff = getAstExpression_Iff(expr);
        AstSyntaxTemplateExpr *test = convertTemplateExpr(iff->test, context);
        int save = PROTECT(test);
        AstSyntaxTemplateNest *consequent =
            convertTemplateNest(iff->consequent, context);
        PROTECT(consequent);
        AstSyntaxTemplateNest *alternative =
            convertTemplateNest(iff->alternative, context);
        PROTECT(alternative);
        AstSyntaxTemplateIff *result =
            newAstSyntaxTemplateIff(CPI(iff), test, consequent, alternative);
        PROTECT(result);
        AstSyntaxTemplateExpr *wrapped =
            newAstSyntaxTemplateExpr_Iff(CPI(expr), result);
        UNPROTECT(save);
        return wrapped;
    }
    case AST_EXPRESSION_TYPE_PRINT: {
        AstPrint *print = getAstExpression_Print(expr);
        AstSyntaxTemplateExpr *inner = convertTemplateExpr(print->exp, context);
        int save = PROTECT(inner);
        AstSyntaxTemplatePrint *result =
            newAstSyntaxTemplatePrint(CPI(print), inner);
        PROTECT(result);
        AstSyntaxTemplateExpr *wrapped =
            newAstSyntaxTemplateExpr_Print(CPI(expr), result);
        UNPROTECT(save);
        return wrapped;
    }
    case AST_EXPRESSION_TYPE_TYPEOF: {
        AstTypeOf *typeOf = getAstExpression_TypeOf(expr);
        AstSyntaxTemplateExpr *inner =
            convertTemplateExpr(typeOf->exp, context);
        int save = PROTECT(inner);
        AstSyntaxTemplateTypeOf *result =
            newAstSyntaxTemplateTypeOf(CPI(typeOf), inner);
        PROTECT(result);
        AstSyntaxTemplateExpr *wrapped =
            newAstSyntaxTemplateExpr_TypeOf(CPI(expr), result);
        UNPROTECT(save);
        return wrapped;
    }
    case AST_EXPRESSION_TYPE_TUPLE: {
        AstSyntaxTemplateExprs *items =
            convertTemplateExprs(getAstExpression_Tuple(expr), context);
        int save = PROTECT(items);
        AstSyntaxTemplateExpr *result =
            newAstSyntaxTemplateExpr_Tuple(CPI(expr), items);
        UNPROTECT(save);
        return result;
    }
    case AST_EXPRESSION_TYPE_STRUCTURE: {
        AstStruct *structure = getAstExpression_Structure(expr);
        AstSyntaxTemplateTaggedExpressions *expressions =
            convertTemplateTaggedExpressions(structure->expressions, context);
        int save = PROTECT(expressions);
        AstSyntaxTemplateStruct *result = newAstSyntaxTemplateStruct(
            CPI(structure), structure->symbol, expressions);
        PROTECT(result);
        AstSyntaxTemplateExpr *wrapped =
            newAstSyntaxTemplateExpr_Structure(CPI(expr), result);
        UNPROTECT(save);
        return wrapped;
    }
    case AST_EXPRESSION_TYPE_ASSERTION: {
        AstSyntaxTemplateExpr *inner =
            convertTemplateExpr(getAstExpression_Assertion(expr), context);
        int save = PROTECT(inner);
        AstSyntaxTemplateExpr *result =
            newAstSyntaxTemplateExpr_Assertion(CPI(expr), inner);
        UNPROTECT(save);
        return result;
    }
    case AST_EXPRESSION_TYPE_ERROR: {
        AstSyntaxTemplateExpr *inner =
            convertTemplateExpr(getAstExpression_Error(expr), context);
        int save = PROTECT(inner);
        AstSyntaxTemplateExpr *result =
            newAstSyntaxTemplateExpr_Error(CPI(expr), inner);
        UNPROTECT(save);
        return result;
    }
    default:
        parserErrorAt(CPI(expr), context->parser,
                      "unsupported expression in syntax template: %s",
                      astExpressionTypeName(expr->type));
        return wrapTemplateNameRef(
            CPI(expr), makeTemplateLiteralRef(CPI(expr), TOK_ERROR()));
    }
}

static HashSymbol *unicodeToSymbol(WCharArray *unicode) {
    size_t len = wcstombs(NULL, unicode->entries, 0);
    SCharVec *mbStr = newSCharVec(len + 1);
    int save = PROTECT(mbStr);
    wcstombs(mbStr->entries, unicode->entries, len + 1);
    setSCharVec(mbStr, len, 0);
    HashSymbol *res = newSymbol(mbStr->entries);
    UNPROTECT(save);
    return res;
}

static void checkTerminal(PrattParser *parser, WCharArray *string) {
    if (countWCharArray(string) == 0) {
        parserError(parser, "empty identifier string");
    }
    if (getWCharArray(string, 0) == 0) {
        parserError(parser, "empty identifier string");
    }
    for (Index i = 0;
         i < countWCharArray(string) && getWCharArray(string, i) != 0; ++i) {
        if (iswspace(getWCharArray(string, i))) {
            parserError(parser, "space in quoted token");
        }
    }
}

static bool symbolArrayContains(SymbolArray *symbols, HashSymbol *symbol) {
    if (symbols == NULL) {
        return false;
    }
    for (Index i = 0; i < sizeSymbolArray(symbols); ++i) {
        if (getSymbolArray(symbols, i) == symbol) {
            return true;
        }
    }
    return false;
}

HashSymbol *prattSyntaxQuoteSymbol(void) {
    static HashSymbol *symbol = NULL;
    if (symbol == NULL) {
        symbol = newSymbol("quote");
    }
    return symbol;
}

HashSymbol *prattSyntaxUnquoteSymbol(void) {
    static HashSymbol *symbol = NULL;
    if (symbol == NULL) {
        symbol = newSymbol("unquote");
    }
    return symbol;
}

bool prattIsSyntaxQuoteToken(PrattToken *token) {
    return prattIsTokenTypeOrAtom(token, prattSyntaxQuoteSymbol());
}

bool prattIsSyntaxPatternBoundaryToken(PrattToken *token) {
    return prattIsSyntaxQuoteToken(token) || token->type == TOK_LCURLY() ||
           token->type == TOK_PIPE() || token->type == TOK_SEMI() ||
           token->type == TOK_EOF();
}

AstExpression *prattMakeSyntaxQuotedTemplate(ParserInfo PI,
                                             AstExpression *body) {
    AstExpression *quoteSymbol =
        newAstExpression_Symbol(PI, prattSyntaxQuoteSymbol());
    int save = PROTECT(quoteSymbol);
    AstExpressions *arguments = newAstExpressions(PI, body, NULL);
    PROTECT(arguments);
    AstExpression *result =
        makeAstExpression_FunCall(PI, quoteSymbol, arguments);
    UNPROTECT(save);
    return result;
}

AstExpression *prattUnwrapSyntaxQuotedTemplate(AstExpression *template) {
    if (template == NULL || template->type != AST_EXPRESSION_TYPE_FUNCALL) {
        return NULL;
    }

    AstFunCall *call = getAstExpression_FunCall(template);
    if (call->function->type != AST_EXPRESSION_TYPE_SYMBOL ||
        getAstExpression_Symbol(call->function) != prattSyntaxQuoteSymbol() ||
        call->arguments == NULL || call->arguments->next != NULL) {
        return NULL;
    }

    return call->arguments->expression;
}

HashSymbol *prattSyntaxSymbol(PrattParser *parser) {
    PrattToken *token = next(parser);
    return prattTokenTypeOrAtom(token);
}

SymbolArray *prattParseSyntaxCallArguments(PrattParser *parser) {
    if (!match(parser, TOK_OPEN())) {
        return NULL;
    }

    SymbolArray *arguments = newSymbolArray();
    int save = PROTECT(arguments);

    for (;;) {
        pushSymbolArray(arguments, prattSyntaxSymbol(parser));
        if (!match(parser, TOK_COMMA())) {
            break;
        }
    }

    consume(parser, TOK_CLOSE());
    UNPROTECT(save);
    return arguments;
}

static PrattMacroHole *typedHole(PrattParser *parser) {
    PrattToken *token = next(parser);
    int save = PROTECT(token);
    HashSymbol *bindingName = prattTokenTypeOrAtom(token);
    consume(parser, TOK_COLON());
    token = next(parser);
    REPLACE_PROTECT(save, token);
    PrattMacroHole *result = NULL;
    if (prattIsTokenTypeOrAtom(token, TOK_EXPRTYPE())) {
        result = newPrattMacroHole(PRATTSYNTAXCLASS_TYPE_EXPR, bindingName);
    } else if (prattIsTokenTypeOrAtom(token, TOK_NAMETYPE())) {
        result = newPrattMacroHole(PRATTSYNTAXCLASS_TYPE_NAME, bindingName);
    } else if (prattIsTokenTypeOrAtom(token, TOK_NESTTYPE())) {
        result = newPrattMacroHole(PRATTSYNTAXCLASS_TYPE_NEST, bindingName);
    } else if (prattIsTokenTypeOrAtom(token, TOK_STRINGTYPE()) ||
               prattIsTokenTypeOrAtom(token, newSymbol("String"))) {
        result = newPrattMacroHole(PRATTSYNTAXCLASS_TYPE_STRING, bindingName);
    } else if (prattIsTokenTypeOrAtom(token, TOK_TYPETYPE())) {
        result = newPrattMacroHole(PRATTSYNTAXCLASS_TYPE_TYPE, bindingName);
    } else if (prattIsTokenTypeOrAtom(token, newSymbol("Syntax"))) {
        result = newPrattMacroHole(PRATTSYNTAXCLASS_TYPE_SYNTAX, bindingName);
        int save2 = PROTECT(result);
        consume(parser, TOK_OPEN());
        result->callTarget = prattSyntaxSymbol(parser);
        result->callArguments = prattParseSyntaxCallArguments(parser);
        consume(parser, TOK_CLOSE());
        UNPROTECT(save2);
    } else {
        parserError(parser, "unrecognised syntax class %s",
                    prattTokenTypeOrAtom(token)->name);
    }
    UNPROTECT(save);
    return result;
}

static PrattMacroPatternItems *macroPatternItems(PrattParser *parser) {
    PrattMacroPatternItems *result = newPrattMacroPatternItems();
    int save = PROTECT(result);
    for (;;) {
        if (check(parser, TOK_STRING())) {
            WCharArray *str = prattRawString(parser);
            int save2 = PROTECT(str);
            checkTerminal(parser, str);
            HashSymbol *symbol = unicodeToSymbol(str);
            PrattMacroPatternItem *this =
                newPrattMacroPatternItem_QuotedTerminal(symbol);
            PROTECT(this);
            pushPrattMacroPatternItems(result, this);
            UNPROTECT(save2);
        } else if (!prattIsSyntaxPatternBoundaryToken(peek(parser))) {
            if (prattIsSyntaxQuoteToken(peek(parser))) {
                break;
            }
            PrattMacroHole *hole = typedHole(parser);
            if (hole == NULL)
                break;
            else {
                int save2 = PROTECT(hole);
                PrattMacroPatternItem *this =
                    newPrattMacroPatternItem_TypedHole(hole);
                PROTECT(this);
                pushPrattMacroPatternItems(result, this);
                UNPROTECT(save2);
            }
        } else {
            break;
        }
    }
    UNPROTECT(save);
    return result;
}

SymbolArray *prattParseOptionalSyntaxParameters(PrattParser *parser) {
    if (!match(parser, TOK_OPEN())) {
        return NULL;
    }

    SymbolArray *parameters = newSymbolArray();
    int save = PROTECT(parameters);

    for (;;) {
        HashSymbol *parameter = prattSyntaxSymbol(parser);
        if (symbolArrayContains(parameters, parameter)) {
            parserError(parser, "duplicate syntax parameter %s",
                        parameter->name);
        } else {
            pushSymbolArray(parameters, parameter);
        }
        if (!match(parser, TOK_COMMA())) {
            break;
        }
    }

    consume(parser, TOK_CLOSE());
    UNPROTECT(save);
    return parameters;
}

PrattMacroAlternative *prattParseSyntaxAlternative(PrattParser *parser,
                                                   SymbolArray *parameters) {
    PrattMacroPatternItems *patternItems = macroPatternItems(parser);
    int save = PROTECT(patternItems);
    AstExpression *template = NULL;
    if (prattIsSyntaxQuoteToken(peek(parser))) {
        PrattToken *quoteToken = next(parser);
        int save2 = PROTECT(quoteToken);

        SymbolArray *overrideSymbols = newSymbolArray();
        int save3 = PROTECT(overrideSymbols);
        pushSymbolArray(overrideSymbols, prattSyntaxUnquoteSymbol());
        for (Index i = 0; i < countPrattMacroPatternItems(patternItems); ++i) {
            PrattMacroPatternItem *item =
                getPrattMacroPatternItems(patternItems, i);
            if (item->type == PRATTMACROPATTERNITEM_TYPE_TYPEDHOLE) {
                HashSymbol *name =
                    getPrattMacroPatternItem_TypedHole(item)->name;
                if (!symbolArrayContains(overrideSymbols, name)) {
                    pushSymbolArray(overrideSymbols, name);
                }
            }
        }
        if (parameters != NULL) {
            for (Index i = 0; i < sizeSymbolArray(parameters); ++i) {
                HashSymbol *name = getSymbolArray(parameters, i);
                if (!symbolArrayContains(overrideSymbols, name)) {
                    pushSymbolArray(overrideSymbols, name);
                }
            }
        }

        Index overrideCount = sizeSymbolArray(overrideSymbols);
        PrattRecord **savedRecords = calloc(
            overrideCount == 0 ? 1 : overrideCount, sizeof(PrattRecord *));
        for (Index i = 0; i < overrideCount; ++i) {
            HashSymbol *symbol = getSymbolArray(overrideSymbols, i);
            getPrattRecordTable(parser->rules, symbol, &savedRecords[i]);
            if (savedRecords[i] != NULL) {
                PrattRecord *overrideRecord = copyPrattRecord(savedRecords[i]);
                int save4 = PROTECT(overrideRecord);
                overrideRecord->prefix.op = prattMakeAtomParselet;
                overrideRecord->prefix.prec = 0;
                setPrattRecordTable(parser->rules, symbol, overrideRecord);
                UNPROTECT(save4);
            }
        }

        AstNest *body = prattNest(parser);

        for (Index i = 0; i < overrideCount; ++i) {
            if (savedRecords[i] != NULL) {
                setPrattRecordTable(parser->rules,
                                    getSymbolArray(overrideSymbols, i),
                                    savedRecords[i]);
            }
        }
        free(savedRecords);
        UNPROTECT(save3);

        if (body != NULL) {
            PROTECT(body);
            AstExpression *quotedBody = newAstExpression_Nest(CPI(body), body);
            PROTECT(quotedBody);
            template =
                prattMakeSyntaxQuotedTemplate(TOKPI(quoteToken), quotedBody);
            PROTECT(template);
        }
        UNPROTECT(save2);
    } else {
        AstNest *body = prattNest(parser);
        if (body != NULL) {
            PROTECT(body);
            template = newAstExpression_Nest(CPI(body), body);
            PROTECT(template);
        }
    }
    PrattMacroAlternative *alternative =
        newPrattMacroAlternative(patternItems, template);
    PROTECT(alternative);
    UNPROTECT(save);
    return alternative;
}

AstSyntaxTemplate *
prattConvertSyntaxExprTemplate(PrattParser *parser, ParserInfo PI,
                               AstExpression *template, SymbolArray *parameters,
                               PrattMacroPatternItems *patternItems) {
    AstSyntaxTemplate *result =
        newAstSyntaxTemplate(PI, AST_SYNTAXRESULTKIND_TYPE_EXPR);
    int save = PROTECT(result);
    SymbolMap *introducedBinders = newSymbolMap();
    PROTECT(introducedBinders);

    TemplateContext context = {
        .parser = parser,
        .patternItems = patternItems,
        .parameters = parameters,
        .introducedBinders = introducedBinders,
        .quotedTemplate = false,
        .nextBinderId = 1,
    };

    if (template != NULL) {
        AstExpression *body = prattUnwrapSyntaxQuotedTemplate(template);
        if (body != NULL) {
            context.quotedTemplate = true;
            result->expr = convertTemplateExpr(body, &context);
        } else {
            result->expr = convertTemplateExpr(template, &context);
        }
    }

    UNPROTECT(save);
    return result;
}

%define api.pure full
%lex-param {void *scanner}
%parse-param {void *scanner}{struct PmModule *mod}

%define parse.trace
%define parse.error verbose
%{

#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>

#include "common.h"
#include "ast_helper.h"
#include "symbol.h"
#include "symbols.h"
#include "bigint.h"
#include "module.h"
#include "parser.h"
#include "lexer.h"

void yyerror (yyscan_t *locp, PmModule *mod, char const *msg);

// #define YYDEBUG 1

static AstFunCall *binOpToFunCall(HashSymbol *op, AstExpression *lhs, AstExpression *rhs) {
    return newAstFunCall(
        newAstExpression(
            AST_EXPRESSION_TYPE_SYMBOL,
            AST_EXPRESSION_VAL_SYMBOL(op)
        ),
        newAstExpressions(
            lhs,
            newAstExpressions(rhs, NULL)
        )
    );
}

static AstFunCall *unOpToFunCall(HashSymbol *op, AstExpression *arg) {
    return newAstFunCall(
        newAstExpression(
            AST_EXPRESSION_TYPE_SYMBOL,
            AST_EXPRESSION_VAL_SYMBOL(op)
        ),
        newAstExpressions(arg, NULL)
    );
}

static AstFunCall *newStringList(AstCharArray *str) {
    AstFunCall *res =
        newAstFunCall(newAstExpression(AST_EXPRESSION_TYPE_SYMBOL, AST_EXPRESSION_VAL_SYMBOL(nilSymbol())), NULL);
    for (int size = str->size; size > 0; size--) {
        res = newAstFunCall(
            newAstExpression(AST_EXPRESSION_TYPE_SYMBOL, AST_EXPRESSION_VAL_SYMBOL(consSymbol())),
            newAstExpressions(
                newAstExpression(AST_EXPRESSION_TYPE_CHARACTER, AST_EXPRESSION_VAL_CHARACTER(str->entries[size-1])),
                newAstExpressions(newAstExpression(AST_EXPRESSION_TYPE_FUNCALL, AST_EXPRESSION_VAL_FUNCALL(res)), NULL)
            )
        );
    }
    return res;
}

static AstArg *newAstNilArg() {
    return newAstArg(AST_ARG_TYPE_SYMBOL, AST_ARG_VAL_SYMBOL(nilSymbol()));
}

static AstUnpack *newStringUnpack(AstCharArray *str) {
    AstUnpack *res = newAstUnpack(nilSymbol(), NULL);
    for (int size = str->size; size > 0; size--) {
        res = newAstUnpack(
            consSymbol(),
            newAstArgList(
                newAstArg(AST_ARG_TYPE_CHARACTER, AST_ARG_VAL_CHARACTER(str->entries[size-1])),
                newAstArgList(newAstArg(AST_ARG_TYPE_UNPACK, AST_ARG_VAL_UNPACK(res)), NULL)
            )
        );
    }
    return res;
}

static void bigint_mul_by_ten(bigint *b) {
    bigint old;
    bigint_init(&old);
    bigint_cpy(&old, b);
    bigint_free(b);
    bigint_init(b);
    bigint ten;
    bigint_init(&ten);
    bigint_from_int(&ten, 10);
    bigint_mul(b, &ten, &old);
    bigint_free(&ten);
    bigint_free(&old);
}

static void bigint_add_n(bigint *b, int n) {
    bigint old;
    bigint_init(&old);
    bigint_cpy(&old, b);
    bigint_free(b);
    bigint_init(b);
    bigint_add_word(b, &old, n);
    bigint_free(&old);
}

static MaybeBigInt *makeIrrational(char *str) {
    double f = atof(str);
    return irrationalBigInt(f);
}

static MaybeBigInt *makeMaybeBigInt(char *digits) {
    bool overflowed = false;
    int a = 0;
    bigint bi;
    for (char *p = digits; *p != '\0'; ++p) {
        int n = *p - '0';
        if(overflowed) {
            bigint_mul_by_ten(&bi);
            bigint_add_n(&bi, n);
        } else {
            int c;
            if (__builtin_mul_overflow(a, 10, &c)) {
                overflowed = true;
                bigint_init(&bi);
                bigint_from_int(&bi, a);
                bigint_mul_by_ten(&bi);
                bigint_add_n(&bi, n);
            } else {
                a = c;
                if (__builtin_add_overflow(a, n, &c)) {
                    overflowed = true;
                    bigint_init(&bi);
                    bigint_from_int(&bi, a);
                    bigint_add_n(&bi, n);
                } else {
                    a = c;
                }
            }
        }
    }
    if (overflowed) {
        MaybeBigInt *bbi = newMaybeBigInt(bi);
        return bbi;
    } else {
        return fakeBigInt(a);
    }
}

static AstCharArray *appendCharArray(AstCharArray *res, char *str) {
    while (*str) {
        pushAstCharArray(res, *str);
        str++;
    }
    return res;
}

static AstCharArray *newCharArray(char *str) {
    AstCharArray *res = newAstCharArray();
    return appendCharArray(res, str);
}

static AstCompositeFunction *makeAstCompositeFunction(AstAltFunction *functions, AstCompositeFunction *rest) {
    for (AstAltArgs *args = functions->altArgs; args != NULL; args = args->next) {
        AstFunction *this = newAstFunction(args->argList, copyAstNest(functions->nest));
        rest = newAstCompositeFunction(this, rest);
    }
    return rest;
}

%}
%code requires
{
#include "module.h"
#include "ast.h"
}

%union {
    char *s;
    char c;
    MaybeBigInt *bi;
    AstArg *arg;
    AstArgList *argList;
    AstCompositeFunction *compositeFunction;
    AstDefine *define;
    AstDefinition *definition;
    AstDefinitions *definitions;
    AstEnv *env;
    AstEnvType *envType;
    AstExpression *expression;
    AstExpressions *expressions;
    AstUserType *userType;
    AstFunCall *funCall;
    AstLoad *load;
    AstNamedArg *namedArg;
    AstNest *nest;
    AstPackage *package;
    AstPrint *print;
    HashSymbol *symbol;
    AstTypeBody *typeBody;
    AstTypeClause *typeClause;
    AstTypeConstructor *typeConstructor;
    AstTypeFunction *typeFunction;
    AstTypeDef *typeDef;
    AstTypeList *typeList;
    AstTypeSymbols *typeSymbols;
    AstType *type;
    AstUnpack *unpack;
    AstIff *iff;
    AstCharArray *chars;
    AstAltFunction *altFunction;
    AstAltArgs * altArgs;
}

%type <chars> str
%type <bi> number
%type <arg> farg
%type <argList> fargs arg_tuple
%type <compositeFunction> composite_function functions fun
%type <define> defun denv
%type <definition> definition
%type <definitions> let_in definitions env_body
%type <env> env env_expr
%type <envType> env_type
%type <expression> expression
%type <expressions> expressions expression_statements tuple
%type <userType> user_type
%type <funCall> fun_call binop conslist unop switch string
%type <load> load
%type <namedArg> named_farg
%type <nest> top nest nest_body iff_nest
%type <package> package extends
%type <print> print
%type <symbol> symbol type_symbol as
%type <typeBody> type_body
%type <typeClause> type_clause
%type <typeConstructor> type_constructor
%type <typeFunction> type_function
%type <typeDef> typedef
%type <typeList> type_list
%type <typeSymbols> type_symbols
%type <type> type
%type <unpack> unpack cons consfargs stringarg
%type <iff> iff
%type <altFunction> alt_function
%type <altArgs> alt_args

%token AS
%token BACK
%token ELSE
%token ENV
%token EXTENDS
%token FALSE
%token FN
%token IF
%token IN
%token KW_CHAR
%token KW_INT
%token LET
%token LOAD
%token PRINT
%token PROTOTYPE
%token SWITCH
%token TRUE
%token TYPEDEF
%token WILDCARD

%token <c> CHAR
%token <s> NUMBER
%token <s> STRING
%token <s> TYPE_VAR
%token <s> VAR
%token <s> IRRATIONAL

%right ARROW
%right THEN
%left AND OR XOR
%nonassoc NOT
%nonassoc EQ NE GT LT GE LE
%nonassoc CMP
%nonassoc '='
%nonassoc ':'
%right CONS APPEND
%left CAR CDR
%left '+' '-'
%left '*' '/' '%'
%right POW
%nonassoc NEG
%nonassoc HERE
%left '('
%right '.'

%start top

%%

top : %empty     { $$ = NULL; }
    | nest_body  { mod->nest = $$; }
    ;

nest_body : let_in expression_statements { $$ = newAstNest($1, $2); }
          | expression_statements        { $$ = newAstNest(NULL, $1); }
          ;

/******************************** definitions */

let_in : LET definitions IN { $$ = $2; }
       ;

definitions : %empty                     { $$ = NULL; }
            | definition definitions     { $$ = newAstDefinitions($1, $2); }
            ;

definition : symbol '=' expression ';' { $$ = newAstDefinition( AST_DEFINITION_TYPE_DEFINE, AST_DEFINITION_VAL_DEFINE(newAstDefine($1, $3))); }
           | load       { $$ = newAstDefinition( AST_DEFINITION_TYPE_LOAD, AST_DEFINITION_VAL_LOAD($1)); }
           | typedef    { $$ = newAstDefinition( AST_DEFINITION_TYPE_TYPEDEF, AST_DEFINITION_VAL_TYPEDEF($1)); }
           | defun      { $$ = newAstDefinition( AST_DEFINITION_TYPE_DEFINE, AST_DEFINITION_VAL_DEFINE($1)); }
           | denv       { $$ = newAstDefinition( AST_DEFINITION_TYPE_DEFINE, AST_DEFINITION_VAL_DEFINE($1)); }
           ;

defun : FN symbol fun { $$ = newAstDefine($2, newAstExpression(AST_EXPRESSION_TYPE_FUN, AST_EXPRESSION_VAL_FUN($3))); }
      ;

denv : ENV symbol env_expr  { $$ = newAstDefine($2, newAstExpression(AST_EXPRESSION_TYPE_ENV, AST_EXPRESSION_VAL_ENV($3))); }
     ;

load : LOAD package as  { $$ = newAstLoad($2, $3); }
     ;

as : %empty     { $$ = NULL; }
   | AS symbol  { $$ = $2; }
   ;

/******************************** types */

typedef : TYPEDEF user_type '{' type_body '}'   { $$ = newAstTypeDef($2, $4); }
        ;

user_type : symbol                      { $$ = newAstUserType($1, NULL); }
          | symbol '(' type_symbols ')' { $$ = newAstUserType($1, $3); }
          ;

type_symbols : type_symbol                  { $$ = newAstTypeSymbols($1, NULL); }
             | type_symbol ',' type_symbols { $$ = newAstTypeSymbols($1, $3); }
             ;

type_symbol : TYPE_VAR  { $$ = newSymbol($1); }
            ;

type_body : type_constructor                { $$ = newAstTypeBody($1, NULL); }
          | type_constructor '|' type_body  { $$ = newAstTypeBody($1, $3); }
          ;

type_constructor : symbol                   { $$ = newAstTypeConstructor($1, NULL); }
                 | symbol '(' type_list ')' { $$ = newAstTypeConstructor($1, $3); }
                 ;

type_function : symbol                   { $$ = newAstTypeFunction($1, NULL); }
              | symbol '(' type_list ')' { $$ = newAstTypeFunction($1, $3); }
              ;

type_list : type                { $$ = newAstTypeList($1, NULL); }
          | type ',' type_list  { $$ = newAstTypeList($1, $3); }
          ;

type : type_clause              { $$ = newAstType($1, NULL); }
     | type_clause ARROW type   { $$ = newAstType($1, $3); }
     | '(' type ')'             { $$ = $2; }
     ;

type_clause : KW_INT                { $$ = newAstTypeClause(AST_TYPECLAUSE_TYPE_INTEGER, AST_TYPECLAUSE_VAL_INTEGER()); }
            | KW_CHAR               { $$ = newAstTypeClause(AST_TYPECLAUSE_TYPE_CHARACTER, AST_TYPECLAUSE_VAL_CHARACTER()); }
            | type_symbol           { $$ = newAstTypeClause(AST_TYPECLAUSE_TYPE_VAR, AST_TYPECLAUSE_VAL_VAR($1)); }
            | type_function         { $$ = newAstTypeClause(AST_TYPECLAUSE_TYPE_TYPEFUNCTION, AST_TYPECLAUSE_VAL_TYPEFUNCTION($1)); }
            ;

/******************************** expressions */

iff : IF '(' expression ')' nest ELSE iff_nest  { $$ = newAstIff($3, $5, $7); }
    ;

iff_nest : iff  { $$ = newAstNest(NULL, newAstExpressions(newAstExpression(AST_EXPRESSION_TYPE_IFF, AST_EXPRESSION_VAL_IFF($1)), NULL)); }
         | nest { $$ = $1; }
         ;

switch : SWITCH '(' expressions ')' composite_function  { $$ = newAstFunCall(newAstExpression(AST_EXPRESSION_TYPE_FUN, AST_EXPRESSION_VAL_FUN($5)), $3); }
       ;

fun : alt_function          { $$ = makeAstCompositeFunction($1, NULL); }
    | composite_function    { $$ = $1; }
    ;

nest : '{' nest_body '}'    { $$ = $2; }
     ;

composite_function : '{' functions '}'  { $$ = $2; }
                   ;

functions : alt_function            { $$ = makeAstCompositeFunction($1, NULL); }
          | alt_function functions  { $$ = makeAstCompositeFunction($1, $2); }
          ;

alt_args : '(' fargs ')'               { $$ = newAstAltArgs($2, NULL); }
         | '(' fargs ')' '|' alt_args  { $$ = newAstAltArgs($2, $5); }
         ;

alt_function : alt_args nest    { $$ = newAstAltFunction($1, $2); }
             ;

fargs : %empty            { $$ = NULL; }
      | farg              { $$ = newAstArgList($1, NULL); }
      | farg ',' fargs    { $$ = newAstArgList($1, $3); }
      ;

consfargs : farg                { $$ = newAstUnpack(consSymbol(), newAstArgList($1, newAstArgList(newAstNilArg(), NULL))); }
          | farg ',' consfargs  { $$ = newAstUnpack(consSymbol(), newAstArgList($1, newAstArgList(newAstArg(AST_ARG_TYPE_UNPACK, AST_ARG_VAL_UNPACK($3)), NULL))); }
          ;

number : NUMBER      { $$ = makeMaybeBigInt($1); }
       | IRRATIONAL  { $$ = makeIrrational($1); }
       ;

farg : symbol              { $$ = newAstArg(AST_ARG_TYPE_SYMBOL, AST_ARG_VAL_SYMBOL($1)); }
     | unpack              { $$ = newAstArg(AST_ARG_TYPE_UNPACK, AST_ARG_VAL_UNPACK($1)); }
     | cons                { $$ = newAstArg(AST_ARG_TYPE_UNPACK, AST_ARG_VAL_UNPACK($1)); }
     | named_farg          { $$ = newAstArg(AST_ARG_TYPE_NAMED, AST_ARG_VAL_NAMED($1)); }
     | '[' ']'             { $$ = newAstNilArg(); }
     | '[' consfargs ']'   { $$ = newAstArg(AST_ARG_TYPE_UNPACK, AST_ARG_VAL_UNPACK($2)); }
     | env_type            { $$ = newAstArg(AST_ARG_TYPE_ENV, AST_ARG_VAL_ENV($1)); }
     | number              { $$ = newAstArg(AST_ARG_TYPE_NUMBER, AST_ARG_VAL_NUMBER($1)); }
     | stringarg           { $$ = newAstArg(AST_ARG_TYPE_UNPACK, AST_ARG_VAL_UNPACK($1)); }
     | CHAR                { $$ = newAstArg(AST_ARG_TYPE_CHARACTER, AST_ARG_VAL_CHARACTER($1)); }
     | WILDCARD            { $$ = newAstArg(AST_ARG_TYPE_WILDCARD, AST_ARG_VAL_WILDCARD()); }
     | arg_tuple           { $$ = newAstArg(AST_ARG_TYPE_TUPLE, AST_ARG_VAL_TUPLE($1)); }
     ;

arg_tuple: '#' '(' fargs ')'  { $$ = $3; }
         ;

cons : farg CONS farg { $$ = newAstUnpack(consSymbol(), newAstArgList($1, newAstArgList($3, NULL))); }
     ;

unpack : symbol '(' fargs ')'   { $$ = newAstUnpack($1, $3); }
       ;

stringarg : str { $$ = newStringUnpack($1); }
       ;

string : str { $$ = newStringList($1); }
       ;

str : STRING            { $$ = newCharArray($1); }
    | str STRING        { $$ = appendCharArray($1, $2); }
    ;

env_type : symbol ':' symbol { $$ = newAstEnvType($1, $3); }
         ;

named_farg : symbol '=' farg  { $$ = newAstNamedArg($1, $3); }
           ;

expression : binop                { $$ = newAstExpression(AST_EXPRESSION_TYPE_FUNCALL, AST_EXPRESSION_VAL_FUNCALL($1)); }
           | fun_call             { $$ = newAstExpression(AST_EXPRESSION_TYPE_FUNCALL, AST_EXPRESSION_VAL_FUNCALL($1)); }
           | unop                 { $$ = newAstExpression(AST_EXPRESSION_TYPE_FUNCALL, AST_EXPRESSION_VAL_FUNCALL($1)); }
           | '[' conslist ']'     { $$ = newAstExpression(AST_EXPRESSION_TYPE_FUNCALL, AST_EXPRESSION_VAL_FUNCALL($2)); }
           | FN fun               { $$ = newAstExpression(AST_EXPRESSION_TYPE_FUN, AST_EXPRESSION_VAL_FUN($2)); }
           | env                  { $$ = newAstExpression(AST_EXPRESSION_TYPE_ENV, AST_EXPRESSION_VAL_ENV($1)); }
           | BACK                 { $$ = newAstExpression(AST_EXPRESSION_TYPE_BACK, AST_EXPRESSION_VAL_BACK()); }
           | iff                  { $$ = newAstExpression(AST_EXPRESSION_TYPE_IFF, AST_EXPRESSION_VAL_IFF($1)); }
           | switch               { $$ = newAstExpression(AST_EXPRESSION_TYPE_FUNCALL, AST_EXPRESSION_VAL_FUNCALL($1)); }
           | symbol               { $$ = newAstExpression(AST_EXPRESSION_TYPE_SYMBOL, AST_EXPRESSION_VAL_SYMBOL($1)); }
           | number               { $$ = newAstExpression(AST_EXPRESSION_TYPE_NUMBER, AST_EXPRESSION_VAL_NUMBER($1)); }
           | string               { $$ = newAstExpression(AST_EXPRESSION_TYPE_FUNCALL, AST_EXPRESSION_VAL_FUNCALL($1)); }
           | CHAR                 { $$ = newAstExpression(AST_EXPRESSION_TYPE_CHARACTER, AST_EXPRESSION_VAL_CHARACTER($1)); }
           | nest                 { $$ = newAstExpression(AST_EXPRESSION_TYPE_NEST, AST_EXPRESSION_VAL_NEST($1)); }
           | print                { $$ = newAstExpression(AST_EXPRESSION_TYPE_PRINT, AST_EXPRESSION_VAL_PRINT($1)); }
           | tuple                { $$ = newAstExpression(AST_EXPRESSION_TYPE_TUPLE, AST_EXPRESSION_VAL_TUPLE($1)); }
           | '(' expression ')'   { $$ = $2; }
           ;

unop : '-' expression %prec NEG   { $$ = unOpToFunCall(negSymbol(), $2); }
     | NOT expression %prec NOT   { $$ = unOpToFunCall(notSymbol(), $2); }
     | LT expression %prec CAR    { $$ = unOpToFunCall(carSymbol(), $2); }
     | GT expression %prec CDR    { $$ = unOpToFunCall(cdrSymbol(), $2); }
     | HERE expression            { $$ = unOpToFunCall(hereSymbol(), $2); }
     ;

fun_call :  expression '(' expressions ')' { $$ = newAstFunCall($1, $3); }
         ;

tuple : '#' '(' expressions ')'   { $$ = $3; }
      ;

binop : expression THEN expression      { $$ = binOpToFunCall(thenSymbol(), $1, $3); }
      | expression AND expression       { $$ = binOpToFunCall(andSymbol(), $1, $3); }
      | expression OR expression        { $$ = binOpToFunCall(orSymbol(), $1, $3); }
      | expression XOR expression       { $$ = binOpToFunCall(xorSymbol(), $1, $3); }
      | expression EQ expression        { $$ = binOpToFunCall(eqSymbol(), $1, $3); }
      | expression NE expression        { $$ = binOpToFunCall(neSymbol(), $1, $3); }
      | expression GT expression        { $$ = binOpToFunCall(gtSymbol(), $1, $3); }
      | expression LT expression        { $$ = binOpToFunCall(ltSymbol(), $1, $3); }
      | expression GE expression        { $$ = binOpToFunCall(geSymbol(), $1, $3); }
      | expression LE expression        { $$ = binOpToFunCall(leSymbol(), $1, $3); }
      | expression CMP expression       { $$ = binOpToFunCall(cmpSymbol(), $1, $3); }
      | expression CONS expression      { $$ = binOpToFunCall(consSymbol(), $1, $3); }
      | expression APPEND expression    { $$ = binOpToFunCall(appendSymbol(), $1, $3); }
      | expression '+' expression       { $$ = binOpToFunCall(addSymbol(), $1, $3); }
      | expression '-' expression       { $$ = binOpToFunCall(subSymbol(), $1, $3); }
      | expression '*' expression       { $$ = binOpToFunCall(mulSymbol(), $1, $3); }
      | expression '/' expression       { $$ = binOpToFunCall(divSymbol(), $1, $3); }
      | expression '%' expression       { $$ = binOpToFunCall(modSymbol(), $1, $3); }
      | expression POW expression       { $$ = binOpToFunCall(powSymbol(), $1, $3); }
      | expression '.' expression       { $$ = binOpToFunCall(dotSymbol(), $1, $3); }
      ;

package : symbol                { $$ = newAstPackage($1, NULL); }
        | symbol '.' package    { $$ = newAstPackage($1, $3); }
        ;

expressions : %empty                        { $$ = NULL; }
            | expression                    { $$ = newAstExpressions($1, NULL); }
            | expression ',' expressions    { $$ = newAstExpressions($1, $3); }
            ;

conslist : %empty                     { $$ = newAstFunCall(newAstExpression(AST_EXPRESSION_TYPE_SYMBOL, AST_EXPRESSION_VAL_SYMBOL(nilSymbol())), NULL); }
         | expression                 { $$ = binOpToFunCall(consSymbol(), $1, newAstExpression(AST_EXPRESSION_TYPE_SYMBOL, AST_EXPRESSION_VAL_SYMBOL(nilSymbol()))); }
         | expression ',' conslist    { $$ = binOpToFunCall(consSymbol(), $1, newAstExpression(AST_EXPRESSION_TYPE_FUNCALL, AST_EXPRESSION_VAL_FUNCALL($3))); }
         ;

expression_statements : expression optional_semicolon           { $$ = newAstExpressions($1, NULL); }
                      | expression ';' expression_statements    { $$ = newAstExpressions($1, $3); }
                      ;

optional_semicolon : %empty
                   | ';'
                   ;

env : ENV env_expr  { $$ = $2; }
    ;

env_expr : extends env_body { $$ = newAstEnv($1, $2); }
         ;

extends : %empty            { $$ = NULL; }
        | EXTENDS package   { $$ = $2; }
        ;

env_body : '{' definitions '}'  { $$ = $2; }
         ;

symbol : VAR    { $$ = newSymbol($1); }
       ;

print : PRINT '(' expression ')' { $$ = newAstPrint($3); }
      ;

%%
void yyerror (yyscan_t *locp, PmModule *mod, char const *msg) {
    fprintf(errout, "%s\n", msg);
    if (mod && mod->bufStack) {
        showModuleState(errout, mod);
    }
    abort();
}

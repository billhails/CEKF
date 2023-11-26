%{

#include <stdbool.h>
#include <stdio.h>
#include <ctype.h>

#include "common.h"
#include "ast_helper.h"
#include "symbol.h"
#include "symbols.h"

// #define YYDEBUG 1

int yylex();
extern char *yytext;
int lineNum = 1;
void yyerror(char *ps, ...) {
    printf("%s at token '%s' on line %d\n", ps, yytext, lineNum);
}

AstNest *result = NULL;

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

/*
 * if (test) { consequent } else { alternative }
 *
 * becomes
 *
 * fn { (true) { consequent } (false) { alternative } }(test)
 */
static AstFunCall *fakeAstConditional(AstExpression *condition, AstNest *consequent, AstNest *alternative) {
    AstFunction *trueFunction = newAstFunction(
        newAstArgList(
            newAstArg(AST_ARG_TYPE_SYMBOL, AST_ARG_VAL_SYMBOL(trueSymbol())),
            NULL
        ),
        consequent
    );
    AstFunction *falseFunction = newAstFunction(
        newAstArgList(
            newAstArg(AST_ARG_TYPE_SYMBOL, AST_ARG_VAL_SYMBOL(falseSymbol())),
            NULL
        ),
        alternative
    );
    return newAstFunCall(
        newAstExpression(
            AST_EXPRESSION_TYPE_FUN,
            AST_EXPRESSION_VAL_FUN(
                newAstCompositeFunction(
                    trueFunction,
                    newAstCompositeFunction(
                        falseFunction,
                        NULL
                    )))),
        newAstExpressions(condition, NULL));
}

static AstFunCall *newStringList(char *str) {
    if (*str == '\0') {
        return newAstFunCall(newAstExpression(AST_EXPRESSION_TYPE_SYMBOL, AST_EXPRESSION_VAL_SYMBOL(nilSymbol())), NULL);
    }
    AstFunCall *rest = newStringList(str + 1);
    return newAstFunCall(
        newAstExpression(AST_EXPRESSION_TYPE_SYMBOL, AST_EXPRESSION_VAL_SYMBOL(consSymbol())),
        newAstExpressions(
            newAstExpression(AST_EXPRESSION_TYPE_CHARACTER, AST_EXPRESSION_VAL_CHARACTER(*str)),
            newAstExpressions(newAstExpression(AST_EXPRESSION_TYPE_FUNCALL, AST_EXPRESSION_VAL_FUNCALL(rest)), NULL)
        )
    );
}

static AstUnpack *newStringUnpack(char *str) {
    if (*str == '\0') {
        return newAstUnpack(nilSymbol(), NULL);
    }
    AstUnpack *rest = newStringUnpack(str + 1);
    return newAstUnpack(
        consSymbol(),
        newAstArgList(
            newAstArg(AST_ARG_TYPE_CHARACTER, AST_ARG_VAL_CHARACTER(*str)),
            newAstArgList(newAstArg(AST_ARG_TYPE_UNPACK, AST_ARG_VAL_UNPACK(rest)), NULL)
        )
    );
}

%}
%union {
    char *s;
    int i;
    char c;
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
    AstFlatType *flatType;
    AstFunCall *funCall;
    AstFunction *function;
    AstLoad *load;
    AstNamedArg *namedArg;
    AstNest *nest;
    AstPackage *package;
    AstPrototypeBody *prototypeBody;
    AstPrototype *prototype;
    AstPrototypeSymbolType *prototypeSymbolType;
    AstSinglePrototype *singlePrototype;
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

}

%type <arg> farg
%type <argList> fargs
%type <compositeFunction> composite_function functions fun
%type <define> defun denv
%type <definition> definition
%type <definitions> let_in definitions env_body
%type <env> env env_expr
%type <envType> env_type
%type <expression> expression
%type <expressions> expressions expression_statements
%type <flatType> flat_type
%type <funCall> fun_call binop conslist unop switch conditional string
%type <function> function
%type <load> load
%type <namedArg> named_farg
%type <nest> top nest nest_body conditional_nest
%type <package> package extends
%type <prototypeBody> prototype_body
%type <prototypeSymbolType> prototype_symbol_type
%type <prototype> prototype
%type <singlePrototype> single_prototype
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
%token PROTOTYPE
%token SWITCH
%token TRUE
%token TYPEDEF
%token WILDCARD

%token <c> CHAR
%token <i> NUMBER
%token <s> STRING
%token <s> TYPE_VAR
%token <s> VAR

%right ARROW
%right THEN
%left AND OR XOR
%nonassoc NOT
%nonassoc EQ NE GT LT GE LE
%nonassoc '='
%nonassoc ':'
%right CONS APPEND
%left '+' '-'
%left '*' '/' '%'
%right '^'
%nonassoc NEG
%nonassoc HERE
%left '('
%right '.'

%start top

%%

top : %empty     { $$ = NULL; }
    | nest_body  { result = $$; }
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
           | prototype  { $$ = newAstDefinition( AST_DEFINITION_TYPE_PROTOTYPE, AST_DEFINITION_VAL_PROTOTYPE($1)); }
           | load       { $$ = newAstDefinition( AST_DEFINITION_TYPE_LOAD, AST_DEFINITION_VAL_LOAD($1)); }
           | typedef    { $$ = newAstDefinition( AST_DEFINITION_TYPE_TYPEDEF, AST_DEFINITION_VAL_TYPEDEF($1)); }
           | defun      { $$ = newAstDefinition( AST_DEFINITION_TYPE_DEFINE, AST_DEFINITION_VAL_DEFINE($1)); }
           | denv       { $$ = newAstDefinition( AST_DEFINITION_TYPE_DEFINE, AST_DEFINITION_VAL_DEFINE($1)); }
           ;

prototype : PROTOTYPE symbol '{' prototype_body '}' { $$ = newAstPrototype($2, $4); }
          ;

defun : FN symbol fun { $$ = newAstDefine($2, newAstExpression(AST_EXPRESSION_TYPE_FUN, AST_EXPRESSION_VAL_FUN($3))); }
      ;

prototype_body : %empty                          { $$ = NULL; }
               | single_prototype prototype_body {
                                                    if ($2 == NULL) $$ = newAstPrototypeBody($1, NULL);
                                                    else $$ = newAstPrototypeBody($1, $2);
                                                 }
               ;

single_prototype : prototype_symbol_type ';'  {
                                            $$ = newAstSinglePrototype(
                                                AST_SINGLEPROTOTYPE_TYPE_SYMBOLTYPE,
                                                AST_SINGLEPROTOTYPE_VAL_SYMBOLTYPE($1)
                                            );
                                        }
                 | prototype            {
                                            $$ = newAstSinglePrototype(
                                                AST_SINGLEPROTOTYPE_TYPE_PROTOTYPE,
                                                AST_SINGLEPROTOTYPE_VAL_PROTOTYPE($1)
                                            );
                                        }
                 ;

prototype_symbol_type : symbol ':' type { $$ = newAstPrototypeSymbolType($1, $3); }
                      ;

denv : ENV symbol env_expr  { $$ = newAstDefine($2, newAstExpression(AST_EXPRESSION_TYPE_ENV, AST_EXPRESSION_VAL_ENV($3))); }
     ;

load : LOAD package as  { $$ = newAstLoad($2, $3); }
     ;

as : %empty     { $$ = NULL; }
   | AS symbol  { $$ = $2; }
   ;

/******************************** types */

typedef : TYPEDEF flat_type '{' type_body '}'   { $$ = newAstTypeDef($2, $4); }
        ;

flat_type : symbol                      { $$ = newAstFlatType($1, NULL); }
          | symbol '(' type_symbols ')' { $$ = newAstFlatType($1, $3); }
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

conditional : IF '(' expression ')' nest ELSE conditional_nest  { $$ = fakeAstConditional($3, $5, $7); }
            ;

conditional_nest : conditional  {
                                    $$ = newAstNest(
                                        NULL,
                                        newAstExpressions(newAstExpression(AST_EXPRESSION_TYPE_FUNCALL, AST_EXPRESSION_VAL_FUNCALL($1)), NULL)
                                    );
                                }
                 | nest         { $$ = $1; }
                 ;

switch : SWITCH '(' expressions ')' composite_function  { $$ = newAstFunCall(newAstExpression(AST_EXPRESSION_TYPE_FUN, AST_EXPRESSION_VAL_FUN($5)), $3); }
       ;

fun : function              { $$ = newAstCompositeFunction($1, NULL); }
    | composite_function    { $$ = $1; }
    ;

nest : '{' nest_body '}'    { $$ = $2; }
     ;

composite_function : '{' functions '}'  { $$ = $2; }
                   ;

functions : function            { $$ = newAstCompositeFunction($1, NULL); }
          | function functions  { $$ = newAstCompositeFunction($1, $2); }
          ;

function : '(' fargs ')' nest   { $$ = newAstFunction($2, $4); }
         ;

fargs : %empty            { $$ = NULL; }
      | farg              { $$ = newAstArgList($1, NULL); }
      | farg ',' fargs    { $$ = newAstArgList($1, $3); }
      ;

consfargs : farg                { $$ = newAstUnpack(consSymbol(), newAstArgList($1, NULL)); }
          | farg ',' consfargs  { $$ = newAstUnpack(consSymbol(), newAstArgList($1, newAstArgList(newAstArg(AST_ARG_TYPE_UNPACK, AST_ARG_VAL_UNPACK($3)), NULL))); }
          ;

farg : symbol              { $$ = newAstArg(AST_ARG_TYPE_SYMBOL, AST_ARG_VAL_SYMBOL($1)); }
     | unpack              { $$ = newAstArg(AST_ARG_TYPE_UNPACK, AST_ARG_VAL_UNPACK($1)); }
     | cons                { $$ = newAstArg(AST_ARG_TYPE_UNPACK, AST_ARG_VAL_UNPACK($1)); }
     | named_farg          { $$ = newAstArg(AST_ARG_TYPE_NAMED, AST_ARG_VAL_NAMED($1)); }
     | '[' ']'             { $$ = newAstArg(AST_ARG_TYPE_SYMBOL, AST_ARG_VAL_SYMBOL(nilSymbol())); }
     | '[' consfargs ']'   { $$ = newAstArg(AST_ARG_TYPE_UNPACK, AST_ARG_VAL_UNPACK($2)); }
     | env_type            { $$ = newAstArg(AST_ARG_TYPE_ENV, AST_ARG_VAL_ENV($1)); }
     | NUMBER              { $$ = newAstArg(AST_ARG_TYPE_NUMBER, AST_ARG_VAL_NUMBER($1)); }
     | stringarg           { $$ = newAstArg(AST_ARG_TYPE_UNPACK, AST_ARG_VAL_UNPACK($1)); }
     | CHAR                { $$ = newAstArg(AST_ARG_TYPE_CHARACTER, AST_ARG_VAL_CHARACTER($1)); }
     | WILDCARD            { $$ = newAstArg(AST_ARG_TYPE_WILDCARD, AST_ARG_VAL_WILDCARD()); }
     ;

unpack : symbol '(' fargs ')'   { $$ = newAstUnpack($1, $3); }
       ;

stringarg : STRING { $$ = newStringUnpack($1); }
       ;

string : STRING { $$ = newStringList($1); }
       ;

cons : farg CONS farg { $$ = newAstUnpack(consSymbol(), newAstArgList($1, newAstArgList($3, NULL))); }
     ;

env_type : symbol ':' symbol { $$ = newAstEnvType($1, $3); }
         ;

named_farg : symbol '=' farg  { $$ = newAstNamedArg($1, $3); }
           ;

expression : binop                      { $$ = newAstExpression(AST_EXPRESSION_TYPE_FUNCALL, AST_EXPRESSION_VAL_FUNCALL($1)); }
           | fun_call                   { $$ = newAstExpression(AST_EXPRESSION_TYPE_FUNCALL, AST_EXPRESSION_VAL_FUNCALL($1)); }
           | unop                       { $$ = newAstExpression(AST_EXPRESSION_TYPE_FUNCALL, AST_EXPRESSION_VAL_FUNCALL($1)); }
           | '[' conslist ']'           { $$ = newAstExpression(AST_EXPRESSION_TYPE_FUNCALL, AST_EXPRESSION_VAL_FUNCALL($2)); }
           | FN fun                     { $$ = newAstExpression(AST_EXPRESSION_TYPE_FUN, AST_EXPRESSION_VAL_FUN($2)); }
           | env                        { $$ = newAstExpression(AST_EXPRESSION_TYPE_ENV, AST_EXPRESSION_VAL_ENV($1)); }
           | BACK                       { $$ = newAstExpression(AST_EXPRESSION_TYPE_BACK, AST_EXPRESSION_VAL_BACK()); }
           | conditional                { $$ = newAstExpression(AST_EXPRESSION_TYPE_FUNCALL, AST_EXPRESSION_VAL_FUNCALL($1)); }
           | switch                     { $$ = newAstExpression(AST_EXPRESSION_TYPE_FUNCALL, AST_EXPRESSION_VAL_FUNCALL($1)); }
           | symbol                     { $$ = newAstExpression(AST_EXPRESSION_TYPE_SYMBOL, AST_EXPRESSION_VAL_SYMBOL($1)); }
           | NUMBER                     { $$ = newAstExpression(AST_EXPRESSION_TYPE_NUMBER, AST_EXPRESSION_VAL_NUMBER($1)); }
           | string                     { $$ = newAstExpression(AST_EXPRESSION_TYPE_FUNCALL, AST_EXPRESSION_VAL_FUNCALL($1)); }
           | CHAR                       { $$ = newAstExpression(AST_EXPRESSION_TYPE_CHARACTER, AST_EXPRESSION_VAL_CHARACTER($1)); }
           | nest                       { $$ = newAstExpression(AST_EXPRESSION_TYPE_NEST, AST_EXPRESSION_VAL_NEST($1)); }
           | '(' expression ')'         { $$ = $2; }
           ;

unop : '-' expression %prec NEG   { $$ = unOpToFunCall(negSymbol(), $2); }
     | NOT expression %prec NOT   { $$ = unOpToFunCall(notSymbol(), $2); }
     | HERE expression            { $$ = unOpToFunCall(hereSymbol(), $2); }
     ;

fun_call :  expression '(' expressions ')' { $$ = newAstFunCall($1, $3); }
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
      | expression CONS expression      { $$ = binOpToFunCall(consSymbol(), $1, $3); }
      | expression APPEND expression    { $$ = binOpToFunCall(appendSymbol(), $1, $3); }
      | expression '+' expression       { $$ = binOpToFunCall(addSymbol(), $1, $3); }
      | expression '-' expression       { $$ = binOpToFunCall(subSymbol(), $1, $3); }
      | expression '*' expression       { $$ = binOpToFunCall(mulSymbol(), $1, $3); }
      | expression '/' expression       { $$ = binOpToFunCall(divSymbol(), $1, $3); }
      | expression '%' expression       { $$ = binOpToFunCall(modSymbol(), $1, $3); }
      | expression '^' expression       { $$ = binOpToFunCall(powSymbol(), $1, $3); }
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

expression_statements : expression                              { $$ = newAstExpressions($1, NULL); }
                      | expression ';' expression_statements    { $$ = newAstExpressions($1, $3); }
                      ;

env : ENV env_expr  { $$ = $2; }
    ;

env_expr : extends env_body { $$ = newAstEnv($1, $2); }
         ;

extends : %empty            { $$ = NULL; }
        | EXTENDS package   { $$ = $2; }
        ;

env_body : '{' definitions '}'  { $$ = $2; }

symbol : VAR    { $$ = newSymbol($1); }
       ;

%%

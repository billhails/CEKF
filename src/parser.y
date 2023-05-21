%{

#include <stdbool.h>
#include <stdio.h>
#include <ctype.h>

#include "common.h"
#include "ast.h"

// #define YYDEBUG 1

int yylex();
extern char *yytext;
int lineNum = 1;
void yyerror(char *ps, ...) {
    printf("%s at token '%s' on line %d\n", ps, yytext, lineNum);
}

AstNest *result = NULL;

%}
%union {
    char *s;
    int i;
    char c;
    AstArg *arg;
    AstArgList *argList;
    AstArgPair *argPair;
    AstBinOp *binOp;
    AstCompositeFunction *compositeFunction;
    AstConditional *conditional;
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
    AstFun *fun;
    AstLoad *load;
    AstNamedArg *namedArg;
    AstNest *nest;
    AstPackage *package;
    AstPrototypeBody *prototypeBody;
    AstPrototype *prototype;
    AstPrototypeSymbolType *prototypeSymbolType;
    AstSinglePrototype *singlePrototype;
    AstString *string;
    AstSwitch *switchStatement;
    AstSymbol *symbol;
    AstTypeBody *typeBody;
    AstTypeClause *typeClause;
    AstTypeConstructor *typeConstructor;
    AstTypeDef *typeDef;
    AstTypeList *typeList;
    AstTypeSymbols *typeSymbols;
    AstType *type;
    AstUnpack *unpack;

}

%type <arg> arg
%type <argList> arguments arglist
%type <argPair> cons
%type <binOp> binop
%type <compositeFunction> composite_function functions
%type <conditional> conditional
%type <define> defun denv
%type <definition> definition
%type <definitions> let_in definitions env_body
%type <env> env env_expr
%type <envType> env_type
%type <expression> expression
%type <expressions> expressions expression_statements
%type <flatType> flat_type
%type <funCall> fun_call
%type <function> function
%type <fun> fun
%type <load> load
%type <namedArg> named_arg
%type <nest> top nest nest_body conditional_nest
%type <package> package extends
%type <prototypeBody> prototype_body
%type <prototypeSymbolType> prototype_symbol_type
%type <prototype> prototype
%type <singlePrototype> single_prototype
%type <string> string
%type <switchStatement> switch
%type <symbol> symbol type_symbol as
%type <typeBody> type_body
%type <typeClause> type_clause
%type <typeConstructor> type_constructor
%type <typeDef> typedef
%type <typeList> type_list
%type <typeSymbols> type_symbols
%type <type> type
%type <unpack> unpack

%token AS
%token BACK
%token ELSE
%token ENV
%token EXTENDS
%token FALSE
%token FN
%token IF
%token IN
%token KW_BOOL
%token KW_CHAR
%token KW_INT
%token KW_LIST
%token KW_STRING
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
            | definition definitions     { $$ = newAstDefinitions($2, $1); }
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
                                                    if ($2 == NULL) $$ = newAstPrototypeBody(NULL, $1);
                                                    else $$ = newAstPrototypeBody($2, $1);
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

typedef : TYPEDEF flat_type '{' type_body '}'   { $$ = newAstTypeDef($2, $4); }
        ;

flat_type : symbol                      { $$ = newAstFlatType($1, NULL); }
          | symbol '(' type_symbols ')' { $$ = newAstFlatType($1, $3); }
          ;

type_symbols : type_symbol                  { $$ = newAstTypeSymbols(NULL, $1); }
             | type_symbol ',' type_symbols { $$ = newAstTypeSymbols($3, $1); }
             ;

type_symbol : TYPE_VAR  { $$ = newAstSymbol(AST_SYMBOLTYPE_TYPE_TYPESYMBOL, hashString($1), safeStrdup($1)); }
            ;

type_body : type_constructor                { $$ = newAstTypeBody(NULL, $1); }
          | type_constructor '|' type_body  { $$ = newAstTypeBody($3, $1); }
          ;

type_constructor : symbol                   { $$ = newAstTypeConstructor($1, NULL); }
                 | symbol '(' type_list ')' { $$ = newAstTypeConstructor($1, $3); }
                 ;

type_list : type                { $$ = newAstTypeList(NULL, $1); }
          | type ',' type_list  { $$ = newAstTypeList($3, $1); }
          ;

type : type_clause              { $$ = newAstType(NULL, $1); }
     | type_clause ARROW type   { $$ = newAstType($3, $1); }
     ;

type_clause : KW_LIST '(' type ')'  { $$ = newAstTypeClause(AST_TYPECLAUSE_TYPE_LIST, AST_TYPECLAUSE_VAL_LIST($3)); }
            | KW_INT                { $$ = newAstTypeClause(AST_TYPECLAUSE_TYPE_INTEGER, AST_TYPECLAUSE_VAL_INTEGER()); }
            | KW_CHAR               { $$ = newAstTypeClause(AST_TYPECLAUSE_TYPE_CHARACTER, AST_TYPECLAUSE_VAL_CHARACTER()); }
            | KW_BOOL               { $$ = newAstTypeClause(AST_TYPECLAUSE_TYPE_BOOLEAN, AST_TYPECLAUSE_VAL_BOOLEAN()); }
            | KW_STRING             { $$ = newAstTypeClause(AST_TYPECLAUSE_TYPE_STRING, AST_TYPECLAUSE_VAL_STRING()); }
            | type_symbol           { $$ = newAstTypeClause(AST_TYPECLAUSE_TYPE_VAR, AST_TYPECLAUSE_VAL_VAR($1)); }
            | type_constructor      { $$ = newAstTypeClause(AST_TYPECLAUSE_TYPE_TYPECONSTRUCTOR, AST_TYPECLAUSE_VAL_TYPECONSTRUCTOR($1)); }
            | '(' type ')'          { $$ = newAstTypeClause(AST_TYPECLAUSE_TYPE_TYPE, AST_TYPECLAUSE_VAL_TYPE($2)); }
            ;

/******************************** expressions */

conditional : IF '(' expression ')' nest ELSE conditional_nest  { $$ = newAstConditional($3, $5, $7); }
            ;

conditional_nest : conditional  {
                                    $$ = newAstNest(
                                        NULL,
                                        newAstExpressions(NULL,
                                            newAstExpression(AST_EXPRESSION_TYPE_CONDITIONAL, AST_EXPRESSION_VAL_CONDITIONAL($1)))
                                    );
                                }
                 | nest         { $$ = $1; }
                 ;

switch : SWITCH '(' expressions ')' composite_function  { $$ = newAstSwitch($3, $5); }
       ;

fun : function              { $$ = newAstFun(AST_FUN_TYPE_FUNCTION, AST_FUN_VAL_FUNCTION($1)); }
    | composite_function    { $$ =  newAstFun(AST_FUN_TYPE_COMPOSITEFUNCTION, AST_FUN_VAL_COMPOSITEFUNCTION($1)); }
    ;

nest : '{' nest_body '}'    { $$ = $2; }
     ;

composite_function : '{' functions '}'  { $$ = $2; }
                   ;

functions : function            { $$ = newAstCompositeFunction(NULL, $1); }
          | function functions  { $$ = newAstCompositeFunction($2, $1); }
          ;

function : arguments nest   { $$ = newAstFunction($1, $2); }
         ;

arguments : '(' arglist ')' { $$ = $2; }
          ;

arglist : %empty            { $$ = NULL; }
        | arg               { $$ = newAstArgList(NULL, $1); }
        | arg ',' arglist   { $$ = newAstArgList($3, $1); }
        ;

arg : symbol            { $$ = newAstArg(AST_ARG_TYPE_SYMBOL, AST_ARG_VAL_SYMBOL($1)); }
    | unpack            { $$ = newAstArg(AST_ARG_TYPE_UNPACK, AST_ARG_VAL_UNPACK($1)); }
    | cons              { $$ = newAstArg(AST_ARG_TYPE_CONS, AST_ARG_VAL_CONS($1)); }
    | named_arg         { $$ = newAstArg(AST_ARG_TYPE_NAMED, AST_ARG_VAL_NAMED($1)); }
    | '[' arglist ']'   { $$ = newAstArg(AST_ARG_TYPE_LIST, AST_ARG_VAL_LIST($2)); }
    | env_type          { $$ = newAstArg(AST_ARG_TYPE_ENV, AST_ARG_VAL_ENV($1)); }
    | NUMBER            { $$ = newAstArg(AST_ARG_TYPE_NUMBER, AST_ARG_VAL_NUMBER($1)); }
    | string            { $$ = newAstArg(AST_ARG_TYPE_STRING, AST_ARG_VAL_STRING($1)); }
    | CHAR              { $$ = newAstArg(AST_ARG_TYPE_CHARACTER, AST_ARG_VAL_CHARACTER($1)); }
    | TRUE              { $$ = newAstArg(AST_ARG_TYPE_YES, AST_ARG_VAL_YES()); }
    | FALSE             { $$ = newAstArg(AST_ARG_TYPE_NO, AST_ARG_VAL_NO()); }
    | WILDCARD          { $$ = newAstArg(AST_ARG_TYPE_WILDCARD, AST_ARG_VAL_WILDCARD()); }
    ;

unpack : symbol arguments   { $$ = newAstUnpack($1, $2); }

string : STRING { $$ = newAstString(safeStrdup($1)); }
       ;

cons : arg CONS arg { $$ = newAstArgPair($1, $3); }
     ;

env_type : symbol ':' symbol { $$ = newAstEnvType($1, $3); }
         ;

named_arg : symbol '=' arg  { $$ = newAstNamedArg($1, $3); }
          ;

expression : binop                      { $$ = newAstExpression(AST_EXPRESSION_TYPE_BINOP, AST_EXPRESSION_VAL_BINOP($1)); }
           | fun_call                   { $$ = newAstExpression(AST_EXPRESSION_TYPE_FUNCALL, AST_EXPRESSION_VAL_FUNCALL($1)); }
           | '-' expression %prec NEG   { $$ = newAstExpression(AST_EXPRESSION_TYPE_NEGATE, AST_EXPRESSION_VAL_NEGATE($2)); }
           | NOT expression %prec NOT   { $$ = newAstExpression(AST_EXPRESSION_TYPE_NOT, AST_EXPRESSION_VAL_NOT($2)); }
           | HERE expression            { $$ = newAstExpression(AST_EXPRESSION_TYPE_HERE, AST_EXPRESSION_VAL_HERE($2)); }
           | '[' expressions ']'        { $$ = newAstExpression(AST_EXPRESSION_TYPE_LIST, AST_EXPRESSION_VAL_LIST($2)); }
           | FN fun                     { $$ = newAstExpression(AST_EXPRESSION_TYPE_FUN, AST_EXPRESSION_VAL_FUN($2)); }
           | env                        { $$ = newAstExpression(AST_EXPRESSION_TYPE_ENV, AST_EXPRESSION_VAL_ENV($1)); }
           | BACK                       { $$ = newAstExpression(AST_EXPRESSION_TYPE_BACK, AST_EXPRESSION_VAL_BACK()); }
           | conditional                { $$ = newAstExpression(AST_EXPRESSION_TYPE_CONDITIONAL, AST_EXPRESSION_VAL_CONDITIONAL($1)); }
           | switch                     { $$ = newAstExpression(AST_EXPRESSION_TYPE_SWITCHSTATEMENT, AST_EXPRESSION_VAL_SWITCHSTATEMENT($1)); }
           | symbol                     { $$ = newAstExpression(AST_EXPRESSION_TYPE_SYMBOL, AST_EXPRESSION_VAL_SYMBOL($1)); }
           | NUMBER                     { $$ = newAstExpression(AST_EXPRESSION_TYPE_NUMBER, AST_EXPRESSION_VAL_NUMBER($1)); }
           | string                     { $$ = newAstExpression(AST_EXPRESSION_TYPE_STRING, AST_EXPRESSION_VAL_STRING($1)); }
           | CHAR                       { $$ = newAstExpression(AST_EXPRESSION_TYPE_CHARACTER, AST_EXPRESSION_VAL_CHARACTER($1)); }
           | TRUE                       { $$ = newAstExpression(AST_EXPRESSION_TYPE_YES, AST_EXPRESSION_VAL_YES()); }
           | FALSE                      { $$ = newAstExpression(AST_EXPRESSION_TYPE_NO, AST_EXPRESSION_VAL_NO()); }
           | '(' expression ')'         { $$ = $2; }
           ;

fun_call :  expression '(' expressions ')' { $$ = newAstFunCall($1, $3); }
         ;

binop : expression THEN expression      { $$ = newAstBinOp(AST_BINOPTYPE_TYPE_THEN, $1, $3); }
      | expression AND expression       { $$ = newAstBinOp(AST_BINOPTYPE_TYPE_AND, $1, $3); }
      | expression OR expression        { $$ = newAstBinOp(AST_BINOPTYPE_TYPE_OR, $1, $3); }
      | expression XOR expression       { $$ = newAstBinOp(AST_BINOPTYPE_TYPE_XOR, $1, $3); }
      | expression EQ expression        { $$ = newAstBinOp(AST_BINOPTYPE_TYPE_EQ, $1, $3); }
      | expression NE expression        { $$ = newAstBinOp(AST_BINOPTYPE_TYPE_NE, $1, $3); }
      | expression GT expression        { $$ = newAstBinOp(AST_BINOPTYPE_TYPE_GT, $1, $3); }
      | expression LT expression        { $$ = newAstBinOp(AST_BINOPTYPE_TYPE_LT, $1, $3); }
      | expression GE expression        { $$ = newAstBinOp(AST_BINOPTYPE_TYPE_GE, $1, $3); }
      | expression LE expression        { $$ = newAstBinOp(AST_BINOPTYPE_TYPE_LE, $1, $3); }
      | expression CONS expression      { $$ = newAstBinOp(AST_BINOPTYPE_TYPE_CONS, $1, $3); }
      | expression APPEND expression    { $$ = newAstBinOp(AST_BINOPTYPE_TYPE_APPEND, $1, $3); }
      | expression '+' expression       { $$ = newAstBinOp(AST_BINOPTYPE_TYPE_ADD, $1, $3); }
      | expression '-' expression       { $$ = newAstBinOp(AST_BINOPTYPE_TYPE_SUB, $1, $3); }
      | expression '*' expression       { $$ = newAstBinOp(AST_BINOPTYPE_TYPE_MUL, $1, $3); }
      | expression '/' expression       { $$ = newAstBinOp(AST_BINOPTYPE_TYPE_DIV, $1, $3); }
      | expression '%' expression       { $$ = newAstBinOp(AST_BINOPTYPE_TYPE_MOD, $1, $3); }
      | expression '^' expression       { $$ = newAstBinOp(AST_BINOPTYPE_TYPE_POW, $1, $3); }
      | expression '.' expression       { $$ = newAstBinOp(AST_BINOPTYPE_TYPE_DOT, $1, $3); }
      ;

package : symbol                { $$ = newAstPackage(NULL, $1); }
        | symbol '.' package    { $$ = newAstPackage($3, $1); }
        ;

expressions : %empty                        { $$ = NULL; }
            | expression                    { $$ = newAstExpressions(NULL, $1); }
            | expression ',' expressions    { $$ = newAstExpressions($3, $1); }
            ;

expression_statements : expression                              { $$ = newAstExpressions(NULL, $1); }
                      | expression ';' expression_statements    { $$ = newAstExpressions($3, $1); }
                      ;

env : ENV env_expr  { $$ = $2; }
    ;

env_expr : extends env_body { $$ = newAstEnv($1, $2); }
         ;

extends : %empty            { $$ = NULL; }
        | EXTENDS package   { $$ = $2; }
        ;

env_body : '{' definitions '}'  { $$ = $2; }

symbol : VAR    { $$ = newAstSymbol(AST_SYMBOLTYPE_TYPE_SYMBOL, hashString($1), safeStrdup($1)); }
       ;

%%

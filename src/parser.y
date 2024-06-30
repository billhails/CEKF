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
#include <string.h>

#include "common.h"
#include "ast_helper.h"
#include "ast_debug.h"
#include "symbol.h"
#include "symbols.h"
#include "bigint.h"
#include "module.h"
#include "parser.h"
#include "lexer.h"
#include "types.h"
#include "print_generator.h"
#include "utf8.h"

AstStringArray *include_paths = NULL;

void yyerror (yyscan_t *locp, PmModule *mod, char const *msg);

// #define YYDEBUG 1

static AstFunCall *binOpToFunCall(PmModule *mod, HashSymbol *op, AstExpression *lhs, AstExpression *rhs) {
    return newAstFunCall(PIM(mod),
        newAstExpression_Symbol(PIM(mod), op),
        newAstExpressions(
            PIM(mod),
            lhs,
            newAstExpressions(PIM(mod), rhs, NULL)
        )
    );
}

static AstFunCall *unOpToFunCall(PmModule *mod, HashSymbol *op, AstExpression *arg) {
    return newAstFunCall(PIM(mod),
        newAstExpression_Symbol(PIM(mod), op),
        newAstExpressions(PIM(mod), arg, NULL)
    );
}

static AstFunCall *makeStringList(PmModule *mod, AstCharArray *str) {
    AstFunCall *res =
        newAstFunCall(PIM(mod), newAstExpression_Symbol(PIM(mod), nilSymbol()), NULL);
    for (int size = str->size; size > 0; size--) {
        res = newAstFunCall(
            PIM(mod),
            newAstExpression_Symbol(PIM(mod), consSymbol()),
            newAstExpressions(PIM(mod),
                newAstExpression_Character(PIM(mod), str->entries[size-1]),
                newAstExpressions(PIM(mod), newAstExpression_FunCall(PIM(mod), res), NULL)
            )
        );
    }
    return res;
}

static AstUnpack *makeAstUnpack(PmModule *mod, HashSymbol *symbol, AstArgList *args) {
    return newAstUnpack(PIM(mod),
        newAstLookupOrSymbol_Symbol(PIM(mod), symbol),
        args
    );
}

static AstUnpack *makeStringUnpack(PmModule *mod, AstCharArray *str) {
    AstUnpack *res = makeAstUnpack(mod, nilSymbol(), NULL);
    for (int size = str->size; size > 0; size--) {
        res = makeAstUnpack(mod,
            consSymbol(),
            newAstArgList(PIM(mod),
                newAstArg_Character(PIM(mod), str->entries[size-1]),
                newAstArgList(PIM(mod), newAstArg_Unpack(PIM(mod), res), NULL)
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

static MaybeBigInt *makeIrrational(char *str, bool imag) {
    Double f = atof(str);
    return irrationalBigInt(f, imag);
}

static MaybeBigInt *makeMaybeBigInt(char *digits, bool imag) {
    bool overflowed = false;
    int a = 0;
    bigint bi;
    for (char *p = digits; *p != '\0' && *p != 'i'; ++p) {
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
        MaybeBigInt *bbi = newMaybeBigInt(bi, imag);
        return bbi;
    } else {
        return fakeBigInt(a, imag);
    }
}

static AstCharArray *appendCharArray(AstCharArray *res, char *str) {
    while (*str) {
#ifdef CHARACTER_IS_CHAR
        pushAstCharArray(res, *str);
        str++;
#else
        Character unicode;
        str = utf8_to_unicode_char(&unicode, str);
        pushAstCharArray(res, unicode);
#endif
    }
    return res;
}

static AstCharArray *newCharArray(char *str) {
    AstCharArray *res = newAstCharArray();
    return appendCharArray(res, str);
}

static AstCompositeFunction *makeAstCompositeFunction(AstAltFunction *functions, AstCompositeFunction *rest) {
    for (AstAltArgs *args = functions->altArgs; args != NULL; args = args->next) {
        AstFunction *this = newAstFunction(CPI(args), args->argList, copyAstNest(functions->nest));
        rest = newAstCompositeFunction(CPI(args), this, rest);
    }
    return rest;
}

static AgnosticFileId *tryFile(char *prefix, char *file) {
    char *buf = malloc(sizeof(char) * (strlen(prefix) + 1 + strlen(file) + 10));
    if (buf == NULL) {
        perror("out of memory");
        exit(1);
    }
    sprintf(buf, "%s/%s", prefix, file);
    AgnosticFileId *result = makeAgnosticFileId(buf);
    if (result == NULL) free(buf);
    return result;
}

static AgnosticFileId *searchForFile(char *initialPrefix, char *fileToFind) {
    AgnosticFileId *result = NULL;
    result = tryFile(initialPrefix, fileToFind);
    if (result != NULL) return result;
    if (include_paths != NULL) {
        for (Index i = 0; i < include_paths->size; i++) {
            result = tryFile(include_paths->entries[i], fileToFind);
            if (result != NULL) return result;
        }
    }
    return NULL;
}

static AgnosticFileId *calculatePath(char *file, PmModule *mod) {
    if (*file == '/') {
        return makeAgnosticFileId(file);
    }
    char *currentFile = currentPmFile(mod);
    if (currentFile == NULL) {
        return searchForFile(".", file);
    }
    currentFile = strdup(currentFile);
    char *slash = strrchr(currentFile, '/');
    if (slash == NULL) {
        free(currentFile);
        return searchForFile(".", file);
    }
    *slash = '\0';
    AgnosticFileId *result = searchForFile(currentFile, file);
    free(currentFile);
    return result;
}

static AstFileIdArray *fileIdStack = NULL;

static bool fileIdInArray(AgnosticFileId *id, AstFileIdArray *array) {
    for (Index i = 0; i < array->size; ++i) {
        if (cmpAgnosticFileId(id, array->entries[i]) == CMP_EQ) {
            return true;
        }
    }
    return false;
}

// Careful. Somewhat accidentally this algorithm stores the namespaces
// in the order that they need to be processed.
// Specifically because a namespace is parsed before it is recorded,
// all of its imports are recorded ahead of it.
static AstNamespace *parseLink(char *file, HashSymbol *symbol, PmModule *mod) {
    if (fileIdStack == NULL) {
        fileIdStack = newAstFileIdArray();
    }
    AgnosticFileId *fileId = calculatePath(file, mod);
    if (fileId == NULL) {
        cant_happen("cannot find file \"%s\"", file);
    }
    int found = lookupNamespace(fileId);
    if (found != -1) {
        return newAstNamespace(PIM(mod), symbol, found);
    }
    if (fileIdInArray(fileId, fileIdStack)) {
        cant_happen("recursive include detected for %s", fileId->name);
    }
    pushAstFileIdArray(fileIdStack, fileId);
    AstDefinitions *definitions = parseNamespaceFromFileName(fileId->name);
    if (definitions == NULL) {
        cant_happen("syntax error parsing %s", fileId->name);
    }
    AstNamespaceImpl *impl = newAstNamespaceImpl(PIM(mod), fileId, definitions);
    found = pushAstNamespaceArray(namespaces, impl);
    popAstFileIdArray(fileIdStack);
    AstNamespace *ns = newAstNamespace(PIM(mod), symbol, found);
    return ns;
}

static void storeNamespace(PmModule *mod, AstNamespace *ns) {
    if (getAstIntTable(mod->namespaces, ns->symbol, NULL)) {
        cant_happen("redefinition of namespace %s", ns->symbol->name);
    }
    setAstIntTable(mod->namespaces, ns->symbol, ns->reference);
}

static AstLookup *makeAstLookup(PmModule *mod, HashSymbol *symbol, AstExpression *expr) {
    int index = 0;
    if (getAstIntTable(mod->namespaces, symbol, &index)) {
        return newAstLookup(PIM(mod), index, symbol, expr);
    } else {
        cant_happen("cannot resolve namespace %s", symbol->name);
    }
}

static AstLookupSymbol *makeAstLookupSymbol(PmModule *mod, HashSymbol *nsName, HashSymbol *symbol) {
    int index = 0;
    if (getAstIntTable(mod->namespaces, nsName, &index)) {
        return newAstLookupSymbol(PIM(mod), index, nsName, symbol);
    } else {
        cant_happen("cannot resolve namespace %s", symbol->name);
    }
}

static AstLookupOrSymbol *makeAstLookupOrSymbol(PmModule *mod, HashSymbol *nsName, HashSymbol *symbol) {
    AstLookupSymbol *als = makeAstLookupSymbol(mod, nsName, symbol);
    return newAstLookupOrSymbol_Lookup(PIM(mod), als);
}

static AstArg *makeAstLookupArg(PmModule *mod, HashSymbol *nsName, HashSymbol *symbol) {
    AstLookupSymbol *als = makeAstLookupSymbol(mod, nsName, symbol);
    return newAstArg_Lookup(PIM(mod), als);
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
    bool b;
    MaybeBigInt *bi;
    AstArg *arg;
    AstArgList *argList;
    AstCompositeFunction *compositeFunction;
    AstDefine *define;
    AstDefinition *definition;
    AstDefinitions *definitions;
    AstExpression *expression;
    AstExpressions *expressions;
    AstUserType *userType;
    AstFunCall *funCall;
    AstNamedArg *namedArg;
    AstNest *nest;
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
    AstUnpackStruct *unpackStruct;
    AstTaggedArgList *taggedArgList;
    AstIff *iff;
    AstCharArray *chars;
    AstAltFunction *altFunction;
    AstAltArgs * altArgs;
    AstNamespace *namespace;
    AstLookup *lookup;
    AstLookupOrSymbol *los;
    AstTypeConstructorArgs *typeConstructorArgs;
    AstTypeMap *typemap;
    AstStruct *structure;
    AstTaggedExpressions *taggedExpressions;
    AstAlias *alias;
}

%type <chars> str
%type <bi> number
%type <arg> farg
%type <argList> fargs arg_tuple
%type <compositeFunction> composite_function functions fun
%type <define> defun
%type <definition> definition
%type <definitions> let_in definitions namespace_definitions
%type <expression> expression assertion
%type <expressions> expressions expression_statements tuple
%type <userType> user_type
%type <funCall> fun_call binop conslist unop switch string
%type <namedArg> named_farg
%type <nest> top nest nest_body iff_nest
%type <print> print
%type <symbol> symbol type_variable
%type <typeBody> type_body
%type <typeClause> type_clause
%type <typeConstructor> type_constructor
%type <typeFunction> type_function
%type <typeDef> typedef
%type <typeList> type_list type_tuple
%type <typeSymbols> type_variables
%type <type> type
%type <unpack> unpack cons consfargs stringarg
%type <unpackStruct> unpack_struct
%type <iff> iff
%type <altFunction> alt_function
%type <altArgs> alt_args
%type <namespace> name_space
%type <lookup> look_up
%type <los> scoped_symbol
%type <typeConstructorArgs> type_constructor_args
%type <typemap> type_map
%type <structure> structure
%type <taggedExpressions> tagged_expressions
%type <taggedArgList> tagged_fargs
%type <alias> alias
%type <b> unsafe_fn unsafe_switch

%token BACK
%token ELSE
%token FALSE
%token FN
%token UNSAFE
%token IF
%token IN
%token KW_CHAR
%token KW_INT
%token LET
%token PRINT
%token SWITCH
%token TRUE
%token TYPEDEF
%token WILDCARD
%token LINK
%token NAMESPACE_TOKEN
%token AS
%token ASSERT
%token ALIAS

%token <c> CHAR
%token <s> NUMBER
%token <s> NUMBER_I
%token <s> STRING
%token <s> TYPE_VAR
%token <s> VAR
%token <s> IRRATIONAL
%token <s> IRRATIONAL_I

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

nest_body : let_in expression_statements { $$ = newAstNest(PIM(mod), $1, $2); }
          | expression_statements        { $$ = newAstNest(PIM(mod), NULL, $1); }
          | namespace_definitions        { $$ = newAstNest(PIM(mod), $1, NULL); }
          ;

/******************************** definitions */

let_in : LET definitions IN { $$ = $2; }
       ;

namespace_definitions : NAMESPACE_TOKEN definitions   { $$ = $2; }
                      ;

definitions : %empty                            { $$ = NULL; }
            | definition definitions            { $$ = newAstDefinitions(PIM(mod), $1, $2); }
            ;

definition : symbol '=' expression ';'  {
                                            $$ = newAstDefinition_Define(
                                                    PIM(mod),
                                                    newAstDefine(PIM(mod), $1, $3));
                                        }
           | typedef                    { $$ = newAstDefinition_TypeDef(PIM(mod), $1); }
           | defun                      { $$ = newAstDefinition_Define(PIM(mod), $1); }
           | name_space ';'             {
                                           storeNamespace(mod, $1);
                                           $$ = newAstDefinition_Blank(PIM(mod));
                                        }
           | alias                      { $$ = newAstDefinition_Alias(PIM(mod), $1); }
           ;

defun : unsafe_fn symbol fun {
                                $3->unsafe = $1;
                                $$ = newAstDefine(PIM(mod), $2,
                                        newAstExpression_Fun(PIM(mod), $3));
                            }
      | PRINT symbol fun    {
                                $$ = newAstDefine(PIM(mod),
                                    makePrintName("print$", $2->name),
                                    newAstExpression_Fun(PIM(mod), $3));
                            }
      ;

name_space : LINK STRING AS symbol { $$ = parseLink($2, $4, mod); }
           ;

alias : ALIAS symbol '=' type ';' { $$ = newAstAlias(PIM(mod), $2, $4); }

/******************************** types */

typedef : TYPEDEF user_type '{' type_body '}'   { $$ = newAstTypeDef(PIM(mod), $2, $4); }
        ;

/* a type function being defined */
user_type : symbol                      { $$ = newAstUserType(PIM(mod), $1, NULL); }
          | symbol '(' type_variables ')' { $$ = newAstUserType(PIM(mod), $1, $3); }
          ;

type_variables : type_variable                  { $$ = newAstTypeSymbols(PIM(mod), $1, NULL); }
             | type_variable ',' type_variables { $$ = newAstTypeSymbols(PIM(mod), $1, $3); }
             ;

type_variable : TYPE_VAR  { $$ = newSymbol($1); }
            ;

type_body : type_constructor                { $$ = newAstTypeBody(PIM(mod), $1, NULL); }
          | type_constructor '|' type_body  { $$ = newAstTypeBody(PIM(mod), $1, $3); }
          ;

/* a type constructor being defined */
type_constructor : symbol                       { $$ = newAstTypeConstructor(PIM(mod), $1, NULL); }
                 | symbol type_constructor_args { $$ = newAstTypeConstructor(PIM(mod), $1, $2); }
                 ;

/* a type function being used in the body of a type constructor */
type_function : scoped_symbol                   { $$ = newAstTypeFunction(PIM(mod), $1, NULL); }
              | scoped_symbol '(' type_list ')' { $$ = newAstTypeFunction(PIM(mod), $1, $3); }
              ;

type_constructor_args : '(' type_list ')'   { $$ = newAstTypeConstructorArgs_List(PIM(mod), $2); }
                      | '{' type_map '}'    { $$ = newAstTypeConstructorArgs_Map(PIM(mod), $2); }
                      ;

scoped_symbol : symbol              { $$ = newAstLookupOrSymbol_Symbol(PIM(mod), $1); }
              | symbol '.' symbol   { $$ = makeAstLookupOrSymbol(mod, $1, $3); }
              ;

type_list : type                { $$ = newAstTypeList(PIM(mod), $1, NULL); }
          | type ',' type_list  { $$ = newAstTypeList(PIM(mod), $1, $3); }
          ;

type_map : symbol ':' type              { $$ = newAstTypeMap(PIM(mod), $1, $3, NULL); }
         | symbol ':' type ',' type_map { $$ = newAstTypeMap(PIM(mod), $1, $3, $5); }
         ;

type : type_clause              { $$ = newAstType(PIM(mod), $1, NULL); }
     | type_clause ARROW type   { $$ = newAstType(PIM(mod), $1, $3); }
     | '(' type ')'             { $$ = $2; }
     ;

type_tuple : '#' '(' type_list ')' { $$ = $3; }
           ;

type_clause : KW_INT                { $$ = newAstTypeClause_Integer(PIM(mod)); }
            | KW_CHAR               { $$ = newAstTypeClause_Character(PIM(mod)); }
            | type_variable           { $$ = newAstTypeClause_Var(PIM(mod), $1); }
            | type_function         { $$ = newAstTypeClause_TypeFunction(PIM(mod), $1); }
            | type_tuple            { $$ = newAstTypeClause_TypeTuple(PIM(mod), $1); }
            ;

/******************************** expressions */

iff : IF '(' expression ')' nest ELSE iff_nest  { $$ = newAstIff(PIM(mod), $3, $5, $7); }
    ;

iff_nest : iff  {
                    $$ = newAstNest(PIM(mod),
                        NULL,
                        newAstExpressions(PIM(mod),
                            newAstExpression_Iff(PIM(mod), $1),
                            NULL));
                }
         | nest { $$ = $1; }
         ;

switch : unsafe_switch '(' expressions ')' composite_function  {
                                                            $5->unsafe = $1;
                                                            $$ = newAstFunCall(PIM(mod),
                                                                newAstExpression_Fun(PIM(mod), $5),
                                                                $3);
                                                        }
       ;

unsafe_fn : FN              { $$ = false; }
          | UNSAFE FN       { $$ = true; }
          ;

unsafe_switch : SWITCH              { $$ = false; }
              | UNSAFE SWITCH       { $$ = true; }
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

alt_args : '(' fargs ')'               { $$ = newAstAltArgs(PIM(mod), $2, NULL); }
         | '(' fargs ')' '|' alt_args  { $$ = newAstAltArgs(PIM(mod), $2, $5); }
         ;

alt_function : alt_args nest    { $$ = newAstAltFunction(PIM(mod), $1, $2); }
             ;

fargs : %empty            { $$ = NULL; }
      | farg              { $$ = newAstArgList(PIM(mod), $1, NULL); }
      | farg ',' fargs    { $$ = newAstArgList(PIM(mod), $1, $3); }
      ;

consfargs : farg                {
                                    $$ = makeAstUnpack(mod,
                                        consSymbol(),
                                        newAstArgList(PIM(mod),
                                        $1,
                                        newAstArgList(PIM(mod),
                                        newAstArg_Symbol(PIM(mod),
                                            nilSymbol()),
                                            NULL)));
                                }
          | farg ',' consfargs  {
                                    $$ = makeAstUnpack(mod,
                                        consSymbol(),
                                        newAstArgList(PIM(mod),
                                            $1,
                                            newAstArgList(PIM(mod),
                                                newAstArg_Unpack(PIM(mod), $3),
                                                NULL)));
                                }
          ;

number : NUMBER        { $$ = makeMaybeBigInt($1, false); }
       | IRRATIONAL    { $$ = makeIrrational($1, false); }
       | NUMBER_I      { $$ = makeMaybeBigInt($1, true); }
       | IRRATIONAL_I  { $$ = makeIrrational($1, true); }
       ;

farg : symbol              { $$ = newAstArg_Symbol(PIM(mod), $1); }
     | symbol '.' symbol   { $$ = makeAstLookupArg(mod, $1, $3); }
     | unpack              { $$ = newAstArg_Unpack(PIM(mod), $1); }
     | unpack_struct       { $$ = newAstArg_UnpackStruct(PIM(mod), $1); }
     | cons                { $$ = newAstArg_Unpack(PIM(mod), $1); }
     | named_farg          { $$ = newAstArg_Named(PIM(mod), $1); }
     | '[' ']'             { $$ = newAstArg_Symbol(PIM(mod), nilSymbol()); }
     | '[' consfargs ']'   { $$ = newAstArg_Unpack(PIM(mod), $2); }
     | number              { $$ = newAstArg_Number(PIM(mod), $1); }
     | stringarg           { $$ = newAstArg_Unpack(PIM(mod), $1); }
     | CHAR                { $$ = newAstArg_Character(PIM(mod), $1); }
     | WILDCARD            { $$ = newAstArg_Wildcard(PIM(mod)); }
     | arg_tuple           { $$ = newAstArg_Tuple(PIM(mod), $1); }
     ;

arg_tuple : '#' '(' fargs ')'  { $$ = $3; }
          ;

cons : farg CONS farg   {
                            $$ = makeAstUnpack(mod,
                                consSymbol(),
                                newAstArgList(PIM(mod),
                                    $1,
                                    newAstArgList(PIM(mod),
                                        $3,
                                        NULL)));
                        }
     ;

unpack : scoped_symbol '(' fargs ')'           { $$ = newAstUnpack(PIM(mod), $1, $3); }
       ;

unpack_struct : scoped_symbol '{' tagged_fargs '}'     { $$ = newAstUnpackStruct(PIM(mod), $1, $3); }
              ;

tagged_fargs : symbol ':' farg                      { $$ = newAstTaggedArgList(PIM(mod), $1, $3, NULL); }
             | symbol ':' farg ',' tagged_fargs     { $$ = newAstTaggedArgList(PIM(mod), $1, $3, $5); }
             ;

stringarg : str { $$ = makeStringUnpack(mod, $1); }
          ;

string : str { $$ = makeStringList(mod, $1); }
       ;

str : STRING            { $$ = newCharArray($1); }
    | str STRING        { $$ = appendCharArray($1, $2); }
    ;

named_farg : symbol '=' farg  { $$ = newAstNamedArg(PIM(mod), $1, $3); }
           ;

expression : binop                { $$ = newAstExpression_FunCall(PIM(mod), $1); }
           | fun_call             { $$ = newAstExpression_FunCall(PIM(mod), $1); }
           | structure            { $$ = newAstExpression_Structure(PIM(mod), $1); }
           | unop                 { $$ = newAstExpression_FunCall(PIM(mod), $1); }
           | '[' conslist ']'     { $$ = newAstExpression_FunCall(PIM(mod), $2); }
           | unsafe_fn fun        { $2->unsafe = $1; $$ = newAstExpression_Fun(PIM(mod), $2); }
           | BACK                 { $$ = newAstExpression_Back(PIM(mod)); }
           | iff                  { $$ = newAstExpression_Iff(PIM(mod), $1); }
           | switch               { $$ = newAstExpression_FunCall(PIM(mod), $1); }
           | symbol               { $$ = newAstExpression_Symbol(PIM(mod), $1); }
           | number               { $$ = newAstExpression_Number(PIM(mod), $1); }
           | string               { $$ = newAstExpression_FunCall(PIM(mod), $1); }
           | CHAR                 { $$ = newAstExpression_Character(PIM(mod), $1); }
           | nest                 { $$ = newAstExpression_Nest(PIM(mod), $1); }
           | print                { $$ = newAstExpression_Print(PIM(mod), $1); }
           | tuple                { $$ = newAstExpression_Tuple(PIM(mod), $1); }
           | look_up              { $$ = newAstExpression_Lookup(PIM(mod), $1); }
           | assertion            { $$ = newAstExpression_Assertion(PIM(mod), $1); }
           | '(' expression ')'   { $$ = $2; }
           ;

assertion : ASSERT '(' expression ')' { $$ = $3; }
          ;

unop : '-' expression %prec NEG   { $$ = unOpToFunCall(mod, negSymbol(), $2); }
     | NOT expression %prec NOT   { $$ = unOpToFunCall(mod, notSymbol(), $2); }
     | LT expression %prec CAR    { $$ = unOpToFunCall(mod, carSymbol(), $2); }
     | GT expression %prec CDR    { $$ = unOpToFunCall(mod, cdrSymbol(), $2); }
     | HERE expression            { $$ = unOpToFunCall(mod, hereSymbol(), $2); }
     ;

fun_call :  expression '(' expressions ')' { $$ = newAstFunCall(PIM(mod), $1, $3); }
         ;

structure : scoped_symbol '{' tagged_expressions '}' { $$ = newAstStruct(PIM(mod), $1, $3); }
          ;

tagged_expressions : symbol ':' expression                         { $$ = newAstTaggedExpressions(PIM(mod), $1, $3, NULL); }
                   | symbol ':' expression ',' tagged_expressions  { $$ = newAstTaggedExpressions(PIM(mod), $1, $3, $5); }
                   ;

tuple : '#' '(' expressions ')'   { $$ = $3; }
      ;

binop : expression THEN expression      { $$ = binOpToFunCall(mod, thenSymbol(), $1, $3); }
      | expression AND expression       { $$ = binOpToFunCall(mod, andSymbol(), $1, $3); }
      | expression OR expression        { $$ = binOpToFunCall(mod, orSymbol(), $1, $3); }
      | expression XOR expression       { $$ = binOpToFunCall(mod, xorSymbol(), $1, $3); }
      | expression EQ expression        { $$ = binOpToFunCall(mod, eqSymbol(), $1, $3); }
      | expression NE expression        { $$ = binOpToFunCall(mod, neSymbol(), $1, $3); }
      | expression GT expression        { $$ = binOpToFunCall(mod, gtSymbol(), $1, $3); }
      | expression LT expression        { $$ = binOpToFunCall(mod, ltSymbol(), $1, $3); }
      | expression GE expression        { $$ = binOpToFunCall(mod, geSymbol(), $1, $3); }
      | expression LE expression        { $$ = binOpToFunCall(mod, leSymbol(), $1, $3); }
      | expression CMP expression       { $$ = binOpToFunCall(mod, cmpSymbol(), $1, $3); }
      | expression CONS expression      { $$ = binOpToFunCall(mod, consSymbol(), $1, $3); }
      | expression APPEND expression    { $$ = binOpToFunCall(mod, appendSymbol(), $1, $3); }
      | expression '+' expression       { $$ = binOpToFunCall(mod, addSymbol(), $1, $3); }
      | expression '-' expression       { $$ = binOpToFunCall(mod, subSymbol(), $1, $3); }
      | expression '*' expression       { $$ = binOpToFunCall(mod, mulSymbol(), $1, $3); }
      | expression '/' expression       { $$ = binOpToFunCall(mod, divSymbol(), $1, $3); }
      | expression '%' expression       { $$ = binOpToFunCall(mod, modSymbol(), $1, $3); }
      | expression POW expression       { $$ = binOpToFunCall(mod, powSymbol(), $1, $3); }
      ;

look_up : symbol '.' symbol         { $$ = makeAstLookup(mod, $1, newAstExpression_Symbol(PIM(mod), $3)); }
        ;

expressions : %empty                        { $$ = NULL; }
            | expression                    { $$ = newAstExpressions(PIM(mod), $1, NULL); }
            | expression ',' expressions    { $$ = newAstExpressions(PIM(mod), $1, $3); }
            ;

conslist : %empty                   {
                                        $$ = newAstFunCall(PIM(mod),
                                            newAstExpression_Symbol(PIM(mod), nilSymbol()),
                                            NULL);
                                    }
         | expression               {
                                        $$ = binOpToFunCall(mod,
                                            consSymbol(),
                                            $1,
                                            newAstExpression_Symbol(PIM(mod), nilSymbol()));
                                    }
         | expression ',' conslist  {
                                        $$ = binOpToFunCall(mod,
                                            consSymbol(),
                                            $1,
                                            newAstExpression_FunCall(PIM(mod), $3));
                                    }
         ;

expression_statements : expression optional_semicolon           { $$ = newAstExpressions(PIM(mod), $1, NULL); }
                      | expression ';' expression_statements    { $$ = newAstExpressions(PIM(mod), $1, $3); }
                      ;

optional_semicolon : %empty
                   | ';'
                   ;

symbol : VAR    { $$ = newSymbol($1); }
       ;

print : PRINT '(' expression ')' { $$ = newAstPrint(PIM(mod), $3); }
      ;

%%
void yyerror (yyscan_t *locp, PmModule *mod, char const *msg) {
    fprintf(errout, "%s\n", msg);
    if (mod && mod->bufStack) {
        showModuleState(errout, mod);
    }
    abort();
}

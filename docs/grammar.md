# Grammar

For my own reference/sanity, no way near correct yet.

```bnf
top : expression ';'
    | definition ';'
    | construct
    | EOF

construct : conditional_nest
          | switch
          | defn
          | typedef
          | prototype
          | denv

conditional_nest : conditional
                 | nest

conditional : IF '(' expression ')' nest ELSE conditional_nest

switch : SWITCH '(' expressions ')' composite_function

defn : FN symbol function
     | FN symbol composite_function

denv : ENV symbol extension nest

extension : empty
          | EXTENDS package

nest : '{' statements '}'

composite_function : '{' functions '}'

functions : function
          | functions function

function : arguments nest

arguments : '(' arglist ')'

arglist : arg
        | arglist ',' arg

arg : arg_2 '@' arg
    | arg_2
    | symbol '=' arg

arg_2 : '[' ']'
      | '[' arglist ']'
      | arg_3

arg_3 : symbol
      | symbol arguments
      | symbol ':' symbol
      | NUMBER
      | STRING
      | CHAR
      | BOOLEAN
      | WILDCARD

statements : expression
           | expression ';' statements
           | definition
           | definition ';' statements
           | construct
           | construct statements
           | empty

expression : binop_and THEN expression
           | binop_and

binop_and : unop_not
          | unop_not AND binop_and
          | unop_not OR binop_and
          | unop_not XOR binop_and

unop_not : NOT unop_not
         | binop_compare

binop_compare : binop_cons EQ binop_cons
              | binop_cons NE binop_cons
              | binop_cons GT binop_cons
              | binop_cons LT binop_cons
              | binop_cons GE binop_cons
              | binop_cons LE binop_cons
              | binop_cons

binop_cons : binop_add CONS binop_cons
           | binop_add APPEND binop_cons
           | binop_add

binop_add : binop_mul '+' binop_mul
          | binop_mul '-' binop_mul
          | binop_mul

binop_mul : op_funcall '*' op_funcall
          | op_funcall '/' op_funcall
          | op_funcall '%' op_funcall
          | op_funcall

op_funcall : env_access '(' expressions ')'
           | env_access

env_access : atom
           | env_access '.' atom

atom : symbol
     | NUMBER
     | STRING
     | CHAR
     | BOOLEAN
     | NOTHING
     | lst
     | FN function
     | FN composite_function
     | env
     | BACK
     | SPAWN
     | conditional
     | switch
     | '(' expression ')'

load : LOAD package [AS symbol]

package : symbol
        | package '.' symbol

import_def : IMPORT symbol OPERATORS ';'
        | IMPORT symbol PREFIX string ';'
        | IMPORT symbol INFIX  string ';'
        | IMPORT symbol POSTFIX string ';'

export_def : EXPORT OPERATORS ';'
        | EXPORT PREFIX string ';'
        | EXPORT INFIX  string ';'
        | EXPORT POSTFIX string ';'

definition : define
           | load
           | import_def
           | export_def

define : symbol '=' expression

expressions : expression
            | expressions ',' expression

env : ENV extension nest

symbol : ID

typedef : TYPEDEF flat_type '{' type_body '}'

flat_type : symbol
          | symbol '(' type_symbols ')'

type_body : type_constructor
          | type_body '|' type_constructor

type_constructor : symbol '(' type_list ')'
                 | symbol

type_list : type
          | type_list ',' type

type : type_clause
     | type '->' type_clause

type_clause : NOTHING
            | KW_LIST '(' type ')'
            | KW_INT
            | KW_CHAR
            | KW_BOOL
            | KW_STRING
            | TYPE_VAR
            | symbol '(' type_list ')'
            | '(' type ')'

prototype : PROTOTYPE symbol '{' prototype_body '}'

prototype_body : empty
               | single_prototype { prototype_body }

single_prototype : symbol ':' type ';'
                 | prototype

```

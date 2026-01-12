# Railroad Diagrams of F♮ Syntax

## Top Level Stuff

### top

```mermaid
flowchart LR
s(("●"))
e(("●"))
s --> nb([nestBody]) --> EOF --> e
```

* [nestBody](#nestbody)

### nest

```mermaid
flowchart LR
s(("●"))
e(("●"))
open["{"]
childNest([childNest])
close["}"]
s --> open --> childNest --> close --> e
```

* [childNest](#childnest)

### nestBody

```mermaid
flowchart LR
s(("●"))
e(("●"))
let[let]
namespace[namespace]
namespace_definitions([definitions])
statements([statements])
let_definitions([definitions]) --> in[in] --> let_statements([statements])
s --> let --> let_definitions
let_statements --> e
s --> namespace --> namespace_definitions --> e
s --> statements --> e
```

* [definitions](#definitions)
* [statements](#statements)

### childNest

```mermaid
flowchart LR
s(("●"))
e(("●"))
subgraph child
nb([nestBody])
end
s --> nb --> e
```

Create a child parser to parse the nest body, N.B. **not** used by `top`.

* [nestBody](#nestbody)

### definitions

```mermaid
flowchart LR
s(("●"))
e(("●"))
definition([definition])
semi[";"]
definitions([definitions])
s --> definition
definition --> semi
semi --> semi
semi --> definitions
definition --> definitions
definitions --> e
s --> e
```

* [definition](#definition)
* [definitions](#definitions)

### statements

```mermaid
flowchart LR
s(("●"))
e(("●"))
expression([expression])
semi[";"]
statements([statements])
s --> expression
expression --> semi
semi --> semi
semi --> statements
expression --> statements
statements --> e
s --> e
```

* [expression](#expression)
* [statements](#statements)

### definition

```mermaid
flowchart LR
s(("●"))
e(("●"))
s --> builtins --> e
s --> atom{atom} --> assignment([assignment]) --> e
s --> tuple["#("] --> multiDefinition([multiDefinition]) --> e
s --> typedef --> typeDefinition([typeDefinition]) --> e
s --> unsafe --> fn --> defun([defun]) --> e
s --> fn
s --> print --> defun
s --> macro --> defMacro([defMacro]) --> e
s --> link --> lnk([link]) --> e
s --> alias --> als([alias]) --> e
s --> import --> importOp([importOp]) --> e
s --> export --> exportOp([exportOp]) --> e
s --> operator --> op([operator]) --> e
s --> else[[else]] --> error(error) --> e
```

* [assignment](#assignment)
* [multiDefinition](#multidefinition)
* [typeDefinition](#typedefinition)
* [defun](#defun)
* [defMacro](#defmacro)
* [link](#link)
* [alias](#alias)
* [importOp](#importop)
* [exportOp](#exportop)
* [operator](#operator)

### expression

```mermaid
flowchart LR
s(("●"))
e(("●"))
s --> ep[/expressionPrecedence\] --> synchronize(synchronize) --> e
```

`expressionPrecedence` is the core Pratt Parser algorithm, and `synchronize` attempts to
resume parsing in the case of an error.
`expression` calls `expressionPrecedence` with the lowest precedence (zero).

N.B. `expression` is also used to parse the formal arguments of functions.

* [expressionPrecedence](#expressionprecedence)

### assignment

```mermaid
flowchart LR
s(("●"))
e(("●"))
s --> symbol([symbol]) --> assign["="] --> expression([expression]) --> e
```

* [expression](#expression)

### multiDefinition

```mermaid
flowchart LR
s(("●"))
e(("●"))
s --> symbolList([symbolList]) --> assign["="] --> expression([expression]) --> e
```

 The `#(` has already been consumed before entering this routine, and `symbolList`
 consumes the matching close `)`.

* [symbolList](#symbollist)
* [expression](#expression)

## Types

### typeDefinition

```mermaid
flowchart LR
s(("●"))
e(("●"))
s --> symbol([symbol])
symbol --> open["("] --> typeVariables([typeVariables]) --> close[")"] --> lcurly["{"]
symbol --> lcurly
lcurly --> typeBody([typeBody]) --> rcurly["}"] --> e
```

* [typeVariables](#typevariables)
* [typeBody](#typebody)

### typeVariables

```mermaid
flowchart LR
s(("●"))
e(("●"))
s --> tv([typeVariable])
tv --> close[")"] --> e
tv --> comma[","] --> close
comma --> tvs([typeVariables]) --> e
```

* [typeVariable](#typevariable)
* [typeVariables](#typevariables)

### typeVariable

```mermaid
flowchart LR
s(("●"))
e(("●"))
s --> hash["#"] --> symbol([symbol]) --> e
```

### typeBody

```mermaid
flowchart LR
s(("●"))
e(("●"))
s --> tc([typeConstructor]) --> e
tc --> pipe["|"] --> tb([typeBody]) --> e
```

* [typeConstructor](#typeconstructor)
* [typeBody](#typebody)

### typeConstructor

```mermaid
flowchart LR
s(("●"))
e(("●"))
s --> symbol([symbol])
symbol --> open["("] --> tl([typeList]) --> close[")"] --> e
symbol --> lcurly["{"] --> tm([typeMap]) --> rcurly["}"] --> e
```

* [typeList](#typelist)
* [typeMap](#typemap)

### typeList

```mermaid
flowchart LR
s(("●"))
e(("●"))
s --> tt([typeType])
tt --> comma[","]
comma --> e
comma --> tl([typeList]) --> e
tt --> e
```

* [typeType](#typetype)
* [typeList](#typelist)

### typeType

```mermaid
flowchart LR
s(("●"))
e(("●"))
s --> open["("] --> tt([typeType]) --> close[")"] --> e
s --> tc([typeClause])
tc --> e
tc --> arrow["->"] --> tt2([typeType]) --> e
```

* [typeClause](#typeclause)
* [typeType](#typetype)

### typeClause

```mermaid
flowchart LR
s(("●"))
e(("●"))
s --> number --> e
s --> char --> e
s --> hash{"#"} --> tv([typeVariable]) --> e
s --> atom{atom} --> fn([typeFunction]) --> e
s --> tuple{"#("} --> tt([typeTuple]) --> e
s --> else[[else]] --> error(error) --> e
```

* [typeVariable](#typevariable)
* [typeFunction](#typefunction)
* [typeTuple](#typetuple)

### typeFunction

```mermaid
flowchart LR
s(("●"))
e(("●"))
s --> ss([scopedSymbol])
ss --> e
ss --> open["("] --> tl([typeList]) --> close[")"] --> e
```

* [scopedSymbol](#scopedsymbol)
* [typeList](#typelist)

### typeMap

```mermaid
flowchart LR
s(("●"))
e(("●"))
s --> symbol([symbol]) --> colon(":") --> tt([typeType])
tt --> comma[","]
comma --> tm([typeMap]) --> e
comma --> e
```

* [typeType](#typetype)
* [typeMap](#typemap)

### typeTuple

```mermaid
flowchart LR
s(("●"))
e(("●"))
s --> tuple["#("] --> typeList([typeList]) --> close[")"] --> e
```

* [typeList](#typelist)

## Other Bits

### scopedSymbol

```mermaid
flowchart LR
s(("●"))
e(("●"))
s --> s1([symbol])
s1 --> e
s1 --> period["."] --> s2([symbol]) --> e
```

### defun

```mermaid
flowchart LR
s(("●"))
e(("●"))
s --> symbol([symbol]) --> cf([compositeFunction]) --> e
```

The `fn` token has already been consumed. There is more going on here than is apparent in the diagram since
the `defun` parser is being passed extra arguments regarding the type of definition (`unsafe` or `print` etc.)

* [compositeFunction](#compositefunction)

### compositeFunction

```mermaid
flowchart LR
s(("●"))
e(("●"))
s --> lcurly["{"] --> functions([functions]) --> rcurly["}"] --> e
s --> af([altFunction]) --> e
```

* [functions](#functions)
* [altFunction](#altfunction)

### functions

```mermaid
flowchart LR
s(("●"))
e(("●"))
s --> af([altFunction])
af --> open{"("} --> fns([functions]) --> e
af --> e
```

* [functions](#functions)
* [altFunction](#altfunction)

### altFunction

```mermaid
flowchart LR
s(("●"))
e(("●"))
s --> altArgs([altArgs]) --> nest([nest]) --> e
```

* [altArgs](#altargs)
* [nest](#nest)

### altArgs

```mermaid
flowchart LR
s(("●"))
e(("●"))
s --> fargs([fargs])
fargs --> pipe["|"] --> aa([altArgs]) --> e
fargs --> e
```

* [fargs](#fargs)
* [altArgs](#altargs)

### fargs

```mermaid
flowchart LR
s(("●"))
e(("●"))
s --> open["("] --> expressions([expressions]) --> close[")"] --> e
```

* [expressions](#expressions)

### expressions

```mermaid
flowchart LR
s(("●"))
e(("●"))
s --> ca([collectArguments]) --> e
s --> e
```

* [collectArguments](#collectarguments)

### collectArguments

```mermaid
flowchart LR
s(("●"))
e(("●"))
s --> ep([expression])
ep --> comma[","]
comma --> e
comma --> ca([collectArguments]) --> e
ep --> e
```

* [collectArguments](#collectarguments)
* [expression](#expression)

### defMacro

```mermaid
flowchart LR
s(("●"))
e(("●"))
s --> symbol([symbol]) --> af([altFunction]) --> e
```

The `macro` token has already been consumed.
The `defMacro` parser only recognises a single `altFunction` (alternative function body of a composite function).

* [altFunction](#altfunction)

### link

```mermaid
flowchart LR
s(("●"))
e(("●"))
s --> rawString([rawString]) --> as --> symbol([symbol]) --> e
```

### alias

```mermaid
flowchart LR
s(("●"))
e(("●"))
s --> symbol([symbol]) --> assign["="] --> typeType([typeType]) --> e
```

* [typeType](#typetype)

### importOp

```mermaid
flowchart LR
s(("●"))
e(("●"))
s --> symbol([symbol])
symbol --> operators --> e
symbol --> operator --> rs([rawString]) --> e
```

### exportOp

```mermaid
flowchart LR
s(("●"))
e(("●"))
s --> operators --> e
s --> operator --> str([rawString]) --> owp([operatorWithPattern]) --> e
```

* [operatorWithPattern](#operatorwithpattern)

### operator

```mermaid
flowchart LR
s(("●"))
e(("●"))
s --> str([rawString]) --> owp([operatorWithPattern]) --> e
```

* [operatorWithPattern](#operatorwithpattern)

### operatorWithPattern

```mermaid
flowchart LR
s(("●"))
e(("●"))
s --> number[NUMBER] --> expr([expression]) --> e
```

* [expression](#expression)

### symbolList

```mermaid
flowchart LR
s(("●"))
e(("●"))
s --> close[")"] --> e
s --> ATOM
ATOM --> comma[","] --> sl([symbolList]) --> e
ATOM --> close
```

The opening `(`  or `#(` has already been consumed.

* [symbolList](#symbollist)

## expressionPrecedence

This is different from the rest of the parser in that it is table-driven. Entries
in the table are tokens associated with prefix, infix and postfix "parselets"
that handle the specifics of the operator.

This is a precis of that table with links to the relevant parselets. There are no
built-in postfix operators so that column is omitted, as are entries where there
are no parselets. Parselets which do no further parsing, such as `makeAtom`, are present
but do not link to a railroad diagram.

Note in the following railroad diagrams many of the parselets call `expressionPrecedence`
directly because they are passing the associated precedence from the table (not shown).

| Token | Prefix | Infix |
| ----- | ------ | ----- |
| `->` | | [infixRight](#infixright) |
| `assert` | [pAssert](#passert) | |
| `=` | | [exprAlias](#expralias) |
| atom | makeAtom | |
| `back` | back | |
| char | makeChar | |
| `#` | [doPrefix](#doprefix) | |
| `if` | [iff](#iff) | |
| `error` | [error](#error) | |
| `{` | [nestExpr](#nestexpr) | [makeStruct](#makestruct) |
| `[` | [list](#list) | |
| `macro` | [macro](#macro) | |
| number | makeNumber | |
| `(` | [grouping](#grouping) | [call](#call) |
| `.` | | [lookUp](#lookup) |
| `print` | [print](#print) | |
| string | makeString | |
| `switch` | [switchExp](#switchexp) | |
| `#(` | [tuple](#tuple) | |
| `typeof` | [typeOfExp](#typeofexp) | |
| `unsafe` | [unsafe](#unsafe) | |
| `_` | wildCard | |

### infixRight

```mermaid
flowchart LR
s(("●"))
e(("●"))
subgraph context
    lhs([expression]) --> arrow["->"]
end
arrow --> s --> ep[/expressionPrecedence\] --> e
```

* [expressionPrecedence](#expressionprecedence)

### pAssert

```mermaid
flowchart LR
s(("●"))
e(("●"))
subgraph context
assert
end
assert --> s --> ep[/expressionPrecedence\] --> e
```

* [expressionPrecedence](#expressionprecedence)

### exprAlias

```mermaid
flowchart LR
s(("●"))
e(("●"))
subgraph context
sy([symbol]) --> eq["="]
end
eq --> s --> ep[/expressionPrecedence\] --> e
```

* [expressionPrecedence](#expressionprecedence)

### doPrefix

```mermaid
flowchart LR
s(("●"))
e(("●"))
subgraph context
hash["#"]
end
hash --> s --> ep[/expressionPrecedence\] --> e
```

This looks like a mistake, but it may be because formal arguments are parsed as
expressions then translated. However it does parse when it shouldn't:

```text
$ bin/fn -e 'let a = 2 in print #a'
undefined variable # in command-line, line 1

unification failed [type mismatch]
unknown:# vs number -> #vczb
while analyzing apply # (type: unknown:#) to a (type: number)
in command-line, line 1
```

* [expressionPrecedence](#expressionprecedence)

### iff

```mermaid
flowchart LR
s(("●"))
e(("●"))
subgraph context
if0["if"]
end
if0 --> s --> open["("] --> test([expression]) --> close[")"]
--> cons([nest]) --> else["else"]
else --> alt([nest]) --> e
else --> if --> iff([iff]) --> e
```

* [expression](#expression)
* [nest](#nest)
* [iff](#iff)

### error

```mermaid
flowchart LR
s(("●"))
e(("●"))
subgraph context
error
end
error --> s --> ep[/expressionPrecedence\] --> e
```

* [expressionPrecedence](#expressionprecedence)

### nestExpr

```mermaid
flowchart LR
s(("●"))
e(("●"))
subgraph context
lcurly["{"]
end
body([childNest]) --> rcurly["}"]
lcurly --> s --> body
rcurly --> e
```

* [childNest](#childnest)

### makeStruct

```mermaid
flowchart LR
s(("●"))
e(("●"))
subgraph context
    sy([symbol]) --> lcurly["{"]
end
lcurly --> s --> te([taggedExpressions]) --> rcurly["}"] --> e
```

* [taggedExpressions](#taggedexpressions)

### list

```mermaid
flowchart LR
s(("●"))
e(("●"))
subgraph context
lsq["["]
end
lsq --> s --> cons([consList]) --> e
```

Slightly redundant but `consList` is recursive and takes fewer arguments
than the `list` parselet.

* [consList](#conslist)

### consList

```mermaid
flowchart LR
s(("●"))
e(("●"))
s --> rsq["]"] --> e
s --> expr([expression])
expr --> comma[","] --> cl([consList]) --> e
expr --> rsq
```

* [expression](#expression)
* [consList](#conslist)

### taggedExpressions

```mermaid
flowchart LR
s(("●"))
e(("●"))
s --> symbol([symbol]) --> colon[":"] --> ep([expression])
ep --> comma[","]
comma --> e
comma --> te([taggedExpressions]) --> e
ep --> e
```

* [expression](#expression)
* [taggedExpressions](#taggedexpressions)

### macro

```mermaid
flowchart LR
s(("●"))
e(("●"))
subgraph context
macro
end
macro --> s --> error(error) --> e
```

Explicit dissalow on creating macros as expressions.

### grouping

```mermaid
flowchart LR
s(("●"))
e(("●"))
subgraph context
open["("]
end
open --> s --> ep([expression]) --> close[")"] --> e
```

* [expression](#expression)

### call

```mermaid
flowchart LR
s(("●"))
e(("●"))
subgraph context
ex([expression]) --> open["("]
end
open --> s --> exps([expressions]) --> close[")"] --> e
```

* [expression](#expression)
* [expressions](#expressions)

### lookUp

```mermaid
flowchart LR
s(("●"))
e(("●"))
subgraph context
sy([symbol]) --> dot["."]
end
dot --> s --> ep[/expressionPrecedence\] --> e
```

* [expressionPrecedence](#expressionprecedence)

### print

```mermaid
flowchart LR
s(("●"))
e(("●"))
subgraph context
print
end
print --> s --> ep[/expressionPrecedence\] --> e
```

* [expressionPrecedence](#expressionprecedence)

### switchExp

```mermaid
flowchart LR
s(("●"))
e(("●"))
subgraph context
switch
end
switch --> s --> sfc([switchFC]) --> e
```

Although this looks redundant, `switchFC` is shared with other parselets, and
`switchExp` wraps the result in a different type.

* [switchFC](#switchfc)

### switchFC

```mermaid
flowchart LR
s(("●"))
e(("●"))
s --> open["("] --> exprs([expressions]) --> close[")"]
--> cf([compositeFunction]) --> e
```

* [expressions](#expressions)
* [compositeFunction](#compositefunction)

### tuple

```mermaid
flowchart LR
s(("●"))
e(("●"))
subgraph context
tuple["#("]
end
tuple --> s --> exprs([expressions]) --> close[")"] --> e
```

* [expressions](#expressions)

### typeOfExp

```mermaid
flowchart LR
s(("●"))
e(("●"))
subgraph context
typeof
end
typeof --> s --> ep[/expressionPrecedence\] --> e
```

* [expressionPrecedence](#expressionprecedence)

### unsafe

```mermaid
flowchart LR
s(("●"))
e(("●"))
subgraph context
unsafe
end
unsafe --> s
s --> fn --> cf([compositeFunction]) --> e
s --> switch --> sw([switchFC]) --> e
```

* [compositeFunction](#compositefunction)
* [switchFC](#switchfc)

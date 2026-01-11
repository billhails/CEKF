# Railroad Diagrams of F♮ Syntax

## Top Level Stuff

### nest

```mermaid
flowchart LR
s(("●"))
e(("●"))
open["{"]
subgraph child
    nestBody([nestBody])
end
close["}"]
s --> open --> nestBody --> close --> e
```

* [nestBody](#nestbody)

### nestBody

```mermaid
flowchart LR
s(("●"))
e(("●"))
let[let]
namespace[namespace]
namespace_definitions([definitions])
statements([statements])
subgraph child
    let_definitions([definitions]) --> in[in] --> let_statements([statements])
end
s --> let --> let_definitions
let_statements --> e
s --> namespace --> namespace_definitions --> e
s --> statements --> e
```

* [definitions](#definitions)
* [statements](#statements)

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
```

* [collectArguments](#collectarguments)

### collectArguments

```mermaid
flowchart LR
s(("●"))
e(("●"))
s --> ep[/expressionPrecedence\]
ep --> comma[","]
comma --> e
comma --> ca([collectArguments]) --> e
ep --> e
```

* [collectArguments](#collectarguments)

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

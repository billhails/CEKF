# Proposed `struct` type

`typedef`s are fine for most use cases but if there are more than just
a few fields they can get a little awkward, so it would be useful to,
in certain circumstances, be able to name the fields.

It would be relatively easy to have a separate struct declaration, like

```
struct named_list(#t) { name: string, values: list(#t) }
```

where `named_list` is both the type function and the type constructor. I
like the use of curlies to delimit structs as it follows well-established
conventions in C-like languages. A second advantage is that if the struct
is it's own type, we can safely unpack its values, something like:

```
struct customer { name: string address: list(string) }

x = customer{ name: "Joe", address: ["1", "somestreet", "sometown"] };;

print(x:name);
```

I don't know how feasible that would be if the struct was just part of
some normal typedef.

On the other hand, allowing structs as alternative type constructors in
normal typedefs has its advantages too, it would allow pattern matching
on those named fields:

```
fn find_address {
    ([], _) { maybe.nothing }
    (customer{ name: x, address: y } @ _, x) { maybe.some(y) }
    (_ @ t, y) { find_address(t, y) }
}
```

Actually that example would work in either case but you get the idea.

There should be no reason to have to name all the fields when unpacking,
the TPMC should be able to provide wildcards for any missing fields, and
to restore canonical order if the fields are not in their declared order.

The alternative then is to allow type constructors in a typedef to
be structs:

```
typedef employee {
    worker{ name: string, id: int } |
    manager{ name: string, id: int, reports: list(employee) }
}
```

There's no reason I can think of that we can't freely mix normal type
constructors with these new structs in a single typedef.

Of course we may end up with both structs and structs in typedefs,
but I want to limit the scope for a first implementation and pick
the more useful option, so I'm leaning towards structs in typedefs.

## Implementation

By the time we reach The ANF conversion stage, or even the constructor
inlining stage, I'd expect the structs to have been subsumed into the
normal type constructors, with the named fields replaced by normal integer
based deconstructors, so the stages that wil require modification are:

* Parser
* Lambda Conversion
* Print Generator
* TPMC
* Type Checking
* Print Compiler
* Constructor Inlining (maybe)

### Parser

As of writing the section of the parser grammar dealing with types is
as follows:

```
typedef : TYPEDEF user_type '{' type_body '}'
        ;

user_type : symbol
          | symbol '(' type_symbols ')'
          ;

type_symbols : type_symbol
             | type_symbol ',' type_symbols
             ;

type_symbol : TYPE_VAR
            ;

type_body : type_constructor
          | type_constructor '|' type_body
          ;

type_constructor : symbol
                 | symbol '(' type_list ')'
                 ;

type_function : scoped_symbol
              | scoped_symbol '(' type_list ')'
              ;

scoped_symbol : symbol
              | symbol '.' symbol
              ;

type_list : type
          | type ',' type_list
          ;

type : type_clause
     | type_clause ARROW type
     | '(' type ')'
     ;

type_tuple : '#' '(' type_list ')'
           ;

type_clause : KW_INT
            | KW_CHAR
            | type_symbol
            | type_function
            | type_tuple
            ;
```

So `type_constructor` will need an extra clause for the new struct

```
type_constructor : symbol
                 | symbol '(' type_list ')'
                 | symbol '{' type_map '}'
                 ;
```

we'll need to define that `type_map`

```
type_map : tagged_type
         | tagged_type ',' type_map
         ;
```

and define `tagged_type`

```
tagged_type : symbol ':' type
            ;
```

The `AstTypeConstructor` defined in `ast.yaml` will probably need its
`typeList: AstTypeList` field replaced with a discriminated union of
`AstTypeList` and a new `AstTypeMap` type that has an additional slot
for the symbol associated with each type. We'll need to preserve the
order of the declarations so a hash is probably not appropriate here.

The other section of the parser that will need extending is the pattern
matching.

```
fargs : %empty
      | farg
      | farg ',' fargs
      ;

unpack : scoped_symbol '(' fargs ')'
       ;
```

Not sure why `fargs` is allowed to be empty, probably an error to fix.
Anyway we'll need an extra clause for `unpack`

```
unpack : scoped_symbol '(' fargs ')'
       | scoped_symbol '{' fargs_map '}'
       ;
```

(An LALR1 parser like Bison might not like this). Anyway we'll also need
that `fargs_map`

```
fargs_map : tagged_farg
          | tagged_farg ',' fargs_map
          ;
```

and `tagged_farg`

```
tagged_farg : symbol ':' farg
            ;
```

`farg` is then unchanged. Similar changes and additions to the AST will
also be required.

Lastly we'll need to extend expressions to allow these new type
constructors to be invoked. It would be nice if we could curry them as
we do with normal constructors, but that could open a can of worms so at
least as a first attempt I'll say that the application of a constructor
is limited to something like

```
struct_application : scoped_symbol '{' tagged_expressions '}'
                   ;
```

### Lambda Conversion

I'm thinking it may be just as easy to make the parser changes
incrementally (just the types first) then see what braks downstream,
rather than trying to guess and plan ahead to this extent.

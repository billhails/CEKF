# Proposed Change to Syntax Declarations

See other `SYNTAX*.md` for context.

Before the syntax system gets too entrenched, I would like to make a change to
how syntax declarations are written.

The goal is only to restore a visible distinction between public entry rules
and helper rules. It is not to change the parser boundary between expression-
initiating syntax, definition-initiating syntax, and helper-only syntax.

I would like to:

1. Reinstate the `macro` keyword to declare initiating syntax.
2. Retain the `syntax` keyword for helper syntax.
3. Simplify `macro` so that its unquoted name is also the initiating token,
   and its body is restricted to the identifier of a single helper syntax
   declaration.

This is a restriction on the surface form of initiating declarations, not a
change in their behaviour.

## Intended Surface

The initiating kind marker remains part of the declaration:

- `macro name: Expr helper;` introduces expression-initiating syntax.
- `macro name: Def helper;` introduces definition-initiating syntax.
- `syntax helper ::= ...;` declares a helper-only rule.

So this proposal keeps the existing `Expr` versus `Def` distinction. The only
surface change is that initiating syntax is written with `macro` rather than as
an initiating form of `syntax`.

## Validity Rules

The helper named by a `macro` declaration must satisfy all of the following:

1. It must resolve to a `syntax` declaration, not another `macro`.
2. It must be helper-only.
3. It must have zero parameters.

That means a `macro` can only name a helper rule that can be entered without
inherited arguments.

Example, currently we have:

```fn
syntax internalLco: Expr ::= "lco" "["
    exp: Expr
    "for" x: Name
    "in" xs: Expr
    filters: Syntax(whereClauses(x, xs))
"]" {
    list.map(fn (x) { exp }, filters)
};

syntax whereClauses(x, xs) ::= { xs }
                            | "where" cond: Expr
                                rest: Syntax(moreFilters(x, xs)) {
    list.filter(fn (x) { cond }, rest)
};

syntax moreFilters(x, xs) ::= { xs }
| "," cond: Expr rest: Syntax(moreFilters(x, xs)) {
    list.filter(fn (x) { cond }, rest)
};
```

That would become:

```fn
macro lco: Expr internalLco;

syntax internalLco ::= "["
    exp: Expr
    "for" x: Name
    "in" xs: Expr
    filters: Syntax(whereClauses(x, xs))
"]" {
    list.map(fn (x) { exp }, filters)
};

syntax whereClauses(x, xs) ::= { xs }
                            | "where" cond: Expr
                                rest: Syntax(moreFilters(x, xs)) {
    list.filter(fn (x) { cond }, rest)
};

syntax moreFilters(x, xs) ::= { xs }
| "," cond: Expr rest: Syntax(moreFilters(x, xs)) {
    list.filter(fn (x) { cond }, rest)
};
```

Notes:

1. Since the `macro` declaration does not and cannot capture any tokens
    itself, the helper syntax it invokes cannot take argument parameters.
2. The `Expr` qualifier is retained. The same surface should work for
    definition-initiating syntax via `macro name: Def helper;`.
3. This proposal intentionally makes the public entry rule visually distinct
    from the private helper rule that owns the full pattern.

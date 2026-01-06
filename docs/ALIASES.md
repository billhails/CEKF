# Aliases

Aliases will be shorthand for complex declarations. Simple examples might be

```fn
alias string = list(char);
alias some = maybe.some; // import
alias boolfn = bool -> bool -> bool;
alias arrayElement(#t) = #(int, #t); // assuming array elements are tuples
```

Ideally they will be completely handled by lambda conversion which will
translate them in to their underlying forms recursively, before any printer
generation or TPMC stage.

The above examples may not all work as the parser won't have enough
context to recognise the various types.

Because of the recursive transformation loops will need to be detected.

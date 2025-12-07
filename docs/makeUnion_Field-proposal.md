# Proposed Extension to the Code Generator

Let's use a concrete example to explain.

In `src/lambda.yaml` is defined a "union" called `LamExp` In fact unions declared like this are  discriminated unions with an `int` type and `union` value.  `LamExp` is a discriminated union of all the possible types of lambda expression. One such type is a `LamIff` struct representing a conditional with `test`, `consequent` and `alternative` fields, themselves instances of `LamExp`.

On encountering the `LamIff` declaration, the code generator will generate, among other things, a `newLamIff` procedure taking `test`, `consequent` and `alternative` arguments (as well as ParserInfo if required).

On encountering the `LamExp` declaration, the code generator will generate the expected `newLamExp` procedure taking ParserInfo I, type flag and field value. However it will also iterate over its fields and generate utility functions i.e. `Lamexp *newLamexp_Iff(ParserInfo, LamIff *)` which supply the correct type flag automatically. The `_Iff` suffix is derived from the field name.

I've noticed a very common use-pattern for these generated constructors, i.e.

```C
    LamIff *iff = newLamIff(PI, test, consequent, alternative);
    PROTECT(iff);
    LamExp *iffExp = newLamExp_Iff(PI, iff);
    PROTECT(iffExp);
```

The proposal is to generate a shorthand for this pattern via a `makeLamExp_Iff` and equivalents:

```C
static inline LamExp *makeLamExp_Iff(ParserInfo PI,
                                     LamExp *test,
                                     LamExp *consequent,
                                     LamExp *alternative) {
    LamIff *x1 = newLamIff(PI, test, consequent, alternative);
    int save = PROTECT(x1);
    LamExp *x2 = newLamExp_Iff(PI, x1);
    UNPROTECT(save);
    return x2;
}
```

Making it `static inline` means we only need declare it in the header.

Things to bear in mind:
* Not all fields of a discriminated union are pointers though, for those scalar and void types we don't need to create a `make<Union>_<Field>` declaration.
* There will almost certainly be conflicts with some existing hand-written implementations of this pattern that can be removed/replaced.
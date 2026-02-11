# Common Generated Utilities Proposal

## Requirement

Among many of the structures defined in the various `src/*.yaml` files there is a good deal of duplication of common types such as sets (hashes with no values), hashes from symbols to various primitives, arrays and vectors of integers, wchar_t, char etc. It would be more efficient to share common declarations of these basic types in a `src/utils.yaml` or similar.

## Naive Solution

Just create the declarations in `utils.yaml` and refer to them from other packages in their `external` sections.

## Problems to Solve

There is a convenience around objects that are identified as "self-initializing"
(`isSelfInitializing` in the generate package). They are objects whose constructors take no arguments. Generated constructors for other objects that
contain self-initializing objects can just create them directly, rather than
taking them as argument. This reduces unnecessary code.

In order to do that the objects self-identify as self-initializing.

If we are to incorporate them into other packages, the other packages would also like to know if they are self identifying, but the `external` section contains only
simple text (the C name of the object, the name of its print function etc.)

## Proposed Solution

Extend the `external` declarations to add a `newFn` field that both provides the name of the constructor and identifies the object as self-initializing.

## Implementation

Add an `isSelfInitilizing` override to `primitives` and have it return `True` if the type has a `newFn` (primitives are synonymous with "external"). Store the `newFn` too and make it available via a `getConstructorName` method.

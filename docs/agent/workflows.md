# Workflows and Cross-Cutting Concerns

This document describes how to implement features that require changes across multiple compiler stages.

## Error Handling

The compiler uses two distinct mechanisms for reporting errors depending on their nature:

1. **User Errors** (`can_happen`):
    * Triggered by valid but incorrect user input (syntax errors, type errors).
    * **Signature**: `void can_happen(ParserInfo I, const char *message, ...)`
    * **Usage**:
      * With location: `can_happen(I, "error message", args...)` or `can_happen(CPI(struct), "error message", args...)`
      * Without location: `can_happen(NULLPI, "error message", args...)` where NULLPI is a macro for when ParserInfo is not available
    * **Behavior**: These functions do not abort. They print an error message with source location (if ParserInfo.lineNo != 0), set a global `errors` flag, then return to the caller.
    * **Location formatting**: Automatically adds " at +{lineNo} {fileName}" if lineNo is non-zero
    * **IMPORTANT**: Since execution continues, the calling code MUST return a valid (dummy) value to prevent crashing the current stage. The pipeline will check the `errors` flag after the stage completes and `exit(1)` if set.
    * **NULLPI macro**: Defined in common.h as `((ParserInfo){.lineNo = 0, .fileName = NULL})` for contexts where location info is unavailable (e.g., type unification)

2. **Internal Compiler Errors** (`cant_happen`):
    * Triggered by logic bugs in the compiler itself (e.g., reaching a "dead" switch case).
    * **Usage**: `cant_happen(fmt, args...)`.
    * Behavior: Prints the message with the file/line of the C source and calls `abort()` (if `DEBUG_DUMP_CORE` is on) or `exit(1)`.

## Adding a Built-in Function

To add a new native function callable from F♮ code:

1. **Implement in C**: Add the implementation to `src/builtins_impl.c`.

    ```c
    // src/builtins_impl.c
    Value *builtin_myFunction(Value **args) {
        Value *v1 = args[0];
        // ... implementation ...
        return result;
    }
    ```

    *Note*: Runtime arguments are `Value*` (from `src/cekfs.yaml`). Use helper macros/functions to unbox integers, strings, etc.

2. **Declare the Implementation**: Add the declaration in `src/builtins_impl.h`.

```c
Value builtin_MyFunction(Vec *args);
```

1. **Create and invoke Registration Helper**: add the helper to `src/builtins_helper.c` and call it.

```c
static void registerMyFunction(BuiltIns *registry);

// in registerBuiltIns()
    ...
    registerMyFunction(res);
    ...

static void registerMyFunction(BuiltIns *registry) {
    BuiltInArgs *args = newBuiltInArgs();
    int save = PROTECT(args);
    TcType *integer = newTcType_BigInteger(); // or other return type as appropriate
    PROTECT(integer);
    pushCharacterArg(args); // or other argument type or types as appropriate
    pushNewBuiltIn(registry, "myfunction", integer, args, (void *)builtin_myFunction);
    UNPROTECT(save);
}
```

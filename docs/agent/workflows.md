# Workflows and Cross-Cutting Concerns

This document describes how to implement features that require changes across multiple compiler stages.

## Error Handling

The compiler uses two distinct mechanisms for reporting errors depending on their nature:

1. **User Errors** (`can_happen`):
    * Triggered by incorrect user input (syntax errors, type errors).
    * **Signature**: `void can_happen(ParserInfo I, const char *message, ...)`
    * **Usage**:
      * With location: `can_happen(I, "error message", args...)` or `can_happen(CPI(struct), "error message", args...)`
      * Without location: `can_happen(NULLPI, "error message", args...)` where NULLPI is a macro for when ParserInfo is not available.
    * **Behavior**: These functions do not abort. They print an error message with source location (if ParserInfo.lineNo != 0), set a global `errors` flag, then return to the caller.
    * **Location formatting**: Automatically adds " at +{lineNo} {fileName}" if lineNo is non-zero
    * **IMPORTANT**: Since execution continues, the calling code MUST return a valid (dummy) value to prevent crashing the current stage. The pipeline will check the `errors` flag after the stage completes and `exit(1)` if set.
    * **NULLPI macro**: Defined in common.h as `((ParserInfo){.lineNo = 0, .fileName = NULL})` for contexts where location info is unavailable (e.g., type unification)

2. **Internal Compiler Errors** (`cant_happen`):
    * Triggered by logic bugs in the compiler itself (e.g., reaching a "dead" switch case).
    * **Signature**: `void cant_happen(const char *message, ...)`
    * Behavior: Prints the message with the file/line of the C source and calls `abort()` (if `DEBUG_DUMP_CORE` is on) or `exit(1)`.

## Utilities

Check `utils_helper.[ch]` before implementing functions over common types defined in `utils.yaml`, if you do need to implement something, consider adding it to `utils_helper.[ch]` if it could be useful elsewhere.

## Root Shell Helpers (`utils.sh`)

There is a project-level shell helper file at `./utils.sh`. Agents working from the command line should be aware of it, and may source it when useful:

```bash
source ./utils.sh
```

Notable quality-of-life helpers include:

* `new_h <name>`: creates `src/<name>.h` with include guards and GPL header text.
* `new_c <name>`: creates `src/<name>.c` with GPL header text.
* `new_ch <name>`: creates matching `.c/.h` pair and adds `#include "<name>.h"` to the `.c` file.
* `new_visitor <stage> <suffix>`: creates `src/<stage>_<suffix>.h` and generates visitor boilerplate C from `src/<stage>.yaml`.

If an agent does not use these helpers directly, it should still follow the same creation patterns (especially GPL header insertion and header guard style) when creating new source files manually.

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

3. **Create and invoke Registration Helper**: add the helper to `src/builtins_helper.c` and call it.

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

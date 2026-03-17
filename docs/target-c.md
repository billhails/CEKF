# Target C as a Compilation Step

As a stopgap before targetting LLVM, but also as a useful exercise that
might inform the LLVM work, I'd like to attempt to generate a standalone
`main` function from the MinExp (`minlam.yaml`) structures after CPS
transformation and closure conversion.

This seems reasonable as CPS allows all function calls to be replaced
with `goto`, and the closure conversion means that all variable access
is local.

Also the recent AMB transform has "un-magic'd" the `amb` operator
to a plain failure continuation passed along with, and to, the CPS
continuation.

It is envisaged that lambdas will be stored in `Value`s as computed gotos:
`void *` pointers created with the gcc/clang `&&LABEL` syntax.

Before we can proceed, we need to decide how to represent, and where
to store, local variables. The "env" structures that are generated as
a result of closure-conversion have been desugared to simple `make-vec`
calls, and the dereferencing of their contents to primitive `VEC` indexed
O(1) lookups. Those envs in turn are either contained by other envs or
directly by local variables so it all comes down to how local variables
are handled.

I *think* that in turn devolves to the choice between a register machine
and a stack.

Once that decision is made, and the details are thrashed out, the next
step is likely to be a variable-annotation pass to convert variables
to locations. After that the C code generation should be fairly
straightforward.

## Update

After consultation, the decision is some sort of register machine, as
a stack is overkill for a known set of variables. The best solution
for performance is a set of named C variables allowing direct access,
but probably an array is more pragmatic as it can be memory-managed
more easily.

Annotation is implemented in `src/minlam_annotate.[ch]` and wired into
`main`.

## Next Steps

### Design the Code Layout

This is just thinking out loud at the moment, no commitment.
Also we're not addressing garbage collection yet, that will need
to be layered on top of this design.

#### `letrec`

```yaml
    MinLetRec:
        data:
            bindings: MinBindings
            body: MinExp

    MinBindings:
        data:
            var: HashSymbol
            val: MinExp
            arity: int=-1
            next: MinBindings
```

| LABEL | INSTRUCTION | Comment |
| --- | --- | --- |
| | `goto SETUP_LETREC_1;` | Jump over the bindings. |
| `LETREC_{FUNCNAME_1}:` | | Use binding names to keep it readable |
| | `...` | Whatever the binding code does |
| `LETREC_{FUNCNAME_2}:` | | |
| | `...` | |
| `SETUP_LETREC_1:` | | Assign the registers |
| | `reg[4] = &&LETREC_{FUNCNAME_1};` | may not start from 0 because of outer lambda bindings and letrecs |
| | `reg[5] = &&LETREC_{FUNCNAME_2};` | |
| | `...` | Body of the `letrec` |

Currently letrec-bound variables (and formal lambda arguments) are not
annotated, it would seem to make sense to annotate those too to make the
`SETUP_LETREC_n` section simpler to generate. However formal arguments
are a `SymbolList` not a `MinExprList` so we'd need to have a distinct
`AnnotatedLambda` type. Also `MinBindings` only hold `HashSymbols`.
Probably the simplest solution is to have the `VisitorContext` include the
current number of vars in use, reset to `countSymbolList(lambda->args)`
on entry to a lambda and incremented by the letrec (letrecs can nest).

On a related topic, a visitor should non-destructively walk the code using
that same logic, colleting the max registers in use for the top-level
`Value reg[MAX_REGISTERS];`.

#### `apply`

```yaml
    MinApply:
        data:
            function: MinExp
            args: MinExprList
            isBuiltin: bool=false
            cc: bool=false
```

| LABEL | INSTRUCTION | Comment |
| --- | --- | --- |
| | `{` | scope |
| | `Value tmp0 = ...;` | collect arguments |
| | `Value tmp1 = ...;` | |
| | `goto AFTERFN_<n>;` | jump over fn |
| | `...` | body of fn |
| `AFTERFN_<n>:` | | |
| | `Value function = ...;` | compute fn |
| | `reg[0] = tmp0;` | assign to registers |
| | `reg[1] = tmp1;` | |
| | `void *ptr = function.addr;` | new value type for computed gotos |
| | `goto *ptr;` | call the func |
| | `}` | scope |

### Concrete Example

```shell
bin/fn -e 'let fn add(a, b) { a + b } in add(2, 3)'
```

```scheme
(letrec
  (
    (add$0$arity_2
      (make-vec
        (λ (env$388 p$110$0 p$111$0 k$213 f$387)
          ((vec 0 k$213<3>) (vec 1 k$213<3>) (+ p$110$0<1> p$111$0<2>) f$387<4>))
        (make-vec)))
    (add$0
      (make-vec
        (λ (env$390 p$110$0 k$211 f$388)
          ((vec 0 k$211<2>) (vec 1 k$211<2>)
            (make-vec
              (λ (env$389 p$111$0 k$212 f$389)
                ((vec 0 k$212<2>) (vec 1 k$212<2>) (+ (vec 0 env$389<0>) p$111$0<1>) f$389<3>))
              (make-vec p$110$0<1>)) f$388<3>))
        (make-vec))))
  ((vec 0 add$0$arity_2<136>) ;; add
    (vec 1 add$0$arity_2<136>) ;; env
    2 ;; arg
    3 ;; arg
    (make-vec ;; halt
      (λ (env$392 k$0 f$390)
        (done))
      (make-vec))
    (make-vec ;; fail
      (λ (env$391)
        (done))
      (make-vec))))
```

AI-generated output

```c
Value reg[MAX_REGISTERS];

// ===== letrec bindings (the lambda bodies) =====

// --- add body: (λ (env$388<0> a<1> b<2> k<3> f<4>) ...) ---
LETREC_add_arity_2:
{
    // body: ((vec 0 k<3>) (vec 1 k<3>) (+ a<1> b<2>) f<4>)
    // This calls continuation k with (a + b), passing f along.
    //
    // k is a closure (2-vec) in reg[3].
    // The call target is k's code:   vec 0 of reg[3]
    // The call passes:
    //   arg 0 (env):    vec 1 of reg[3]   -- k's captured env
    //   arg 1 (result): a + b             -- the computed value!
    //   arg 2 (fail):   reg[4]            -- thread the failure cont through

    void *target = reg[3].vec[0];   // k's code label
    Value env    = reg[3].vec[1];   // k's env
    Value result = add(reg[1], reg[2]);  // a + b
    Value fail   = reg[4];

    reg[0] = env;       // callee's param 0: env
    reg[1] = result;    // callee's param 1: the answer (5)
    reg[2] = fail;      // callee's param 2: fail cont
    goto *target;       // jump to k's code
}

// --- halt continuation: (λ (env$392<0> k$0<1> f<2>) (done)) ---
HALT_CONT:
{
    // The program is finished. reg[1] holds the result value.
    // reg[0] is env (empty), reg[2] is fail (unused).
    printf("%d\n", reg[1].integer);  // or however done prints
    exit(0);
}

// --- fail continuation: (λ (env$391<0>) (done)) ---
FAIL_CONT:
{
    // Backtracking exhausted. No solution.
    // reg[0] is env (empty). No other args.
    exit(1);  // or however failure is signaled
}

// ===== letrec setup =====
SETUP_LETREC:
    // Build the closure for add$0$arity_2: [&&LETREC_add_arity_2, []]
    reg[ADD_REG] = make_2vec(&&LETREC_add_arity_2, make_vec(/*empty*/));

// ===== body of the letrec: the top-level call =====
{
    // ((vec 0 add<R>) (vec 1 add<R>) 2 3 halt_closure fail_closure)
    //
    // Build halt closure: [&&HALT_CONT, []]
    Value halt = make_2vec(&&HALT_CONT, make_vec(/*empty*/));
    // Build fail closure: [&&FAIL_CONT, []]
    Value fail = make_2vec(&&FAIL_CONT, make_vec(/*empty*/));

    void *target = reg[ADD_REG].vec[0];  // &&LETREC_add_arity_2
    Value env    = reg[ADD_REG].vec[1];  // [] (empty env)

    reg[0] = env;          // env$388
    reg[1] = int_val(2);   // a
    reg[2] = int_val(3);   // b
    reg[3] = halt;         // k (the halt continuation closure)
    reg[4] = fail;         // f (the fail continuation closure)
    goto *target;          // jumps to LETREC_add_arity_2
}
```

So to summarize, any lambda body in a letrec gets added to a big bag of lambdas
that are all written to main with their binding names as labels. Any other anonymous
lambdas have a label invented for them and also added to the big bag. All the make-vec
code that creates the lambda closures and installs them in their registers is added
to a local bag and written to the preamble of the letrec concerned, and the body of
the letrec is appended.

## Structures we will need

An `Opaque` wrapper around a membuf:

```c
typedef struct EmitBuffer {
  FILE *fh;
  char *buffer;
  size_t size;
} EmitBuffer;
```

We can use lower-level ALLOCATE/FREE macros to manage this, it can't be directly
memory managed (can't have a header) because the mark/sweep isn't smart enough
to clean it up.

```c
EmitBuffer *newEmitBuffer() {
  EmitBuffer *result = ALLOCATE(EmitBuffer);
  result->buffer = NULL;
  result->size = 0;
  result->fh = open_memstream(&result->buffer, &size);
  return result;
}

char *getEmitBuffer(EmitBuffer *buffer) {
  fflush(buffer->fh);
  return buffer->buffer;
}

void cleanEmitBuffer(void *buffer) {
  EmitBuffer *b = (EmitBuffer *)buffer;
  fclose(b->fh);
  free(b->buffer);
  FREE(b);
}

void printEmitBuffer(void *buffer) {
  EmitBuffer *b = (EmitBuffer *)buffer;
  fflush(b->fh);
  if (b->buffer != NULL)
    eprintf("%s", b->buffer);
}

EmitBuffer *buffer = newEmitBuffer();

Opaque *container = newOpaque(buffer, cleanEmitBuffer, printEmitBuffer, NULL);
int save = PROTECT(container); // back in a safe place
```

We should probably bundle the EmitBuffer and Opaque wrapper creation, and
the get and put operations too.

```c
static inline Opaque *newOpaque_EmitBuffer() {
  EmitBuffer *buffer = newEmitBuffer();
  return newOpaque(buffer, cleanEmitBuffer, printEmitBuffer, NULL);
}

static inline char *opaqueEmitBufferContent(Opaque *container) {
  return getEmitBuffer((EmitBuffer *)container->data);
}

static inline FILE *opaqueEmitBufferFh(Opaque *container) {
  return ((EmitBuffer *)(container->data))->fh;
}

```

We're going to need a new `emit.yaml` to hold memory-managed helpers:

```yaml
structs:
  EmitContext:
    meta:
      brief: VisitorContext for emitMinExp
    data:
      lambdas: BufferBag
      letrecBindings: BufferBag
      body: opaque
      maxReg: int=0

hashes:
  BufferBag:
    data:
      entries: opaque

primitives: !include primitives.yaml
```

## Temporary Variables

After the initial implementation attempt, one problem that becomes very apparent
is the huge proliferation of temporary variables. These are the results of holding
intermediate results of primitive operations. We mitigate that to an extent by
categorizing primitive operations as atomic or non-atomic, where atomic operations
do not require memory allocation and the operation can be directly substituted for
the variable use. However there are still many many temps created.

Instead of creating temps on the fly, it is envisaged
that an initially empty pool is created. At the end
of processing, all temps in that pool are declared at
the top-level of `main()`.

Let's look at a hypothetical tree of primitive operations and try to analyse the
minimum number of temps that would be needed to perform the operations.

```mermaid
flowchart TD
op0([start]) --tmp0--> op1

op1 --tmp0--> op2
op1 --tmp1--> op3
op1 --tmp2--> op4

op2 --tmp0--> op5
op2 --tmp1--> op6

op3 --tmp1--> op7
op3 --tmp2--> op8

op4 --tmp2--> op9
```

* When `start` requests a temp it gets `tmp0`.
* When `op1` requests a temp it also gets `tmp0`.
* When `op1` requests a second temp it gets `tmp1`.

That might be the insight, temps are claimed by assignment **to**, and released by assignment **from**.

Assuming depth first left to right RPN, the sequence is:

| STEP | TEMPS IN USE |
| --- | --- |
| `op5` claims `tmp0` | `tmp0` |
| `op6` claims `tmp1` | `tmp0`, `tmp1` |
| `op2` releases `tmp0` and `tmp1`; claims `tmp0` | `tmp0` |
| `op7` claims `tmp1` | `tmp0`, `tmp1` |
| `op8` claims `tmp2` | `tmp0`, `tmp1`, `tmp2` |
| `op3` releases `tmp1` and `tmp2`; claims `tmp1` | `tmp0`, `tmp1` |
| `op9` claims `tmp2` | `tmp0`, `tmp1`, `tmp2` |
| `op4` releases `tmp2`, claims `tmp2` | `tmp0`, `tmp1`, `tmp2` |
| `op1` releases `tmp0`, `tmp1` and `tmp2`; claims `tmp0` | `tmp0` |

The current pattern is something like:

```c
// foreach arg expr
  if (!isAtomic(expr)) {
    HashSymbol target = genSym("tmp_");
    emit("Value %s;", target);
    emitSimpleExpr(expr, target); // emits the assignment to target
  }
// end foreach
emit("%s(", op);
// foreach arg expr
  if (isAtomic(expr)) {
    emitAtomic(expr); // perform operation in-place
  } else {
    emit("%s", target)
  }
// end foreach
emit(")");
```

So the complication is that the `emitSimpleExpr` is recursive, and the current pattern requires that the temp target is declared up-front.

Rather than accepting an ordained target (which has been claimed too
early), we should instead return the target. This is probably going to
be an `Opaque` wrapper around an `EmitBuffer` rather than a `char*`,
simply because it's more convenient for fprintf etc.  The "target" then
becomes either a temp variable name or an atomic expression.  Either way
it is slotted in to the argument list of the containing expression.

### Problem - right-associative operators

Arrays are lists and the cons operator is right-associative.
That means to build an array of non-atomic elements requires as many temps as there are elements.

```mermaid
flowchart TD
op0([start]) --tmp0--> op1

op1 --tmp0--> op2
op1 --tmp1--> op3

op3 --tmp1--> op4
op3 --tmp2--> op5

op5 --tmp2--> op6
op5 --tmp3--> op7
```

Thankfully this is not true for strings, since chars are atomic they do not require
temps:

```mermaid
flowchart TD
op0([start]) --tmp0--> op1

op1 --> char1
op1 --tmp0--> op2

op2 --> char2
op2 --tmp0--> op3

op3 --> char3
op3 --tmp0--> op4
```

But returning to arrays in general, if we traverse right to left, we only need two temps:

```mermaid
flowchart TD
op0([start]) --tmp0--> op1

op1 --tmp1--> op2
op1 --tmp0--> op3

op3 --tmp1--> op4
op3 --tmp0--> op5

op5 --tmp1--> op6
op5 --tmp0--> op7
```

It might be adequate to traverse all make-vec right to left for the common case of
arrays, but the smarter approach would be to choose the longer path first. This might not
be too inneficient if we compare lengths in tandem and stop when the shorter path expires,
choosing the longer one.

Of course this will need to generalise to vecs of any width.

## Outstanding Issues

### Memory Management

We've completely ignored issues of protecting intermediate values from
being garbage collected. It would be relatively simple to add a `markReg`
to iterate over the registers, but protecting, and the more difficult
unprotecting of temps is not addressed.

I'm wondering if it might not be easier to re-purpose the current temps
system to instead use the free registers above the current call frame.

We keep the concept of `claimTemp()` and `releaseTemp()`, but each lambda
starts with a fresh empty register set. That means the `copyContext()`
for lambdas gets a fresh symbols hash, or whatever the replacement is.
Let's write a replacement from scratch rather than rewriting the exiting
one, using new `claimSlot()` and `releaseSlot()` functions.  The lambda
first makes cals to `claimSlot()` for as many slots as it needs to
perform its `goto`, then additional `claimSlot()` and `releaseSlot()`
calls to get temporary registers.

The context `currentDepth` can be the source of fresh slots.

The `claimSlot()` function would do pretty much the same thing that
`claimTemp()` does, except the `BoolMap` would be replaced with a
`SlotMap`, where `Slot` is a struct:

```yaml
structs:
  Slot:
    meta:
      description: Represents a register allocation
    data:
      isAvailable: bool
      text: SCharArray
```

```c
static HashSymbol *claimSlot(EmitterContext *context) {
    HashSymbol *result = NULL;
    Slot *slot = NULL;
    Index i = 0;
    while ((result = iterateSlotMap(context->slots, &i, &slot)) != NULL) {
        if (slot->isAvailable) {
            slot->isAvailable = false;
            return result;
        }
    }
    result = genSym("tmp_");
    SCharArray *text = newSCharArray():
    int save = PROTECT(text);
    char buf[64];
    sprintf(buf, "reg[%d]", context->currentDepth);
    incrDepth(context);
    for (char *c = buf; *c; c++) {
      pushSCharArray(text, *c);
    }
    pushSCharArray(text, '\0');
    slot = newSlot(false, text);
    PROTECT(slot);
    setSlotMap(context->slots, result, slot);
    UNPROTECT(save);
    return result;
}
```

`releaseSlot()` and `releaseSlotSymbol()` are also similar:

```c
static void releaseSlotSymbol(HashSymbol *temp, EmitterContext *context) {
    Slot *slot = NULL;
    if (getSlotMap(context->slots, temp, &slot)) {
      slot->isAvailable = true;
    } else {
      cant_happen("slot not found");
    }
}

static void releaseSlot(EmitResult *result, EmitterContext *context) {
    if (isEmitResult_Buf(result))
        return;
    releaseSlotSymbol(getEmitResult_Var(result), context);
}
```

but we'll need an additional `getSlot()` for the slot:

```c
static Slot getSlot(HashSymbol *temp, EmitterContext *context) {
    Slot *slot = NULL;
    if (getSlotMap(context->slots, temp, &slot)) {
      return slot;
    } else {
      cant_happen("slot not found");
    }
}
```

and `emitResultText()` would need to take an additional context:

```c
static char *emitResultText(EmitResult *data, EmitterContext *context) {
    switch (data->type) {
    case EMITRESULT_TYPE_BUF:
        return opaqueEmitBufferContent(getEmitResult_Buf(data));
    case EMITRESULT_TYPE_VAR:
        return getSlot(getEmitResult_Var(data), context)->text->entries;
    default:
        cant_happen("unrecognised %s", emitResultTypeName(data->type));
    }
}
```

So far so good. We need a rule for each of `apply`, `letrec` and `lambda` to co-ordinate
the value of that `currentDepth` context field.

If a `letrec` occurs within a `lambda` it must claim slots after the arguments to the lambda.
So on entry to the body of a `lambda`, `currentDepth` should be set to the length of the
lambda arguments.

Back-patching the closure environments is a separate step that iterates over the (literal `reg[%d]`) slots
that were claimed by the `letrec` first pass. Since `currentDepth` by this stage is already above
the last `letrec` slot, any temporary slots claimed during back-patching are safely out of the way.

The body of the letrec is evaluated in the same context, with any lambda arguments and letrec slots
already below `currentDepth`.

`apply` can calculate its arguments in the normal way with the same context, but must literally (`reg[%d]`)
poke slots `0` - `n` for the arguments to the continuation before the final `goto`.

The top-level `letrec` has no enclosing `lambda` so it starts naturally with a `currentDepth` of `0`.

With this in mind we should first refactor to ensure each of these behaviours are cleanly encapsulated.

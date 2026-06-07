# Delimited Continuations

Standared `shift/reset` semantics:

* `reset (1 + shift k { 10 + k(2) }) = 13`
* `reset (1 + shift k { 2 }) = 2`
* `reset (1 + shift k { k(2) }) = 3`
* `reset (1 + shift k { k(2) + k(3) }) = 7`
* `reset (1 + shift k { 100 + k(k(2)) }) = 104`
* `reset (1 + shift k { 100 + k(2) + 1000 }) = 1103`
* `reset (1 + (reset (2 + shift k { k(3) }))) = 6`
* `reset (1) = 1`
* `reset (1 + shift k { 2 }) = 2`
* `1 + reset (shift k { 2 }) = 3`

## Difference between `shift/reset` and `control/prompt`

* `reset ((1 + shift k { 2 + k(3) }) + shift h { h(4) + h(5) }) = 19`
* `prompt ((1 + control k { 2 + k(3) }) + control h { h(4) + h(5) }) = 21`

### The `shift/reset` example

* `reset ((1 + shift k { 2 + k(3) }) + shift h { h(4) + h(5) }) = 19`

To understand that `shift/reset` example first, let

```fn
T = reset (
    (1 + shift k { 2 + k(3) })
  + shift h { h(4) + h(5) }
)
```

The first `shift` captures the context:

```fn
E1 = (1 + [ ]) + shift h { h(4) + h(5) }
```

So under `shift/reset`:

```txt
k(v) = reset( (1 + v) + shift h { h(4) + h(5) } )
//     ^^^^^
```

That extra `reset` is the crucial part. So the term becomes:

```fn
T = reset( 2 + k(3) )
  = reset( 2 + reset( (1 + 3) + shift h { h(4) + h(5) } ) )
```

Now look at the inner `reset`:

```fn
  reset( (1 + 3) + shift h { h(4) + h(5) } )
= reset( 4       + shift h { h(4) + h(5) } )
```

`h` captures only

```fn
E2 = 4 + [ ]
```

so

```fn
h(v) = reset(4 + v)
```

Hence:

```fn
h(4) = 8
h(5) = 9
```

so the inner reset is:

```fn
  reset( h(4) + h(5) )
= reset( 8 + 9 )
= 17
```

and therefore the whole term is:

```fn
reset( 2 + 17 ) = 19
```

## The Difference

```fn
shift/reset:
reset (
    (1 + shift k { 2 + k(3) })
  + shift h { h(4) + h(5) }
)
```

```fn
control/prompt:
prompt (
    (1 + control k { 2 + k(3) })
  + control h { h(4) + h(5) }
)
```

### shift/reset

```fn
reset          ((1 + shift k { 2 + k(3) }) + shift h { h(4) + h(5) }) = 19
E1 =            (1 + [                  ]) + shift h { h(4) + h(5) }
k(v) =    reset((1 + v                   ) + shift h { h(4) + h(5) })
reset(2 + reset((1 + 3                   ) + shift h { h(4) + h(5) }))
E2 =             4                         + [                     ]
h(v) =     reset(4                         + v)
h(4) = 8
h(5) = 9
inner reset: 17
final result: 19
```

### control/prompt

```fn
prompt     ((1 + control k { 2 + k(3) }) + control h { h(4) + h(5) }) = 21
E1 =        (1 + [                    ]) + control h { h(4) + h(5) }
k(v) =      (1 + v                     ) + control h { h(4) + h(5) }
prompt(2 + ((1 + 3                     ) + control h { h(4) + h(5) }))
prompt(6                                 + control h { h(4) + h(5) })
E2 =   6                                 + [                       ]
h(v) = 6                                 + v
h(4) = 10
h(5) = 11
final result: 21
```

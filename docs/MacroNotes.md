# Observations on macro processing in `lambda_conversion.c`

```text
lamConvert
+---convertFuncDefs(defs, env)
    +---checkMacro(def, env)
        +---setLamMacroSet(env->macros, name) // if macro
    +---prependDefinition(def, env, next)
        +---prependMacro(macro, env, next)
            +---convertAstMacro(macro, env)
                +---lamerformMacroSubstitutions(body, args)
                +---setLamMacroSet(env->macros, name) // redundant?

+---+---convertExpression
        +---convertFunCall(funCall, env)
            +---isMacro(under, env)
            +---thunkMacroExp(fuction, args)
                +---thunkMacroArgs(args)
                +---makeLamExp_Apply(...)
            +---makePrimApp(symbol, args, env)
                +---isMacro(symbol, env)
                +---thunkMacroSymbol(symbol, args)
                    +---thunkMacroExp(args)
```

lamPerformMacroSubstitutions commented out:

```scheme
(λ ()
    (letrec ((COMPUTE (λ (a b c)
                (let (((sum (opMacro$9 (λ () a) (λ () b)))
                       (prod (opMacro$13 (λ () sum) (λ () c)))))
                      (begin prod)))))
            (let (((result (COMPUTE (λ () 2) (λ () 3) (λ () 4)))))
                 ...)))
```

lamPerformMacroSubstitutions uncommented:

```scheme
(λ ()
    (letrec ((COMPUTE (λ (a b c)
                (let (((sum (opMacro$9 (λ () ((a))) (λ () ((b)))))
                       (prod (opMacro$13 (λ () sum) (λ () ((c)))))))
                     (begin prod)))))
            (let (((result (COMPUTE (λ () 2) (λ () 3) (λ () 4)))))
                 ...)))
```

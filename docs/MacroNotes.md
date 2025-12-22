Observations on macro processing in `lambda_conversion.c`

```
lamConvert
+---convertFuncDefs(defs, env)
    +---checkMacro(def, env)
        +---setLamMacroSet(env->macros, name) // if macro
    +---prependDefinition(def, env, next)
        +---prependMacro(macro, env, next)
            +---convertAstMacro(macro, env)
                +---lamPerformMacroSubstitutions(body, args)
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
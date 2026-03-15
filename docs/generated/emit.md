# emit

Code emission utilities

```mermaid
flowchart LR
EmitContext --lambdas--> BufferBag
EmitContext --indexes--> IntMap
EmitContext --body--> opaque
EmitContext --builtIns--> BuiltIns
EmitContext --symbols--> SymbolStash
EmitContext --maxReg--> int
EmitContext --currentDepth--> int
SymbolStash --array--> SymbolArray
SymbolStash --parent--> SymbolStash
```

> Generated from src/emit.yaml by tools/generate.py

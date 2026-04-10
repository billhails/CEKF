# emit_c

C code emission utilities

```mermaid
flowchart LR
CBufferBag --entries--> opaque
CEmitterContext --currentBinding--> HashSymbol
CEmitterContext --lambdas--> CBufferBag
CEmitterContext --body--> opaque
CEmitterContext --builtIns--> BuiltIns
CEmitterContext --registers--> RegisterSlots
CEmitterContext --activeSlots--> int
CEmitterContext --totalSlots--> int
CEmitterContext --maxReg--> int
CEmitterContext --currentReg--> int
CEmitterContext --needsUnprotect--> bool
EmitCResult --var--> HashSymbol
EmitCResult --buf--> opaque
EmitCResult --constant--> opaque
CResultArray["CResultArray[]"] --entries--> EmitCResult
```

> Generated from src/emit_c.yaml by tools/generate.py

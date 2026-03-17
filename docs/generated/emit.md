# emit

Code emission utilities

```mermaid
flowchart LR
BufferBag --entries--> opaque
SlotMap --entries--> Slot
EmitterContext --currentBinding--> HashSymbol
EmitterContext --lambdas--> BufferBag
EmitterContext --body--> opaque
EmitterContext --builtIns--> BuiltIns
EmitterContext --slots--> SlotMap
EmitterContext --maxReg--> int
EmitterContext --currentDepth--> int
Slot --isAvailable--> bool
Slot --text--> SCharArray
EmitResult --var--> HashSymbol
EmitResult --buf--> opaque
ResultArray["ResultArray[]"] --entries--> EmitResult
```

> Generated from src/emit.yaml by tools/generate.py

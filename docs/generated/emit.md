# emit

Code emission utilities

```mermaid
flowchart LR
BufferBag --entries--> opaque
SlotPool --entries--> Slot
EmitterContext --currentBinding--> HashSymbol
EmitterContext --lambdas--> BufferBag
EmitterContext --body--> opaque
EmitterContext --builtIns--> BuiltIns
EmitterContext --slots--> SlotPool
EmitterContext --activeSlots--> int
EmitterContext --totalSlots--> int
EmitterContext --maxReg--> int
EmitterContext --currentReg--> int
EmitterContext --needsUnprotect--> bool
Slot --isAvailable--> bool
Slot --text--> SCharArray
Slot --index--> int
EmitResult --var--> HashSymbol
EmitResult --buf--> opaque
EmitResult --constant--> opaque
ResultArray["ResultArray[]"] --entries--> EmitResult
```

> Generated from src/emit.yaml by tools/generate.py

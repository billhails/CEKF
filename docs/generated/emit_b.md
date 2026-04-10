# emit_b

New byteode emission utilities

```mermaid
flowchart LR
BBufferBag --entries--> BBuffer
BEmitterContext --currentBinding--> HashSymbol
BEmitterContext --lambdas--> BBufferBag
BEmitterContext --body--> BBuffer
BEmitterContext --builtIns--> BuiltIns
BEmitterContext --slots--> SlotPool
BEmitterContext --heap--> SymbolArray
BEmitterContext --activeSlots--> int
BEmitterContext --totalSlots--> int
BEmitterContext --maxReg--> int
BEmitterContext --currentReg--> int
BEmitterContext --needsUnprotect--> bool
BBuffer --codes--> UIntArray
BBuffer --locations--> BLocationArray
BBuffer --comments--> BCommentArray
BLocation --index--> index
BLocation --fileName--> string
BLocation --lineNo--> int
BComment --index--> index
BComment --text--> SCharArray
EmitBResult --var--> HashSymbol
EmitBResult --buf--> BBuffer
EmitBResult --constant--> BBuffer
BResultArray["BResultArray[]"] --entries--> EmitBResult
BLocationArray["BLocationArray[]"] --entries--> BLocation
BCommentArray["BCommentArray[]"] --entries--> BComment
```

> Generated from src/emit_b.yaml by tools/generate.py

# emit

Shared code emission utilities

```mermaid
flowchart LR
SlotPool --entries--> Slot
Slot --isAvailable--> bool
Slot --text--> SCharArray
Slot --index--> int
SlotAssign --source--> HashSymbol
SlotAssign --dest--> HashSymbol
EmitterContext --currentBinding--> HashSymbol
EmitterContext --builtIns--> BuiltIns
EmitterContext --slots--> SlotPool
EmitterContext --slotSymbols--> SymbolArray
EmitterContext --heap--> SymbolArray
EmitterContext --activeSlots--> index
EmitterContext --totalSlots--> index
EmitterContext --maxReg--> index
EmitterContext --currentReg--> index
EmitterContext --needsUnprotect--> bool
SlotAssignArray["SlotAssignArray[]"] --entries--> SlotAssign
```

> Generated from src/emit.yaml by tools/generate.py

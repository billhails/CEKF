# emit

Shared code emission utilities

```mermaid
flowchart LR
SlotPool --entries--> Slot
Slot --isAvailable--> bool
Slot --text--> SCharArray
Slot --index--> int
RegisterSlots --slots--> SlotPool
RegisterSlots --heap--> SymbolArray
```

> Generated from src/emit.yaml by tools/generate.py

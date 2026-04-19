# emit_b

New byteode emission utilities

```mermaid
flowchart LR
BBufferBag --entries--> BBuffer
BEmitterContext --lambdas--> BBufferBag
BEmitterContext --body--> BBuffer
BEmitterContext --context--> EmitterContext
BEmitterContext --constants--> BConstantArray
BAssemblyPlan --buffers--> BBufferBag
BAssemblyPlan --order--> SymbolArray
BAssemblyPlan --entryLabel--> HashSymbol
BLayout --bufferBases--> IndexMap
BLayout --globalLabels--> IndexMap
BLayout --entryPoint--> index
BLinkedImage --codes--> UIntArray
BLinkedImage --constants--> BConstantArray
BLinkedImage --locations--> BLocationArray
BLinkedImage --comments--> BCommentArray
BLinkedImage --entryPoint--> index
BBuffer --codes--> UIntArray
BBuffer --constants--> BConstantArray
BBuffer --labels--> IndexMap
BBuffer --fixups--> BFixupArray
BBuffer --locations--> BLocationArray
BBuffer --comments--> BCommentArray
BBuffer --intConds--> IntCondTable
BBuffer --charConds--> CharCondTable
BBuffer --matches--> MatchTable
BLocation --index--> index
BLocation --fileName--> string
BLocation --lineNo--> int
BComment --index--> index
BComment --text--> SCharArray
IntCondCase --const_index--> index
IntCondCase --target--> index
CharCondCase --codepoint--> character
CharCondCase --target--> index
IntCondSwitch --default_target--> index
IntCondSwitch --cases--> IntCondCaseArray
CharCondSwitch --default_target--> index
CharCondSwitch --cases--> CharCondCaseArray
CondFixup --label--> HashSymbol
CondFixup --tableId--> index
CondFixup --tableIndex--> index
CodeFixup --label--> HashSymbol
CodeFixup --location--> index
TableFixup --tableId--> index
TableFixup --location--> index
EmitBResult --slot--> HashSymbol
EmitBResult --immediate--> int
BFixup --code--> CodeFixup
BFixup --charTable--> TableFixup
BFixup --intTable--> TableFixup
BFixup --matchTable--> TableFixup
BFixup --charCond--> CondFixup
BFixup --intCond--> CondFixup
BFixup --matchCond--> CondFixup
BBC["enum BBC"]
BResultArray["BResultArray[]"] --entries--> EmitBResult
BFixupArray["BFixupArray[]"] --entries--> BFixup
BLocationArray["BLocationArray[]"] --entries--> BLocation
BCommentArray["BCommentArray[]"] --entries--> BComment
BConstantArray["BConstantArray[]"] --entries--> Value
IntCondCaseArray["IntCondCaseArray[]"] --entries--> IntCondCase
CharCondCaseArray["CharCondCaseArray[]"] --entries--> CharCondCase
IntCondTable["IntCondTable[]"] --entries--> IntCondSwitch
CharCondTable["CharCondTable[]"] --entries--> CharCondSwitch
MatchTable["MatchTable[]"] --entries--> IndexArray
```

> Generated from src/emit_b.yaml by tools/generate.py

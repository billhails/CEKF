# cekfs

The structures of the CEKFs machine

```mermaid
flowchart LR
CEKF --C--> control
CEKF --E--> Env
CEKF --K--> Kont
CEKF --F--> Fail
CEKF --S--> Stack
CEKF --B--> ByteCodeArray
CEKF --L--> LocationArray
OverApplyFrame --count--> int
OverApplyFrame --index--> int
OverApplyFrame --extras--> Vec
OverApplyFrame --ready--> bool
Env --S--> Frame
Env --E--> Env
Kont --C--> control
Kont --E--> Env
Kont --S--> Stack
Kont --K--> Kont
Clo --pending--> int
Clo --C--> control
Clo --E--> Env
Fail --C--> control
Fail --E--> Env
Fail --K--> Kont
Fail --S--> Stack
Fail --F--> Fail
Location --loc--> index
Location --lineNo--> int
Location --fileName--> string
Vec["(Vec)"] --entries--> Value
Value --none--> void_ptr
Value --stdint--> int
Value --bigint--> BigInt
Value --rational--> Vec
Value --rational_imag--> Vec
Value --irrational--> double
Value --stdint_imag--> int
Value --bigint_imag--> BigInt
Value --irrational_imag--> double
Value --complex--> Vec
Value --character--> character
Value --clo--> Clo
Value --pclo--> Clo
Value --kont--> Kont
Value --vec--> Vec
Value --builtIn--> BuiltInImplementation
Value --nameSpace--> Vec
Value --opaque--> opaque
ByteCodeArray["ByteCodeArray[]"] --entries--> byte
Stack["Stack[]"] --entries--> Value
ByteCodes["enum ByteCodes"]
CharacterArray["CharacterArray[]"] --entries--> character
ByteArray["ByteArray[]"] --entries--> byte
Frame["Frame[]"] --entries--> Value
LocationArray["LocationArray[]"] --entries--> Location
OverApplyStack["OverApplyStack[]"] --entries--> OverApplyFrame
```

> Generated from src/cekfs.yaml by tools/generate.py

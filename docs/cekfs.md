# cekfs

The structures of the CEKFs machine

```mermaid
flowchart TD
CEKF --C--> control
CEKF --E--> Env
CEKF --K--> Kont
CEKF --F--> Fail
CEKF --S--> Stack
CEKF --B--> ByteCodeArray
Env --stack--> Stack
Env --next--> Env
Kont --body--> control
Kont --env--> Env
Kont --stack--> Stack
Kont --next--> Kont
Clo --pending--> int
Clo --ip--> control
Clo --env--> Env
Fail --exp--> control
Fail --env--> Env
Fail --kont--> Kont
Fail --stack--> Stack
Fail --next--> Fail
Vec["Vec[]"] --entries--> Value
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
Value --character--> char
Value --clo--> Clo
Value --pclo--> Clo
Value --kont--> Kont
Value --vec--> Vec
Value --builtIn--> BuiltInImplementation
Value --namespace--> Vec
Value --opaque--> opaque
Stack["Stack[]"] --entries--> Value
ByteCodeArray["ByteCodeArray[]"] --entries--> byte
ByteCodes["enum ByteCodes"]
CharArray["CharArray[]"] --entries--> char
ValueVal
ValueType
```

> Generated from src/cekfs.yaml by tools/makeAST.py

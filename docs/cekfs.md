# cekfs

The structures of the CEKFs machine

```mermaid
flowchart TD
CekfsState --C--> control
CekfsState --E--> Env
CekfsState --K--> Kont
CekfsState --F--> Fail
CekfsState --S--> Stack
CekfsState --B--> ByteCodeArray
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
Stack["Stack[]"] --entries--> Value
ByteCodes["enum ByteCodes"]
ByteCodeArray["ByteCodeArray[]"] --entries--> byte
ValueVal
ValueType
```

> Generated from src/cekfs.yaml by tools/makeAST.py

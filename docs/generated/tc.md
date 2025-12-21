# tc

Structures to support type inference

```mermaid
flowchart LR
TcTypeTable --entries--> TcType
TcTypeSigTable --entries--> TcTypeSig
TcEnv --table--> TcTypeTable
TcEnv --typeSigs--> TcTypeSigTable
TcEnv --next--> TcEnv
TcNg --table--> TcTypeTable
TcNg --next--> TcNg
TcFunction --arg--> TcType
TcFunction --result--> TcType
TcPair --first--> TcType
TcPair --second--> TcType
TcThunk --type--> TcType
TcTypeSig --name--> HashSymbol
TcTypeSig --args--> TcTypeSigArgs
TcTypeSig --ns--> int
TcTypeSigArgs --type--> TcType
TcTypeSigArgs --next--> TcTypeSigArgs
TcVar --name--> HashSymbol
TcVar --id--> int
TcVar --instance--> TcType
TcType --function--> TcFunction
TcType --pair--> TcPair
TcType --thunk--> TcThunk
TcType --var--> TcVar
TcType --smallinteger--> void_ptr
TcType --biginteger--> void_ptr
TcType --character--> void_ptr
TcType --unknown--> HashSymbol
TcType --opaque--> HashSymbol
TcType --typeSig--> TcTypeSig
TcType --tuple--> TcTypeArray
TcType --namespaces--> TcNamespaceArray
TcType --env--> TcEnv
TcType --nsid--> int
TcTypeArray["TcTypeArray[]"] --entries--> TcType
TcNamespaceArray["TcNamespaceArray[]"] --entries--> TcType
```

> Generated from src/tc.yaml by tools/generate.py

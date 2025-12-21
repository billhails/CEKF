# builtins

Support for declaring builtins

```mermaid
flowchart LR
BuiltInMemBufHash --entries--> BuiltInMemBuf
BuiltIn --externalName--> HashSymbol
BuiltIn --internalName--> HashSymbol
BuiltIn --result--> TcType
BuiltIn --args--> BuiltInArgs
BuiltIn --implementation--> void_ptr
BuiltInImplementation --name--> HashSymbol
BuiltInImplementation --implementation--> void_ptr
BuiltInImplementation --nargs--> int
BuiltInMemBuf --buffer--> string
BuiltInMemBuf --index--> index
BuiltInMemBuf --size--> size
BuiltInArgs["BuiltInArgs[]"] --entries--> TcType
BuiltIns["BuiltIns[]"] --entries--> BuiltIn
```

> Generated from src/builtins.yaml by tools/generate.py

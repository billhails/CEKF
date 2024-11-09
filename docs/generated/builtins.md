# builtins

Support for declaring builtins

```mermaid
flowchart TD
BuiltInMemBufHash --entries--> entries
BuiltIn --name--> HashSymbol
BuiltIn --result--> TcType
BuiltIn --args--> BuiltInArgs
BuiltIn --implementation--> void_ptr
BuiltInImplementation --implementation--> void_ptr
BuiltInImplementation --nargs--> int
BuiltInMemBuf --buffer--> string
BuiltInMemBuf --size--> size
BuiltInArgs["BuiltInArgs[]"] --entries--> TcType
BuiltIns["BuiltIns[]"] --entries--> BuiltIn
```

> Generated from src/builtins.yaml by tools/makeAST.py

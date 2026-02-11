# Proposal for Replacing the `external` Section in Yaml Files

Currenty, and embarrassingly, the `external` section in the yaml files is just a synonym for the `primitives` section. The intention of the `external` section was to describe specifically memory-managed types from other yaml files, but that requires adding information that is already available from those files.

The idea then is quite simple: rather than the `external` section listing details of those other types:

```yaml
external:
    TcType:
        meta:
            brief: external type from the type-checker
            description: A type-checker type referenced by the ANF code.
        data:
            cname: "struct TcType *"
            printFn: printTcType
            markFn: markTcType
            valued: true
    IntMap:
        meta:
        ...
```

it would just contain references to the files themselves:

```yaml
external:
- !include tc.yaml
```

Those includes could be parsed recursively by the existing parser and added to the current catalog.
This causes a couple of problems, but they should be easy enough to solve:

1. How to avoid generating code for the external nodes? Each entry in the catalog has an additional `external` flag, if true it is skipped over by the catalog dispatchers.
2. How to avoid mutually recursive includes? The `include` feature of the yaml is written in `generate/loader.py` and that code could be extended to quietly return an empty object if it sees a file it has already (started to) load.
3. How to avoid primitives being re-entered and causing duplicates? Actually the same solution for recursive includes, `primitives.yaml` would only be parsed once.

I'd imagine that the entire parse and catalog-injection section in `generate.py` would become a function that gets handed an object resulting from a yaml file, along with an `external` boolean flag. It would call itself recursively with each element of any `internal` section that it finds.

The `config` section could be ignored or only partially used if it is `external`.

Of course there wil likely be other problems, probably `#include` directives will need looking at, but the advantages of having direct access to the original definitions is obvious.

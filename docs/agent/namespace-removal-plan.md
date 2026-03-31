# Namespace Removal Plan

## Background

Namespaces are fully desugared by `ast_ns.c` immediately after parsing.
After it runs:

- `AstProg.nameSpaces` is an **empty** array
- All `AstLookUp` nodes have been replaced with plain expressions using qualified symbols
- All `AstLookUpOrSymbol.lookUp` variants have been converted to `.symbol`

Every namespace-related construct downstream in lambda/tc/ANF/bytecode is
therefore either dead code or trivially constant (`nsId` always equals
`NS_GLOBAL`, defined as `-1` in `common.h`).

## What exists today

### tc.yaml

| Construct | Purpose | Status post-desugaring |
|-----------|---------|----------------------|
| `TcType.nameSpaces` variant | Array of namespace types | Dead — never instantiated |
| `TcType.nsId` variant | Namespace identifier in type | Dead — never instantiated |
| `TcType.env` variant | Environment for namespace lookup | Dead — never instantiated |
| `TcNameSpaceArray` | Array type for above | Dead — never instantiated |
| `TcTypeSig.ns` field | Namespace of a type signature | Always `NS_GLOBAL` |

Affected C files: `tc_analyze.c`, `tc_helper.c`

Dead functions in `tc_analyze.c`:

- `analyzeNameSpaces()` — processes `LamExp.nameSpaces` (never created)
- `analyzeLookUp()` — processes `LamExp.lookUp` (never created)
- `lookUpNsRef()` — finds namespace env by index (never called)
- `addNameSpacesToEnv()` — the branch creating `TcNameSpaceArray` (never taken)

Live but simplifiable:

- `lookUpConstructorType(name, nsId, env)` — branches on `nsId == NS_GLOBAL`;
  since `nsId` is always `NS_GLOBAL`, the `else` branch (calling `lookUpNsRef`)
  is dead
- `findNameSpace()` — maps `LamLookUpOrSymbol` to `nsId`; only needed because
  `LamLookUpOrSymbol` exists
- `makeTypeSig()` — accepts `nsId` parameter, stores in `TcTypeSig.ns`

### lambda.yaml

| Construct | Purpose | Status post-desugaring |
|-----------|---------|----------------------|
| `LamExp.nameSpaces` variant | Holds namespace array | Dead — never instantiated |
| `LamExp.lookUp` variant | Expression in another namespace | Dead — never instantiated |
| `LamNameSpaceArray` | Array type for above | Dead — never instantiated |
| `LamLookUp` struct | nsId + nsSymbol + exp | Dead — never instantiated |
| `LamLookUpSymbol` struct | nsId + nsSymbol + symbol | Dead — never instantiated |
| `LamLookUpOrSymbol` union | symbol \| lookUp | Always `.symbol` variant |
| `LamDeconstruct.nsId` field | Namespace of constructor | Always `NS_GLOBAL` |
| `LamIntList.nsId` field | Namespace in pattern match | Always `NS_GLOBAL` |
| `LamTypeConstructorInfo.nsId` field | Namespace of type constructor | Always `NS_GLOBAL` |
| `LamInfo.nameSpaceInfo` variant | Stores namespace context | Dead — never instantiated |
| `LamInfo.nsId` variant | Stores current namespace id | Only stores `NS_GLOBAL` |

Affected C files: `lambda_conversion.c`, `lambda_helper.c`, `lambda_pp.c`,
`lambda_substitution.c`, `lazy_substitution.c`, `lambda_simplfication.c`,
`inline.c`, `tpmc_translate.c`, `print_generator.c`

Key dead/simplifiable lambda functions:

- `addCurrentNameSpaceToContext()` — stores `NS_GLOBAL` in context
- `addNameSpaceInfoToLamContext()` — stores namespace context (never called
  when array is empty)
- `lookUpNameSpaceInLamContext()` — retrieves namespace context (never needed)
- `lookUpCurrentNameSpaceInLamContext()` — always returns `NS_GLOBAL`
- `convertAstLookUpOrSymbol()` — handles lookUp variant (never taken)

### anf.yaml

| Construct | Purpose | Status post-desugaring |
|-----------|---------|----------------------|
| `AexpNameSpaces` struct | Namespace array + body | Dead — never instantiated |
| `AexpNameSpace` struct | Single namespace (nBindings + body) | Dead — never instantiated |
| `AnfExpLookUp` struct | Namespace lookup in ANF | Dead — never instantiated |

Affected C files: `annotate.c`, `bytecode.c`, `anf_pp.c`

### cekfs.yaml — bytecodes

| Bytecode | Purpose | Status |
|----------|---------|--------|
| `NS_START` | Allocate namespace slots | Dead — never emitted |
| `NS_END` | Snapshot namespace | Dead — never emitted |
| `NS_FINISH` | Finalize namespace array | Dead — never emitted |
| `NS_PUSHSTACK` | Push namespace from stack | Dead — never emitted |
| `NS_PUSHENV` | Push namespace from env | Dead — never emitted |
| `NS_POP` | Pop namespace frame | Dead — never emitted |

Affected C files: `step.c`, `bytecode.c`

### minlam.yaml

No namespace constructs — already clean. (`MinIntList` has no `nsId` field;
`lambda_desugar.c` strips it during desugaring.)

## Staged removal plan

Each step produces a buildable, testable state (`make && make test`).
Each step should be a separate commit for easy bisect/revert.

### Phase A — Type checker (downstream first)

#### Step A1: Remove dead TcType variants and TcNameSpaceArray

Files: `tc.yaml`, `tc_analyze.c`, `tc_helper.c`

- tc.yaml: remove `TcType.nameSpaces`, `TcType.nsId`, `TcType.env` variants
  and the `TcNameSpaceArray` type
- tc_analyze.c: remove `analyzeNameSpaces()`, `analyzeLookUp()`,
  `lookUpNsRef()`, the branch in `addNameSpacesToEnv()` that creates
  `TcNameSpaceArray`, and all switch cases dispatching to them
- tc_helper.c: remove namespace pretty-printer

#### Step A2: Simplify lookUpConstructorType — stop branching on nsId

Files: `tc_analyze.c`

- Since `nsId` is always `NS_GLOBAL`, the condition `nsId == NS_GLOBAL` is
  always true — collapse to only the current-environment path
- Remove `findNameSpace()` (maps `LamLookUpOrSymbol` to `nsId`)
- Still accept `nsId` parameter for now (lambda structs still carry it)

#### Step A3: Remove TcTypeSig.ns field

Files: `tc.yaml`, `tc_analyze.c`

- tc.yaml: remove `ns: int` from `TcTypeSig`
- Fix all creation sites and usages

### Phase B — Lambda IR

#### Step B1: Remove dead LamExp namespace variants

Files: `lambda.yaml`, `lambda_conversion.c`, `inline.c`,
`lambda_simplfication.c`, `lambda_substitution.c`, `lazy_substitution.c`,
`lambda_pp.c`

- lambda.yaml: remove `LamExp.nameSpaces`, `LamExp.lookUp` variants,
  `LamNameSpaceArray`, `LamLookUp`
- Remove/fix switch cases in all consumer files

#### Step B2: Remove nsId from lambda structs

Files: `lambda.yaml`, `lambda_conversion.c`, `tpmc_translate.c`,
`print_generator.c`, `tc_analyze.c`

- lambda.yaml: remove `nsId` from `LamDeconstruct`, `LamIntList`,
  `LamTypeConstructorInfo`
- Fix creation sites in `lambda_conversion.c`, `tpmc_translate.c`,
  `print_generator.c`
- Remove `nsId` parameter from `lookUpConstructorType()` in `tc_analyze.c`
  (now always ignored since A2)

#### Step B3: Collapse LamLookUpOrSymbol to HashSymbol*

Files: `lambda.yaml`, `tc_analyze.c`, `lambda_conversion.c`, `lambda_pp.c`,
`lambda_helper.c`, `print_generator.c`, `tpmc_translate.c`

- lambda.yaml: remove `LamLookUpOrSymbol` union and `LamLookUpSymbol` struct
- Replace all usages (~42 sites across 7 files) with direct `HashSymbol*`

#### Step B4: Remove LamInfo namespace variants + context helpers

Files: `lambda.yaml`, `lambda_conversion.c`, `lambda_helper.c`

- lambda.yaml: remove `LamInfo.nameSpaceInfo` and `LamInfo.nsId` variants
  (leaving only `LamInfo.typeConstructorInfo` — consider collapsing union to
  plain struct)
- lambda_conversion.c: remove `addCurrentNameSpaceToContext()`,
  `addNameSpaceInfoToLamContext()`
- lambda_helper.c: remove `lookUpNameSpaceInLamContext()`,
  `lookUpCurrentNameSpaceInLamContext()`

#### Step B5: Collapse LamInfo to LamTypeConstructorInfo

Files: `lambda.yaml`, `lambda_conversion.c`, `lambda_helper.c`

- lambda.yaml: remove `LamInfo` union entirely; change `LamInfoTable` entries
  type from `LamInfo` to `LamTypeConstructorInfo`
- Fix all creation sites (`setLamInfoTable` calls) and access sites
  (`getLamInfo_TypeConstructorInfo` → direct use)
- `LamInfoTable` becomes a `HashSymbol* → LamTypeConstructorInfo*` map

### Phase C — ANF and bytecode

#### Step C1: Remove ANF namespace constructs + NS bytecodes

Files: `anf.yaml`, `cekfs.yaml`, `annotate.c`, `bytecode.c`, `anf_pp.c`,
`step.c`

- anf.yaml: remove `AexpNameSpaces`, `AexpNameSpace`, `AnfExpLookUp`
- cekfs.yaml: remove `NS_START`, `NS_END`, `NS_FINISH`, `NS_PUSHSTACK`,
  `NS_PUSHENV`, `NS_POP`
- Fix switch cases and dead functions in `annotate.c`, `bytecode.c`,
  `anf_pp.c`, `step.c`

### Phase D — Final cleanup

#### Step D1: Remove NS_GLOBAL and remaining traces

Files: `common.h`, any remaining references

- common.h: remove `NS_GLOBAL` definition
- Grep for any remaining `nsId`, `nameSpace`, `NS_` references and clean up

## Risk assessment

| Step | Risk | Rationale |
|------|------|-----------|
| A1 | Low | Dead code — variants never instantiated |
| A2 | Low | Simplifying always-true branch |
| A3 | Low | Small field removal |
| B1 | Low | Dead code — variants never created |
| B2 | Medium | Touches multiple files, changes struct layouts |
| B3 | Medium | Replaces union type with raw pointer across ~42 sites |
| B4 | Medium | Removes context-building helpers, needs care |
| B5 | Medium | Replaces union with plain struct in hash table |
| C1 | Low-Medium | Dead code, but touches runtime VM |
| D1 | Low | Final cleanup |

## Notes

- `LamContext` itself is **not** removed — it is the general compilation
  context, not namespace-specific. Only the namespace-related entries stored
  in its `LamInfoTable` via `LamInfo.nameSpaceInfo`/`LamInfo.nsId` are
  removed (Step B4).
- After B4, `LamInfo` becomes a single-variant union (only
  `typeConstructorInfo`). Step B5 collapses this to a plain
  `LamTypeConstructorInfo*`.
- The AST-level namespace constructs (`AstNameSpace`, `AstLookUp`,
  `AstNameSpaceArray`, etc. in `ast.yaml`) are **not** part of this plan —
  they are used by `ast_ns.c` itself and the parser, and are still needed.

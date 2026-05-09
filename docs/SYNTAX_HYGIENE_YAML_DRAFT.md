# Syntax Hygiene YAML Draft

## Purpose

This document is a concrete companion to `SYNTAX_HYGIENE_DESIGN.md`.

Its job is narrower: propose the actual generated type shapes for the stage-5
carrier model before touching `src/ast.yaml` or `src/pratt.yaml`.

The intent is to de-risk two questions.

1. What durable syntax data must survive parsing?
2. How to divide responsibilities between `ast.yaml` and `pratt.yaml`.

## Approach

- `ast.yaml` owns the durable syntax declaration, syntax use, binding, and
  template IR types.
- `pratt.yaml` owns only the parser-side matcher metadata needed during parsing.
- The two layers are tied together by a shared `declarationId`.

After `prattParseFile()` returns, parser-owned structures are no longer the
right place to store stage-5 data. The later passes need syntax declarations
and syntax uses to survive until `nsAstProg()`, `syntaxPrepareAst()`, and
`syntaxLowerAst()`, so the durable rule/template data lives in `ast.yaml`.

`pratt.yaml` retains only enough metadata to recognize syntax entry heads,
match pattern items, perform rollback, collect captures, and emit syntax-use
carriers.

## AST Draft

The AST side needs four groups of types.

1. Syntax declaration carriers.
2. Syntax use carriers.
3. Syntax rule components reused by declarations.
4. Template IR.

### Proposed `ast.yaml` Additions

```yaml
structs:
    AstSyntaxDecl:
        meta:
            brief: AST syntax declaration
            description: >-
                A durable syntax declaration that survives parsing and remains
                available until syntax lowering removes it from the AST.
        data:
            declarationId: int
            ruleName: HashSymbol
            surfaceHead: HashSymbol=NULL
            parameters: SymbolArray=NULL
            entryKind: AstSyntaxEntryKind=ASTSYNTAXENTRYKIND_TYPE_HELPER
            resultKind: AstSyntaxResultKind=ASTSYNTAXRESULTKIND_TYPE_EXPR
            alternatives: AstSyntaxAlternatives

    AstSyntaxAlternative:
        meta:
            brief: AST syntax alternative
            description: >-
                One ordered pattern plus its template body.
        data:
            patternItems: AstSyntaxPatternItems
            template: AstSyntaxTemplate

    AstSyntaxHole:
        meta:
            brief: AST syntax hole
            description: >-
                One bound syntax component in a syntax pattern.
        data:
            syntaxClass: AstSyntaxClass
            name: HashSymbol
            callTarget: HashSymbol=NULL
            callArguments: SymbolArray=NULL

    AstSyntaxBinding:
        meta:
            brief: Captured or inherited syntax binding
            description: >-
                One binding collected during syntax matching. All phase-1
                captures lower to ordinary AstExpression fragments.
        data:
            name: HashSymbol
            value: AstExpression
            inherited: bool=false

    AstExprSyntaxUse:
        meta:
            brief: Expression-position syntax use
            description: >-
                A matched syntax use in expression position. It refers back to
                the declaration by id and stores the bindings captured at the
                use site.
        data:
            declarationId: int
            alternativeIndex: int
            bindings: AstSyntaxBindings

    AstDefSyntaxUse:
        meta:
            brief: Definition-position syntax use
            description: >-
                A matched syntax use in definition position.
        data:
            declarationId: int
            alternativeIndex: int
            bindings: AstSyntaxBindings

    AstSyntaxTemplate:
        meta:
            brief: Syntax template root
            description: >-
                A syntax template with an explicit result kind and a dedicated
                template IR rather than ordinary AST.
        data:
            resultKind: AstSyntaxResultKind
            expr: AstSyntaxTemplateExpr=NULL
            definition: AstSyntaxTemplateDefinition=NULL

    AstSyntaxLiteralRef:
        meta:
            brief: Definition-site literal reference
            description: >-
                An identifier written directly in the template. The prepare pass
                resolves it to the declaration-site binding.
        data:
            writtenName: HashSymbol
            resolvedName: HashSymbol=NULL

    AstSyntaxIntroducedBinder:
        meta:
            brief: Template-introduced binder
            description: >-
                A binder introduced by the template itself. The lowerer will
                freshen it and rewrite matching introduced references.
        data:
            binderId: int
            writtenName: HashSymbol

    AstSyntaxIntroducedRef:
        meta:
            brief: Reference to a template-introduced binder
            description: >-
                A reference tied to a previously parsed introduced binder by id.
        data:
            binderId: int

    AstSyntaxUnquote:
        meta:
            brief: Template unquote site
            description: >-
                A splice of a captured or inherited syntax binding.
        data:
            bindingName: HashSymbol
            syntaxClass: AstSyntaxClass

    AstSyntaxTemplateFunCall:
        meta:
            brief: Template function call
        data:
            function: AstSyntaxTemplateExpr
            arguments: AstSyntaxTemplateExprs

    AstSyntaxTemplateLookUp:
        meta:
            brief: Template namespace lookup
        data:
            nsSymbol: HashSymbol
            expression: AstSyntaxTemplateExpr

    AstSyntaxTemplateIff:
        meta:
            brief: Template conditional expression
        data:
            test: AstSyntaxTemplateExpr
            consequent: AstSyntaxTemplateNest
            alternative: AstSyntaxTemplateNest

    AstSyntaxTemplatePrint:
        meta:
            brief: Template print expression
        data:
            expression: AstSyntaxTemplateExpr

    AstSyntaxTemplateTypeOf:
        meta:
            brief: Template typeof expression
        data:
            expression: AstSyntaxTemplateExpr

    AstSyntaxTemplateStruct:
        meta:
            brief: Template structure expression
        data:
            symbol: AstLookUpOrSymbol
            expressions: AstSyntaxTemplateTaggedExpressions

    AstSyntaxTemplateNest:
        meta:
            brief: Template nest
        data:
            definitions: AstSyntaxTemplateDefinitions
            expressions: AstSyntaxTemplateExprs

    AstSyntaxTemplateFunction:
        meta:
            brief: Template composite function
        data:
            function: AstSyntaxTemplateAltFunction
            next: AstSyntaxTemplateFunction=NULL
            unsafe: bool=false

    AstSyntaxTemplateAltFunction:
        meta:
            brief: Template alternative function body
        data:
            altArgs: AstSyntaxTemplateAltArgs
            nest: AstSyntaxTemplateNest

    AstSyntaxTemplateDefine:
        meta:
            brief: Template single definition
        data:
            symbol: AstSyntaxTemplateBinder
            expression: AstSyntaxTemplateExpr

    AstSyntaxTemplateMultiDefine:
        meta:
            brief: Template multi-definition
        data:
            symbols: AstSyntaxTemplateBinders
            expression: AstSyntaxTemplateExpr

    AstSyntaxTemplateDefLazy:
        meta:
            brief: Template lazy definition
        data:
            name: AstSyntaxTemplateBinder
            definition: AstSyntaxTemplateAltFunction

    AstSyntaxTemplateAlias:
        meta:
            brief: Template type alias
        data:
            name: AstSyntaxTemplateBinder
            type: AstType

    AstSyntaxTemplateTypeDef:
        meta:
            brief: Template typedef
        data:
            typeSig: AstTypeSig
            typeBody: AstTypeBody

unions:
    AstSyntaxTemplateBinder:
        meta:
            brief: Template binder occurrence
            description: >-
                A binder position in a template. Phase 1 needs either a fresh
                introduced binder or an explicit unquote of a captured name.
        data:
            introduced: AstSyntaxIntroducedBinder
            unquote: AstSyntaxUnquote

    AstSyntaxTemplateNameRef:
        meta:
            brief: Template name reference
            description: >-
                A non-binder identifier occurrence in a template.
        data:
            literal: AstSyntaxLiteralRef
            introduced: AstSyntaxIntroducedRef
            unquote: AstSyntaxUnquote

    AstSyntaxPatternItem:
        meta:
            brief: Syntax pattern item stored in the AST
        data:
            quotedTerminal: HashSymbol
            typedHole: AstSyntaxHole

    AstSyntaxTemplateDefinition:
        meta:
            brief: Template definition node
        data:
            define: AstSyntaxTemplateDefine
            multi: AstSyntaxTemplateMultiDefine
            lazy: AstSyntaxTemplateDefLazy
            alias: AstSyntaxTemplateAlias
            typeDef: AstSyntaxTemplateTypeDef

    AstSyntaxTemplateExpr:
        meta:
            brief: Template expression node
            description: >-
                The phase-1 template expression IR. It mirrors the expression
                forms currently needed by quote bodies while replacing raw names
                with explicit provenance-carrying variants.
        data:
            back: void_ptr
            wildCard: void_ptr
            nameRef: AstSyntaxTemplateNameRef
            number: MaybeBigInt
            character: character
            funCall: AstSyntaxTemplateFunCall
            lookUp: AstSyntaxTemplateLookUp
            fun: AstSyntaxTemplateFunction
            nest: AstSyntaxTemplateNest
            iff: AstSyntaxTemplateIff
            print: AstSyntaxTemplatePrint
            typeOf: AstSyntaxTemplateTypeOf
            tuple: AstSyntaxTemplateExprs
            structure: AstSyntaxTemplateStruct
            assertion: AstSyntaxTemplateExpr
            error: AstSyntaxTemplateExpr

    AstSyntaxTemplateFarg:
        meta:
            brief: Template formal argument node
            description: >-
                Binder-aware mirror of AstFarg for the parts of function syntax
                that phase 1 quote bodies need.
        data:
            wildCard: void_ptr
            binder: AstSyntaxTemplateBinder
            lookUp: AstLookUpSymbol
            named: AstSyntaxTemplateNamedArg
            unpack: AstSyntaxTemplateUnpack
            unpackStruct: AstSyntaxTemplateUnpackStruct
            number: MaybeBigInt
            character: character
            tuple: AstSyntaxTemplateFargList

structs:
    AstSyntaxTemplateNamedArg:
        data:
            name: HashSymbol
            arg: AstSyntaxTemplateFarg

    AstSyntaxTemplateUnpack:
        data:
            symbol: AstLookUpOrSymbol
            argList: AstSyntaxTemplateFargList

    AstSyntaxTemplateUnpackStruct:
        data:
            symbol: AstLookUpOrSymbol
            argList: AstSyntaxTemplateTaggedArgs

    AstSyntaxTemplateAltArgs:
        data:
            argList: AstSyntaxTemplateFargList
            next: AstSyntaxTemplateAltArgs=NULL

    AstSyntaxTemplateTaggedArg:
        data:
            tag: HashSymbol
            arg: AstSyntaxTemplateFarg

    AstSyntaxTemplateTaggedExpression:
        data:
            tag: HashSymbol
            expression: AstSyntaxTemplateExpr

    AstSyntaxTemplateDefinitions:
        data:
            definition: AstSyntaxTemplateDefinition
            next: AstSyntaxTemplateDefinitions=NULL

arrays:
    AstSyntaxAlternatives:
        data:
            entries: AstSyntaxAlternative

    AstSyntaxBindings:
        data:
            entries: AstSyntaxBinding

    AstSyntaxPatternItems:
        data:
            entries: AstSyntaxPatternItem

    AstSyntaxTemplateBinders:
        data:
            entries: AstSyntaxTemplateBinder

    AstSyntaxTemplateExprs:
        data:
            entries: AstSyntaxTemplateExpr

    AstSyntaxTemplateFargList:
        data:
            entries: AstSyntaxTemplateFarg

    AstSyntaxTemplateTaggedArgs:
        data:
            entries: AstSyntaxTemplateTaggedArg

    AstSyntaxTemplateTaggedExpressions:
        data:
            entries: AstSyntaxTemplateTaggedExpression

enums:
    AstSyntaxEntryKind:
        data:
          - EXPR
          - DEF
          - HELPER

    AstSyntaxResultKind:
        data:
          - EXPR
          - DEF

    AstSyntaxClass:
        data:
          - EXPR
          - NAME
          - NEST
          - STRING
          - TYPE
          - SYNTAX
```

## Notes On This AST Draft

### 1. Durable Declarations Live In The AST

`AstSyntaxDecl` is the durable rule storage. It is what later passes use when
lowering a syntax use.

The parser should still also register a matcher-side rule in `parser->macros`,
but that parser table becomes a transient index, not the durable owner.

### 2. `AstSyntaxBinding.value` Stays As `AstExpression`

That is deliberate.

The current parser already normalizes phase-1 captures to `AstExpression`
fragments:

- `Name` becomes a symbol expression
- `Nest` becomes a nest expression
- `String` becomes the string-list expression form
- helper `Syntax(...)` calls return expression results in phase 1

That means the syntax-use carrier does not need a second capture union yet.

### 3. Binder Versus Reference Is Explicit In The Template IR

This is the key hygiene requirement.

The mirrored template IR is not valuable because it is different syntax. It is
valuable because it can express the distinctions ordinary AST cannot:

- declaration-site literal ref
- introduced binder
- introduced ref
- unquote splice

### 4. The Template IR Is Intentionally Smaller Than Full AST

This draft mirrors the expression and binder forms needed for the phase-1
examples and ordinary `let`-based templates.

It does not try to solve every future template feature yet.

Most notably:

- no explicit general splice node
- no multi-definition syntax result
- no syntax declaration inside a quote template
- no dedicated template type IR yet

If type-position unquote becomes required, add a parallel `AstSyntaxTemplateType`
family later rather than forcing it into the first stage-5 cut.

### 5. Reuse Existing `AstType*` For Now

The draft keeps `AstType`, `AstTypeSig`, and `AstTypeBody` ordinary. That keeps
phase 1 smaller.

If a later slice needs hygienic identifier handling inside quoted type syntax,
that should become a separate follow-up schema change.

## Pratt Draft

The parser-side schema should become thinner than it is today.

The durable template body leaves `pratt.yaml`. The Pratt layer keeps only the
matcher information needed while parsing.

### Proposed `pratt.yaml` Changes

```yaml
structs:
    PrattMacroSpec:
        meta:
            brief: Parser metadata for a phase-1 syntax rule.
            description: >-
                Stores the parser-only information needed to recognize and match
                a syntax rule while parsing. Durable declaration content lives
                in AstSyntaxDecl.
        data:
            declarationId: int
            ruleName: HashSymbol
            surfaceHead: HashSymbol=NULL
            parameters: SymbolArray=NULL
            alternatives: PrattMacroAlternatives
            entryKind: PrattSyntaxEntryKind=PRATTSYNTAXENTRYKIND_TYPE_HELPER
            resultKind: PrattSyntaxResultKind=PRATTSYNTAXRESULTKIND_TYPE_EXPR
            export: bool=false
            importNsRef: int=-1
            importNsSymbol: HashSymbol=NULL

    PrattMacroAlternative:
        meta:
            brief: Parser-side syntax alternative.
            description: >-
                Stores the ordered pattern items needed during parser-time
                matching. The durable template body is stored in the AST.
        data:
            patternItems: PrattMacroPatternItems

    PrattSyntaxBinding:
        meta:
            brief: Parser-side binding collected during matching.
            description: >-
                Temporary parser-owned binding data used to build an
                AstExprSyntaxUse or AstDefSyntaxUse.
        data:
            name: HashSymbol
            value: AstExpression
            inherited: bool=false

arrays:
    PrattSyntaxBindings:
        data:
            entries: PrattSyntaxBinding

enums:
    PrattSyntaxEntryKind:
        data:
          - EXPR
          - DEF
          - HELPER

    PrattSyntaxResultKind:
        data:
          - EXPR
          - DEF
```

## Notes On This Pratt Draft

### 1. `template: AstExpression` Goes Away

That is the critical change.

Once parser-time substitution is removed, the matcher no longer needs a
template body.

### 2. `patternItems` On `PrattMacroSpec` Can Also Go Away

The current `PrattMacroSpec` duplicates the first alternative on the spec itself
for convenience.

After this refactor, use `alternatives` as the single source of truth and drop
spec-level `patternItems`.

That makes the parser API more regular and removes another source of migration
confusion.

### 3. Keep Existing `PrattMacro*` Names In The First Implementation

Even though the surface says `syntax`, the quickest implementation path is still
probably to keep `PrattMacroSpec`, `PrattMacroAlternative`, and related helper
names until stage 7 cleanup.

This draft changes ownership and fields first, not terminology.

## Smallest Migration Path

If the goal is the least risky schema migration, do it in this order.

1. Add the new AST carrier and template IR types.
2. Add `declarationId`, `ruleName`, `surfaceHead`, `entryKind`, and
   `resultKind` to `PrattMacroSpec`.
3. Stop writing `AstDefinition_Blank` for syntax declarations and instead emit
   `AstDefinition_SyntaxDecl`.
4. Stop storing templates in `PrattMacroAlternative`.
5. Stop calling the `substituteSyntax*` family and emit syntax-use carriers.

That sequence minimizes the period where both the parser table and the AST are
trying to own the same durable template information.

## Immediate Questions This Draft Settles

This draft makes the following concrete.

1. The AST does need explicit syntax declaration and syntax use variants.
2. The long-lived template IR should not stay in `pratt.yaml`.
3. `declarationId` should be the shared join key between parser-time matching
   and post-parse lowering.
4. Captures can stay as `AstExpression` in phase 1.
5. The template IR needs explicit binder/reference provenance rather than raw
   symbols.

## Questions Deliberately Left For The Implementation Step

This draft does not yet choose:

- the exact parser API boundary between `pratt_parser.c` and `syntax_parse.c`
- whether `AstExprSyntaxUse` and `AstDefSyntaxUse` should later collapse into a
  shared struct used by two union variants
- whether `AstSyntaxTemplateFunction` should mirror `AstCompositeFunction`
  exactly or be simplified during parsing
- whether `AstSyntaxTemplateLookUp` should carry `AstLookUpOrSymbol` instead of
  `nsSymbol + expression`

Those are local engineering choices. They do not change the main ownership
split recommended here.

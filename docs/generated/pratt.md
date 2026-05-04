# pratt

Pratt Parser support

```mermaid
flowchart LR
PrattRecordTable --entries--> PrattRecord
PrattNsIdTable --entries--> int
PrattMacroTable --entries--> PrattMacroSpec
PrattTrie --character--> character
PrattTrie --terminal--> HashSymbol
PrattTrie --siblings--> PrattTrie
PrattTrie --children--> PrattTrie
PrattBuffer --data--> WCharVec
PrattBuffer --start--> wstring
PrattBuffer --offset--> int
PrattBufList --lineNo--> int
PrattBufList --fileName--> HashSymbol
PrattBufList --buffer--> PrattBuffer
PrattBufList --next--> PrattBufList
PrattToken --type--> HashSymbol
PrattToken --fileName--> HashSymbol
PrattToken --lineNo--> int
PrattToken --value--> PrattValue
PrattToken --next--> PrattToken
PrattLexer --bufList--> PrattBufList
PrattLexer --tokenHead--> PrattToken
PrattLexer --tokenTail--> PrattToken
PrattParser --rules--> PrattRecordTable
PrattParser --macros--> PrattMacroTable
PrattParser --nameSpaces--> PrattNsIdTable
PrattParser --lexer--> PrattLexer
PrattParser --trie--> PrattTrie
PrattParser --panicMode--> bool
PrattParser --suppressErrors--> bool
PrattParser --isPreamble--> bool
PrattParser --quotedAtomSymbols--> SymbolArray
PrattParser --next--> PrattParser
PrattRecord --symbol--> HashSymbol
PrattRecord --prefix--> PrattFixityConfig
PrattRecord --infix--> PrattFixityConfig
PrattRecord --postfix--> PrattFixityConfig
PrattExportedOps --exportedRules--> PrattRecordTable
PrattMixfixPattern --keywords--> PrattStrings
PrattMixfixPattern --arity--> int
PrattMixfixPattern --associativity--> PrattAssoc
PrattMixfixPattern --startsWithHole--> bool
PrattMixfixPattern --endsWithHole--> bool
PrattMacroHole --syntaxClass--> PrattSyntaxClass
PrattMacroHole --name--> HashSymbol
PrattMacroHole --callTarget--> HashSymbol
PrattMacroHole --callArguments--> SymbolArray
PrattMacroSpec --declarationId--> int
PrattMacroSpec --entryKind--> PrattSyntaxEntryKind
PrattMacroSpec --resultKind--> PrattSyntaxResultKind
PrattMacroSpec --headSymbol--> HashSymbol
PrattMacroSpec --parameters--> SymbolArray
PrattMacroSpec --patternItems--> PrattMacroPatternItems
PrattMacroSpec --template--> AstExpression
PrattMacroSpec --alternatives--> PrattMacroAlternatives
PrattMacroSpec --isExprEntry--> bool
PrattMacroSpec --export--> bool
PrattMacroSpec --importNsRef--> int
PrattMacroSpec --importNsSymbol--> HashSymbol
PrattMacroAlternative --patternItems--> PrattMacroPatternItems
PrattMacroAlternative --template--> AstExpression
PrattMacroAlternative --quotedTemplate--> bool
SyntaxExprBindings --names--> SymbolArray
SyntaxExprBindings --values--> AstExpressionArray
SyntaxLexerSnapshot --bufList--> PrattBufList
SyntaxLexerSnapshot --start--> wstring
SyntaxLexerSnapshot --offset--> int
SyntaxLexerSnapshot --lineNo--> int
SyntaxLexerSnapshot --next--> SyntaxLexerSnapshot
SyntaxLexerCheckpoint --bufList--> PrattBufList
SyntaxLexerCheckpoint --queuedTokens--> PrattTokens
SyntaxLexerCheckpoint --snapshots--> SyntaxLexerSnapshot
SyntaxLexerCheckpoint --panicMode--> bool
PrattSyntaxBinding --name--> HashSymbol
PrattSyntaxBinding --value--> AstExpression
PrattSyntaxBinding --inherited--> bool
PrattFixityConfig --op--> PrattParselet
PrattFixityConfig --prec--> int
PrattFixityConfig --originalImpl--> AstExpression
PrattFixityConfig --hygienicFunc--> HashSymbol
PrattFixityConfig --isBareSymbol--> bool
PrattFixityConfig --export--> bool
PrattFixityConfig --pattern--> PrattMixfixPattern
PrattFixityConfig --isLazy--> bool
PrattFixityConfig --importNsRef--> int
PrattFixityConfig --importNsSymbol--> HashSymbol
PrattMacroPatternItem --quotedTerminal--> HashSymbol
PrattMacroPatternItem --typedHole--> PrattMacroHole
PrattValue --string--> WCharArray
PrattValue --number--> MaybeBigInt
PrattValue --character--> character
PrattValue --atom--> HashSymbol
PrattAssoc["enum PrattAssoc"]
PrattNumberState["enum PrattNumberState"]
PrattStringState["enum PrattStringState"]
PrattFixity["enum PrattFixity"]
PrattSyntaxClass["enum PrattSyntaxClass"]
PrattSyntaxEntryKind["enum PrattSyntaxEntryKind"]
PrattSyntaxResultKind["enum PrattSyntaxResultKind"]
PrattStrings["PrattStrings[]"] --entries--> WCharArray
PrattParsers["PrattParsers[]"] --entries--> PrattParser
PrattTokens["PrattTokens[]"] --entries--> PrattToken
PrattNsOpsArray["PrattNsOpsArray[]"] --entries--> PrattExportedOps
PrattMacroPatternItems["PrattMacroPatternItems[]"] --entries--> PrattMacroPatternItem
PrattMacroAlternatives["PrattMacroAlternatives[]"] --entries--> PrattMacroAlternative
PrattSyntaxBindings["PrattSyntaxBindings[]"] --entries--> PrattSyntaxBinding
```

> Generated from src/pratt.yaml by tools/generate.py

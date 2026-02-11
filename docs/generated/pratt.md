# pratt

Pratt Parser support

```mermaid
flowchart LR
PrattRecordTable --entries--> PrattRecord
PrattNsIdTable --entries--> int
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
PrattParser --nameSpaces--> PrattNsIdTable
PrattParser --lexer--> PrattLexer
PrattParser --trie--> PrattTrie
PrattParser --panicMode--> bool
PrattParser --isPreamble--> bool
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
PrattValue --string--> WCharArray
PrattValue --number--> MaybeBigInt
PrattValue --character--> character
PrattValue --atom--> HashSymbol
PrattAssoc["enum PrattAssoc"]
PrattNumberState["enum PrattNumberState"]
PrattStringState["enum PrattStringState"]
PrattFixity["enum PrattFixity"]
PrattStrings["PrattStrings[]"] --entries--> WCharArray
PrattParsers["PrattParsers[]"] --entries--> PrattParser
PrattNsOpsArray["PrattNsOpsArray[]"] --entries--> PrattExportedOps
```

> Generated from src/pratt.yaml by tools/generate.py

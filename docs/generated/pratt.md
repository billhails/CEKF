# pratt

Pratt Parser support

```mermaid
flowchart LR
PrattRecordTable --entries--> PrattRecord
PrattNsIdTable --entries--> int
PrattTrie --character--> byte
PrattTrie --terminal--> HashSymbol
PrattTrie --siblings--> PrattTrie
PrattTrie --children--> PrattTrie
PrattBuffer --data--> ustring
PrattBuffer --start--> ustring
PrattBuffer --length--> int
PrattBufList --lineno--> int
PrattBufList --filename--> HashSymbol
PrattBufList --buffer--> PrattBuffer
PrattBufList --next--> PrattBufList
PrattToken --type--> HashSymbol
PrattToken --filename--> HashSymbol
PrattToken --lineno--> int
PrattToken --value--> PrattValue
PrattToken --next--> PrattToken
PrattLexer --bufList--> PrattBufList
PrattLexer --tokenHead--> PrattToken
PrattLexer --tokenTail--> PrattToken
PrattParser --rules--> PrattRecordTable
PrattParser --namespaces--> PrattNsIdTable
PrattParser --lexer--> PrattLexer
PrattParser --trie--> PrattTrie
PrattParser --panicMode--> bool
PrattParser --isPreamble--> bool
PrattParser --next--> PrattParser
PrattRecord --symbol--> HashSymbol
PrattRecord --prefix--> PrattFixityConfig
PrattRecord --infix--> PrattFixityConfig
PrattRecord --postfix--> PrattFixityConfig
PrattRecord --importNsRef--> int
PrattRecord --importNsSymbol--> HashSymbol
PrattExportedOps --exportedRules--> PrattRecordTable
PrattMixfixPattern --keywords--> PrattStrings
PrattMixfixPattern --arity--> int
PrattMixfixPattern --associativity--> PrattAssoc
PrattMixfixPattern --starts_with_hole--> bool
PrattMixfixPattern --ends_with_hole--> bool
PrattFixityConfig --op--> PrattParselet
PrattFixityConfig --prec--> int
PrattFixityConfig --originalImpl--> AstExpression
PrattFixityConfig --hygienicFunc--> HashSymbol
PrattFixityConfig --isBareSymbol--> bool
PrattFixityConfig --export--> bool
PrattFixityConfig --pattern--> PrattMixfixPattern
PrattValue --string--> PrattUTF8
PrattValue --number--> MaybeBigInt
PrattValue --character--> PrattUTF8
PrattValue --atom--> HashSymbol
PrattAssoc["enum PrattAssoc"]
PrattNumberState["enum PrattNumberState"]
PrattStringState["enum PrattStringState"]
PrattFixity["enum PrattFixity"]
PrattUTF8["PrattUTF8[]"] --entries--> uchar
PrattStrings["PrattStrings[]"] --entries--> PrattUTF8
PrattParsers["PrattParsers[]"] --entries--> PrattParser
PrattUnicode["PrattUnicode[]"] --entries--> character
PrattNsOpsArray["PrattNsOpsArray[]"] --entries--> PrattExportedOps
```

> Generated from src/pratt.yaml by tools/makeAST.py

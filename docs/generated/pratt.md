# pratt

Pratt Parser support

```mermaid
flowchart TD
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
PrattRecord --prefixOp--> PrattParselet
PrattRecord --prefixPrec--> int
PrattRecord --prefixOriginalImpl--> AstExpression
PrattRecord --prefixHygienicFunc--> HashSymbol
PrattRecord --prefixIsBareSymbol--> bool
PrattRecord --prefixExport--> bool
PrattRecord --infixOp--> PrattParselet
PrattRecord --infixPrec--> int
PrattRecord --infixOriginalImpl--> AstExpression
PrattRecord --infixHygienicFunc--> HashSymbol
PrattRecord --infixIsBareSymbol--> bool
PrattRecord --infixExport--> bool
PrattRecord --postfixOp--> PrattParselet
PrattRecord --postfixPrec--> int
PrattRecord --postfixOriginalImpl--> AstExpression
PrattRecord --postfixHygienicFunc--> HashSymbol
PrattRecord --postfixIsBareSymbol--> bool
PrattRecord --postfixExport--> bool
PrattRecord --importNsRef--> int
PrattRecord --importNsSymbol--> HashSymbol
PrattRecord --associativity--> PrattAssoc
PrattRecord --mixfixPattern--> PrattMixfixPattern
PrattExportedOps --exportedRules--> PrattRecordTable
PrattMixfixPattern --keywords--> PrattStrings
PrattMixfixPattern --arity--> int
PrattMixfixPattern --starts_with_hole--> bool
PrattMixfixPattern --ends_with_hole--> bool
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

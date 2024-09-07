# pratt

Pratt Parser support

```mermaid
flowchart TD
PrattTable --entries--> entries
PrattTrie --character--> byte
PrattTrie --terminal--> HashSymbol
PrattTrie --siblings--> PrattTrie
PrattTrie --children--> PrattTrie
PrattBuffer --data--> string
PrattBuffer --start--> string
PrattBuffer --length--> int
PrattBufList --lineno--> int
PrattBufList --filename--> HashSymbol
PrattBufList --buffer--> PrattBuffer
PrattBufList --next--> PrattBufList
PrattToken --type--> HashSymbol
PrattToken --filename--> HashSymbol
PrattToken --lineno--> int
PrattToken --value--> PrattBuffer
PrattToken --next--> PrattToken
PrattLexer --bufList--> PrattBufList
PrattLexer --trie--> PrattTrie
PrattLexer --tokenHead--> PrattToken
PrattLexer --tokenTail--> PrattToken
PrattParser --rules--> PrattTable
PrattParser --next--> PrattParser
PrattRecord --symbol--> HashSymbol
PrattRecord --prefixOp--> PrattPrefixOp
PrattRecord --infixOp--> PrattPostfixOp
PrattRecord --postfixOp--> PrattPostfixOp
PrattRecord --precedence--> int
PrattNumberState["enum PrattNumberState"]
PrattStringState["enum PrattStringState"]
PrattCharState["enum PrattCharState"]
```

> Generated from src/pratt.yaml by tools/makeAST.py

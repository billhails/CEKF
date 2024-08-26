# pratt

Pratt Parser support

```mermaid
flowchart TD
PrattParser --entries--> entries
PrattBinary --token--> HashSymbol
PrattBinary --left--> PrattExpr
PrattBinary --right--> PrattExpr
PrattUnary --token--> HashSymbol
PrattUnary --expr--> PrattExpr
PrattRecord --token--> HashSymbol
PrattRecord --prefixOp--> PrattPrefixOp
PrattRecord --infixOp--> PrattPostfixOp
PrattRecord --postfixOp--> PrattPostfixOp
PrattRecord --precedence--> int
PrattTrie --character--> byte
PrattTrie --terminal--> HashSymbol
PrattTrie --siblings--> PrattTrie
PrattTrie --children--> PrattTrie
PrattExpr --unary--> PrattUnary
PrattExpr --binary--> PrattBinary
PrattExpr --number--> int
PrattExpr --atom--> HashSymbol
PrattToken --op--> HashSymbol
PrattToken --atom--> HashSymbol
PrattToken --number--> int
PrattToken --eof--> void_ptr
PrattLexer["PrattLexer[]"] --entries--> PrattToken
PrattExprVal
PrattExprType
PrattTokenVal
PrattTokenType
```

> Generated from src/pratt.yaml by tools/makeAST.py

# pratt

Pratt Parser support

```mermaid
flowchart TD
TestBinary --token--> HashSymbol
TestBinary --left--> TestExpr
TestBinary --right--> TestExpr
TestUnary --token--> HashSymbol
TestUnary --expr--> TestExpr
TestExpr --unary--> TestUnary
TestExpr --binary--> TestBinary
TestExpr --atom--> int
TestToken --op--> HashSymbol
TestToken --atom--> int
TestToken --eof--> void_ptr
TestLexer["TestLexer[]"] --entries--> TestToken
TestExprVal
TestExprType
TestTokenVal
TestTokenType
```

> Generated from src/pratt.yaml by tools/makeAST.py

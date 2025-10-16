# Operator Precedence

Subject to change, but this is collated from the built-in operators in
`pratt_parser.c` and the "user defined" operators in `preamble.fn`.

| Precedence | Operators |
|------------|-----------|
| 0 | prefix `print` |
| 1 | infix `->` |
| 2 | infix `then` |
| 3 | infix `and`, `or`, `xor`, `nand`, `nor`, `xnor` |
| 4 | prefix `not` |
| 5 | infix `!=`, `>`, `<`, `>=`, `<=`, `<=>`|
| 6 | prefix `<`, `>` |
| 7 | infix `=` (alias)|
| 8 | |
| 9 | infix `@@` |
| 10 | infix `+`, `-`, `@` |
| 11 | prefix `#`, infix `*`, `/` |
| 12 | infix `**` |
| 13 | prefix `here`, `of` |
| 14 | |
| 15 | infix `.` |
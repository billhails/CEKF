# Parsing MixFix Operator Patterns

MixFix operators are defined by patterns consisting of underscores representing
"holes" where a parameter would slot and sequences of other characters representing
the literal tokens of the operator. For example the infix addition operator would
be represented as `_+_` and the ternary conditional as `_?_:_`. There are only
a few simple rules to be followed when parsing:

1. No consecutive holes, each parameter must be separated from the others by an operator symbol.
2. At leeast one hole. You cannot have an operator without parameters.
3. At least one operator, you cannot have parameters without operators.

This converts nicely into a little state machine:


```mermaid
flowchart TD
classDef error fill:#844;
classDef final fill:#484;
classDef state fill:#448;
s([S]):::final
s --EoS--> e((Error)):::error
s --_--> u((U)):::state
s --*--> c((C)):::state
u --_--> e
u --EoS--> e
u --*--> uc((UC)):::state
c --_--> cu((CU)):::state
c --EoS--> e
c --*--> c
uc --_--> ucu((UCU)):::state
uc --EoS--> UC((UC)):::final
uc --*--> uc
cu --_--> e
cu --EoS--> CU((CU)):::final
cu --*--> cuc((CUC)):::state
cuc --_--> cu
cuc --Eos--> CUC((CUC)):::final
cuc --*--> cuc
ucu --_--> e
ucu --EoS--> UCU((UCU)):::final
ucu --*--> uc
```

Which in turn can be translated into a state table

| STATE | CHAR | UNDERSCORE | END |
|-------|------|------------|-----|
| S     | C    | U          | ERR |
| C     | C    | CU         | ERR |
| CU    | CUC  | ERR        | CUF |
| CUC   | CUC  | CU         | CUCF |
| U     | UC   | ERR        | ERR |
| UC    | UC   | UCU        | UCF |
| UCU   | UC   | ERR        | UCUF |
| ERR   | ERR  | ERR        | ERR |
| CUF   | ERR  | ERR        | ERR |
| UCF   | ERR  | ERR        | ERR |
| UCUF  | ERR  | ERR        | ERR |
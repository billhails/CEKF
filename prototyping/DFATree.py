import collections.abc

class Fargs:
    def __init__(self, fargs, action):
        self.fargs = fargs
        self.action = action

    def accept(self, visitor, state):
        return visitor.visitFargs(self, state)

class Farg:
    pass

class NumericFarg(Farg):
    def __init__(self, n):
        self.n = n

    def accept(self, visitor, state):
        return visitor.visitNumericFarg(self, state)

class EnumFarg(Farg):
    def __init__(self, label, args):
        self.label = label
        self.args = args

    def accept(self, visitor, state):
        return visitor.visitEnumFarg(self, state)

class VarFarg(Farg):
    def __init__(self, name):
        self.name = name

    def accept(self, visitor, state):
        return visitor.visitVarFarg(self, state)

class ComparisonFarg(Farg):
    def __init__(self, name):
        self.name = name

    def accept(self, visitor, state):
        return visitor.visitComparisonFarg(self, state)

class WildcardFarg(Farg):
    def accept(self, visitor, state):
        return visitor.visitWildcardFarg(self, state)

class Compound:
    def __init__(self, fargss):
        self.fargss = fargss

    def accept(self, visitor):
        return visitor.visitCompoundFargs(self)

class AArgs:
    def __init__(self, aargs):
        self.aargs = aargs

class AArg:
    pass

class NumericAArg(AArg):
    def __init__(self, n):
        self.n = n

class EnumAArg(AArg):
    def __init__(self, label, args):
        self.label = label
        self.args = args

class DFAState:
    counter = 0

    def __init__(self, action=None):
        self.action = action
        self.transitions = {}
        self.name = 'S' + str(DFAState.counter)
        DFAState.counter += 1

    def add(self, other):
        if self.action is None:
            self.action = other.action
        else:
            other.action = self.action
        for transition in other.transitions:
            if transition in self.transitions:
                self.transitions[transition].add(other.transitions[transition])
            else:
                self.transitions[transition] = other.transitions[transition]

    def getName(self):
        if self.action is None:
            return self.name
        return self.name + ' ' + self.action

    def addTransition(self, nfaTransition):
        key = nfaTransition.key()
        dfa = nfaTransition.to.toDFA()
        if key in self.transitions:
            self.transitions[key].add(dfa)
        else:
            self.transitions[key] = dfa

    def adjustEscape(self, transition, escape):
        if escape is None:
            return None
        if transition.stackCost() == 1:
            newState = DFAState()
            newState.transitions[DFARetTransition()] = escape
            return newState
        if transition.stackCost == -1:
            return escape.transitions[DFARetTransition()]
        return escape

    def patchEscapes(self, escape=None):
        wildcard = DFAVarTransition()
        if wildcard in self.transitions:
            escape2 = self.transitions[wildcard]
            for transition in self.transitions:
                if not transition.isWildcard():
                    self.transitions[transition].patchEscapes(self.adjustEscape(transition, escape2))
        else:
            for transition in self.transitions:
                self.transitions[transition].patchEscapes(self.adjustEscape(transition, escape))
            if escape is not None and any([transition.isConditional() for transition in self.transitions]):
                self.transitions[wildcard] = escape

    def __str__(self):
        return f'({self.getName()}' + ' '.join([f' -{t}-> {str(self.transitions[t])}' for t in self.transitions]) + ')'

    def mermaid(self, seen={}):
        if self.name in seen:
            return
        seen[self.name] = True
        print(f'{self.name}[{self.getName()}]')
        for transition in self.transitions:
            self.transitions[transition].mermaid(seen)
        for transition in self.transitions:
            print(f'{self.name}--"{str(transition)}"-->{self.transitions[transition].name}')


class DFATransition:
    def __ne__(self, other):
        return not(self == other)

    def stackCost(self):
        return 0

    def isConditional(self):
        return False

    def isWildcard(self):
        return False


class DFANumericTransition(DFATransition):
    def __init__(self, n):
        self.n = n

    def __hash__(self):
        return hash(('numeric', self.n))

    def __eq__(self, other):
        return isinstance(other, DFANumericTransition) and self.n == other.n

    def __str__(self):
        return f'#{str(self.n)}'

    def isConditional(self):
        return True


class DFAEnumTransition(DFATransition):
    def __init__(self, label):
        self.label = label

    def __hash__(self):
        return hash(('enum', self.label))

    def __eq__(self, other):
        return isinstance(other, DFAEnumTransition) and self.label == other.label

    def __str__(self):
        return f'{{{str(self.label)}}}'

    def isConditional(self):
        return True


class DFAIndexTransition(DFATransition):
    def __init__(self, index):
        self.index = index

    def __hash__(self):
        return hash(('index', self.index))

    def __eq__(self, other):
        return isinstance(other, DFAIndexTransition) and self.index == other.index

    def __str__(self):
        return f'[[{str(self.index)}]]'

    def stackCost(self):
        return 1


class DFAArgTransition(DFATransition):
    def __init__(self, index):
        self.index = index

    def __hash__(self):
        return hash(('arg', self.index))

    def __eq__(self, other):
        return isinstance(other, DFAArgTransition) and self.index == other.index

    def __str__(self):
        return f'[{str(self.index)}]'

    def stackCost(self):
        return 1


class DFAVarTransition(DFATransition):
    def __hash__(self):
        return hash(('var', ''))

    def __eq__(self, other):
        return isinstance(other, DFAVarTransition)

    def __str__(self):
        return '*'

    def isWildcard(self):
        return True


class DFAComparisonTransition(DFATransition):
    def __init__(self, var):
        self.var = var

    def __hash__(self):
        return hash(('cmp', self.var))

    def __eq__(self, other):
        return isinstance(other, DFAComparisonTransition) and self.var == other.var

    def __str__(self):
        return f'=={str(self.var)}'

    def isConditional(self):
        return True


class DFARetTransition(DFATransition):
    def __hash__(self):
        return hash(('ret', ''))

    def __eq__(self, other):
        return isinstance(other, DFARetTransition)

    def __str__(self):
        return 'ret'

    def stackCost(self):
        return -1

class NFAState:
    def __init__(self, out, action=None):
        assert isinstance(out, collections.abc.Sequence)
        self.out = out
        self.action = action

    def isTerminal(self):
        return len(self.out) == 0

    def __str__(self):
        if self.isTerminal():
            return f'(end {self.action})'
        return 'O ' + ',\n  '.join([str(out) for out in self.out])

    def toDFA(self):
        dfa = DFAState(self.action)
        for out in self.out:
            if out.isEmpty():
                dfa.add(out.to.toDFA())
            else:
                dfa.addTransition(out)
        return dfa

class NFATransition:
    def __init__(self, to):
        assert isinstance(to, NFAState)
        self.to = to

    def isEmpty(self):
        return False

    def key(self):
        raise Exception('key cannot be called on base transition')

class NFAEmptyTransition(NFATransition):
    def __str__(self):
        return '-> ' + str(self.to)

    def isEmpty(self):
        return True

    def key(self):
        raise Exception('key cannot be called on empty transition')

class NFANumericTransition(NFATransition):
    def __init__(self, to, n):
        super().__init__(to)
        self.n = n

    def __str__(self):
        return '-' + str(self.n) + '-> ' + str(self.to)

    def key(self):
        return DFANumericTransition(self.n)

class NFAEnumTransition(NFATransition):
    def __init__(self, to, label):
        super().__init__(to)
        self.label = label

    def __str__(self):
        return '-{' + self.label + '}-> ' + str(self.to)

    def key(self):
        return DFAEnumTransition(self.label)

class NFAIndexTransition(NFATransition):
    def __init__(self, to, index):
        self.index = index
        self.to = to

    def __str__(self):
        return '-[' + str(self.index) + ']-> ' + str(self.to)

    def key(self):
        return DFAIndexTransition(self.index)

class NFAArgTransition(NFATransition):
    def __init__(self, to, index):
        self.index = index
        self.to = to

    def __str__(self):
        return '-.[' + str(self.index) + '].-> ' + str(self.to)

    def key(self):
        return DFAArgTransition(self.index)

class NFAVarTransition(NFATransition):
    def __init__(self, name, to):
        super().__init__(to)
        self.name = name

    def __str__(self):
        return '-$' + self.name + '-> ' + str(self.to)

    def key(self):
        return DFAVarTransition()

class NFAWildcardTransition(NFATransition):
    def __str__(self):
        return '-*-> ' + str(self.to)

    def key(self):
        return DFAVarTransition()

class NFAComparisonTransition(NFATransition):
    def __init__(self, name, to):
        super().__init__(to)
        self.name = name

    def __str__(self):
        return f'-=={self.name}-> ' + str(self.to)

    def key(self):
        return DFAComparisonTransition(self.name)

class NFARetTransition(NFATransition):
    def __str__(self):
        return '-ret-> ' + str(self.to)

    def key(self):
        return DFARetTransition()

class FargToNfaVisitor:
    def visitCompoundFargs(self, compound):
        count = 0;
        nfas = [fargs.accept(self, NFAState([], fargs.action)) for fargs in compound.fargss]
        return NFAState([NFAEmptyTransition(nfa) for nfa in nfas])

    def visitFargs(self, fargs, state):
        count = len(fargs.fargs)
        return self.recursivelyVisitFargs(fargs.fargs, count, state)

    def recursivelyVisitFargs(self, fargs, count, state):
        if count == 0:
            return state
        state = self.recursivelyVisitFargs(fargs, count - 1, state)
        return NFAState([NFAArgTransition(fargs[len(fargs) - count].accept(self, NFAState([NFARetTransition(state)])), len(fargs) - count)])

    def visitNumericFarg(self, numeric, state):
        return NFAState([NFANumericTransition(state, numeric.n)])

    def visitEnumFarg(self, enum, state):
        count = len(enum.args)
        fields = self.recursivelyVisitEnum(enum.args, count, state)
        return NFAState([NFAEnumTransition(fields, enum.label)])

    def recursivelyVisitEnum(self, args, count, state):
        if count == 0:
            return state
        state = self.recursivelyVisitEnum(args, count - 1, state)
        return NFAState([NFAIndexTransition(args[len(args) - count].accept(self, NFAState([NFARetTransition(state)])), len(args) - count)])

    def visitVarFarg(self, var, state):
        return NFAState([NFAVarTransition(var.name, state)])

    def visitWildcardFarg(self, wildcard, state):
        return NFAState([NFAWildcardTransition(state)])

    def visitComparisonFarg(self, comparison, state):
        return NFAState([NFAComparisonTransition(comparison.name, state)])

# (f, [])
# (f, h @ t)
mapArgs = Compound([
    Fargs([VarFarg('f'), EnumFarg('nil', [])], 'finish'),
    Fargs([VarFarg('f'), EnumFarg('cons', [VarFarg('h'), EnumFarg('cons', [VarFarg('t'), EnumFarg('nil', [])])])], 'recurse')
])

# nfa = mapArgs.accept(FargToNfaVisitor())

# print(str(nfa))

# dfa = nfa.toDFA()
# print(str(dfa))

# dfa.mermaid()

memberArgs = Compound([
    Fargs([VarFarg('x'), EnumFarg('nil', [])], 'fail'),
    Fargs([VarFarg('x'), EnumFarg('cons', [ComparisonFarg('x'), WildcardFarg()])], 'succeed'),
    Fargs([VarFarg('x'), EnumFarg('cons', [WildcardFarg(), VarFarg('t')])], 'continue')
])

# nfa2 = memberArgs.accept(FargToNfaVisitor())

# dfa2 = nfa2.toDFA()

# dfa2.mermaid()

testArgs = Compound([
    Fargs([EnumFarg('cons', [NumericFarg(1), EnumFarg('cons', [NumericFarg(1), EnumFarg('nil', [])])])], 'a'),
    Fargs([EnumFarg('cons', [NumericFarg(1), EnumFarg('cons', [NumericFarg(2), EnumFarg('nil', [])])])], 'b'),
    Fargs([EnumFarg('cons', [NumericFarg(2), EnumFarg('cons', [NumericFarg(1), EnumFarg('nil', [])])])], 'c'),
    Fargs([EnumFarg('cons', [NumericFarg(2), EnumFarg('cons', [NumericFarg(2), EnumFarg('nil', [])])])], 'd'),
    Fargs([WildcardFarg()], 'e'),
])

testNfa = testArgs.accept(FargToNfaVisitor())

testDfa = testNfa.toDFA()

testDfa.patchEscapes()

testDfa.mermaid()

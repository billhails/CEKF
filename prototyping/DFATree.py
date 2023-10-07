import collections.abc



class Fargs:
    """
    List of Formal Arguments
    """
    def __init__(self, fargs, action):
        self.fargs = fargs
        self.action = action

    def accept(self, visitor, state):
        return visitor.visitFargs(self, state)


class Farg:
    """
    Abstract base class for single Formal Arguments
    """
    pass


class NumericFarg(Farg):
    def __init__(self, n):
        self.n = n

    def accept(self, visitor, state):
        return visitor.visitNumericFarg(self, state)


class VecFarg(Farg):
    def __init__(self, fields):
        self.fields = fields

    def accept(self, visitor, state):
        return visitor.visitVecFarg(self, state)


class LabelFarg(Farg):
    def __init__(self, label):
        self.label = label

    def accept(self, visitor, state):
        return visitor.visitLabelFarg(self, state)


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
    def __init__(self, fields):
        self.fields = fields


class StateTable:
    def __init__(self, state):
        self.states = sorted(state.allReachableStates())
        transitions = set()
        for state in self.states:
            for transition in state.transitions:
                transitions.add(transition)
        self.transitions = sorted([x for x in transitions])
        transitionMap = {x.key(): x for x in self.transitions}
        self.width = len(self.states)
        for i in range(self.width):
            self.states[i].setStateTableIndex(i)
        self.height = len(self.transitions)
        for i in range(self.height):
            self.transitions[i].setStateTableIndex(i)
        self.table = [[None for i in range(self.width)] for j in range(self.height)]
        for state in self.states:
            for transition in state.transitions:
                myTransition = transitionMap[transition.key()]
                x = state.getStateTableIndex()
                y = myTransition.getStateTableIndex()
                to = state.transitions[transition].getStateTableIndex()
                # print(y, x, to)
                self.table[y][x] = to

    def toStr(self, n):
        if n is None:
            return ''
        return str(n)

    def markdown(self):
        print('|     | ' + ' | '.join([str(s.getStateTableLabel()) for s in self.states]) + ' |')
        print('| --- | ' + ' | '.join(['---'] * self.width) + ' |')
        for y in range(self.height):
            print(f'| { self.transitions[y].key() } | ' + ' | '.join([self.toStr(x) for x in self.table[y]]) + ' |')


class DFAState:
    counter = 0

    def __init__(self, action=None):
        self.action = action
        self.transitions = {}
        self.name = 'S' + str(DFAState.counter)
        self.sortKey = DFAState.counter
        self.stateTableIndex = 0
        DFAState.counter += 1

    def toStateTable(self):
        return StateTable(self)

    def allReachableStates(self):
        return [x for x in self._allReachableStates(set())]

    def _allReachableStates(self, seen):
        if self in seen:
            return seen
        seen.add(self)
        for transition in self.transitions:
            self.transitions[transition]._allReachableStates(seen)
        return seen

    def __lt__(self, other):
        return self.sortKey < other.sortKey

    def __cmp__(self, other):
        return cmp(self.sortKey, other.sortKey)

    def getStateTableIndex(self):
        return self.stateTableIndex

    def isTerminal(self):
        return self.action is not None

    def getStateTableLabel(self):
        return str(self.stateTableIndex) +  ((' ' + self.action) if self.isTerminal() else '')

    def setStateTableIndex(self, index):
        self.stateTableIndex = index

    def merge(self, other):
        if self.action is None:
            self.action = other.action
        else:
            other.action = self.action
        for transition in other.transitions:
            if transition in self.transitions:
                self.transitions[transition].merge(other.transitions[transition])
            else:
                self.transitions[transition] = other.transitions[transition]

    def getName(self):
        if self.action is None:
            return self.name
        return self.name + ' ' + self.action

    def getLabel(self):
        if self.action is None:
            return str(self.sortKey)
        return str(self.sortKey) + ' ' + self.action

    def addTransition(self, nfaTransition):
        key = nfaTransition.key()
        dfa = nfaTransition.to.toDFA()
        if key in self.transitions:
            self.transitions[key].merge(dfa)
        else:
            self.transitions[key] = dfa

    def adjustEscape(self, transition, escape):
        if escape is None:
            return None
        if transition.stackCost() == 1:
            newState = DFAState()
            newState.transitions[DFARetTransition('adjusted')] = escape
            return newState
        if transition.stackCost == -1:
            return escape.transitions[DFARetTransition()]
        return escape

    def isConditional(self):
        return any([transition.isConditional() for transition in self.transitions])

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
            if escape is not None and self.isConditional():
                self.transitions[wildcard] = escape

    def replaceDuplicates(self):
        duplicates = self.findDuplicates()
        while len(duplicates) > 0:
            self._replaceDuplicates(duplicates)
            duplicates = self.findDuplicates()

    def _replaceDuplicates(self, duplicates):
        for a in duplicates:
            for b in duplicates[a]:
                print(f'{a.getName()}/{b.getName()}')
                self._replaceDuplicate(a, b)

    def _replaceDuplicate(self, a, b):
        allStates = self.allReachableStates()
        for state in allStates:
            state._replaceOneDuplicate(a, b)

    def _replaceOneDuplicate(self, a, b):
        for transition in self.transitions:
            if self.transitions[transition] == a:
                self.transitions[transition] = b

    def findDuplicates(self):
        duplicates = dict()
        allStates = self.allReachableStates()
        for instance in allStates:
            instance._findDuplicates(allStates, duplicates)
        return duplicates
        """
        for a in duplicates:
            for b in duplicates[a]:
                print(f'{a.getName()} == {b.getName()}');
        """

    def _findDuplicates(self, allStates, duplicates):
        for instance in allStates:
            if instance.sortKey <= self.sortKey:
                pass
            else:
                if self.sameTransitions(instance):
                    if self not in duplicates:
                        duplicates[self] = set()
                    duplicates[self].add(instance)

    def sameTransitions(self, other):
        return len(self.transitions) > 0 and self._sameTransitions(other) and other._sameTransitions(self)

    def _sameTransitions(self, other):
        for transition in self.transitions:
            if not other.hasSameTransition(transition, self.transitions[transition]):
                return False
        return True

    def hasSameTransition(self, transition, target):
        if transition in self.transitions:
            if self.transitions[transition] == target:
                return True
        return False

    def __str__(self):
        return f'({self.getName()}' + ' '.join([f' -{t}-> {str(self.transitions[t])}' for t in self.transitions]) + ')'

    def mermaid(self, seen={}):
        if self.name in seen:
            return
        seen[self.name] = True
        print(f'{self.name}(({self.getLabel()}))')
        for transition in self.transitions:
            self.transitions[transition].mermaid(seen)
        for transition in self.transitions:
            print(f'{self.name}--"{str(transition)}"-->{self.transitions[transition].name}')


class DFATransition:
    counter = 0

    def __init__(self):
        self.stateTableIndex = 0
        self.id = DFATransition.counter
        DFATransition.counter += 1

    def __ne__(self, other):
        return not(self == other)

    def getStateTableIndex(self):
        return self.stateTableIndex

    def setStateTableIndex(self, index):
        self.stateTableIndex = index

    def stackCost(self):
        return 0

    def isConditional(self):
        return False

    def __lt__(self, other):
        return self.id < other.id

    def __cmp__(self, other):
        return cmp(self.id, other.id)

    def isWildcard(self):
        return False

    def key(self):
        return str(self)

class DFANumericTransition(DFATransition):
    def __init__(self, n):
        super().__init__()
        self.n = n

    def __hash__(self):
        return hash(('numeric', self.n))

    def __eq__(self, other):
        return isinstance(other, DFANumericTransition) and self.n == other.n

    def __str__(self):
        return f'#{str(self.n)}'

    def isConditional(self):
        return True


class DFALabelTransition(DFATransition):
    def __init__(self, label):
        super().__init__()
        self.label = label

    def __hash__(self):
        return hash(('label', self.label))

    def __eq__(self, other):
        return isinstance(other, DFALabelTransition) and self.label == other.label

    def __str__(self):
        return f'{{{self.label}}}'

    def isConditional(self):
        return True


class DFAVecTransition(DFATransition):
    def __hash__(self):
        return hash(('vec', ''))

    def __eq__(self, other):
        return isinstance(other, DFAVecTransition)

    def __str__(self):
        return 'vec'

    def isConditional(self):
        return True


class DFAIndexTransition(DFATransition):
    def __init__(self, index):
        super().__init__()
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
        super().__init__()
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
        super().__init__()
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
    def __init__(self, label='?'):
        super().__init__()
        self.label = label

    def __hash__(self):
        return hash(('ret', ''))

    def __eq__(self, other):
        return isinstance(other, DFARetTransition)

    def __str__(self):
        return f'ret {self.label}'

    def key(self):
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
                dfa.merge(out.to.toDFA())
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


class NFALabelTransition(NFATransition):
    def __init__(self, label, to):
        super().__init__(to)
        self.label = label

    def __str__(self):
        return '-{' + self.label + '}-> ' + str(self.to)

    def key(self):
        return DFALabelTransition(self.label)


class NFAIndexTransition(NFATransition):
    def __init__(self, to, index):
        self.index = index
        self.to = to

    def __str__(self):
        return '-[' + str(self.index) + ']-> ' + str(self.to)

    def key(self):
        return DFAIndexTransition(self.index)


class NFAVecTransition(NFATransition):
    def __init__(self, to):
        self.to = to

    def __str__(self):
        return '-vec-> ' + str(self.to)

    def key(self):
        return DFAVecTransition()


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
    def __init__(self, label, to):
        super().__init__(to)
        self.label = label

    def __str__(self):
        return f'-ret {self.label}-> ' + str(self.to)

    def key(self):
        return DFARetTransition(self.label)


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
        index = len(fargs) - count
        return NFAState([NFAArgTransition(fargs[index].accept(self, NFAState([NFARetTransition(f'[{index}]', state)])), index)])

    def visitNumericFarg(self, numeric, state):
        return NFAState([NFANumericTransition(state, numeric.n)])

    def visitVecFarg(self, vec, state):
        count = len(vec.fields)
        newState = self.recursivelyVisitVec(vec.fields, count, NFAState([NFARetTransition('vec', state)]))
        return NFAState([NFAVecTransition(newState)])

    def recursivelyVisitVec(self, fields, count, state):
        if count == 0:
            return state
        state = self.recursivelyVisitVec(fields, count - 1, state)
        index = len(fields) - count
        return NFAState([NFAIndexTransition(fields[index].accept(self, NFAState([NFARetTransition(f'[[{index}]]', state)])), index)])

    def visitLabelFarg(self, label, state):
        return NFAState([NFALabelTransition(label.label, state)])

    def visitVarFarg(self, var, state):
        return NFAState([NFAVarTransition(var.name, state)])

    def visitWildcardFarg(self, wildcard, state):
        return NFAState([NFAWildcardTransition(state)])

    def visitComparisonFarg(self, comparison, state):
        return NFAState([NFAComparisonTransition(comparison.name, state)])


# (f, [])
# (f, h @ t)
mapArgs = Compound([
    Fargs([VarFarg('f'), VecFarg([LabelFarg('nil')])], 'finish'),
    Fargs([VarFarg('f'), VecFarg([LabelFarg('cons'), VarFarg('h'), VecFarg([LabelFarg('cons'), VarFarg('t'), VecFarg([LabelFarg('nil')])])])], 'recurse')
])

# nfa = mapArgs.accept(FargToNfaVisitor())

# print(str(nfa))

# dfa = nfa.toDFA()
# print(str(dfa))

# dfa.mermaid()

memberArgs = Compound([
    Fargs([VarFarg('x'), VecFarg([LabelFarg('nil')])], 'fail'),
    Fargs([VarFarg('x'), VecFarg([LabelFarg('cons'), ComparisonFarg('x'), WildcardFarg()])], 'succeed'),
    Fargs([VarFarg('x'), VecFarg([LabelFarg('cons'), WildcardFarg(), VarFarg('t')])], 'continue')
])

# nfa2 = memberArgs.accept(FargToNfaVisitor())

# dfa2 = nfa2.toDFA()

# dfa2.mermaid()

testArgs = Compound([
    Fargs([VecFarg([LabelFarg('cons'), NumericFarg(1), VecFarg([LabelFarg('cons'), NumericFarg(1), VecFarg([LabelFarg('nil')])])])], 'a'),
    Fargs([VecFarg([LabelFarg('cons'), NumericFarg(1), VecFarg([LabelFarg('cons'), NumericFarg(2), VecFarg([LabelFarg('nil')])])])], 'b'),
    Fargs([VecFarg([LabelFarg('cons'), NumericFarg(2), VecFarg([LabelFarg('cons'), NumericFarg(1), VecFarg([LabelFarg('nil')])])])], 'c'),
    Fargs([VecFarg([LabelFarg('cons'), NumericFarg(2), VecFarg([LabelFarg('cons'), NumericFarg(2), VecFarg([LabelFarg('nil')])])])], 'd'),
    Fargs([WildcardFarg()], 'e'),
])

testNfa = testArgs.accept(FargToNfaVisitor())

testDfa = testNfa.toDFA()

testDfa.patchEscapes()

testDfa.mermaid({})

testStateTable = testDfa.toStateTable()

testStateTable.markdown()

testDfa.replaceDuplicates()
print("")
testDfa.mermaid({})

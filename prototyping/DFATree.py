import collections.abc



class Fargs:
    """
    List of Formal Arguments
    """
    def __init__(self, action, *fargs):
        self.action = action
        self.fargs = fargs
        for i in range(len(self.fargs)):
            self.fargs[i].notePosition([i])

    def accept(self, visitor, state):
        return visitor.visitFargs(self, state)

    def __str__(self):
        return '(' + ', '.join([str(x) for x in self.fargs]) + ') => ' + self.action


class Farg:
    """
    Abstract base class for single Formal Arguments
    """
    def notePosition(self, i):
        self.position = i


class NumericFarg(Farg):
    def __init__(self, n):
        self.n = n

    def accept(self, visitor, state):
        return visitor.visitNumericFarg(self, state)

    def __str__(self):
        return str(self.n)


class VecFarg(Farg):
    def __init__(self, label, *fields):
        self.label = label
        self.fields = fields

    def accept(self, visitor, state):
        return visitor.visitVecFarg(self, state)

    def notePosition(self, i):
        super().notePosition(i)
        for j in range(len(self.fields)):
            self.fields[j].notePosition(i + [j + 1])

    def __str__(self):
        return self.label + '[' + ', '.join([str(x) for x in self.fields]) + ']'


class VarFarg(Farg):
    def __init__(self, name):
        self.name = name

    def accept(self, visitor, state):
        return visitor.visitVarFarg(self, state)

    def __str__(self):
        return self.name


class ComparisonFarg(Farg):
    def __init__(self, name):
        self.name = name

    def accept(self, visitor, state):
        return visitor.visitComparisonFarg(self, state)

    def __str__(self):
        return self.name


class WildcardFarg(Farg):
    def accept(self, visitor, state):
        return visitor.visitWildcardFarg(self, state)

    def __str__(self):
        return '*'


class Compound:
    def __init__(self, *fargss):
        self.fargss = fargss

    def accept(self, visitor):
        return visitor.visitCompoundFargs(self)

    def __str__(self):
        return "{\n" + ''.join(['  ' + str(x) + "\n" for x in self.fargss]) + '}'


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


class DfaState:
    counter = 0

    def __init__(self, action=None):
        self.action = action
        self.transitions = {}
        self.name = 'S' + str(DfaState.counter)
        self.sortKey = DfaState.counter
        self.stateTableIndex = 0
        DfaState.counter += 1

    def collapseActions(self, seen=None):
        if seen is None:
            seen = set()
        if self in seen:
            return self
        seen.add(self)
        if self.isConditional():
            for transition in self.transitions:
                self.transitions[transition] = self.transitions[transition].collapseActions(seen)
        else:
            newTransitions = dict()
            for transition in self.transitions:
                (newTransition, nextConditional) = self.collapseActionChain(transition, seen)
                nextConditional.collapseActions(seen)
                newTransitions[newTransition] = nextConditional
            self.transitions = newTransitions
        return self

    def getTransition(self):
        if len(self.transitions) == 1:
            for transition in self.transitions:
                return transition
        else:
            raise Exception(f'cannot call getTransition on non-action state: {self.getName()}')

    def collapseActionChain(self, transition, seen):
        if not len(self.transitions) == 1:
            raise Exception(f'action state with other than 1 transition: {self.getName()}')
        if self.transitions[transition].isConditional() or self.transitions[transition].isTerminal():
            return (transition, self.transitions[transition])
        else:
            nextTransition = self.transitions[transition].getTransition()
            (newTransition, nextState) = self.transitions[transition].collapseActionChain(nextTransition, seen)
            return (DfaActionTransition(transition, newTransition), nextState)

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
        dfa = nfaTransition.to.toDfa()
        if key in self.transitions:
            self.transitions[key].merge(dfa)
        else:
            self.transitions[key] = dfa

    def adjustEscape(self, transition, escape):
        if escape is None:
            return None
        if transition.stackCost() == 1:
            newState = DfaState()
            newState.transitions[DfaRetTransition('adjusted')] = escape
            return newState
        if transition.stackCost == -1:
            return escape.transitions[DfaRetTransition()]
        return escape

    def isConditional(self):
        return any([transition.isConditional() for transition in self.transitions])

    def isReturn(self):
        return any([transition.isReturn() for transition in self.transitions])

    def getReturn(self):
        for transition in self.transitions:
            return self.transitions[transition]

    def patchEscapes(self, escape=None):
        wildcard = DfaWildcardTransition()
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

    def removeReturns(self):
        returns = self.findReturns()
        while len(returns) > 0:
            self._removeReturns(returns)
            returns = self.findReturns()
        return self

    def findReturns(self):
        returns = dict()
        allStates = self.allReachableStates()
        for state in allStates:
            if state.isReturn():
                returns[state] = state.getReturn()
        return returns

    def _removeReturns(self, returns):
        allStates = self.allReachableStates()
        for state in allStates:
            state._removeReturn(returns)

    def _removeReturn(self, returns):
        for ret in returns:
            for transition in self.transitions:
                # print(f"compare {ret}\nwith    {self.transitions[transition]}")
                if ret == self.transitions[transition]:
                    self.transitions[transition] = returns[ret]

    def replaceDuplicates(self):
        duplicates = self.findDuplicates()
        while len(duplicates) > 0:
            self._replaceDuplicates(duplicates)
            duplicates = self.findDuplicates()

    def _replaceDuplicates(self, duplicates):
        for a in duplicates:
            for b in duplicates[a]:
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

    def mermaid(self, seen=None):
        if seen is None:
            seen = {}
        if self.name in seen:
            return
        seen[self.name] = True
        if self.isConditional():
            print(f'{self.name}{{{self.getLabel()}}}')
        else:
            print(f'{self.name}(({self.getLabel()}))')
        for transition in self.transitions:
            self.transitions[transition].mermaid(seen)
        for transition in self.transitions:
            if transition.isConditional():
                print(f'{self.name}--"{str(transition)}"-->{self.transitions[transition].name}')
            else:
                print(f'{self.name}-."{str(transition)}".->{self.transitions[transition].name}')


class DfaTransition:
    counter = 0

    def __init__(self):
        self.stateTableIndex = 0
        self.id = DfaTransition.counter
        DfaTransition.counter += 1

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

    def isReturn(self):
        return False

    def __lt__(self, other):
        return self.id < other.id

    def __cmp__(self, other):
        return cmp(self.id, other.id)

    def isWildcard(self):
        return False

    def key(self):
        return str(self)

class DfaActionTransition(DfaTransition):
    def __init__(self, head, tail):
        super().__init__()
        self.head = head
        self.tail = tail

    def __hash__(self):
        return hash(('action', hash(self.head), hash(self.tail)))

    def __eq__(self, other):
        return isinstance(other, DfaActionTransition) and self.head == other.head and self.tail == other.tail

    def __str__(self):
        return f'{str(self.head)}\\n{str(self.tail)}'

    def isReturn(self):
        return self.head.isReturn() and self.tail.isReturn()
        

class DfaNumericTransition(DfaTransition):
    def __init__(self, n):
        super().__init__()
        self.n = n

    def __hash__(self):
        return hash(('numeric', self.n))

    def __eq__(self, other):
        return isinstance(other, DfaNumericTransition) and self.n == other.n

    def __str__(self):
        return f'#{str(self.n)}'

    def isConditional(self):
        return True


class DfaLabelTransition(DfaTransition):
    def __init__(self, label):
        super().__init__()
        self.label = label

    def __hash__(self):
        return hash(('label', self.label))

    def __eq__(self, other):
        return isinstance(other, DfaLabelTransition) and self.label == other.label

    def __str__(self):
        return f'{{{self.label}}}'

    def isConditional(self):
        return True


class DfaVecTransition(DfaTransition):
    def __init__(self, label):
        super().__init__()
        self.label = label

    def __hash__(self):
        return hash(('vec', self.label))

    def __eq__(self, other):
        return isinstance(other, DfaVecTransition) and self.label == other.label

    def __str__(self):
        return f'vec {self.label}'

    def isConditional(self):
        return True


class DfaIndexTransition(DfaTransition):
    def __init__(self, index):
        super().__init__()
        self.index = index

    def __hash__(self):
        return hash(('index', str(self.index)))

    def __eq__(self, other):
        return isinstance(other, DfaIndexTransition) and self.index == other.index

    def __str__(self):
        return f'{str(self.index)}'

    def stackCost(self):
        return 1


class DfaArgTransition(DfaTransition):
    def __init__(self, index):
        super().__init__()
        self.index = index

    def __hash__(self):
        return hash(('arg', str(self.index)))

    def __eq__(self, other):
        return isinstance(other, DfaArgTransition) and self.index == other.index

    def __str__(self):
        return f'{str(self.index)}'

    def stackCost(self):
        return 1


class DfaVarTransition(DfaTransition):
    def __init__(self, var):
        super().__init__()
        self.var = var

    def __hash__(self):
        return hash(('var', self.var))

    def __eq__(self, other):
        return isinstance(other, DfaVarTransition) and self.var == other.var

    def __str__(self):
        return 'bind ' + self.var

    def isWildcard(self):
        return True

    def isConditional(self):
        return True


class DfaWildcardTransition(DfaTransition):
    def __hash__(self):
        return hash(('var', '*'))

    def __eq__(self, other):
        return isinstance(other, DfaWildcardTransition)

    def __str__(self):
        return 'else'

    def isWildcard(self):
        return True

    def isConditional(self):
        return True


class DfaComparisonTransition(DfaTransition):
    def __init__(self, var):
        super().__init__()
        self.var = var

    def __hash__(self):
        return hash(('cmp', self.var))

    def __eq__(self, other):
        return isinstance(other, DfaComparisonTransition) and self.var == other.var

    def __str__(self):
        return f'=={str(self.var)}'

    def isConditional(self):
        return True


class DfaRetTransition(DfaTransition):
    def __init__(self, label='?'):
        super().__init__()
        self.label = label

    def __hash__(self):
        return hash(('ret', ''))

    def __eq__(self, other):
        return isinstance(other, DfaRetTransition)

    def __str__(self):
        return f'ret {self.label}'

    def key(self):
        return 'ret'

    def stackCost(self):
        return -1

    def isReturn(self):
        return True


class NfaState:
    def __init__(self, out, action=None):
        assert isinstance(out, collections.abc.Sequence)
        self.out = out
        self.action = action

    def isTerminal(self):
        return len(self.out) == 0

    def __str__(self):
        if self.isTerminal():
            return f'(end {self.action})'
        return '\n'.join([str(out) for out in self.out])

    def toDfa(self):
        dfa = DfaState(self.action)
        for out in self.out:
            if out.isEmpty():
                dfa.merge(out.to.toDfa())
            else:
                dfa.addTransition(out)
        return dfa


class NfaTransition:
    def __init__(self, to):
        assert isinstance(to, NfaState)
        self.to = to

    def isEmpty(self):
        return False

    def key(self):
        raise Exception('key cannot be called on base transition')


class NfaEmptyTransition(NfaTransition):
    def __str__(self):
        return str(self.to)

    def isEmpty(self):
        return True

    def key(self):
        raise Exception('key cannot be called on empty transition')


class NfaNumericTransition(NfaTransition):
    def __init__(self, to, n):
        super().__init__(to)
        self.n = n

    def __str__(self):
        return '(=' + str(self.n) + ') ' + str(self.to)

    def key(self):
        return DfaNumericTransition(self.n)


class NfaIndexTransition(NfaTransition):
    def __init__(self, to, index):
        self.index = index
        self.to = to

    def __str__(self):
        return 'inspect index ' + str(self.index) + ' ' + str(self.to)

    def key(self):
        return DfaIndexTransition(self.index)


class NfaVecTransition(NfaTransition):
    def __init__(self, label, to):
        self.label = label
        self.to = to

    def __str__(self):
        return f'(={str(self.label)}) {str(self.to)}'

    def key(self):
        return DfaVecTransition(self.label)


class NfaArgTransition(NfaTransition):
    def __init__(self, to, index):
        self.index = index
        self.to = to

    def __str__(self):
        return 'inspect var ' + str(self.index) + ' ' + str(self.to)

    def key(self):
        return DfaArgTransition(self.index)


class NfaVarTransition(NfaTransition):
    def __init__(self, name, to):
        super().__init__(to)
        self.name = name

    def __str__(self):
        return 'bind ' + self.name + ' ' + str(self.to)

    def key(self):
        return DfaVarTransition(self.name)


class NfaWildcardTransition(NfaTransition):
    def __str__(self):
        return '(=*) ' + str(self.to)

    def key(self):
        return DfaWildcardTransition()


class NfaComparisonTransition(NfaTransition):
    def __init__(self, name, to):
        super().__init__(to)
        self.name = name

    def __str__(self):
        return f'(={self.name}) ' + str(self.to)

    def key(self):
        return DfaComparisonTransition(self.name)


class FargToNfaVisitor:
    def visitCompoundFargs(self, compound):
        count = 0;
        nfas = [fargs.accept(self, NfaState([], fargs.action)) for fargs in compound.fargss]
        return NfaState([NfaEmptyTransition(nfa) for nfa in nfas])

    def visitFargs(self, fargs, finalState):
        return self.recursivelyVisitFargs(fargs.fargs, len(fargs.fargs), finalState)

    def recursivelyVisitFargs(self, fargs, count, finalState):
        if count == 0:
            return finalState
        nextState = self.recursivelyVisitFargs(fargs, count - 1, finalState)
        index = len(fargs) - count
        return NfaState([NfaArgTransition(fargs[index].accept(self, nextState), fargs[index].position)])

    def visitNumericFarg(self, numeric, state):
        return NfaState([NfaNumericTransition(state, numeric.n)])

    def visitVecFarg(self, vec, finalState):
        return NfaState([NfaVecTransition(vec.label, self.recursivelyVisitVec(vec.fields, len(vec.fields), finalState))])

    def recursivelyVisitVec(self, fields, count, finalState):
        if count == 0:
            return finalState
        nextState = self.recursivelyVisitVec(fields, count - 1, finalState)
        index = len(fields) - count
        return NfaState([NfaIndexTransition(fields[index].accept(self, nextState), fields[index].position)])

    def visitVarFarg(self, var, state):
        return NfaState([NfaVarTransition(var.name, state)])

    def visitWildcardFarg(self, wildcard, state):
        return NfaState([NfaWildcardTransition(state)])

    def visitComparisonFarg(self, comparison, state):
        return NfaState([NfaComparisonTransition(comparison.name, state)])


def makeMermaid(args):
    print('```plaintext')
    print(str(args))
    print('```')
    nfa = args.accept(FargToNfaVisitor())
    dfa = nfa.toDfa()
    dfa.patchEscapes()
    dfa.replaceDuplicates()
    print('```mermaid')
    print('flowchart')
    dfa.collapseActions().removeReturns().mermaid()
    print('```')
    print("")

mapArgs = Compound(
    Fargs('finish', VarFarg('f'), VecFarg('nil')),
    Fargs('recurse', VarFarg('f'), VecFarg('cons', VarFarg('h'), VecFarg('cons', VarFarg('t'), VecFarg('nil'))))
)
makeMermaid(mapArgs)

memberArgs = Compound(
    Fargs('false', VarFarg('x'), VecFarg('nil')),
    Fargs('true', VarFarg('x'), VecFarg('cons', ComparisonFarg('x'), WildcardFarg())),
    Fargs('continue', VarFarg('x'), VecFarg('cons', WildcardFarg(), VarFarg('t')))
)
makeMermaid(memberArgs)

testArgs = Compound(
    Fargs('a', VecFarg('cons', NumericFarg(1), VecFarg('cons', NumericFarg(1), VecFarg('nil')))),
    Fargs('b', VecFarg('cons', NumericFarg(1), VecFarg('cons', NumericFarg(2), VecFarg('nil')))),
    Fargs('c', VecFarg('cons', NumericFarg(2), VecFarg('cons', NumericFarg(1), VecFarg('nil')))),
    Fargs('d', VecFarg('cons', NumericFarg(2), VecFarg('cons', NumericFarg(2), VecFarg('nil')))),
    Fargs('e', WildcardFarg()),
)
makeMermaid(testArgs)


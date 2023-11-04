#! /usr/bin/python3

import functools
import subprocess

def debug(*args):
    print('DEBUG', *args)

## basic functional support

def flatten(S):
    if S == []:
        return S
    if isinstance(S[0], list):
        return flatten(S[0]) + flatten(S[1:])
    return S[:1] + flatten(S[1:])

def flip(func):
    @functools.wraps(func)
    def newfunc(x, y):
        return func(y, x)
    return newfunc

def foldr(func, acc, xs):
    return functools.reduce(flip(func), reversed(xs), acc)

def List(*args):
    return foldr(lambda car, cdr: Pair(car, cdr), Null(), args)

def any(fn, lst):
    return len([x for x in lst if fn(x)]) > 0

## Expression classes (the expressions in the match rules)

class Exp:
    """
    Basic linked-list for lambda expressions
    """
    @classmethod
    def wrap(cls, thing):
        if type(thing) is str:
            return Symbol(thing)
        if type(thing) is list:
            return List(*thing)
        if type(thing) is int:
            return Int(thing)
        return thing

    def performSubstitutions(self, subst):
        return self


class Null(Exp):
    def __str__(self):
        return '()'


class Pair(Exp):
    def __init__(self, car, cdr):
        self.car = Exp.wrap(car)
        self.cdr = cdr

    def performSubstitutions(self, subst):
        return Pair(self.car.performSubstitutions(subst), self.cdr.performSubstitutions(subst))

    def __str__(self):
        return f'({self.innerStr()})'

    def innerStr(self):
        if type(self.cdr) is Null:
            return str(self.car)
        return str(self.car) + ' ' + self.cdr.innerStr()


class Scalar(Exp):
    def __init__(self, value):
        self.value = value

    def __eq__(self, other):
        return type(other) is type(self) and other.value == self.value

    def __str__(self):
        return str(self.value)


class Symbol(Scalar):
    def performSubstitutions(self, subst):
        if self.value in subst:
            return Symbol(subst[self.value])
        return self


class Int(Scalar):
    pass


class String(Scalar):
    def __str__(self):
        return f'"{self.value}"'


class Char(Scalar):
    def __str__(self):
        return f"'{self.value}'"

## Match rule classes

class MatchRules:
    counter = 0

    def __init__(self, *rules):
        self.rules = [*rules]
        if len(self.rules) == 0:
            raise Exception("MatchRules constructor called with 0 arguments")
        self.rootVariables = [f'p{i}' for i in range(rules[0].length())];
        self.errorState = None
        self.knownStates = None
        self.id = MatchRules.counter
        MatchRules.counter += 1

    def acceptErrorState(self, errorState):
        self.errorState = errorState

    def acceptKnownStates(self, knownStates):
        self.knownStates = knownStates

    def getErrorState(self):
        return self.errorState
        
    def identifyComparisons(self):
        for rule in self.rules:
            rule.identifyComparisons()

    def redundantActions(self):
        redundants = []
        for rule in self.rules:
            redundant = rule.redundantAction()
            if redundant is not None:
                redundants += [redundant]
        return redundants

    def hasErrorTransitions(self):
        return self.errorState.refCount > 0

    def renameVariables(self):
        for rule in self.rules:
            rule.renameVariables(self.rootVariables)

    def performSubstitutions(self):
        for rule in self.rules:
            rule.performSubstitutions()

    def replaceVarsWithWildcards(self):
        for rule in self.rules:
            rule.replaceVarsWithWildcards()

    def match(self):
        state = None
        if self.rules[0].isAllVariables():
            state = self.variableRule()
        else:
            state = self.mixtureRule()
        if str(state) in self.knownStates:
            return self.knownStates[str(state)]
        self.knownStates[str(state)] = state
        return state

    def variableRule(self):
        return self.rules[0].action

    def mixtureRule(self):
        index = self.findColumnIndexWithConstructor()
        column = self.columnAtIndex(index)
        otherColumns = self.columnsNotAtIndex(index)
        constructorIndices = column.findConstructorIndices()
        testState = TestState(self.getConstructorPath(index))
        for constructorIndex in constructorIndices:
            constructor = column.patterns[constructorIndex]
            matchingRowIndices = column.findIndicesMatching(constructor)
            patterns = column.getPatterns(matchingRowIndices)
            arity = constructor.arity()
            matrix = [pattern.extractSubPatterns(arity) for pattern in patterns]
            otherRows = self.rowsFromColumns(otherColumns, matchingRowIndices)
            fullMatrix = [a + b for a, b in zip(matrix, otherRows)]
            actions = [rule.action for rule in [self.rules[i] for i in matchingRowIndices]]
            rules = MatchRules(*[MatchRule(action, *patterns) for action, patterns in zip(actions, fullMatrix)])
            rules.acceptErrorState(self.errorState)
            rules.acceptKnownStates(self.knownStates)
            state = rules.match()
            arc = Arc(constructor.replaceSubPatternsWithWildcards(), state)
            testState.acceptArc(arc)
        constructors = [column.patterns[i] for i in constructorIndices]
        if constructors[0].isExhaustive(constructors):
            return testState
        else:
            wildcardIndices = column.findWildcardIndices()
            if len(wildcardIndices) > 0:
                otherRows = self.rowsFromColumns(otherColumns, wildcardIndices)
                actions = [rule.action for rule in [self.rules[i] for i in wildcardIndices]]
                rules = MatchRules(*[MatchRule(action, *patterns) for action, patterns in zip(actions, otherRows)])
                rules.acceptErrorState(self.errorState)
                rules.acceptKnownStates(self.knownStates)
                state = rules.match()
                wildcard = Wildcard()
                wildcard.acceptPath(self.getConstructorPath(index))
                arc = Arc(wildcard, state)
                testState.acceptArc(arc)
                return testState
            else:
                state = self.getErrorState()
                wildcard = Wildcard()
                wildcard.acceptPath(self.getConstructorPath(index))
                arc = Arc(wildcard, state)
                testState.acceptArc(arc)
                return testState


    def getConstructorPath(self, index):
        return self.rules[0].getConstructorPath(index)

    def rowsFromColumns(self, columns, indices):
        return [[column.patternAt(index) for column in columns] for index in indices]

    def findColumnIndexWithConstructor(self):
        index = self.rules[0].findFirstConstructor()
        if index is None:
            raise Exception("cannot find column with constructor")
        return index

    def columnAtIndex(self, index):
        return Column([rule.patterns[index] for rule in self.rules])

    def columnsNotAtIndex(self, index):
        return [self.columnAtIndex(i) for i in range(self.rules[0].length()) if i != index]

    def __str__(self):
        return f'; {self.id} {{\n;    ' + '\n;    '.join([str(rule) for rule in self.rules]) + '\n; }'


class Column:
    def __init__(self, patterns):
        self.patterns = patterns

    def findConstructorIndices(self):
        return [i for i in range(len(self.patterns)) if self.patterns[i].isNotVariable()]

    def findIndicesMatching(self, constructor):
        return [i for i in range(len(self.patterns)) if self.patterns[i].matches(constructor.getTerminal())]

    def getPatterns(self, indices):
        return [self.patterns[i] for i in indices]

    def patternAt(self, index):
        return self.patterns[index]

    def findWildcardIndices(self):
        return [i for i in range(len(self.patterns)) if not self.patterns[i].isNotVariable()]

class MatchRule:
    def __init__(self, action, *patterns):
        if type(action) is FinalState:
            self.action = action
        elif issubclass(type(action), Exp):
            self.action = FinalState(action)
        else:
            self.action = FinalState(Exp.wrap(action))
        self.patterns = [*patterns]
        for pat in self.patterns:
            if type(pat) is str:
                raise Exception("pattern cannot be simple string")

    def length(self):
        return len(self.patterns)

    def renameVariables(self, rootVariables):
        if len(rootVariables) != len(self.patterns):
            raise Exception("wrong number of arguments")
        for variable, pattern in zip(rootVariables, self.patterns):
            pattern.acceptPath(variable)

    def performSubstitutions(self):
        substitutions = foldr(lambda pat, subst: pat.addSubst(subst), dict(), self.patterns)
        self.action = self.action.performSubstitutions(substitutions)

    def replaceVarsWithWildcards(self):
        for i in range(len(self.patterns)):
            self.patterns[i] = self.patterns[i].replaceVarsWithWildcards()

    def isAllVariables(self):
        return self.findFirstConstructor() is None

    def findFirstConstructor(self):
        for i in range(len(self.patterns)):
            if self.patterns[i].isNotVariable():
                return i
        return None
        
    def getConstructorPath(self, index):
        return self.patterns[index].path

    def identifyComparisons(self):
        seen = dict()
        for i in range(len(self.patterns)):
            self.patterns[i] = self.patterns[i].identifyComparisons(seen)

    def redundantAction(self):
        if self.action.refCount == 0:
            return self.action

    def __str__(self):
        return '[' + ', '.join([str(pat) for pat in self.patterns]) + '] => ' + str(self.action)

### Patterns

class Pattern:
    def __init__(self):
        self.path = None

    def acceptPath(self, path):
        self.path = path

    def prefix(self):
        if self.path is None:
            return ''
        return f'{self.path}='

    def addSubst(self, subst):
        return subst

    def isComparison(self):
        return False

    def isConstant(self):
        return False

    def isRealConstructor(self):
        return False

    def isAssignment(self):
        return False

    def isNotVariable(self):
        return False

    def isStructured(self):
        return False

    def replaceVarsWithWildcards(self):
        return self

    def replaceSubPatternsWithWildcards(self):
        return self

    def getComponents(self):
        return []

    def arity(self):
        return 0

    def getTerminal(self):
        return self

    def identifyComparisons(self, seen):
        return self

    def __str__(self):
        return f'{self.prefix()}{self.str()}'

    def __eq__(self, other):
        raise Exception(f'equality test on {type(self)} == {type(other)}')


class Var(Pattern):
    def __init__(self, name):
        super().__init__()
        self.name = Exp.wrap(name)

    def addSubst(self, subst):
        subst[str(self.name)] = self.path
        return subst

    def replaceVarsWithWildcards(self):
        new = Wildcard()
        new.acceptPath(self.path)
        return new

    def identifyComparisons(self, seen):
        if str(self.name) in seen:
            if seen[str(self.name)].isAssignment():
                raise Exception("cannot perform equality tests on assignments")
            return Comparison(seen[str(self.name)], self)
        seen[str(self.name)] = self
        return self

    def str(self):
        return str(self.name) if self.path is None else '_'


class Comparison(Pattern):
    def __init__(self, previous, current):
        super().__init__()
        self.previous = previous
        self.current = current

    def acceptPath(self, path):
        super().acceptPath(path)
        self.current.acceptPath(path)

    def addSubst(self, subst):
        self.current.addSubst(subst)
        return subst

    def replaceVarsWithWildcards(self):
        self.previous = self.previous.replaceVarsWithWildcards()
        self.current = self.current.replaceVarsWithWildcards()
        return self

    def matches(self, constructor):
        return self.previous.matches(constructor) and self.current.matches(constructor)

    def arity(self):
        if self.previous.arity() > 0:
            return self.previous.arity()
        if self.current.arity() > 0:
            return self.current.arity()
        return 0

    def extractSubPatterns(self, arity):
        return self.current.extractSubPatterns(arity)

    def isExhaustive(self, otherPatterns):
        return False

    def isNotVariable(self):
        return True

    def isComparison(self):
        return True

    def getVariable(self):
        return self.previous.path

    def getTerminal(self):
        return self.previous.getTerminal()

    def __str__(self):
        return f'{str(self.previous)} == {str(self.current)}'


class Assignment(Pattern):
    def __init__(self, name, value):
        super().__init__()
        self.name = Exp.wrap(name)
        self.value = Exp.wrap(value)

    def acceptPath(self, path):
        super().acceptPath(path)
        self.value.acceptPath(path)

    def addSubst(self, subst):
        subst[str(self.name)] = self.path
        return self.value.addSubst(subst)
        
    def isNotVariable(self):
        return self.value.isNotVariable()

    def replaceVarsWithWildcards(self):
        self.value = self.value.replaceVarsWithWildcards()
        return self

    def matches(self, constructor):
        return self.value.matches(constructor)

    def arity(self):
        return self.value.arity()

    def extractSubPatterns(self, arity):
        return self.value.extractSubPatterns(arity)

    def replaceSubPatternsWithWildcards(self):
        self.value.replaceSubPatternsWithWildcards()
        return self

    def isExhaustive(self, otherPatterns):
        return self.value.isExhaustive(otherPatterns)

    def getTerminal(self):
        return self.value.getTerminal()

    def isRealConstructor(self):
        return self.value.isRealConstructor()

    def allIndices(self):
        return self.value.allIndices()

    def index(self):
        return self.value.index()

    def isConstant(self):
        return self.value.isConstant()

    def boundVariables(self):
        return self.value.boundVariables()

    def str(self):
        return str(self.name) + '=' + str(self.value)

    def getComponents(self):
        return self.value.getComponents()

    def identifyComparisons(self, seen):
        if str(self.name) in seen:
            raise Exception("cannot perform equality checks on assignments")
        seen[str(self.name)] = self
        self.value = self.value.identifyComparisons(seen)
        return self

    def isAssignment(self):
        return True

    def __eq__(self, other):
        return self.value == other


class Wildcard(Pattern):
    def str(self):
        return '_'

    def matches(self, constructor):
        return True

    def extractSubPatterns(self, arity):
        if arity == 0:
            return []
        wildcards = []
        for path in [f'{self.path}${i+1}' for i in range(arity)]:
            wildcard = Wildcard()
            wildcard.acceptPath(path)
            wildcards.append(wildcard)
        return wildcards

    def boundVariables(self):
        ret = set()
        if '$' in self.path:
            ret.add(self.path)
        return ret

    def __eq__(self, other):
        return type(other) is Wildcard and self.path == other.path


class Constant(Pattern):
    def __init__(self, value):
        super().__init__()
        self.value = Exp.wrap(value)

    def isNotVariable(self):
        return True

    def str(self):
        return str(self.value)

    def arity(self):
        return 0

    def extractSubPatterns(self, arity):
        if arity == 0:
            return []
        else:
            raise Exception(f"arity {arity} on constant")

    def isExhaustive(self, otherPatterns):
        return False

    def matches(self, constructor):
        return type(constructor) is Constant and self.value == constructor.value

    def isConstant(self):
        return True

    def boundVariables(self):
        return set()

    def __eq__(self, other):
        return type(other) is Constant and self.value == other.value


class Constructor(Pattern):
    knownTags = {
        "null": {
            "siblings": [ "cons" ],
            "arity": 0,
            "structured": True,
            "index": 0
        },
        "cons": {
            "siblings": [ "null" ],
            "arity": 2,
            "structured": True,
            "index": 1
        },
        "false": {
            "siblings": [ "true" ],
            "arity": 0,
            "structured": False,
            "index": 0
        },
        "true": {
            "siblings": [ "false" ],
            "arity": 0,
            "structured": False,
            "index": 1
        },
    }

    def __init__(self, tag, *components):
        super().__init__()
        if tag not in self.knownTags:
            raise Exception(f"unrecognised constructor tag {tag}")
        if self.knownTags[tag]['arity'] != len(components):
            raise Exception(f"wrong number of arguments ({len(components)}) to constructor {tag}, expected {self.knownTags[tag]['arity']}")
        self.tag = tag
        self.components = [*components]

    def getComponents(self):
        return self.components

    def acceptPath(self, path):
        super().acceptPath(path)
        for i in range(len(self.components)):
            self.components[i].acceptPath(f'{path}${i+1}')

    def addSubst(self, subst):
        for i in range(len(self.components)):
            self.components[i].addSubst(subst)
        return subst

    def allIndices(self):
        res = set()
        for i in range(len(self.knownTags[self.tag]['siblings']) + 1):
            res.add(i)
        return res

    def index(self):
        return self.knownTags[self.tag]['index']

    def boundVariables(self):
        init = { self.path }
        return functools.reduce(lambda bvs, component: bvs | {component.path}, self.components, init)

    def replaceVarsWithWildcards(self):
        for i in range(len(self.components)):
            self.components[i] = self.components[i].replaceVarsWithWildcards()
        return self

    def isRealConstructor(self):
        return True

    def isNotVariable(self):
        return True

    def isStructured(self):
        return self.knownTags[self.tag]["structured"]

    def matches(self, constructor):
        return type(constructor) is Constructor and self.tag == constructor.tag

    def arity(self):
        return len(self.components)

    def isExhaustive(self, otherPatterns):
        for required in self.knownTags[self.tag]['siblings']:
            if len([x for x in otherPatterns if str(x.getTerminal().tag) == required]) == 0:
                return False
        return True

    def extractSubPatterns(self, arity):
        if arity == len(self.components):
            return self.components
        else:
            raise Exception(f"wrong arity {arity} for constructor {self.tag}")

    def identifyComparisons(self, seen):
        for i in range(len(self.components)):
            self.components[i] = self.components[i].identifyComparisons(seen)
        return self

    def str(self):
        return str(self.tag) + ('' if len(self.components) == 0 else ('(' + ', '.join([str(comp) for comp in self.components]) + ')' ))

    def __eq__(self, other):
        return type(other) is Constructor and self.path == other.path and self.tag == other.tag

### Intermediate DFA classes

class State:
    counter = 0

    def __init__(self):
        self.refCount = 0
        self.stamp = f'q{State.counter}'
        State.counter += 1
        self.freeVariables = set()

    def convertToIntermediate(self, variables):
        self.recalculateRefCounts()
        lambdas = self.collectLambdas(dict())
        if len(lambdas) > 0:
            return List('lambda', variables, ['letrec', [lam.convertToLambda() for lam in lambdas.values()], self.convertToCode()])
        return List('lambda', variables, self.convertToCode())

    def convertToCode(self):
        if self.refCount > 1:
            return self.convertToCall()
        return self.convertToAction()

    def convertToCall(self):
        return List(self.stamp, *sorted(self.calculateFreeVariables()))

    def convertToLambda(self):
        return List(self.stamp, ['lambda', sorted(self.calculateFreeVariables()), self.convertToAction()])

    def recalculateRefCounts(self):
        self.resetRefCountsToZero()
        self.incrementRefCounts()

    def calculateFreeVariables(self):
        return self.freeVariables


class TestState(State):
    def __init__(self, path):
        super().__init__()
        self.path = path
        self.arcs = []

    def resetRefCountsToZero(self):
        self.refCount = 0
        for arc in self.arcs:
            arc.resetRefCountsToZero()

    def incrementRefCounts(self):
        self.refCount += 1
        if self.refCount == 1:
            for arc in self.arcs:
                arc.incrementRefCounts()

    def collectLambdas(self, lambdas):
        if self.refCount > 1:
            lambdas[str(self)] = self
        for arc in self.arcs:
            arc.collectLambdas(lambdas)
        return lambdas

    def acceptArc(self, arc):
        for existing in self.arcs:
            if arc == existing:
                return
        self.arcs.append(arc)

    def calculateFreeVariables(self):
        init = set()
        if '$' in self.path:
            init.add(self.path)
        res = functools.reduce(lambda fvs, arc: fvs | arc.calculateFreeVariables(), self.arcs, init)
        return res

    def convertToAction(self):
        # (match var (...))
        # (if (eq var val) ... )
        # (match (vec n var) ... )
        constructors = [arc for arc in self.arcs if arc.isNotVariable()]
        if len(constructors) > 0:
            for arc in self.arcs:
                arc.noteConstructor(constructors)
            if arc.isStructured():
                return List('match', ['vec', 0, self.path], *[arc.matchClause() for arc in self.arcs])
            return List('match', self.path, *[arc.matchClause() for arc in self.arcs])
        constants = [arc for arc in self.arcs if arc.isConstant()]
        if len(constants) > 0:
            return List('cond', self.path, *[arc.condClause() for arc in self.arcs])
        comparisons = [arc for arc in self.arcs if arc.isComparison()]
        if len(comparisons) > 0:
            return List('cond', self.path, *[arc.condClause() for arc in self.arcs])
        return List('todo')

    def __eq__(self, other):
        if type(other) is not TestState:
            return False
        if self.stamp == other.stamp:
            return True
        if self.path != other.path:
            return False
        return len([a for a, b in zip(self.arcs, other.arcs) if a != b]) > 0

    def __hash__(self):
        return hash(str(self))

    def __str__(self):
        return f'({self.path}: [{", ".join([str(arc) for arc in self.arcs])}])'


class FinalState(State):
    def __init__(self, action):
        super().__init__()
        self.action = action

    def resetRefCountsToZero(self):
        self.refCount = 0

    def incrementRefCounts(self):
        self.refCount += 1

    def collectLambdas(self, lambdas):
        if self.refCount > 1:
            lambdas[str(self)] = self
        return lambdas

    def performSubstitutions(self, subst):
        self.action = self.action.performSubstitutions(subst)
        for var in subst.values():
            if '$' in var:
                self.freeVariables.add(var)
        return self

    def __eq__(self, other):
        return type(other) is FinalState and self.stamp == other.stamp

    def __hash__(self):
        return hash(self.stamp)

    def convertToAction(self):
        return self.action

    def __str__(self):
        return f'{self.stamp}={str(self.action)}'


class ErrorState(State):
    def __str__(self):
        return 'ERROR'

    def resetRefCountsToZero(self):
        pass

    def incrementRefCounts(self):
        pass

    def convertToCode(self):
        return List('ERROR')

    def collectLambdas(self, lambdas):
        return lambdas


class Arc:
    def __init__(self, test, state):
        self.test = test
        self.state = state
        self.state.refCount += 1
        self.indices = None

    def resetRefCountsToZero(self):
        self.state.resetRefCountsToZero()

    def incrementRefCounts(self):
        self.state.incrementRefCounts()

    def collectLambdas(self, lambdas):
        return self.state.collectLambdas(lambdas)

    def isNotVariable(self):
        return self.test.isRealConstructor()

    def isConstant(self):
        return self.test.isConstant()

    def isComparison(self):
        return self.test.isComparison()

    def isStructured(self):
        return self.test.isStructured()

    def calculateFreeVariables(self):
        fv = self.state.calculateFreeVariables()
        bv = self.test.boundVariables()
        return fv - bv

    def noteConstructor(self, constructors):
        if not self.isNotVariable():
           self.indices = self.missingIndices(constructors)

    def missingIndices(self, constructors):
        allIndices = constructors[0].allIndices()
        for constructor in constructors:
            allIndices.discard(constructor.index())
        return [x for x in allIndices]

    def allIndices(self):
        return self.test.allIndices()
        
    def index(self):
        return self.test.index()

    def matchClause(self):
        if self.indices is None:
            self.indices = [self.index()]
        return [self.indices, self.createBindings(self.state.convertToCode())]

    def condClause(self):
        code = self.createBindings(self.state.convertToCode())
        if self.isConstant():
            return [self.test.value, code]
        if self.isComparison():
            return [self.test.getVariable(), code]
        return ['else', code]

    def createBindings(self, code):
        components = self.test.getComponents()
        return foldr(lambda i, c: ['let', [components[i].path, ['vec', i + 1, self.test.path]], c], code, range(len(components)))

    def __eq__(self, other):
        if type(other) is not Arc:
            raise Exception(f"cannot compare Arc with {type(other)}")
        return self.test == other.test and self.state == other.state

    def __str__(self):
        return f'({str(self.test)} => {str(self.state)})'

### Tests

def printIt(schemeStr):
    with open("/tmp/junk", "w") as outFile:
        print(schemeStr, file=outFile)
    subprocess.run(['pp', '/tmp/junk'])
    subprocess.run(['rm', '/tmp/junk'])

def testIt(example):
    print(str(example))
    example.acceptErrorState(ErrorState())
    example.acceptKnownStates(dict())
    example.identifyComparisons()
    example.renameVariables()
    example.performSubstitutions()
    example.replaceVarsWithWildcards()
    dfa = example.match()
    # print(str(dfa))
    printIt(dfa.convertToIntermediate(example.rootVariables))
    if example.hasErrorTransitions():
        print("; non-exhaustive pattern match detected")
    redundancies = example.redundantActions()
    if len(redundancies) > 0:
        print(f"; redundant actions detected: {[str(x) for x in redundancies]}")
    print()
    print()
    print()

testIt(MatchRules(
    MatchRule('null', Wildcard(), Constructor('null')),
    MatchRule(["cons", ["f", "h"], ["map", "f", "t"]],  Var('f'), Constructor('cons', Var('h'), Var('t')))
))

testIt(MatchRules(
    MatchRule(["cons", ["f", "h"], ["map", "f", "t"]],  Var('f'), Constructor('cons', Var('h'), Var('t')))
))

testIt(MatchRules(
    MatchRule('null', Wildcard(), Constructor('null')),
))

testIt(MatchRules(
    MatchRule(1, Constant(0)),
    MatchRule(['*', 'n', ['factorial', ['-', 'n', 1]]], Var('n'))
))

testIt(MatchRules(
    MatchRule(['*', 'n', ['factorial', ['-', 'n', 1]]], Var('n'))
))

testIt(MatchRules(
    MatchRule(1, Constant(0)),
))

testIt(MatchRules(
    MatchRule('A', Constructor('null'), Constructor('null')),
    MatchRule(['B', 'xs', 'ys'], Var('xs'), Var('ys'))
))

testIt(MatchRules(
    MatchRule('E1', Constructor('null'), Var('ys')),
    MatchRule('E2', Var('xs'), Constructor('null')),
    MatchRule('E3', Constructor('cons', Var('x'), Var('xs')), Constructor('cons', Var('y'), Var('ys')))
))

testIt(MatchRules(
    MatchRule('v', Assignment('v', Constructor('cons', Constant(1), Constructor('null')))),
    MatchRule('w', Wildcard())
))

testIt(MatchRules(
    MatchRule('a', Constructor('cons', Constant(1), Constructor('cons', Constant(1), Constructor('null')))),
    MatchRule('b', Constructor('cons', Constant(1), Constructor('cons', Constant(2), Constructor('null')))),
    MatchRule('c', Constructor('cons', Constant(2), Constructor('cons', Constant(1), Constructor('null')))),
    MatchRule('d', Constructor('cons', Constant(2), Constructor('cons', Constant(2), Constructor('null')))),
    MatchRule('e', Wildcard())
))

testIt(MatchRules(
    MatchRule('false', Wildcard(), Constructor('null')),
    MatchRule('true', Var('x'), Constructor('cons', Var('x'), Wildcard())),
    MatchRule(['member', 'x', 't'], Var('x'), Constructor('cons', Wildcard(), Var('t')))
))

testIt(MatchRules(
    MatchRule('false', Constructor('null'), Wildcard()),
    MatchRule('true', Constructor('cons', Var('x'), Wildcard()), Var('x')),
    MatchRule(['member', 'x', 't'], Constructor('cons', Wildcard(), Var('t')), Var('x'))
))

testIt(MatchRules(
    MatchRule('false', Constructor('true')),
    MatchRule('true', Constructor('false'))
))

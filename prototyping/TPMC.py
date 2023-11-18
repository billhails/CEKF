import functools

"""
Prototype for https://www.classes.cs.uchicago.edu/archive/2011/spring/22620-1/papers/pettersson92.pdf

A Term Pattern-Match Compiler
Inspired by Finite Automata Theory
"""

baseVar = 'p'

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

############################### action classes and helper functions

def List(*args):
    return foldr(lambda car, cdr: Pair(car, cdr), None, args)

def wrap(thing):
    if type(thing) is list:
        return List(*thing)
    elif type(thing) is int:
        return Int(thing)
    elif type(thing) is str:
        return Var(thing)
    else:
        return thing


class Pair:
    """
    Basic linked list to make substitutions on actions easier
    """
    def __init__(self, car, cdr):
        self.car = wrap(car)
        self.cdr = cdr

    def __str__(self):
        return '(' + self._innerStr() + ')'

    def _innerStr(self):
        if self.cdr is None:
            return str(self.car)
        else:
            return str(self.car) + ' ' + self.cdr._innerStr()

    def substitute(self, subst):
        car = self.car.substitute(subst)
        if self.cdr is None:
            cdr = None
        else:
            cdr = self.cdr.substitute(subst)
        return Pair(car, cdr)


class Scalar:
    """
    Action Types
    """
    def __init__(self, value):
        self.value = value
    
    def substitute(self, subst):
        return self


class Var(Scalar):
    def __str__(self):
        return self.value

    def substitute(self, subst):
        if self.value in subst:
            return Var(subst[self.value])
        else:
            return self


class Int(Scalar):
    def __str__(self):
        return str(self.value)

    def __eq__(self, other):
        return type(other) is Int and other.value == self.value

class String(Scalar):
    def __str__(self):
        return f'"{self.value}"'

    def __eq__(self, other):
        return type(other) is String and other.value == self.value

class Char(Scalar):
    def __str__(self):
        return f"'{self.value}'"

    def __eq__(self, other):
        return type(other) is Char and other.value == self.value


###################################### function structural classes

class MatchRule:
    """
    FargList plus action (body)
    """
    def __init__(self, action, *fargs):
        self.fargs = FargList(fargs)
        self.fargs.notePosition([])
        subst = self.fargs.makeSubstitutions()
        self.action = wrap(action).substitute(subst)

    def __str__(self):
        return f'{str(self.fargs)} => {{ {str(self.action)} }}'

    def convertArgs(self):
        self.fargs.convertArgs()

    def toTests(self):
        return self.fargs.toTests()

    def toFinal(self):
        return FinalState(self.action)


class FargList:
    """
    List of Formal Arguments
    """
    def __init__(self, fargs):
        self.fargs = [*fargs]

    def __str__(self):
        return '(' + ', '.join([str(x) for x in self.fargs]) + ')'

    def prepare(self, seen):
        for farg in self.fargs:
            farg.prepare(seen)

    def notePosition(self, pos):
        for i in range(len(self.fargs)):
            self.fargs[i].notePosition(pos + [i])

    def makeSubstitutions(self):
        return functools.reduce(lambda a, b: a | b, [farg.makeSubstitutions() for farg in self.fargs], dict())

    def convertArgs(self):
        for i in range(len(self.fargs)):
            self.fargs[i] = self.fargs[i].convertArgs()

    def toTests(self):
        return [farg.toTest() for farg in self.fargs]


class VecFargList(FargList):
    """
    Arguments to (components of) a vec constructor
    """
    def __str__(self):
        if len(self.fargs) == 0:
            return ''
        return super().__str__()

    def notePosition(self, pos):
        for i in range(len(self.fargs)):
            self.fargs[i].notePosition(pos + [i + 1])
        

class Farg:
    """
    Abstract base class for single Formal Arguments
    """
    def notePosition(self, pos):
        self.position = pos

    def prepare(self, seen):
        pass

    def path(self):
        return f'{baseVar}' + '$'.join([str(pos) for pos in self.position])

    def makeSubstitutions(self):
        return dict()

    def convertArgs(self):
        return AssignmentFarg(self.path(), self)


class NumericFarg(Farg):
    """
    literal number
    """
    def __init__(self, n):
        self.n = n

    def __str__(self):
        return str(self.n)

    def toTest(self):
        return TestState(self.pattern(), Int(self.n))


class CharFarg(Farg):
    """
    literal character
    """
    def __init__(self, c):
        self.c = c

    def __str__(self):
        return f"'{str(self.c)}'"

    def toTest(self):
        return TestState(self.pattern(), Char(self.c))


"""
Information about various vecs
"""
cons = {
    "name": "cons",
    "siblings": ["0"],
    "id": 1,
    "hasFields": True
}

nil = {
    "name": "nil",
    "siblings": ["1"],
    "id": 0,
    "hasFields": True # sibling has fields
}

red = {
    "name": "red",
    "siblings": ["1", "2"],
    "id": 0,
    "hasFields": False
}

green = {
    "name": "green",
    "siblings": ["0", "2"],
    "id": 1,
    "hasFields": False
}

blue = {
    "name": "blue",
    "siblings": ["0", "1"],
    "id": 2,
    "hasFields": False
}


class VecFarg(Farg):
    """
    vector type like pair or maybe
    """
    def __init__(self, label, *fields):
        self.label = label
        self.fields = VecFargList(fields)

    def notePosition(self, pos):
        super().notePosition(pos)
        self.fields.notePosition(pos)

    def __str__(self):
        return self.label['name'] + str(self.fields)

    def prepare(self, seen):
        self.fields.prepare(seen)

    def makeSubstitutions(self):
        return self.fields.makeSubstitutions()

    def convertArgs(self):
        self.fields.convertArgs()
        return super().convertArgs()
        

class VarFarg(Farg):
    """
    variable
    """
    def __init__(self, name):
        self.name = name
        self.prior = False

    def __str__(self):
        return self.name

    def prepare(self, seen):
        if self.name in seen:
            self.prior = True
        else:
            seen.add(self.name)

    def makeSubstitutions(self):
        return {self.name: self.path()}

    def convertArgs(self):
        return AssignmentFarg(self.path(), WildcardFarg())


class AssignmentFarg(Farg):
    """
    name = value
    """
    def __init__(self, name, value):
        self.name = name
        self.value = value
        self.prior = False

    def __str__(self):
        return f'{self.name}={str(self.value)}'

    def notePosition(self, pos):
        super().notePosition(pos)
        self.value.notePosition(pos)

    def prepare(self, seen):
        if self.name in seen:
            self.prior = True
        else:
            seen.add(self.name)

    def makeSubstitutions(self):
        return {self.name: self.path()} | self.value.makeSubstitutions()

    def convertArgs(self):
        return self.value.convertArgs()


class WildcardFarg(Farg):
    """
    Wildcard _
    """
    def __str__(self):
        return '_'

    def convertArgs(self):
        return AssignmentFarg(self.path(), self)


class Compound:
    """
    Compound function with 1..n bodies
    """
    def __init__(self, name, nargs, *matchRules):
        self.name = name
        self.nargs = nargs
        self.matchRules = [*matchRules]
        for rule in self.matchRules:
            rule.convertArgs()

    def __str__(self):
        return f"fn {self.name} {{\n" + ''.join(['  ' + str(x) + "\n" for x in self.matchRules]) + '}'

    def toMatrix(self):
        return { "tests": [rule.toTests() for rule in self.matchRules], "finals": [rule.toFinal() for rule in self.matchRules] }


########################################## DFA classes


class State:
    def __init__(self):
        self.refCount = 0


class TestState(State)
    pass


class FinalState(State)
    def __init__(self, action):
        super().__init__()
        self.action = action


########################################## tests

mapArgs = Compound(
    "map", 2,
    MatchRule(['make-vec', 0], WildcardFarg(), VecFarg(nil)),
    MatchRule(['make-vec', 1, ['f', 'h'], ['map', 'f', 't']], VarFarg('f'), VecFarg(cons, VarFarg('h'), VarFarg('t')))
)
print(str(mapArgs))

factArgs = Compound(
    "factorial", 1,
    MatchRule(1, NumericFarg(0)),
    MatchRule(['*', 'n', ['factorial', ['-', 'n', 1]]], VarFarg('n'))
)
print(str(factArgs))

memberArgs = Compound(
    "member", 2,
    MatchRule('false', WildcardFarg(), VecFarg(nil)),
    MatchRule('true', VarFarg('x'), VecFarg(cons, VarFarg('x'), WildcardFarg())),
    MatchRule(['member', 'x', 't'], VarFarg('x'), VecFarg(cons, WildcardFarg(), VarFarg('t')))
)
print(str(memberArgs))

testArgs = Compound(
    "test", 1,
    MatchRule('a', VecFarg(cons, NumericFarg(1), VecFarg(cons, NumericFarg(1), VecFarg(nil)))),
    MatchRule('b', VecFarg(cons, NumericFarg(1), VecFarg(cons, NumericFarg(2), VecFarg(nil)))),
    MatchRule('c', VecFarg(cons, NumericFarg(2), VecFarg(cons, NumericFarg(1), VecFarg(nil)))),
    MatchRule('d', VecFarg(cons, NumericFarg(2), VecFarg(cons, NumericFarg(2), VecFarg(nil)))),
    MatchRule('e', WildcardFarg()),
)
print(str(testArgs))

assignmentArgs = Compound(
    "test2", 2,
    MatchRule('a', AssignmentFarg('x', VecFarg(cons, CharFarg('a'), VecFarg(nil))), VecFarg(cons, AssignmentFarg('y', VecFarg(cons, NumericFarg(2), VecFarg(nil))), VecFarg(nil)))
)
print(str(assignmentArgs))

colourArgs = Compound(
    "colourToChar", 1,
    MatchRule('r', VecFarg(red)),
    MatchRule('g', VecFarg(green)),
    MatchRule('b', VecFarg(blue))
)
print(str(colourArgs))

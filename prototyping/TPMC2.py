import functools

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
    pass


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
    def __init__(self, *rules):
        self.rules = [*rules]
        self.rootVariables = [f'p{i}' for i in range(rules[0].length())];

    def renameVariables(self):
        for rule in self.rules:
            rule.renameVariables(self.rootVariables)

    def performSubstitutions(self):
        for rule in self.rules:
            rule.performSubstitutions()

    def __str__(self):
        return f'({", ".join([str(v) for v in self.rootVariables])}) {{\n' + '\n'.join([str(rule) for rule in self.rules]) + '\n}'


class MatchRule:
    def __init__(self, action, *patterns):
        self.action = Exp.wrap(action)
        self.patterns = [*patterns]

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

    def __str__(self):
        return '[' + ', '.join([str(pat) for pat in self.patterns]) + '] => ' + str(self.action)


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

    def __str__(self):
        return f'{self.prefix()}{self.str()}'


class Var(Pattern):
    def __init__(self, name):
        super().__init__()
        self.name = Exp.wrap(name)

    def addSubst(self, subst):
        subst[str(self.name)] = self.path
        return subst

    def str(self):
        return str(self.name) if self.path is None else '_'


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
        
    def str(self):
        return str(self.name) + '=' + str(self.value)


class Wildcard(Pattern):
    def str(self):
        return '_'


class Constant(Pattern):
    def __init__(self, value):
        super().__init__()
        self.value = Exp.wrap(value)

    def str(self):
        return str(self.value)


class Constructor(Pattern):
    def __init__(self, tag, *components):
        super().__init__()
        self.tag = Exp.wrap(tag)
        self.components = [*components]

    def acceptPath(self, path):
        super().acceptPath(path)
        for i in range(len(self.components)):
            self.components[i].acceptPath(f'{path}${i+1}')

    def addSubst(self, subst):
        for i in range(len(self.components)):
            self.components[i].addSubst(subst)
        return subst


    def str(self):
        return str(self.tag) + ('' if len(self.components) == 0 else ('(' + ', '.join([str(comp) for comp in self.components]) + ')' ))


### Intermediate DFA classes

class DFA:
    def __init__(self):
        self.testStates = []
        self.finalStates = [Error()]


class State:
    pass


class TestState(State):
    pass


class FinalState(State):
    pass


class Error(FinalState):
    pass

### Tests

mapExample = MatchRules(
    MatchRule(['null'], Wildcard(), Constructor('null')),
    MatchRule([["cons", ["f", "h"], ["map", "f", "t"]]],  Var('f'), Constructor('cons', Var('h'), Var('t')))
)
mapExample.renameVariables()
mapExample.performSubstitutions()
print(str(mapExample))

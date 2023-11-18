import functools

def flip(func):
    @functools.wraps(func)
    def newfunc(x, y):
        return func(y, x)
    return newfunc

def foldr(func, acc, xs):
    return functools.reduce(flip(func), reversed(xs), acc)


class Pair:
    def __init__(self, car, cdr):
        self.car = car
        self.cdr = cdr

    def __str__(self):
        return '(' + self._innerStr() + ')'

    def _innerStr(self):
        if self.cdr is None:
            return str(self.car)
        else:
            return str(self.car) + ' ' + self.cdr._innerStr()


def List(*args):
    return foldr(lambda car, cdr: Pair(car, cdr), None, args)

class String:
    def __init__(self, value):
        self.value = value

    def __str__(self):
        return f'"{self.value}"'


class Single:
    """
    FargList plus action (body)
    """
    def __init__(self, action, *fargs):
        self.action = action
        self.fargs = FargList(fargs)
        self.fargs.notePosition([])

    def __str__(self):
        return f'{str(self.fargs)} => {{ {str(self.action)} }}'

    def toParser(self):
        self.fargs.prepare(set())
        return self.fargs.toParser(List('cut', self.action))


class FargList:
    """
    List of Formal Arguments
    """
    def __init__(self, fargs, offset = None):
        self.fargs = fargs
        if offset is not None:
            self.offset = offset
        else:
            self.offset = 0

    def __str__(self):
        return '(' + ', '.join([str(x) for x in self.fargs]) + ')'

    def toParser(self, rest):
        return foldr(lambda a, b: a.toParser(b), rest, self.fargs)

    def prepare(self, seen):
        for farg in self.fargs:
            farg.prepare(seen)

    def notePosition(self, position):
        for i in range(len(self.fargs)):
            self.fargs[i].notePosition(position + [i + self.offset])


class Farg:
    """
    Abstract base class for single Formal Arguments
    """
    def notePosition(self, i):
        self.position = i

    def toParser(self, rest):
        return f'(stub {rest})'

    def prepare(self, seen):
        pass

    def accessArg(self):
        return functools.reduce(lambda a, b: List('vec', b, a), self.position[1:], f'arg{self.position[0]}')


class NumericFarg(Farg):
    """
    literal number
    """
    def __init__(self, n):
        self.n = n

    def __str__(self):
        return str(self.n)

    def toParser(self, rest):
        return List('if',  List('eq', self.accessArg(), self.n), rest, List('back'))


class CharFarg(Farg):
    """
    literal character
    """
    def __init__(self, c):
        self.c = c

    def __str__(self):
        return f"'{str(self.c)}'"

    def toParser(self, rest):
        return List('if',  List('eq', self.accessArg(), str(self)), rest, List('back'))


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
        self.fields = FargList(fields, 1)

    def notePosition(self, pos):
        super().notePosition(pos)
        self.fields.notePosition(pos)

    def __str__(self):
        return self.label['name'] + str(self.fields)

    def toParser(self, rest):
        if self.label['hasFields']:
            access = List('vec', 0, self.accessArg())
        else:
            access = self.accessArg()
        return List('match', access, List(List(self.label["id"]), self.fields.toParser(rest)), List(List(*self.label["siblings"]), List('back')))

    def prepare(self, seen):
        self.fields.prepare(seen)
        

class VarFarg(Farg):
    """
    variable
    """
    def __init__(self, name):
        self.name = name
        self.prior = False

    def __str__(self):
        return self.name

    def toParser(self, rest):
        if self.prior:
            return List('if', List('eq', self.name, self.accessArg()), rest, List('back'))
        else:
            return List('let', List(self.name, self.accessArg()), rest)

    def prepare(self, seen):
        if self.name in seen:
            self.prior = True
        else:
            seen.add(self.name)


class AssignmentFarg(Farg):
    """
    name = value
    """
    def __init__(self, name, value):
        self.name = name
        self.value = value
        self.prior = False

    def __str__(self):
        return self.name + '=' + str(self.value)

    def notePosition(self, i):
        super().notePosition(i)
        self.value.notePosition(i)

    def prepare(self, seen):
        if self.name in seen:
            self.prior = True
        else:
            seen.add(self.name)

    def toParser(self, rest):
        if self.prior:
            return List('if', List('eq', self.name, self.accessArg()), self.value.toParser(rest), List('back'))
        else:
            return List('let', List(self.name, self.accessArg()), self.value.toParser(rest))


class WildcardFarg(Farg):
    """
    Wildcard _
    """
    def __str__(self):
        return '_'

    def toParser(self, rest):
        return rest


class Compound:
    def __init__(self, name, nargs, *fargss):
        self.name = name
        self.nargs = nargs
        self.fargss = fargss

    def __str__(self):
        return f"fn {self.name} {{\n" + ''.join(['  ' + str(x) + "\n" for x in self.fargss]) + '}'

    def toParser(self):
        return List('define', self.name, List('lambda', self.makeArgs(), foldr(lambda a, b: List('amb', a.toParser(), b), List('error', String(f"patterns exhausted in function {self.name}")), self.fargss)))

    def makeArgs(self):
        return List(*[f'arg{n}' for n in range(self.nargs)])


mapArgs = Compound(
    "map", 2,
    Single(List('make-vec', 0), WildcardFarg(), VecFarg(nil)),
    Single(List('make-vec', 1, List('f', 'h'), List('map', 'f', 't')), VarFarg('f'), VecFarg(cons, VarFarg('h'), VecFarg(cons, VarFarg('t'), VecFarg(nil))))
)
print(str(mapArgs))
print(mapArgs.toParser())

factArgs = Compound(
    "factorial", 1,
    Single('1', NumericFarg(0)),
    Single(List('*', 'n', List('factorial', List('-', 'n', 1))), VarFarg('n'))
)
print(str(factArgs))
print(factArgs.toParser())

memberArgs = Compound(
    "member", 2,
    Single('false', WildcardFarg(), VecFarg(nil)),
    Single('true', VarFarg('x'), VecFarg(cons, VarFarg('x'), WildcardFarg())),
    Single(List('member', 'x', 't'), VarFarg('x'), VecFarg(cons, WildcardFarg(), VarFarg('t')))
)
print(str(memberArgs))
print(memberArgs.toParser())

testArgs = Compound(
    "test", 1,
    Single('a', VecFarg(cons, NumericFarg(1), VecFarg(cons, NumericFarg(1), VecFarg(nil)))),
    Single('b', VecFarg(cons, NumericFarg(1), VecFarg(cons, NumericFarg(2), VecFarg(nil)))),
    Single('c', VecFarg(cons, NumericFarg(2), VecFarg(cons, NumericFarg(1), VecFarg(nil)))),
    Single('d', VecFarg(cons, NumericFarg(2), VecFarg(cons, NumericFarg(2), VecFarg(nil)))),
    Single('e', WildcardFarg()),
)
print(str(testArgs))
print(testArgs.toParser())

assignmentArgs = Compound(
    "test2", 2,
    Single('a', AssignmentFarg('x', VecFarg(cons, CharFarg('a'), VecFarg(nil))), VecFarg(cons, AssignmentFarg('y', VecFarg(cons, NumericFarg(2), VecFarg(nil))), VecFarg(nil)))
)
print(str(assignmentArgs))
print(assignmentArgs.toParser())

colourArgs = Compound(
    "colourToChar", 1,
    Single("'r'", VecFarg(red)),
    Single("'g'", VecFarg(green)),
    Single("'b'", VecFarg(blue))
)
print(str(colourArgs))
print(colourArgs.toParser())

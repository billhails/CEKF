#! /usr/bin/env python3

import functools

def flip(func):
    @functools.wraps(func)
    def newfunc(x, y):
        return func(y, x)
    return newfunc

def foldr(func, acc, xs):
    return functools.reduce(flip(func), reversed(xs), acc)

class Base:
    counter = 0
    def genSym(self):
        Base.counter += 1
        return "t$" + str(Base.counter)

    def normalize_term(self):
        return self.normalize(lambda x: x)

    def normalize_name(self, k):
        return self.normalize(lambda n: n.normalize_helper(n, k))

    def normalize_helper(self, n, k):
        t = self.genSym()
        return Let(t, n, k(t))


class Lambda(Base):
    def __init__(self, args, body):
        self.args = args
        self.body = body

    def normalize(self, k):
        return k(Lambda(self.args, self.body.normalize_term()))

    def __str__(self):
        return "(lambda (" + " ".join([str(x) for x in self.args]) + ") " + str(self.body) + ")"


class Let(Base):
    def __init__(self, var, val, body):
        self.var = var
        self.val = val
        self.body = body

    def normalize(self, k):
        return self.val.normalize(lambda n1: Let(self.var, n1, self.body.normalize(k)))

    def __str__(self):
        return "(let (" + str(self.var) + " " + str(self.val) + ") " + str(self.body) + ")"

class If(Base):
    def __init__(self, test, consequent, alternative):
        self.test = test
        self.consequent = consequent
        self.alternative = alternative

    def normalize(self, k):
        return self.test.normalize_name(lambda t: k(If(t, self.consequent.normalize_term(), self.alternative.normalize_term())))

    def __str__(self):
        return "(if " + str(self.test) + " " + str(self.consequent) + " " + str(self.alternative) + ")"


class Apply(Base):
    def __init__(self, fun, *args):
        self.fun = fun
        self.args = Args.build([x for x in args])

    def normalize(self, k):
        return self.fun.normalize_name(lambda t: self.args.normalize_name(lambda t2: k(Apply(t, t2))))

    def __str__(self):
        return "(" + str(self.fun) + " " + str(self.args) + ")"


class Null(Base):
    def normalize_name(self, k):
        return k(self)

    def __str__(self):
        return ""

class Args(Base):
    def __init__(self, val, rest):
        self.val = val
        self.rest = rest

    def normalize_name(self, k):
        return self.val.normalize_name(lambda t: self.rest.normalize_name(lambda t2: k(Args(t, t2))))

    def __str__(self):
        return str(self.val) + " " + str(self.rest)

    @classmethod
    def build(cls, args):
        return foldr(lambda val, acc: cls(val, acc), Null(), args)

class Value(Base):
    def __init__(self, val):
        self.val = val

    def normalize(self, k):
        return k(self)

    def normalize_helper(self, n, k):
        return k(self)

    def __str__(self):
        return str(self.val)


def test(testexpr):
    print(str(testexpr))
    result = testexpr.normalize_term()
    print(str(result))
    print()

test(Apply(Value("a"), Apply(Value("b"), Value("c"))))

test(Lambda([Value("x"), Value("y")], Apply(Value("+"), Value("x"), Apply(Value("-"), Value("y")))))

test(Lambda([Value("x"), Value("y")], If(Apply(Value("+"), Value("x"), Apply(Value("-"), Value("y"))), Value("x"), Value("y"))))

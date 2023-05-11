#
# CEKF - VM supporting amb
# Copyright (C) 2022-2023  Bill Hails
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <https://www.gnu.org/licenses/>.
#

# parser that outputs C code to build CEKF A-Normal form trees from scheme input

import re
import sys

class AexpBase:
    def is_aexp(self):
        return True


class CexpBase:
    def is_aexp(self):
        return False


class LetExpBase:
    def is_aexp(self):
        return False


class AexpInt(AexpBase):
    def __init__(self, val):
        self.val = val

    def __str__(self):
        return str(self.val)

    def makeC(self):
        return str(self.val)

    def expCType(self):
        return"AEXP_TYPE_INT"

    def expCVal(self):
        return"AEXP_VAL_INT(" + self.makeC() + ")"


class AexpFalse(AexpBase):
    def __str__(self):
        return '#f'

    def expCType(self):
        return "AEXP_TYPE_FALSE"

    def expCVal(self):
        return "AEXP_VAL_FALSE()"


class AexpTrue(AexpBase):
    def __str__(self):
        return '#t'

    def expCType(self):
        return "AEXP_TYPE_TRUE"

    def expCVal(self):
        return "AEXP_VAL_TRUE()"


class AexpLam(AexpBase):
    def __init__(self, args, body):
        self.args = args
        self.body = body

    def __str__(self):
        return "(lambda " + str(self.args) + " " + str(self.body) + ")"

    def makeC(self):
        return "newAexpLam(" + self.args.makeC() + "," + self.body.makeC() + ")"

    def expCType(self):
        return "AEXP_TYPE_LAM"

    def expCVal(self):
        return "AEXP_VAL_LAM(" + self.makeC() + ")"


class AexpVarList:
    def __init__(self, rest, var):
        self.rest = rest
        self.var = var

    def __str__(self):
        return "(" + self.inner_str() + ")"

    def inner_str(self):
        local = str(self.var)
        if self.rest is not None:
            return local + " " + self.rest.inner_str()
        return local

    def makeC(self):
        rest = "NULL"
        if self.rest is not None:
            rest = self.rest.makeC()
        return "newAexpVarList(" + rest + "," + self.var.makeC() + ")"


class AexpVar(AexpBase):
    def __init__(self, name):
        self.name = name

    def __str__(self):
        return self.name

    def makeC(self):
        return "newAexpVar(\"" + self.name + "\")"

    def expCType(self):
        return "AEXP_TYPE_VAR"

    def expCVal(self):
        return "AEXP_VAL_VAR(" + self.makeC() + ")"


class AexpPrimApp(AexpBase):
    def __init__(self, op, lhs, rhs):
        lhs.assert_aexp("primap " + op + " expects lhs aexp")
        rhs.assert_aexp("primap " + op + " expects rhs aexp")
        self.op = op
        self.lhs = lhs
        self.rhs = rhs

    def __str__(self):
        return "(" + str(self.op) + " " + str(self.lhs) + " " + str(self.rhs) + ")"

    def expCType(self):
        return "AEXP_TYPE_PRIM"

    def expCVal(self):
        return "AEXP_VAL_PRIM(" + self.makeC() + ")"

    def makeCOp(self):
        match self.op:
            case '+':
                return "AEXP_PRIM_ADD"
            case '-':
                return "AEXP_PRIM_SUB"
            case '*':
                return "AEXP_PRIM_MUL"
            case '/':
                return "AEXP_PRIM_DIV"
            case '==':
                return "AEXP_PRIM_EQ"
            case '<':
                return "AEXP_PRIM_LT"
            case '>':
                return "AEXP_PRIM_GT"
            case '<=':
                return "AEXP_PRIM_LE"
            case '>=':
                return "AEXP_PRIM_GE"
            case '!=':
                return "AEXP_PRIM_NE"

    def makeC(self):
        op = self.makeCOp()
        return "newAexpPrimApp(" + op + "," + self.lhs.makeC() + "," + self.rhs.makeC() + ")"


class AexpList:
    def __init__(self, rest, exp):
        exp.assert_aexp("aexp list expects aexps")
        self.rest = rest
        self.exp = exp

    def __str__(self):
        return "(" + self.inner_str() + ")"

    def inner_str(self):
        local = str(self.exp)
        if self.rest is not None:
            return local + " " + self.rest.inner_str()
        return local

    def makeC(self):
        rest = "NULL"
        if self.rest is not None:
            rest = self.rest.makeC()
        return "newAexpList(" + rest + "," + self.exp.makeC() + ")"


class CexpApply(CexpBase):
    def __init__(self, function, args):
        function.assert_aexp("function being applied must be an aexp")
        self.function = function
        self.args = args

    def __str__(self):
        return "(" + str(self.function) + " " + self.args.inner_str() + ")"

    def makeC(self):
        return "newCexpApply(" + self.function.makeC() + "," + self.args.makeC() + ")"

    def expCType(self):
        return "CEXP_TYPE_APPLY"

    def expCVal(self):
        return "CEXP_VAL_APPLY(" + self.makeC() + ")"


class CexpCond(CexpBase):
    def __init__(self, condition, consequent, alternative):
        condition.assert_aexp("condition")
        self.condition = condition
        self.consequent = consequent
        self.alternative = alternative

    def __str__(self):
        return "(if " + str(self.condition) + " " + str(self.consequent) + " " + str(self.alternative) + ")"

    def makeC(self):
        return "newCexpCond(" + self.condition.makeC() + "," + self.consequent.makeC() + "," + self.alternative.makeC() + ")"

    def expCType(self):
        return "CEXP_TYPE_COND"

    def expCVal(self):
        return "CEXP_VAL_COND(" + self.makeC() + ")"


class CexpLetRec(CexpBase):
    def __init__(self, bindings, body):
        self.bindings = bindings
        self.body = body

    def __str__(self):
        return "(letrec " + str(self.bindings) + " " + str(self.body) + ")"

    def makeC(self):
        return "newCexpLetRec(" + self.bindings.makeC() + "," + self.body.makeC() + ")"

    def expCType(self):
        return "CEXP_TYPE_LETREC"

    def expCVal(self):
        return "CEXP_VAL_LETREC(" + self.makeC() + ")"


class LetRecBindings:
    def __init__(self, rest, var, val):
        val.assert_aexp("letrec values must be aexp")
        self.rest = rest
        self.var = var
        self.val = val

    def __str__(self):
        return "(" + self.inner_str() + ")"

    def inner_str(self):
        local = "(" + str(self.var) + " " + str(self.val) + ")"
        if self.rest is not None:
            return local + " " + self.rest.inner_str()
        return local

    def makeC(self):
        rest = "NULL";
        if self.rest is not None:
            rest = self.rest.makeC()
        return "newLetRecBindings(" + rest + "," + self.var.makeC() + "," + self.val.makeC() + ")"


class CexpAmb(CexpBase):
    def __init__(self, exp1, exp2):
        self.exp1 = exp1
        self.exp2 = exp2

    def __str__(self):
        return "(amb " + str(self.exp1) + " " + str(self.exp2) + ")"

    def makeC(self):
        return "newCexpAmb(" + self.exp1.makeC() + "," + self.exp2.makeC() + ")"

    def expCType(self):
        return "CEXP_TYPE_AMB"

    def expCVal(self):
        return "CEXP_VAL_AMB(" + self.makeC() + ")"


class ExpLet(LetExpBase):
    def __init__(self, var, val, body):
        self.var = var
        self.val = val
        self.body = body

    def __str__(self):
        return "(let (" + str(self.var) + " " + str(self.val) + ") " + str(self.body) + ")"

    def makeC(self):
        return "newExpLet(" + self.var.makeC() + "," + self.val.makeC() + "," + self.body.makeC() + ")"

    def expCType(self):
        return "EXP_TYPE_LET"

    def expCVal(self):
        return "EXP_VAL_LET(" + self.makeC() + ")"


class Aexp(AexpBase):
    def __init__(self, aexp):
        self.aexp = aexp;

    def is_aexp(self):
        return self.aexp.is_aexp()

    def __str__(self):
        return str(self.aexp);

    def makeC(self):
        return "newAexp(" + self.aexp.expCType() + "," + self.aexp.expCVal() + ")"

    def expCType(self):
        return "EXP_TYPE_AEXP"

    def expCVal(self):
        return "EXP_VAL_AEXP(" + self.makeC() + ")"

    def assert_aexp(self, context):
        if not self.aexp.is_aexp():
            raise Exception(context + " expects an aexp not a cexp")


class Cexp(CexpBase):
    def __init__(self, cexp):
        self.cexp = cexp;

    def is_aexp(self):
        return False

    def __str__(self):
        return str(self.cexp);

    def makeC(self):
        return "newCexp(" + self.cexp.expCType() + "," + self.cexp.expCVal() + ")"

    def expCType(self):
        return "EXP_TYPE_CEXP"

    def expCVal(self):
        return "EXP_VAL_CEXP(" + self.makeC() + ")"


class Exp:
    def __init__(self, exp):
        self.exp = exp

    def __str__(self):
        return "EXP[" + str(self.exp) + "]"

    def makeC(self):
        return "newExp(" + self.exp.expCType() + "," + self.exp.expCVal() + ")"

    def assert_aexp(self, context):
        if not self.exp.is_aexp():
            raise Exception(context + " expects an aexp not a cexp")


class CexpBack(CexpBase):
    def __str__(self):
        return "back"

    def expCType(self):
        return "CEXP_TYPE_BACK"

    def expCVal(self):
        return "CEXP_VAL_BACK()"


class CexpCallCC(CexpBase):
    def __init__(self, exp):
        exp.assert_aexp("call/cc expects aexp");
        self.exp = exp

    def __str__(self):
        return "(call/cc " + str(self.exp) + ")"

    def makeC(self):
        return "newExp(CEXP_TYPE_CALLCC, CEXP_VAL_CALLCC(" + self.exp.makeC() + "))"

    def expCType(self):
        return "CEXP_TYPE_CALLCC"

    def expCVal(self):
        return "CEXP_VAL_CALLCC(" + self.exp.makeC() + ")"

############## PARSER

class Token:
    OPEN = 0
    CLOSE = 1
    INTEGER = 2
    VAR = 3
    PRIM = 4
    AMB = 5
    BACK = 6
    LETREC = 7
    LAMBDA = 8
    COND = 9
    LET = 10
    CALLCC = 11
    TRUE = 12
    FALSE = 13

    def __init__(self, kind, val):
        self.kind = kind
        self.val = val

    def __str__(self):
        return 'Token<' + self.val + '>'


class Lexer:
    def __init__(self, file):
        self.stream = self.lexer(file)
        self.buffer = []

    def next(self):
        if len(self.buffer) > 0:
            token = self.buffer.pop()
            return token
        else:
            token = next(self.stream)
            return token

    def peek(self):
        if len(self.buffer) == 0:
            self.buffer.append(next(self.stream))
        return self.buffer[-1]

    def pushback(self, token):
        self.buffer.append(token)

    def lexer(self, file):
        with open(file) as fh:
            for line in fh.readlines():
                line = line.strip()
                while line:
                    if line[0] == '(':
                        line = line[1:]
                        line = line.strip()
                        yield Token(Token.OPEN, '(')
                    elif line[0] == ')':
                        line = line[1:]
                        line = line.strip()
                        yield Token(Token.CLOSE, ')')
                    else:
                        x = re.search("^[0-9]+", line)
                        if x:
                            line = re.sub("^[0-9]+", "", line)
                            line = line.strip()
                            yield Token(Token.INTEGER, x.group())
                        else:
                            x = re.search("^[^\s()]+", line)
                            if x:
                                line = re.sub("^[^\s()]+", "", line)
                                line = line.strip()
                                res = x.group()
                                match res:
                                    case 'amb':
                                        yield Token(Token.AMB, res)
                                    case 'back':
                                        yield Token(Token.BACK, res)
                                    case 'letrec':
                                        yield Token(Token.LETREC, res)
                                    case 'lambda':
                                        yield Token(Token.LAMBDA, res)
                                    case 'if':
                                        yield Token(Token.COND, res)
                                    case 'let':
                                        yield Token(Token.LET, res)
                                    case 'call/cc':
                                        yield Token(Token.CALLCC, res)
                                    case '+':
                                        yield Token(Token.PRIM, res)
                                    case '-':
                                        yield Token(Token.PRIM, res)
                                    case '*':
                                        yield Token(Token.PRIM, res)
                                    case '/':
                                        yield Token(Token.PRIM, res)
                                    case '==':
                                        yield Token(Token.PRIM, res)
                                    case '<':
                                        yield Token(Token.PRIM, res)
                                    case '>':
                                        yield Token(Token.PRIM, res)
                                    case '<=':
                                        yield Token(Token.PRIM, res)
                                    case '>=':
                                        yield Token(Token.PRIM, res)
                                    case '!=':
                                        yield Token(Token.PRIM, res)
                                    case '#t':
                                        yield Token(Token.TRUE, res)
                                    case '#f':
                                        yield Token(Token.FALSE, res)
                                    case _:
                                        yield Token(Token.VAR, res)
                            else:
                                raise Exception("can't parse \"" + line + '"')


def parse_aexp_list(tokens):
    if tokens.peek().kind == Token.PRIM:
        return parse_primapp(tokens.next(), tokens)
    if tokens.peek().kind == Token.LAMBDA:
        tokens.next()
        return parse_lambda(tokens)
    if tokens.peek().val == ')':
        return None
    exp = parse_aexp(tokens)
    return AexpList(parse_aexp_list(tokens), exp)

def parse_varlist_body(tokens):
    if tokens.peek().val == ')':
        tokens.next()
        return None
    var = parse_var(tokens)
    return AexpVarList(parse_varlist_body(tokens), var)

def parse_varlist(tokens):
    if tokens.next().val != '(':
        raise Exception("expected '(' before variable list")
    return parse_varlist_body(tokens)

def parse_var(tokens):
    token = tokens.next()
    if token.kind == Token.VAR:
        return AexpVar(token.val);
    raise Exception("Expected var, got " + token.val)

def parse_letrec_bindings_list(tokens):
    token = tokens.next()
    if token.val == '(':
        var = parse_var(tokens)
        val = parse_aexp(tokens)
        token = tokens.next()
        if token.val != ')':
            raise Exception("expected ')' after letrec binding");
        return LetRecBindings(parse_letrec_bindings_list(tokens), var, val)
    elif token.val == ')':
        return None
    else:
        raise Exception("unexpected token " + token.val + " in letrec bindings")

def parse_letrec_bindings(tokens):
    if tokens.next().val != '(':
        raise Exception("expected '(' after 'letrec'")
    return parse_letrec_bindings_list(tokens)

def parse_amb(tokens):
    exp1 = parse_exp(tokens)
    exp2 = parse_exp(tokens)
    return Cexp(CexpAmb(exp1, exp2))

def parse_back(tokens):
    return Cexp(CexpBack())

def parse_letrec(tokens):
    bindings = parse_letrec_bindings(tokens)
    body = parse_exp(tokens)
    return Cexp(CexpLetRec(bindings, body))

def parse_lambda(tokens):
    args = parse_varlist(tokens)
    body = parse_exp(tokens)
    return Aexp(AexpLam(args, body))

def parse_cond(tokens):
    condition = parse_aexp(tokens)
    consequent = parse_exp(tokens)
    alternative = parse_exp(tokens)
    return Cexp(CexpCond(condition, consequent, alternative))

def parse_var(tokens):
    token = tokens.next()
    if token.kind != Token.VAR:
        raise Exception("expected var, got " + token.val);
    return AexpVar(token.val)

def parse_let(tokens):
    token = tokens.next()
    if token.val != '(':
        raise Exception("expected '(' after 'let', got " + token.val)
    var = parse_var(tokens)
    val = parse_exp(tokens)
    token = tokens.next()
    if token.val != ')':
        raise Exception("expected ')' after 'let' bindings, got " + token.val)
    body = parse_exp(tokens)
    return ExpLet(var, val, body)

def parse_callcc(tokens):
    exp = parse_aexp(tokens)
    return Cexp(CexpCallCC(exp))

def parse_primapp(token, tokens):
    exp1 = parse_aexp(tokens)
    exp2 = parse_aexp(tokens)
    return Aexp(AexpPrimApp(token.val, exp1, exp2))

def parse_apply(tokens):
    function = parse_aexp(tokens)
    args = parse_aexp_list(tokens)
    return Cexp(CexpApply(function, args))

def parse_list(tokens):
    token = tokens.next()
    match token.kind:
        case Token.AMB:
            return parse_amb(tokens)
        case Token.BACK:
            return parse_back(tokens)
        case Token.LETREC:
            return parse_letrec(tokens)
        case Token.LAMBDA:
            return parse_lambda(tokens)
        case Token.COND:
            return parse_cond(tokens)
        case Token.LET:
            return parse_let(tokens)
        case Token.CALLCC:
            return parse_callcc(tokens)
        case Token.PRIM:
            return parse_primapp(token, tokens)
        case Token.INTEGER:
            return Aexp(AexpInt(token))
        case Token.TRUE:
            return Aexp(AexpTrue())
        case Token.FALSE:
            return Aexp(AexpFalse())
        case Token.VAR:
            tokens.pushback(token)
            return parse_apply(tokens)
        case Token.CLOSE:
            raise Exception("unexpected closing brace in expression");

def parse_aexp(tokens):
    token = tokens.next()
    if token.val == '(':
        exp = parse_aexp_list(tokens)
        token = tokens.next()
        if token.val != ')':
            raise Exception("expected ')' after expression")
        return exp
    elif token.val == ')':
        raise Exception("unexpected closing brace");
    else:
        match token.kind:
            case Token.INTEGER:
                return Aexp(AexpInt(token.val))
            case Token.TRUE:
                return Aexp(AexpTrue())
            case Token.FALSE:
                return Aexp(AexpFalse())
            case Token.VAR:
                return Aexp(AexpVar(token.val))
            case Token.LAMBDA:
                return parse_lambda(tokens)
            case _:
                raise Exception("unexpected token while parsing aexp: " + token.val);

def parse_exp(tokens):
    token = tokens.next()
    if token.val == '(':
        exp = parse_list(tokens)
        token = tokens.next()
        if token.val != ')':
            raise Exception("expected ')' after expression")
        return Exp(exp)
    elif token.val == ')':
        raise Exception("unexpected closing brace");
    else:
        match token.kind:
            case Token.INTEGER:
                return Exp(Aexp(AexpInt(token.val)))
            case Token.TRUE:
                return Exp(Aexp(AexpTrue()))
            case Token.FALSE:
                return Exp(Aexp(AexpFalse()))
            case Token.VAR:
                return Exp(Aexp(AexpVar(token.val)))
            case _:
                raise Exception("unexpected token while parsing expression: " + token.val);


expr = parse_exp(Lexer(sys.argv[1]))
print(str(expr))
print(parse_exp(Lexer(sys.argv[1])).makeC())

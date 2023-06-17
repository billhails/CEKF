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


class AexpVoid(AexpBase):
    def __str__(self):
        return 'nil'

    def expCType(self):
        return "AEXP_TYPE_VOID"

    def expCVal(self):
        return "AEXP_VAL_VOID()"


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
            case '=':
                return "AEXP_PRIM_EQ"
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
            case 'cons':
                return "AEXP_PRIM_CONS"
            case 'vec':
                return "AEXP_PRIM_VEC"
            case 'xor':
                return "AEXP_PRIM_XOR"

    def makeC(self):
        op = self.makeCOp()
        return "newAexpPrimApp(" + op + "," + self.lhs.makeC() + "," + self.rhs.makeC() + ")"


class AexpUnaryApp(AexpBase):
    def __init__(self, op, exp):
        exp.assert_aexp("primap " + op + " expects aexp")
        self.op = op
        self.exp = exp

    def __str__(self):
        return "(" + str(self.op) + " " + str(self.exp) + ")"

    def expCType(self):
        return "AEXP_TYPE_UNARY"

    def expCVal(self):
        return "AEXP_VAL_UNARY(" + self.makeC() + ")"

    def makeCOp(self):
        match self.op:
            case 'car':
                return "AEXP_UNARY_CAR"
            case 'cdr':
                return "AEXP_UNARY_CDR"
            case 'not':
                return "AEXP_UNARY_NOT"
            case 'print':
                return "AEXP_UNARY_PRINT"

    def makeC(self):
        return "newAexpUnaryApp(" + self.makeCOp() + "," + self.exp.makeC() + ")"


class AexpList:
    def __init__(self, rest, exp):
        exp.assert_aexp("aexp list expects aexps")
        self.rest = rest
        self.exp = exp

    def assert_aexp(self, context):
        raise Exception("AexpList assert_aexp: " + context + ", got " + str(self))

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


class AexpMakeList(AexpBase):
    def __init__(self, lst):
        self.lst = lst

    def __str__(self):
        if self.lst is None:
            return "(list)"
        else:
            return "(list " + self.lst.inner_str() + ")"

    def expCType(self):
        return "AEXP_TYPE_LIST"

    def expCVal(self):
        if self.lst is None:
            return "AEXP_VAL_LIST(NULL)"
        else:
            return "AEXP_VAL_LIST(" + self.lst.makeC() + ")"

class AexpMakeVec(AexpBase):
    def __init__(self, lst):
        self.lst = lst

    def __str__(self):
        if self.lst is None:
            return "(make-vec)"
        else:
            return "(make-vec " + self.lst.inner_str() + ")"

    def expCType(self):
        return "AEXP_TYPE_MAKEVEC"

    def makeC(self):
        rest = "NULL"
        if self.lst is not None:
            rest = self.lst.makeC()
        return "newAexpMakeVec(" + rest + ")";

    def expCVal(self):
        return "AEXP_VAL_MAKEVEC(" + self.makeC() + ")"

class CexpApply(CexpBase):
    def __init__(self, function, args):
        function.assert_aexp("function being applied must be an aexp")
        self.function = function
        self.args = args

    def __str__(self):
        print("CexpApply(" + str(self.function) + ")")
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



class CexpBool(CexpBase):
    def __init__(self, token, exp1, exp2):
        self.name = token.val;
        self.exp1 = exp1
        self.exp2 = exp2

    def __str__(self):
        return "(" +self.name + " " + str(self.exp1) + " " + str(self.exp2) + ")"

    def cexpBoolType(self):
        if self.name == "and":
            return "BOOL_TYPE_AND"
        return "BOOL_TYPE_OR"

    def makeC(self):
        return "newCexpBool(" + self.cexpBoolType() + "," + self.exp1.makeC() + "," + self.exp2.makeC() + ")"

    def expCType(self):
        return "CEXP_TYPE_BOOL"

    def expCVal(self):
        return "CEXP_VAL_BOOL(" + self.makeC() + ")"


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
    VOID = 14
    UNARY = 15
    BOOL = 16
    LIST = 17
    MAKEVEC = 18

    def __init__(self, kind, val, line):
        self.kind = kind
        self.val = val
        self.line = line

    def __str__(self):
        return 'Token<' + self.val + '>(' + str(self.line) + ')'


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
            token = next(self.stream)
            self.buffer.append(token)
        return self.buffer[-1]

    def pushback(self, token):
        self.buffer.append(token)

    def lexer(self, file):
        line_number = 0;
        with open(file) as fh:
            for line in fh.readlines():
                line_number = line_number + 1;
                line = re.sub(";.*", "", line)
                line = line.strip()
                while line:
                    if line[0] == '(':
                        line = line[1:]
                        line = line.strip()
                        yield Token(Token.OPEN, '(', line_number)
                    elif line[0] == ')':
                        line = line[1:]
                        line = line.strip()
                        yield Token(Token.CLOSE, ')', line_number)
                    else:
                        x = re.search("^[0-9]+", line)
                        if x:
                            line = re.sub("^[0-9]+", "", line)
                            line = line.strip()
                            yield Token(Token.INTEGER, x.group(), line_number)
                        else:
                            x = re.search("^[^\s()]+", line)
                            if x:
                                line = re.sub("^[^\s()]+", "", line)
                                line = line.strip()
                                res = x.group()
                                match res:
                                    case 'amb':
                                        yield Token(Token.AMB, res, line_number)
                                    case 'back':
                                        yield Token(Token.BACK, res, line_number)
                                    case 'letrec':
                                        yield Token(Token.LETREC, res, line_number)
                                    case 'lambda':
                                        yield Token(Token.LAMBDA, res, line_number)
                                    case 'if':
                                        yield Token(Token.COND, res, line_number)
                                    case 'let':
                                        yield Token(Token.LET, res, line_number)
                                    case 'call/cc':
                                        yield Token(Token.CALLCC, res, line_number)
                                    case 'make-vec':
                                        yield Token(Token.MAKEVEC, res, line_number)
                                    case '+':
                                        yield Token(Token.PRIM, res, line_number)
                                    case '-':
                                        yield Token(Token.PRIM, res, line_number)
                                    case '*':
                                        yield Token(Token.PRIM, res, line_number)
                                    case '/':
                                        yield Token(Token.PRIM, res, line_number)
                                    case '==':
                                        yield Token(Token.PRIM, res, line_number)
                                    case '<':
                                        yield Token(Token.PRIM, res, line_number)
                                    case '>':
                                        yield Token(Token.PRIM, res, line_number)
                                    case '<=':
                                        yield Token(Token.PRIM, res, line_number)
                                    case '>=':
                                        yield Token(Token.PRIM, res, line_number)
                                    case '!=':
                                        yield Token(Token.PRIM, res, line_number)
                                    case 'xor':
                                        yield Token(Token.PRIM, res, line_number)
                                    case 'and':
                                        yield Token(Token.BOOL, res, line_number)
                                    case 'or':
                                        yield Token(Token.BOOL, res, line_number)
                                    case 'cons':
                                        yield Token(Token.PRIM, res, line_number)
                                    case 'vec':
                                        yield Token(Token.PRIM, res, line_number)
                                    case 'car':
                                        yield Token(Token.UNARY, res, line_number)
                                    case 'cdr':
                                        yield Token(Token.UNARY, res, line_number)
                                    case 'not':
                                        yield Token(Token.UNARY, res, line_number)
                                    case 'print':
                                        yield Token(Token.UNARY, res, line_number)
                                    case '#t':
                                        yield Token(Token.TRUE, res, line_number)
                                    case '#f':
                                        yield Token(Token.FALSE, res, line_number)
                                    case 'list':
                                        yield Token(Token.LIST, res, line_number)
                                    case 'nil':
                                        yield Token(Token.VOID, res, line_number)
                                    case _:
                                        yield Token(Token.VAR, res, line_number)
                            else:
                                raise Exception("can't parse \"" + line + '"')


def parse_aexp_list_expression(tokens):
    print("parse_aexp_list_expression", str(tokens.peek()))
    if tokens.peek().kind == Token.PRIM:
        return parse_primapp(tokens.next(), tokens)
    if tokens.peek().kind == Token.UNARY:
        return parse_unaryapp(tokens.next(), tokens)
    if tokens.peek().kind == Token.LAMBDA:
        tokens.next()
        return parse_lambda(tokens)
    if tokens.peek().kind == Token.LIST:
        tokens.next()
        return parse_make_list(tokens)
    if tokens.peek().val == ')':
        return None
    raise Exception("unexpected start token " + str(tokens.peek()) + " in parse_aexp_list_expression")

def parse_aexp_list(tokens):
    print("parse_aexp_list", str(tokens.peek()))
    if tokens.peek().val == ')':
        return None
    exp = parse_aexp(tokens)
    return AexpList(parse_aexp_list(tokens), exp)

def parse_varlist_body(tokens):
    print("parse_varlist_body", str(tokens.peek()))
    if tokens.peek().val == ')':
        tokens.next()
        return None
    var = parse_var(tokens)
    return AexpVarList(parse_varlist_body(tokens), var)

def parse_varlist(tokens):
    print("parse_varlist", str(tokens.peek()))
    if tokens.next().val != '(':
        raise Exception("expected '(' before variable list")
    return parse_varlist_body(tokens)

def parse_var(tokens):
    print("parse_var", str(tokens.peek()))
    token = tokens.next()
    if token.kind == Token.VAR:
        return AexpVar(token.val);
    raise Exception("Expected var, got " + token.val)

def parse_letrec_bindings_list(tokens):
    print("parse_letrec_bindings_list", str(tokens.peek()))
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
        raise Exception("unexpected token " + token.val + " in parse_letrec_bindings_list")

def parse_letrec_bindings(tokens):
    print("parse_letrec_bindings", str(tokens.peek()))
    if tokens.next().val != '(':
        raise Exception("expected '(' after 'letrec'")
    return parse_letrec_bindings_list(tokens)

def parse_bool(token, tokens):
    print("parse_letrec_bindings", token)
    exp1 = parse_exp(tokens)
    exp2 = parse_exp(tokens)
    return Cexp(CexpBool(token, exp1, exp2))

def parse_amb(tokens):
    print("parse_amb", str(tokens.peek()))
    exp1 = parse_exp(tokens)
    exp2 = parse_exp(tokens)
    return Cexp(CexpAmb(exp1, exp2))

def parse_back(tokens):
    print("parse_back", str(tokens.peek()))
    return Cexp(CexpBack())

def parse_letrec(tokens):
    print("parse_letrec", str(tokens.peek()))
    bindings = parse_letrec_bindings(tokens)
    body = parse_exp(tokens)
    return Cexp(CexpLetRec(bindings, body))

def parse_lambda(tokens):
    print("parse_lambda", str(tokens.peek()))
    args = parse_varlist(tokens)
    body = parse_exp(tokens)
    return Aexp(AexpLam(args, body))

def parse_cond(tokens):
    print("parse_cond", str(tokens.peek()))
    condition = parse_aexp(tokens)
    consequent = parse_exp(tokens)
    alternative = parse_exp(tokens)
    return Cexp(CexpCond(condition, consequent, alternative))

def parse_var(tokens):
    print("parse_var", str(tokens.peek()))
    token = tokens.next()
    if token.kind != Token.VAR:
        raise Exception("expected var, got " + token.val);
    return AexpVar(token.val)

def parse_let(tokens):
    print("parse_let", str(tokens.peek()))
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
    print("parse_callcc", str(tokens.peek()))
    exp = parse_aexp(tokens)
    return Cexp(CexpCallCC(exp))

def parse_makevec(tokens):
    print("parse_makevec", str(tokens.peek()))
    args = parse_aexp_list(tokens)
    return Aexp(AexpMakeVec(args))

def parse_primapp(token, tokens):
    print("parse_primapp", str(tokens.peek()))
    exp1 = parse_aexp(tokens)
    exp2 = parse_aexp(tokens)
    return Aexp(AexpPrimApp(token.val, exp1, exp2))

def parse_unaryapp(token, tokens):
    print("parse_unaryapp", str(token))
    exp = parse_aexp(tokens)
    return Aexp(AexpUnaryApp(token.val, exp))

def parse_apply(tokens):
    print("parse_apply", str(tokens.peek()))
    function = parse_aexp(tokens)
    args = parse_aexp_list(tokens)
    return Cexp(CexpApply(function, args))

def parse_make_list(tokens):
    print("parse_apply", str(tokens.peek()))
    lst = parse_aexp_list(tokens)
    return Aexp(AexpMakeList(lst))

def parse_list(tokens):
    print("parse_list", str(tokens.peek()))
    token = tokens.next()
    match token.kind:
        case Token.BOOL:
            return parse_bool(token, tokens)
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
        case Token.MAKEVEC:
            return parse_makevec(tokens)
        case Token.PRIM:
            return parse_primapp(token, tokens)
        case Token.UNARY:
            return parse_unaryapp(token, tokens)
        case Token.INTEGER:
            return Aexp(AexpInt(token))
        case Token.TRUE:
            return Aexp(AexpTrue())
        case Token.FALSE:
            return Aexp(AexpFalse())
        case Token.VOID:
            return Aexp(AexpVoid())
        case Token.VAR:
            tokens.pushback(token)
            return parse_apply(tokens)
        case Token.LIST:
            return parse_make_list(tokens)
        case Token.CLOSE:
            raise Exception("unexpected closing brace in parse_list");

def parse_aexp(tokens):
    print("parse_aexp", str(tokens.peek()))
    token = tokens.next()
    if token.val == '(':
        exp = parse_aexp_list_expression(tokens)
        token = tokens.next()
        if token.val != ')':
            raise Exception("expected ')' after expression in parse_aexp")
        return exp
    elif token.val == ')':
        raise Exception("unexpected closing brace in parse_aexp");
    else:
        match token.kind:
            case Token.INTEGER:
                return Aexp(AexpInt(token.val))
            case Token.TRUE:
                return Aexp(AexpTrue())
            case Token.FALSE:
                return Aexp(AexpFalse())
            case Token.VOID:
                return Aexp(AexpVoid())
            case Token.VAR:
                return Aexp(AexpVar(token.val))
            case Token.LAMBDA:
                return parse_lambda(tokens)
            case Token.LIST:
                return parse_make_list(tokens)
            case _:
                raise Exception("unexpected token in parse_aexp: " + token.val);

def parse_exp(tokens):
    print("parse_exp", str(tokens.peek()))
    token = tokens.next()
    if token.val == '(':
        exp = parse_list(tokens)
        token = tokens.next()
        if token.val != ')':
            raise Exception("expected ')' after expression in parse_exp")
        return Exp(exp)
    elif token.val == ')':
        raise Exception("unexpected closing brace in parse_exp");
    else:
        match token.kind:
            case Token.INTEGER:
                return Exp(Aexp(AexpInt(token.val)))
            case Token.TRUE:
                return Exp(Aexp(AexpTrue()))
            case Token.FALSE:
                return Exp(Aexp(AexpFalse()))
            case Token.VOID:
                return Exp(Aexp(AexpVoid()))
            case Token.VAR:
                return Exp(Aexp(AexpVar(token.val)))
            case _:
                raise Exception("unexpected token in parse_exp: " + token.val);


expr = parse_exp(Lexer(sys.argv[1]))
print(str(expr))
print(parse_exp(Lexer(sys.argv[1])).makeC())

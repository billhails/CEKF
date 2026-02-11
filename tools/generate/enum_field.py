#
# CEKF - VM supporting amb
# Copyright (C) 2022-2025  Bill Hails
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

"""
Base classes for all AST entity types
"""

from .utils import pad
from .comment_gen import CommentGen



class EnumField:
    """
    Serves as the class for simple enumeration fields and as
    the base class for discriminated union enum fields
    """
    def __init__(self, owner, name):
        if name is True:
            raise Exception("EnumField passed a boolean")
        self.owner = owner
        self.name = name

    def getName(self):
        return self.name

    def comment(self, method):
        """Generate method comment using class name automatically."""
        return CommentGen.method_comment(self.__class__.__name__, method)

    def isSimpleField(self):
        return False

    def isSelfInitializing(self, catalog):
        return False

    def printEnumTypedefLine(self, count):
        field = self.makeTypeName()
        print(f"    {field}, // {count}")

    def printNameFunctionLine(self):
        c = self.comment('printNameFunctionLine')
        field = self.makeTypeName()
        print(f'        case {field}: return "{field}"; {c}')

    def makeTypeName(self):
        v = self.owner + '_type_' + self.name
        v = v.upper().replace('AST', 'AST_')
        return v

    def printEqCase(self, depth):
        c = self.comment('printEqCase')
        typeName = self.makeTypeName()
        pad(depth)
        print(f'case {typeName}: {c}')
        pad(depth + 1)
        print(f"if (a != b) return false; {c}")
        pad(depth + 1)
        print(f'break; {c}')

    def printPrintCase(self, depth):
        c = self.comment('printPrintCase')
        typeName = self.makeTypeName()
        pad(depth)
        print(f'case {typeName}: {c}')
        pad(depth + 1)
        print(f'pad(depth + 1); {c}')
        pad(depth + 1)
        print(f'eprintf("{typeName}"); {c}')
        pad(depth + 1)
        print(f'break; {c}')

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
Primitive types - built-in types defined in primitives.yaml
"""

from .base import Base
from .utils import pad
from .comment_gen import CommentGen
from .type_helper import TypeHelper


class Primitive(Base):
    """
    Primitive types declared by the yaml and added to the catalog
    """
    def __init__(self, name, body):
        super().__init__(name, body)
        # HASENTRIES
        if "data" in body:
            data = body["data"]
            self.cname = data['cname']
            if 'markFn' in data:
                self.markFn = data['markFn']
            else:
                self.markFn = None
            if 'newFn' in data:
                self.newFn = data['newFn']
            else:
                self.newFn = None
            if 'printf' in data:
                self.printFn = 'printf'
                self.printf = data['printf']
            else:
                self.printFn = data['printFn']
            self.valued = data['valued']
            if 'compareFn' in data:
                self.compareFn = data['compareFn']
            else:
                self.compareFn = None
            if 'copyFn' in data:
                self.copyFn = data['copyFn']
            else:
                self.copyFn = None
        else:
            raise ValueError(f"Primitive {name} must have 'data' field")

    def printMermaid(self, catalog):
        pass

    def isInline(self, catalog):
        return True

    def needsProtection(self, catalog):
        """
        Primitives don't need protection if they are valued (pass-by-value types).
        HashSymbol is a special case - it's a pointer but doesn't need protection.
        """
        return False  # Primitives are never GC-allocated

    def printMarkCase(self, isInline, catalog):
        c = self.comment('printMarkCase')
        if self.markFn is not None:
            typeName = self.makeTypeName()
            print(f"        case {typeName}: {c}")
            self.printMarkField(isInline, self.name, 3, 'val.')
            print("            break; {c}")

    def hasMarkFn(self):
        return self.markFn is not None

    def printMarkHashField(self, depth):
        c = self.comment('printMarkHashField')
        if self.markFn is not None:
            pad(depth)
            print(f'{self.markFn}(*({self.cname}*)ptr); {c}')

    # Primitive
    def printMarkField(self, isInline, field, depth, prefix=''):
        if self.markFn is not None:
            c = self.comment('printMarkField')
            markFn=self.markFn
            pad(depth)
            a = '.' if isInline else '->'
            if markFn == 'markHashTable':
                print(f'{markFn}((HashTable *)(_x{a}{prefix}{field})); {c}')
            else:
                print(f"{markFn}(_x{a}{prefix}{field}); {c}")

    def printProtectField(self, isInline, field, depth, prefix=''):
        c = self.comment('printProtectField')
        pad(depth)
        if self.markFn is None:
            print(f"return PROTECT(NULL); {c}")
        else:
            a = '.' if isInline else '->'
            print(f"return PROTECT(_x{a}{prefix}{field}); {c}")

    def getTypeDeclaration(self, catalog):
        return TypeHelper.primitive_type(self.cname)

    def printCompareField(self, catalog, isInline, field, depth, prefix=''):
        c = self.comment('printCompareField')
        pad(depth)
        a = '.' if isInline else '->'
        if self.compareFn is None:
            print(f"if (a{a}{prefix}{field} != b{a}{prefix}{field}) return false; {c}")
        else:
            print(f"if (!{self.compareFn}(a{a}{prefix}{field}, b{a}{prefix}{field})) return false; {c}")

    def printPrintHashField(self, depth):
        c = self.comment('printPrintHashField')
        pad(depth)
        print(f'eprintf("%*s", depth * PAD_WIDTH, ""); {c}')
        pad(depth)
        if self.printFn == 'printf':
            print(f'eprintf("{self.cname} {self.printf}", *({self.cname} *)ptr); {c}')
        else:
            print(f'{self.printFn}(*({self.cname} *)ptr, depth + 1); {c}')


    def printPrintField(self, isInline, field, depth, prefix=''):
        c = self.comment('printPrintField')
        a = '.' if isInline else '->'
        if self.printFn == 'printf':
            pad(depth)
            print(f'pad(depth + 1); {c}')
            pad(depth)
            print(f'eprintf("{self.cname} {self.printf}", _x{a}{prefix}{field}); {c}')
        else:
            pad(depth)
            print(f'{self.printFn}(_x{a}{prefix}{field}, depth + 1); {c}')

    def printCopyField(self, isInline, field, depth, prefix=''):
        c = self.comment('printCopyField')
        pad(depth)
        a = '.' if isInline else '->'
        if self.copyFn is None:
            print(f"_x{a}{prefix}{field} = o{a}{prefix}{field}; {c}")
        elif self.copyFn == 'copyHashTable':
            print(f'_x{a}{prefix}{field} = {self.newFn}(); {c}')
            print(f'copyHashTable((HashTable *)_x{a}{prefix}{field}, (HashTable *)o{a}{prefix}{field}); {c}')
        else:
            print(f"_x{a}{prefix}{field} = {self.copyFn}(o{a}{prefix}{field}); {c}")

    def getDefineValue(self):
        return '_x' if self.valued else 'NULL'

    def getDefineArg(self):
        return '_x' if self.valued else ''

    def isSelfInitializing(self):
        return self.newFn is not None
    
    def getConstructorName(self):
        if self.newFn is None:
            raise ValueError(f"Primitive {self.name} has no known constructor")
        return self.newFn
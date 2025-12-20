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
Enum structures
"""

from .base import Base
from .enum_field import EnumField
from .utils import pad
from .comment_gen import CommentGen
from .type_helper import TypeHelper


class SimpleEnum(Base):
    """
    Contains enums declared directly by the yaml
    """
    def __init__(self, name, body):
        super().__init__(name, body)
        # HASENTRIES
        if "data" in body:
            data = body["data"]
            self.fields = [EnumField(name, _x) for _x in data]
        else:
            raise ValueError(f"SimpleEnum {name} must have 'data' field")

    def getTypeDeclaration(self, catalog):
        return "enum {name} ".format(name=self.getName())

    def printMermaid(self, catalog):
        print(f'{self.getName()}["enum {self.getName()}"]')

    def getDefineValue(self):
        return '_x'

    def getDefineArg(self):
        return '_x'

    def printTypedef(self, catalog):
        c = self.comment('printTypedef')
        self.noteTypedef()
        name = self.getName()
        self.printBaseDocumentation()
        print(f"typedef enum {name} {{ {c}")
        count = 0
        for  field in self.fields:
            field.printEnumTypedefLine(count)
            count += 1
        print(f"}} {name}; {c}\n")

    def isEnum(self):
        return True

    def printCompareField(self, catalog, isInline, field, depth, prefix=''):
        pad(depth)
        c = self.comment('printCompareField')
        a = '.' if isInline else '->'
        print(f"switch (a{a}{prefix}{field}) {{ {c}")
        for field in self.fields:
            field.printCompareCase(depth + 1)
        pad(depth)
        print(f'}} {c}')

    def printPrintHashField(self, depth):
        c = self.comment('printPrintHashField')
        myName = self.getName()
        pad(depth)
        print(f'{myName} *_{myName} = *({myName} **)ptr; {c}')
        pad(depth)
        print(f'switch (_{myName}->type) {{ {c}')
        for field in self.fields:
            field.printPrintCase(depth + 1)
        pad(depth)
        print(f'}} {c}')

    def printPrintField(self, isInline, field, depth, prefix=''):
        pad(depth)
        c = self.comment('printPrintField')
        a = '.' if isInline else '->'
        print(f'switch (_x{a}{prefix}{field}) {{ {c}')
        for field in self.fields:
            field.printPrintCase(depth + 1)
        pad(depth)
        print(f'}} {c}')

    def printCopyField(self, isInline, field, depth, prefix=''):
        c = self.comment('printCopyField')
        pad(depth)
        a = '.' if isInline else '->'
        print(f'_x{a}{field} = o{a}{field}; {c}')

    def getNameFunctionDeclaration(self):
        name = self.getName()
        camel = name[0].lower() + name[1:]
        return f"char * {camel}Name(enum {name} type)"

    def printNameFunctionDeclaration(self):
        c = self.comment('printNameFunctionDeclaration')
        decl = self.getNameFunctionDeclaration()
        print(f"{decl}; {c}")

    def printNameFunctionBody(self):
        decl = self.getNameFunctionDeclaration()
        c = self.comment('printNameFunctionDeclaration')
        print(f"/**")
        print(f" * Returns the name of the enum value as a string.")
        print(f" * This is used for debugging and error messages.")
        print(f" */")
        print(f"{decl} {{ {c}")
        print(f"    switch(type) {{ {c}")
        for  field in self.fields:
            field.printNameFunctionLine()
        print(f"        default: {{ {c}")
        print(f"            static char buf[64]; {c}")
        print(f'            sprintf(buf, "%d", type); {c}')
        print(f"            return buf; {c}")
        print(f"        }} {c}")
        print(f"    }} {c}")
        print(f"}} {c}")
        print("")



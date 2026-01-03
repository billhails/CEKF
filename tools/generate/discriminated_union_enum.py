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



class DiscriminatedUnionEnum(Base):
    """
    Built and added to the catalog by DiscriminatedUnion.build()
    contains DiscriminatedUnionField objects
    """
    def __init__(self, name, fields, body):
        super().__init__(name, body)
        self.fields = fields

    def getName(self):
        return self.name + "Type"

    def printMermaid(self, catalog):
        pass

    def getFieldName(self):
        return 'type'

    def getNameFunctionDeclaration(self):
        name = self.getName()
        camel = name[0].lower() + name[1:]
        return f"char * {camel}Name(enum {name} type)"

    def printNameFunctionDeclaration(self):
        c = self.comment('printNameFunctionDeclaration')
        decl = self.getNameFunctionDeclaration()
        print(f"/**")
        print(f" * Returns the name of the discriminating enum value as a string.")
        print(f" * This is used for debugging and error messages.")
        print(f" */")
        print(f"{decl}; {c}")

    def printNameFunctionBody(self):
        decl = self.getNameFunctionDeclaration()
        c = self.comment('printNameFunctionBody')
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

    def getTypeDeclaration(self, catalog):
        return TypeHelper.enum_type(self.getName())

    def printTypedef(self, catalog):
        c = self.comment('printTypedef')
        self.noteTypedef()
        name=self.getName()
        self.printBaseDocumentation()
        print(f"typedef enum {name} {{ {c}")
        count = 0
        for  field in self.fields:
            field.printEnumTypedefLine(count)
            count += 1
        print(f"}} {name}; {c}\n")

    def getSignature(self, catalog):
        return "{type} type".format(type=self.getTypeDeclaration(catalog))

    def isEnum(self):
        return True

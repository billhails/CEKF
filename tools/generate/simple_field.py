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

"""Field classes for struct and union members."""

import re
from .enum_field import EnumField
from .comment_gen import CommentGen


class SimpleField:
    """
    Represents a field in a SimpleStruct object
    """
    def __init__(self, owner, name, typeName):
        self.owner = owner
        parts = re.split(r"\s*=\s*", typeName, 1)
        self.name = name
        if len(parts) == 2:
            self.typeName = parts[0]
            self.default = parts[1]
        else:
            self.typeName = typeName
            self.default = None

    def isSimpleField(self):
        return True

    def getName(self):
        return self.name

    def comment(self, method):
        """Generate method comment using class name automatically."""
        from .comment_gen import CommentGen
        return CommentGen.method_comment(self.__class__.__name__, method)

    def getObj(self, catalog):
        return catalog.get(self.typeName)

    def getObjName(self, catalog):
        return self.getObj(catalog).getName()

    def isInline(self, catalog):
        obj = catalog.get(self.typeName)
        return obj.isInline(catalog)

    def isSelfInitializing(self, catalog):
        obj = catalog.get(self.typeName)
        return obj.isSelfInitializing()

    def getConstructorName(self, catalog):
        obj = catalog.get(self.typeName)
        return obj.getConstructorName()

    def printPrintDeclaration(self, catalog):
        obj = catalog.get(self.typeName)
        obj.printPrintDeclaration(catalog)

    def getTypeDeclaration(self, catalog):
        obj = catalog.get(self.typeName)
        return obj.getTypeDeclaration(catalog)

    def getSignature(self, catalog):
        return "{type} {name}".format(type=self.getTypeDeclaration(catalog), name=self.name)

    def getArraySignature(self, catalog):
        return "{type} *{name}".format(type=self.getTypeDeclaration(catalog), name=self.name)

    def getVectorSignature(self, catalog):
        return "{type} {name}[0]".format(type=self.getTypeDeclaration(catalog), name=self.name)

    def getFieldName(self):
        return self.name

    def hasMarkFn(self, catalog):
        obj = catalog.get(self.typeName)
        return obj.hasMarkFn()

    def getCopyCall(self, arg, catalog):
        obj = catalog.get(self.typeName)
        return obj.makeCopyCommand(arg, catalog)

    def printMarkLine(self, isInline, catalog, depth):
        obj = catalog.get(self.typeName)
        obj.printMarkField(isInline, self.name, depth)

    def printMarkArrayLine(self, isInline, catalog, key, depth):
        obj = catalog.get(self.typeName)
        obj.printMarkField(isInline, f"{self.name}[{key}]", depth)

    def printMarkHashLine(self, catalog, depth):
        obj = catalog.get(self.typeName)
        obj.printMarkHashField(depth)

    def printPrintHashLine(self, catalog, depth):
        obj = catalog.get(self.typeName)
        obj.printPrintHashField(depth)

    def printCompareLine(self, isInline, catalog, depth):
        obj = catalog.get(self.typeName)
        obj.printCompareField(catalog, isInline, self.name, depth)

    def printPrintLine(self, isInline, catalog, depth):
        obj = catalog.get(self.typeName)
        obj.printPrintField(isInline, self.name, depth)

    def printCopyLine(self, isInline, catalog, depth):
        obj = catalog.get(self.typeName)
        obj.printCopyField(isInline, self.name, depth)

    def printPrintArrayLine(self, isInline, catalog, key, depth):
        obj = catalog.get(self.typeName)
        obj.printPrintField(isInline, f"{self.name}[{key}]", depth)

    def printCopyArrayLine(self, catalog, key, depth):
        obj = catalog.get(self.typeName)
        obj.printCopyField(obj.isInline(catalog), f"{self.name}[{key}]", depth)

    def printCompareArrayLine(self, isInline, catalog, key, depth):
        obj = catalog.get(self.typeName)
        obj.printCompareField(catalog, isInline, f"{self.name}[{key}]", depth)

    def printStructTypedefLine(self, catalog):
        c = self.comment('printStructTypedefLine')
        decl=self.getSignature(catalog)
        print(f"    {decl}; {c}")

    def printArrayTypedefLine(self, catalog):
        c = self.comment('printArrayTypedefLine')
        decl=self.getArraySignature(catalog)
        print(f"    {decl}; {c}")

    def printVectorTypedefLine(self, catalog):
        c = self.comment('printVectorTypedefLine')
        decl=self.getVectorSignature(catalog)
        print(f"    {decl}; {c}")



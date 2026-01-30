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


class Base:
    """
    Base class for top-level entities
    Contains default no-op behavior
    All classes contained directly by the catalog should use this base class
    """
    def __init__(self, name, body):
        self.name = name
        self.tagged = False
        self.bespokeEqImplementation = False
        self.extraEqArgs = {}
        self.hasDocs = False
        self.brief = None
        self.description = None
        self.eqIgnore = []  # List of field names to ignore in equality comparison
        if "parserInfo" in body:
            self.parserInfo = body["parserInfo"]
        else:
            self.parserInfo = None
        if "meta" in body:
            meta = body["meta"]
            if "brief" in meta:
                self.hasDocs = True
                self.brief = meta["brief"]
            if "description" in meta:
                self.hasDocs = True
                self.description = meta["description"]
            if "eqIgnore" in meta:
                self.eqIgnore = meta["eqIgnore"]

    def printBaseDocumentation(self):
        if self.hasDocs:
            print("/**")
            if self.brief is not None:
                print(f" * @brief: {self.brief}")
                if self.description is not None:
                    print(" *")
            for line in self.formatDescription():
                print(f" * {line}")
            print(" */")

    def getParserInfo(self):
        if self.parserInfo is None:
            return False
        return self.parserInfo
    
    def setParserInfo(self, info):
        if self.parserInfo is None:
            self.parserInfo = info

    def setExternal(self, external):
        self.external = external
    
    def isExternal(self):
        return getattr(self, 'external', False)

    def formatDescription(self):
        """
        Format the description for documentation.
        Splits the description into lines of a maximum length of 70 characters.
        """
        if self.description is None:
            return []
        words = self.description.split()
        lines = []
        current_line = ""
        for word in words:
            if len(current_line) + len(word) + 1 > 70:
                lines.append(current_line.strip())
                current_line = word + " "
            else:
                current_line += word + " "
        if current_line:
            lines.append(current_line.strip())
        return lines

    def generateVisitorDecl(self, target):
        """Generate forward declaration for visitor function - default stub"""
        pass

    def generateVisitor(self, catalog, target):
        """Generate visitor function - default stub"""
        pass

    def isInline(self, catalog):
        return False

    def needsProtection(self, catalog):
        """
        Returns True if values of this type need GC protection.
        By default, non-inline (pointer) types need protection.
        Override in subclasses for special cases (e.g., primitives).
        """
        return not self.isInline(catalog)

    def noteTypedef(self):
        with open(".typedefs", "a") as f:
            print(f'-T {self.name}', file=f)

    def isSelfInitializing(self):
        return False

    def noteExtraEqArgs(self, args):
        self.extraEqArgs = args

    def objTypeArray(self):
        return []

    def tag(self):
        self.tagged = True

    def getName(self):
        return self.name

    def comment(self, method):
        """Generate method comment using class name automatically."""
        return CommentGen.method_comment_with_impl(self, method)

    def hasParserInfo(self, catalog):
        return False

    def build(self, catalog):
        pass

    def printTypedef(self, catalog):
        pass

    def printHelperNewDeclarations(self, catalog):
        pass

    def printGetterDeclarations(self, catalog):
        pass

    def printSetterDeclarations(self, catalog):
        pass

    def printNameFunctionDeclaration(self):
        pass

    def printNameFunctionBody(self):
        pass

    def printFreeDeclaration(self, catalog):
        pass

    def printProtectDeclaration(self, catalog):
        pass

    def printProtectFunction(self, catalog):
        pass

    def printMarkDeclaration(self, catalog):
        pass

    def printMarkFunction(self, catalog):
        pass

    def printFreeFunction(self, catalog):
        pass

    def printInitDeclaration(self, catalog):
        pass

    def printInitFunction(self, catalog):
        pass

    def printNewDeclaration(self, catalog):
        pass

    def printCopyDeclaration(self, catalog):
        pass

    def printNewFunction(self, catalog):
        pass

    def printCopyFunction(self, catalog):
        pass

    def printPrintDeclaration(self, catalog):
        pass

    def printEqDeclaration(self, catalog):
        pass

    def printPrintFunction(self, catalog):
        pass

    def printEqFunction(self, catalog):
        pass

    def printMarkObjCase(self, catalog):
        pass

    def printFreeObjCase(self, catalog):
        pass

    def printTypeObjCase(self, catalog):
        pass

    def printDefines(self, catalog):
        pass

    def printAccessDeclarations(self, catalog):
        pass

    def printPushDeclaration(self, catalog):
        pass

    def printPushFunction(self, catalog):
        pass

    def printPopDeclaration(self, catalog):
        pass

    def printPopnDeclaration(self, catalog):
        pass

    def printMoveDeclaration(self, catalog):
        pass

    def printPushnDeclaration(self, catalog):
        pass

    def printCopyTopDeclaration(self, catalog):
        pass

    def printCopyExceptTopDeclaration(self, catalog):
        pass

    def printCopyEntriesDeclaration(self, catalog):
        pass

    def printClearDeclaration(self, catalog):
        pass

    def printPeekDeclaration(self, catalog):
        pass

    def printPeeknDeclaration(self, catalog):
        pass

    def printPokeDeclaration(self, catalog):
        pass

    def printExtendDeclaration(self, catalog):
        pass

    def printAppendDeclaration(self, catalog):
        pass

    def printAddDeclaration(self, catalog):
        pass

    def printSizeDeclaration(self, catalog):
        pass

    def printPopFunction(self, catalog):
        pass

    def printPopnFunction(self, catalog):
        pass

    def printMoveFunction(self, catalog):
        pass

    def printPushnFunction(self, catalog):
        pass

    def printCopyTopFunction(self, catalog):
        pass

    def printCopyExceptTopFunction(self, catalog):
        pass

    def printCopyEntriesFunction(self, catalog):
        pass

    def printPeekFunction(self, catalog):
        pass

    def printPeeknFunction(self, catalog):
        pass

    def printPokeFunction(self, catalog):
        pass

    def printAppendFunction(self, catalog):
        pass

    def printAddFunction(self, catalog):
        pass

    def printExtendFunction(self, catalog):
        pass

    def printCountFunction(self, catalog):
        pass

    def isEnum(self):
        return False

    def isUnion(self):
        return False

    def isStruct(self):
        return False

    def isArray(self):
        return False

    def isHash(self):
        return False

    def isVector(self):
        return False

    def noteBespokeEqImplementation(self):
        self.bespokeEqImplementation = True

    def makeCopyCommand(self, arg, catalog):
        return arg

    def printMarkField(self, isInline, field, depth, prefix=''):
        pass

    def printSetDeclaration(self, catalog):
        pass

    def printGetDeclaration(self, catalog):
        pass

    def printIteratorDeclaration(self, catalog):
        pass

    def printCountDeclaration(self, catalog):
        pass

    def printSetFunction(self, catalog):
        pass

    def printGetFunction(self, catalog):
        pass

    def printIteratorFunction(self, catalog):
        pass

    def hasMarkFn(self):
        return True

    def printMermaid(self, catalog):
        pass



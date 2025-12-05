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
Catalog class - central registry for all generated entities.

The Catalog manages the collection of structures, unions, enums, arrays, etc.
that are defined in a YAML schema file, and orchestrates their code generation.
"""

from .comment_gen import CommentGen
from .switch_helper import SwitchHelper


class Catalog:
    """Central registry managing all entities in a schema."""
    
    def __init__(self, typeName):
        self.typeName = typeName
        self.contents = {}
        self.parserInfo = False

    def add(self, value):
        name = value.getName()
        if name in self.contents:
            raise Exception("attempt to overwtite " + name + " in catalog")
        self.contents[name] = value

    def tag(self, t):
        if t in self.contents:
            self.contents[t].tag()

    def noteExtraCmpArgs(self, args):
        for key in self.contents:
            self.contents[key].noteExtraCmpArgs(args)

    def noteParserInfo(self):
        self.parserInfo = True

    def noteBespokeCmpImplementation(self, name):
        if name in self.contents:
            self.contents[name].noteBespokeCmpImplementation()
        else:
            raise Exception("bespoke cmp implementation declared for nonexistant entry " + name)

    def get(self, key):
        key = key.strip()
        if key in self.contents:
            return self.contents[key]
        else:
            raise Exception("key '" + key + "' not found in catalog")

    def build(self):
        values = []
        for entity in list(self.contents.values()):
            entity.build(self)
    
    def _dispatch(self, method_name, *args):
        """
        Generic dispatcher - calls method_name on all entities.
        
        Args:
            method_name: Name of method to call on each entity
            *args: Arguments to pass to the method (usually self for catalog)
        """
        for entity in self.contents.values():
            method = getattr(entity, method_name)
            method(*args)

    def printHelperNewDeclarations(self):
        self._dispatch('printHelperNewDeclarations', self)

    def printTypedefs(self):
        for entity in self.contents.values():
            if entity.isEnum():
                entity.printTypedef(self)
        for entity in self.contents.values():
            if entity.isArray():
                entity.printTypedef(self)
        for entity in self.contents.values():
            if entity.isUnion():
                entity.printTypedef(self)
        for entity in self.contents.values():
            if entity.isStruct():
                entity.printTypedef(self)
        for entity in self.contents.values():
            if entity.isHash():
                entity.printTypedef(self)
        for entity in self.contents.values():
            if entity.isVector():
                entity.printTypedef(self)

    def printInitDeclarations(self):
        self._dispatch('printInitDeclaration', self)

    def printInitFunctions(self):
        self._dispatch('printInitFunction', self)

    def printMarkDeclarations(self):
        self._dispatch('printMarkDeclaration', self)

    def printCountDeclarations(self):
        self._dispatch('printCountDeclaration', self)

    def printCountFunctions(self):
        self._dispatch('printCountFunction', self)

    def printAccessDeclarations(self):
        self._dispatch('printAccessDeclarations', self)

    def printPushDeclarations(self):
        self._dispatch('printPushDeclaration', self)

    def printPushFunctions(self):
        self._dispatch('printPushFunction', self)

    def printPopDeclarations(self):
        self._dispatch('printPopDeclaration', self)

    def printPopnDeclarations(self):
        self._dispatch('printPopnDeclaration', self)

    def printMoveDeclarations(self):
        self._dispatch('printMoveDeclaration', self)

    def printPushnDeclarations(self):
        self._dispatch('printPushnDeclaration', self)

    def printCopyTopDeclarations(self):
        self._dispatch('printCopyTopDeclaration', self)

    def printCopyExceptTopDeclarations(self):
        self._dispatch('printCopyExceptTopDeclaration', self)

    def printCopyEntriesDeclarations(self):
        self._dispatch('printCopyEntriesDeclaration', self)

    def printClearDeclarations(self):
        self._dispatch('printClearDeclaration', self)

    def printPeekDeclarations(self):
        self._dispatch('printPeekDeclaration', self)

    def printPeeknDeclarations(self):
        self._dispatch('printPeeknDeclaration', self)

    def printPokeDeclarations(self):
        self._dispatch('printPokeDeclaration', self)

    def printExtendDeclarations(self):
        self._dispatch('printExtendDeclaration', self)

    def printSizeDeclarations(self):
        self._dispatch('printSizeDeclaration', self)

    def printPopFunctions(self):
        self._dispatch('printPopFunction', self)

    def printPopnFunctions(self):
        self._dispatch('printPopnFunction', self)

    def printMoveFunctions(self):
        self._dispatch('printMoveFunction', self)

    def printPushnFunctions(self):
        self._dispatch('printPushnFunction', self)

    def printCopyTopFunctions(self):
        self._dispatch('printCopyTopFunction', self)

    def printCopyExceptTopFunctions(self):
        self._dispatch('printCopyExceptTopFunction', self)

    def printCopyEntriesFunctions(self):
        self._dispatch('printCopyEntriesFunction', self)

    def printPeekFunctions(self):
        self._dispatch('printPeekFunction', self)

    def printPeeknFunctions(self):
        self._dispatch('printPeeknFunction', self)

    def printPokeFunctions(self):
        self._dispatch('printPokeFunction', self)

    def printExtendFunctions(self):
        self._dispatch('printExtendFunction', self)

    def printSetDeclarations(self):
        self._dispatch('printSetDeclaration', self)

    def printGetDeclarations(self):
        self._dispatch('printGetDeclaration', self)

    def printSetFunctions(self):
        self._dispatch('printSetFunction', self)

    def printGetFunctions(self):
        self._dispatch('printGetFunction', self)

    def printIteratorDeclarations(self):
        self._dispatch('printIteratorDeclaration', self)

    def printIteratorFunctions(self):
        self._dispatch('printIteratorFunction', self)

    def printFreeDeclarations(self):
        self._dispatch('printFreeDeclaration', self)

    def printProtectDeclarations(self):
        self._dispatch('printProtectDeclaration', self)

    def printProtectFunctions(self):
        self._dispatch('printProtectFunction', self)

    def printNewDeclarations(self):
        self._dispatch('printNewDeclaration', self)

    def printCopyDeclarations(self):
        self._dispatch('printCopyDeclaration', self)

    def printNameFunctionDeclarations(self):
        self._dispatch('printNameFunctionDeclaration')

    def printNameFunctionBodies(self):
        self._dispatch('printNameFunctionBody')

    def printPrintFunctions(self):
        self._dispatch('printPrintFunction', self)

    def printCompareFunctions(self):
        self._dispatch('printCompareFunction', self)

    def printPrintDeclarations(self):
        self._dispatch('printPrintDeclaration', self)

    def printCompareDeclarations(self):
        self._dispatch('printCompareDeclaration', self)

    def printDefines(self):
        self._dispatch('printDefines', self)

    def printNewFunctions(self):
        self._dispatch('printNewFunction', self)

    def printCopyFunctions(self):
        self._dispatch('printCopyFunction', self)

    def printMarkFunctions(self):
        self._dispatch('printMarkFunction', self)

    def printFreeFunctions(self):
        self._dispatch('printFreeFunction', self)

    def printMermaid(self):
        self._dispatch('printMermaid', self)

    def comment(self, method):
        return CommentGen.method_comment('Catalog', method)

    def printMarkObjFunction(self):
        SwitchHelper.print_switch_function(
            self, 'printMarkObjFunction', 'mark{Type}Obj', 'struct Header *h',
            'printMarkObjCase',
            f'cant_happen("unrecognised type %d in mark{self.typeName.capitalize()}Obj\\n", h->type);'
        )

    def printFreeObjFunction(self):
        SwitchHelper.print_switch_function(
            self, 'printFreeObjFunction', 'free{Type}Obj', 'struct Header *h',
            'printFreeObjCase',
            f'cant_happen("unrecognised type %d in free{self.typeName.capitalize()}Obj\\n", h->type);'
        )

    def printTypeObjFunction(self):
        SwitchHelper.print_switch_function(
            self, 'printTypeObjFunction', 'typename{Type}Obj', 'int type',
            'printTypeObjCase',
            'return "???"; // no error, can be used during error reporting',
            'char *'
        )

    def printObjTypeDefine(self):
        objTypeArray = []
        for entity in self.contents.values():
            objTypeArray += entity.objTypeArray()
        print("#define {typeName}_OBJTYPES() \\\n{a}".format(a=', \\\n'.join(objTypeArray), typeName=self.typeName.upper()))

    def printObjCasesDefine(self):
        print(f"#define {self.typeName.upper()}_OBJTYPE_CASES() \\")
        for entity in self.contents.values():
            objType = entity.objTypeArray()
            if len(objType) == 1:
                print(f'case {objType[0]}:\\')
        print("")
